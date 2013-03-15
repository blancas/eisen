;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Eisen Expressions for Clojure Core Macros"
      :author "Armando Blancas"}
  blancas.eisen.clojure
  (:use [clojure.set :only (difference)]
	[blancas.kern core i18n]
        [blancas.morph.core :only (monad seqm)]
	[blancas.morph.monads :only (left right either)]
	[blancas.morph.transf :only (state-t get-st modify-st)]
	[blancas.eisen parser trans]))


;; +-------------------------------------------------------------+
;; | 'when' boolean-expr                                         |
;; | 'do' [expr ( ';' expr )*] 'end'                             |
;; +-------------------------------------------------------------+

(def whenex
  "Parses a when expression."
  (bind [test (>> (word "when") orex)
	 body doex]
    (return {:token :when-expr :test test :body body})))


(defn trans-whenex
  "Translates a when expression."
  [ast]
  (monad [test (trans-expr (:test ast))
	  body (trans-expr (:body ast))]
    (make-right `(if ~test ~body))))


;; +-------------------------------------------------------------+
;; | 'while' boolean-expr                                        |
;; | 'do' [expr ( ';' expr )*] 'end'                             |
;; +-------------------------------------------------------------+

(def whileex
  "Parses a while expression."
  (bind [test (>> (word "while") orex)
	 body doex]
    (return {:token :while-expr :test test :body body})))


(defn trans-whileex
  "Translates a while expression."
  [ast]
  (monad [test (trans-expr (:test ast))
	  body (trans-expr (:body ast))]
    (make-right `(clojure.core/while ~test ~body))))


;; +-------------------------------------------------------------+
;; | 'loop' ( (val decl) | (fun decl) )*                         |
;; | 'in' expr ( ';' expr )* 'end'                               |
;; +-------------------------------------------------------------+

(def loopex
  "Parses a loop expression."
  (bind [_ (word "loop") decls bindings exprs in-sequence]
    (return {:token :loop-expr :decls decls :exprs exprs})))


(defn trans-loopex
  "Translates a loop expression."
  [{:keys [decls exprs]}]
  (if (empty? decls)
    (monad [exprs (seqm (map trans-expr exprs))]
      (make-right `(loop [] ~@exprs)))
    (let [env (map (comp symbol :name) decls)]
      (monad [_     (modify-st right into env)
	      decls (seqm (map trans-binding decls))
              exprs (seqm (map trans-expr exprs))
	      _     (modify-st right difference env)]
        (make-right `(loop [~@(apply concat decls)] ~@exprs))))))


;; +-------------------------------------------------------------+
;; | 'whenfirst' <name> '<-' expr                                |
;; | 'in' expr ( ';' expr )* 'end'                               |
;; +-------------------------------------------------------------+

(def whenfirstex
  "Parses a when-first expression."
  (bind [name (>> (word "whenfirst") sym-arg)
	 coll (>> (word "<-") expr)
	 expr in-sequence]
    (return {:token :whenfirst-expr :name name :coll coll :exprs expr})))


(defn trans-whenfirstex
  "Translates a when-first expression."
  [{:keys [name coll exprs]}]
  (monad [symbol (trans-expr name)
	  _      (modify-st right conj symbol)
	  source (trans-expr coll)
          body   (seqm (map trans-expr exprs))
	  _      (modify-st right difference [symbol])]
    (make-right `(clojure.core/when-first [~symbol ~source] ~@body))))


;; +-------------------------------------------------------------+
;; | 'for' '[' ( <name> '<-' expr [;] )*                         |
;; | ( let decl | when expr | while expr )* ']' expr             |
;; +-------------------------------------------------------------+

(def generator
  "Parses a binding to a generator. Returns the same record as
   a val declaration and it's translated with (trans-binding)."
  (bind [name sym-arg _ (word "<-") coll expr]
    (return {:token :val :name (:value name) :value coll})))


(def let-pred
  "Parses a let predicate in a for expression."
  (bind [_ (word "let") decls bindings]
    (return {:token :let-pred :decls decls})))


(def while-pred
  "Parses a while predicate in a for expression."
  (bind [_ (word "while") e expr]
    (return {:token :while-pred :expr e})))


(def when-pred
  "Parses a when predicate in a for expression."
  (bind [_ (word "when") e expr]
    (return {:token :when-pred :expr e})))


(def forex
  "Parses a for expression."
  (>> (word "for")
      (bind [_     (sym \[)
	     colls (comma-sep1 generator)
             preds (many (<|> let-pred while-pred when-pred))
	     _     (sym \])
	     body  expr]
        (return {:token :for-expr :colls colls :preds preds :body body}))))


(defn trans-predicate
  "Translates a list-comprehension predicate."
  [pred]
  (case (:token pred)
    :while-pred
      (monad [expr (trans-expr (:expr pred))]
        (make-right [:while expr]))
    :when-pred
      (monad [expr (trans-expr (:expr pred))]
        (make-right [:when expr]))
    :let-pred
      (monad [decls (seqm (map trans-binding (:decls pred)))]
        (make-right [:let (vec (apply concat decls))]))))


(defn optional-predicates
  "Translates an optional list of predicates."
  [preds]
  (if (seq preds)
    (seqm (map trans-predicate preds))
    (make-right [])))


(defn trans-forex
  "Translates a for expression."
  [{:keys [colls preds body]}]
  (let [env (map (comp symbol :name) colls)]
    (monad [_     (modify-st right into env)
	    coll (seqm (map trans-binding colls)) 
	    pred (optional-predicates preds)
            body (trans-expr body)
	    _    (modify-st right difference env)]
      (let [decls (concat coll pred)]
        (make-right `(clojure.core/for [~@(apply concat decls)] ~body))))))


;; +-------------------------------------------------------------+
;; | 'doseq' '[' ( <name> '<-' expr [;] )*                       |
;; | ( let decl | when expr | while expr )* ']'                  |
;; | 'in' expr ( ';' expr )* 'end'                               |
;; +-------------------------------------------------------------+

(def doseqex
  "Parses a doseq expression."
  (>> (word "doseq")
      (bind [_     (sym \[)
	     colls (comma-sep1 generator)
             preds (many (<|> let-pred while-pred when-pred))
	     _     (sym \])
             body  in-sequence]
        (return {:token :doseq-expr :colls colls :preds preds :body body}))))


(defn trans-doseqex
  "Translates a doseq expression."
  [{:keys [colls preds body]}]
  (let [env (map (comp symbol :name) colls)]
    (monad [_     (modify-st right into env)
	    coll (seqm (map trans-binding colls)) 
	    pred (optional-predicates preds)
            body (seqm (map trans-expr body))
	    _    (modify-st right difference env)]
      (let [decls (concat coll pred)]
        (make-right `(clojure.core/doseq [~@(apply concat decls)] ~@body))))))
