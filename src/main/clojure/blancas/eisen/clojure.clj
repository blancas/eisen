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
	[clojure.core.match :only (match)]
	[blancas.kern core i18n]
        [blancas.morph.core :only (monad seqm)]
	[blancas.morph.transf :only (->left ->right get-se modify-se)]
	[blancas.eisen parser trans]))


;; +-------------------------------------------------------------+
;; | 'when' boolean-expr                                         |
;; | 'do' [expr ( ';' expr )*] 'end'                             |
;; +-------------------------------------------------------------+


(def whenex
  "Parses a when expression.

   'when' boolean-expr
   'do' [expr ( ';' expr )*] 'end'"
  (bind [test (>> (word "when") orex)
	 body doex]
    (return {:token :when-expr :test test :body body})))


(defn trans-whenex
  "Translates a when expression."
  [ast]
  (monad [test (trans-expr (:test ast))
	  body (trans-expr (:body ast))]
    (->right `(if ~test ~body))))


;; +-------------------------------------------------------------+
;; | 'while' boolean-expr                                        |
;; | 'do' [expr ( ';' expr )*] 'end'                             |
;; +-------------------------------------------------------------+


(def whileex
  "Parses a while expression.

   'while' boolean-expr
   'do' [expr ( ';' expr )*] 'end'"
  (bind [test (>> (word "while") orex)
	 body doex]
    (return {:token :while-expr :test test :body body})))


(defn trans-whileex
  "Translates a while expression."
  [ast]
  (monad [test (trans-expr (:test ast))
	  body (trans-expr (:body ast))]
    (->right `(clojure.core/while ~test ~body))))


;; +-------------------------------------------------------------+
;; | 'loop' ( (val decl) | (fun decl) )*                         |
;; | 'in' expr ( ';' expr )* 'end'                               |
;; +-------------------------------------------------------------+


(def loopex
  "Parses a loop expression.

   'loop' ( (val decl) | (fun decl) )*
   'in' expr ( ';' expr )* 'end'"
  (bind [_ (word "loop") decls bindings exprs in-sequence]
    (return {:token :loop-expr :decls decls :exprs exprs})))


(defn trans-loopex
  "Translates a loop expression."
  [{:keys [decls exprs]}]
  (let [env (map (comp symbol :name) decls)]
    (monad [_     (modify-se into env)
	    decls (trans-bindings decls)
            exprs (trans-exprs exprs)
	    _     (modify-se difference env)]
      (->right `(loop [~@(apply concat decls)] ~@exprs)))))


;; +-------------------------------------------------------------+
;; | 'whenFirst' <name> '<-' expr                                |
;; | 'do' expr ( ';' expr )* 'end'                               |
;; +-------------------------------------------------------------+


(def whenfex
  "Parses a when-first expression.

   'whenFirst' <name> '<-' expr
   'do' expr ( ';' expr )* 'end'"
  (bind [name (>> (word "whenFirst") sym-arg)
	 coll (>> (word "<-") expr)
	 body doex]
    (return {:token :whenf-expr :name name :coll coll :body body})))


(defn trans-whenfex
  "Translates a when-first expression."
  [{:keys [name coll body]}]
  (monad [symbol (trans-expr name)
	  _      (modify-se conj symbol)
	  source (trans-expr coll)
          body   (trans-expr body)
	  _      (modify-se difference [symbol])]
    (->right `(clojure.core/when-first [~symbol ~source] ~@body))))


;; +-------------------------------------------------------------+
;; | 'cond' expr => expr                                         |
;; | ( ';' expr => expr )* 'end'                                 |
;; +-------------------------------------------------------------+


(def cljcond
  "Parses a cond expression.

   'cond' expr => expr
   ( ';' expr => expr )* 'end'"
  (bind [_    (word "cond")
	 body (sep-end-by semi (<*> expr (>> (word "=>") expr)))
	 _    (word "end")]
    (return {:token :cljcond-expr :test test :body (apply concat body)})))


(defn trans-cljcond
  "Translates a cond expression."
  [ast]
  (monad [body (trans-exprs (:body ast))]
    (->right `(clojure.core/cond ~@body))))


;; +-------------------------------------------------------------+
;; | 'case' expr 'of' expr '=>' expr                             |
;; | ( ';' expr => expr )* 'end'                                 |
;; +-------------------------------------------------------------+


(def caseex
  "Parses a case expression.

   'case' expr 'of' expr '=>' expr
   ( ';' expr => expr )* 'end'"
  (bind [test (between (word "case") (word "of") expr)
	 body (sep-end-by semi (<*> pattern (>> (word "=>") expr)))
	 _    (word "end")]
    (return {:token :case-expr :test test :body (apply concat body)})))


(defn trans-caseex
  "Translates a case expression."
  [ast]
  (monad [test (trans-expr (:test ast))
	  body (trans-exprs (:body ast))]
    (->right `(clojure.core.match/match ~test ~@body))))


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
  "Parses a for expression.

   'for' '[' ( <name> '<-' expr [;] )*
   ( let decl | when expr | while expr )* ']' expr"
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
        (->right [:while expr]))
    :when-pred
      (monad [expr (trans-expr (:expr pred))]
        (->right [:when expr]))
    :let-pred
      (monad [decls (trans-bindings (:decls pred))]
        (->right [:let (vec (apply concat decls))]))))


(defn trans-predicates
  "Translates a collection of predicates."
  [coll]
  (if (seq coll)
    (seqm (map trans-predicate coll))
    (->right [])))


(defn trans-forex
  "Translates a for expression."
  [{:keys [colls preds body]}]
  (let [env (map (comp symbol :name) colls)]
    (monad [_    (modify-se into env)
	    coll (trans-bindings colls) 
	    pred (trans-predicates preds)
            body (trans-expr body)
	    _    (modify-se difference env)]
      (let [decls (concat coll pred)]
        (->right `(clojure.core/for [~@(apply concat decls)] ~body))))))


;; +-------------------------------------------------------------+
;; | 'doseq' '[' ( <name> '<-' expr [;] )*                       |
;; | ( let decl | when expr | while expr )* ']'                  |
;; | expr ( ';' expr )* 'end'                                    |
;; +-------------------------------------------------------------+


(def doseqex
  "Parses a doseq expression.

   'doseq' '[' ( <name> '<-' expr [;] )*
   ( let decl | when expr | while expr )* ']'
   expr ( ';' expr )* 'end'"
  (>> (word "doseq")
      (bind [_     (sym \[)
	     colls (comma-sep1 generator)
             preds (many (<|> let-pred while-pred when-pred))
	     _     (sym \])
             exprs end-sequence]
        (return {:token :doseq-expr :colls colls :preds preds :exprs exprs}))))


(defn trans-doseqex
  "Translates a doseq expression."
  [{:keys [colls preds exprs]}]
  (let [env (map (comp symbol :name) colls)]
    (monad [_    (modify-se into env)
	    coll (trans-bindings colls) 
	    pred (trans-predicates preds)
            body (trans-exprs exprs)
	    _    (modify-se difference env)]
      (let [decls (concat coll pred)]
        (->right `(clojure.core/doseq [~@(apply concat decls)] ~@body))))))


;; +-------------------------------------------------------------+
;; | 'withOpen' (val decl)*                                      |
;; | 'do' expr ( ';' expr )* 'end'                               |
;; +-------------------------------------------------------------+


(def wopenex
  "Parses a with-open expression.

   'withOpen' (val decl)*
   'do' expr ( ';' expr )* 'end'"
  (bind [decls (>> (word "withOpen") bindings)
	 body  doex]
    (return {:token :wopen-expr :decls decls :body body})))


(defn trans-wopenex
  "Translates a with-open expression."
  [{:keys [decls body]}]
  (let [env (map (comp symbol :name) decls)]
    (monad [_     (modify-se into env)
	    decls (trans-bindings decls)
            exprs (trans-expr body)
	    _     (modify-se difference env)]
      (->right `(with-open [~@(apply concat decls)] ~exprs)))))


;; +-------------------------------------------------------------+
;; | 'asString'                                                  |
;; | [expr ( ';' expr )*] 'end'                                  |
;; +-------------------------------------------------------------+


(def strex
  "Parses a with-out-str expression.

   'asString'
   [expr ( ';' expr )*] 'end'"
  (bind [body (>> (word "asString") end-sequence)]
    (return {:token :str-expr :body body})))


(defn trans-strex
  "Translates a with-out-str expression."
  [ast]
  (monad [body (trans-exprs (:body ast))]
    (->right `(clojure.core/with-out-str ~@body))))


;; +-------------------------------------------------------------+
;; | 'withString' expr                                           |
;; | 'do' [expr ( ';' expr )*] 'end'                             |
;; +-------------------------------------------------------------+


(def wstrex
  "Parses a with-in-str expression.

   'withString' expr
   'do' [expr ( ';' expr )*] 'end'"
  (bind [sval (>> (word "withString") expr) body doex]
    (return {:token :wstr-expr :sval sval :body body})))


(defn trans-wstrex
  "Translates a with-in-str expression."
  [{:keys [sval body]}]
  (monad [sval (trans-expr sval)
	  body (trans-expr body)]
    (->right `(clojure.core/with-in-str ~sval ~body))))


;; +-------------------------------------------------------------+
;; | ( 'locking' | 'io' | 'sync' | 'dosync' )                    |
;; | [expr ( ';' expr )*] 'end'                                  |
;; +-------------------------------------------------------------+


(def transex
  "Parses a transaction statement.

   ( 'locking' | 'io' | 'sync' | 'dosync' )
   [expr ( ';' expr )*] 'end'"
  (bind [key (word "locking" "io!" "sync" "dosync") body end-sequence]
    (let [stmt (symbol (str "clojure.core/" key))]
      (return {:token :trans-expr :stmt stmt :body body}))))


(defn trans-transex
  "Translates a transaction statement."
  [{:keys [stmt body]}]
  (monad [body (trans-exprs body)]
    (->right `(~stmt ~@body))))


;; +-------------------------------------------------------------+
;; | 'setq' <name> expr                                          |
;; +-------------------------------------------------------------+


(def setqex
  "Parses a setq statement.

   'setq' <name> expr"
  (bind [name  (>> (word "setq") lisp-id) value expr]
    (return {:token :setq-expr :name name :value value})))


(defn trans-setqex
  "Translates a setq statement."
  [{:keys [name value]}]
  (monad [source (trans-expr value)]
    (->right `(alter-var-root (var ~(symbol name)) (constantly ~source)))))


;; +-------------------------------------------------------------+
;; | 'setv' <host-name> <eisen-name>                             |
;; +-------------------------------------------------------------+


(def setvex
  "Parses a setv statement.

   'setv' <host-name> <eisen-name>"
  (bind [name  (>> (word "setv") (lexeme lisp-id)) id identifier]
    (return {:token :setv-expr :name name :value (:value id)})))


(defn trans-setvex
  "Translates a setv statement."
  [{:keys [name value]}]
  (->right `(alter-var-root (var ~(symbol name))
			    (constantly (var-get (var ~(symbol value)))))))
