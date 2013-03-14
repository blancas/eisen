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
;; | 'doseq' <name> '<-' expr                                    |
;; | 'in' expr ( ';' expr )* 'end'                               |
;; +-------------------------------------------------------------+

(def doseqex
  "Parses a doseq expression."
  (bind [name (>> (word "doseq") sym-arg)
	 coll (>> (word "<-") expr)
	 expr in-sequence]
    (return {:token :doseq-expr :name name :coll coll :exprs expr})))


(defn trans-doseqex
  "Translates a doseq expression."
  [{:keys [name coll exprs]}]
  (monad [symbol (trans-expr name)
	  _      (modify-st right conj symbol)
	  source (trans-expr coll)
          body   (seqm (map trans-expr exprs))
	  _      (modify-st right difference [symbol])]
    (make-right `(clojure.core/doseq [~symbol ~source] ~@body))))


;; +-------------------------------------------------------------+
;; | 'loop' ( (val decl) | (fun decl) )*                         |
;; | 'in' expr ( ';' expr )* 'end'                               |
;; +-------------------------------------------------------------+

(def loopex
  "Parses a loop expression."
  (bind [_ (word "loop") decls bindings exprs in-sequence]
    (return {:token :loop-expr :decls decls :exprs exprs})))
