;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "The Eisen Translator."
      :author "Armando Blancas"}
  blancas.eisen.trans
  (:use [blancas.morph.core :only (monad seqm)]
	[blancas.morph.monads :only (left right either)]))


(defn error
  "Composes an error message."
  [pos fmt & more]
  (let [row (:line pos)
	col (:col pos)
	loc (if (empty? (:src pos))
	      (format "line %d, column %d\n" row col)
	      (format "%s: line %d, column %d\n" (:src pos) row col))]
    (str loc (apply format fmt more))))


(defn function?
  "Tests if the symbol is a function or macro."
  [s] (-> s resolve var-get fn?))


(defn exp [x n]
  "Implements the power-of (**) operator."
  (reduce * (repeat n x)))


(defn conj-rev
  "A version of conj with the arguments reversed."
  [x coll] (conj coll x))


(declare trans-expr trans-exprs)


(defn trans-def
  "Translates an AST into a Clojure var definition."
  [ast]
  (let [name (symbol (:name ast))]
    (either [val (trans-expr (:value ast))]
      (right `(def ~name ~val)))))


(defn trans-defn
  "Translates an AST into a Clojure function definition."
  [ast] nil)


(defn val-call
  "Translates a constant or a function call."
  [name args]
  (let [sym-name (-> name :value symbol)]
    (if (zero? (count args))
      (if (function? sym-name)
	(right `(~sym-name))
	(right sym-name))
      (if (function? sym-name)
	(monad [v (seqm (map trans-expr args))]
	  (right (list* sym-name v)))
	(left (error (:pos name) "%s is not a function" (:value name)))))))

  
(defn trans-binop
  "Translates the application of a binary operator."
  [ast]
  (monad [x (trans-expr (:left ast))
	  y (trans-expr (:right ast))]
    (let [f (-> ast :op :value str symbol)]
      (right `(~f ~x ~y)))))


(defn trans-uniop
  "Translates the application of a unary operator."
  [ast]
  (monad [y (trans-expr (:right ast))]
    (let [f (-> ast :op :value str symbol)]
      (right `(~f ~y)))))


(defn trans-expr
  "Translates an AST into a Clojure expression."
  [ast]
  (case (:token ast)
    (:new-line
     :char-lit
     :string-lit
     :dec-lit
     :oct-lit
     :hex-lit
     :float-lit
     :bool-lit
     :nil-lit
     :semi
     :comma
     :colon
     :dot
     :keyword
     :re-lit)
      (right (:value ast))

    :identifier
      (right (-> ast :value symbol))

    :val-call
      (let [val (:value ast)]
        (val-call (first val) (rest val)))

    :list-lit
      (let [vals (trans-exprs (:value ast))]
        (if (:ok vals)
          (right `(list ~@(:decls vals)))
	  (left (:error vals))))

    :vector-lit
      (let [vals (trans-exprs (:value ast))]
        (if (:ok vals)
          (right (:decls vals))
	  (left (:error vals))))

    :set-lit
      (let [vals (trans-exprs (:value ast))]
        (if (:ok vals)
          (right (set (:decls vals)))
	  (left (:error vals))))

    :map-lit
      (let [vals (trans-exprs (:value ast))]
        (if (:ok vals)
          (right (apply hash-map (:decls vals)))
	  (left (:error vals))))

    :BINOP
      (trans-binop ast)

    :UNIOP
      (trans-uniop ast)))


(defn trans-exprs
  "Translates a collection of ASTs into Clojure expressions."
  [coll]
  (if (empty? coll)
    {:ok true :decls ()}
    (either [res (monad [v (seqm (map trans-expr coll))] (right v))]
      {:ok false :error res}
      {:ok true :decls res})))


(defn trans-ast
  "Translates a collection of AST maps into unevaluated Clojure forms."
  [ast]
  (cond (= (:token ast) :val) (trans-def ast)
	(= (:token ast) :fun) (trans-defn ast)
	:else                 (trans-expr ast)))


(defn eval-ast
  "Translates and evaluates an AST; returns a vector with the
   generated code and the result of the evaluation."
  [ast]
  (monad [code (trans-ast ast)]
    (right [code (eval code)])))


(defn trans
  "Translates and evaluates a collection of top-level ASTs.
   Returns a map with the following fields:
   :ok     true on success; false otherwise
   :value  if ok, the value of the last form
   :decls  if ok, a vector of Clojure forms
   :error  if not ok, the error or warning message"
  [coll]
  (either [res (monad [v (seqm (map eval-ast coll))] (right v))]
    {:ok false :error res}
    {:ok true :decls (map first res) :value (-> res last second)}))
