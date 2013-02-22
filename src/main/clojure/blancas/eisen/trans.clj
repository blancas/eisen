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


(defn exp [x n]
  "Implements the power-of (**) operator."
  (reduce * (repeat n x)))


(declare trans-expr trans-ast trans)


(defn trans-def
  "Translates an AST into a Clojure var definition."
  [ast]
  (let [name (symbol (:name ast))]
    (either [val (trans-expr (:value ast))]
      (right `(def ~name ~val)))))


(defn trans-defn
  "Translates an AST into a Clojure function definition."
  [ast] nil)


(defn trans-binop
  "Translates the application of a binary operator."
  [ast]
  (monad [x (trans-ast (:left ast))
	  y (trans-ast (:right ast))]
    (let [f (-> ast :op :value str symbol)]
      (right `(~f ~x ~y)))))


(defn trans-uniop
  "Translates the application of a unary operator."
  [ast]
  (monad [y (trans-ast (:right ast))]
    (let [f (-> ast :op :value str symbol)]
      (right `(~f ~y)))))


(defn trans-expr
  "Translates an AST into a Clojure expression."
  [ast]
  (case (:token ast)
    (:new-line
     :identifier
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

    :list-lit
      (let [vals (trans (:value ast))]
        (if (:ok vals)
          (right `(list ~@(:decls vals)))
	  (left (:error vals))))

    :vector-lit
      (let [vals (trans (:value ast))]
        (if (:ok vals)
          (right (:decls vals))
	  (left (:error vals))))

    :set-lit
      (let [vals (trans (:value ast))]
        (if (:ok vals)
          (right (set (:decls vals)))
	  (left (:error vals))))

    :map-lit
      (let [vals (trans (:value ast))]
        (if (:ok vals)
          (right (apply hash-map (:decls vals)))
	  (left (:error vals))))

    :BINOP
      (trans-binop ast)

    :UNIOP
      (trans-uniop ast)))


(defn trans-ast
  "Translates a collection of AST maps into unevaluated Clojure forms."
  [ast]
  (cond (= (:token ast) :val) (trans-def ast)
	(= (:token ast) :fun) (trans-defn ast)
	:else                 (trans-expr ast)))


(defn trans
  "Translates a collection of AST maps into unevaluated Clojure forms."
  [coll]
  (either [res (monad [v (seqm (map trans-ast coll))] (right v))]
    {:ok false :error res}
    {:ok true :decls res}))
