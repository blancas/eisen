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


(declare trans-expr trans)


(defn trans-def
  "Translates an AST into a Clojure var definition."
  [ast]
  (let [name (symbol (:name ast))]
    (either [val (trans-expr (:value ast))]
      (right `(def ~name ~val)))))


(defn trans-defn
  "Translates an AST into a Clojure function definition."
  [ast] nil)


(defn trans-expr
  "Translates an AST into a Clojure expression."
  [ast]
  (case (:tok ast)
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

    (:list-lit)
      (let [vals (trans (:value ast))]
        (if (:ok vals)
          (right `(list ~@(:decls vals)))
	  (left (:error vals))))

    (:vector-lit)
      (let [vals (trans (:value ast))]
        (if (:ok vals)
          (right (:decls vals))
	  (left (:error vals))))

    (:set-lit)
      (let [vals (trans (:value ast))]
        (if (:ok vals)
          (right (set (:decls vals)))
	  (left (:error vals))))

    (:map-lit)
      (let [vals (trans (:value ast))]
        (if (:ok vals)
          (right (apply hash-map (:decls vals)))
	  (left (:error vals))))))


(defn trans-ast
  "Translates a collection of AST maps into unevaluated Clojure forms."
  [ast]
  (cond (= (:tok ast) :def)  (trans-def ast)
	(= (:tok ast) :defn) (trans-defn ast)
	:else                (trans-expr ast)))


(defn trans
  "Translates a collection of AST maps into unevaluated Clojure forms."
  [coll]
  (either [res (monad [v (seqm (map trans-ast coll))] (right v))]
    {:ok false :error res}
    {:ok true :decls res}))
