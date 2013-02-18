;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "The Eisen Translator."
      :author "Armando Blancas"}
  blancas.eisen.trans)


(declare trans-expr)


(defn trans-def
  "Translates an AST into a Clojure var definition."
  [ast]
  (let [name (symbol (:name ast))
	value (trans-expr (:value ast))]
    (println "value" (:value value))
    (if (:ok value)
      {:ok true :value `(def ~name ~(:value value)) :error nil}
      value)))


(defn trans-defn
  "Translates an AST into a Clojure function definition."
  [ast] nil)


(defn trans-expr
  "Translates an AST into a Clojure expression."
  [ast]
  (case (:tok ast)
    :dec-lit {:ok true :value (:value ast) :error nil}))


(defn trans
  "Translates an AST into a Clojure definition or expression."
  [ast]
  (cond (= (:tok ast) :def)  (trans-def ast)
	(= (:tok ast) :defn) (trans-defn ast)
	:else                (trans-expr ast)))

