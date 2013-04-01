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
  (:use [clojure.set :only (difference)]
        [blancas.morph.core :only (monad seqm)]
	[blancas.morph.monads :only (either)]
	[blancas.morph.transf :only (->left ->right get-se modify-se run-se)]))


;; +-------------------------------------------------------------+
;; |                       Extensibility.                        |
;; +-------------------------------------------------------------+

(def predefs #{'recur})     ;; Initial state of the environment.

(def expr-trans (atom {}))  ;; User-defined table of expression ranslators.
(def decl-trans (atom {}))  ;; User-defined table of declaration translators.
(def auto-decls (atom {}))  ;; User-defined table of auto use decls for modules.

(def ^:dynamic code-hook "A transformation of the generated code." identity)

(def using-model (atom false)) ;; Using a host model for overrides?


(defn add-expr-trans
  "Adds a translator function from an expression AST to Clojure code,
   assigned to the supplied key."
  [key trans] (swap! expr-trans assoc key trans))


(defn add-decl-trans
  "Adds a translator function from a declaration AST to Clojure code,
   assigned to the supplied key."
  [key trans] (swap! expr-trans assoc key trans))


(defn add-auto-decl
  "Adds a use declaration to run upon declaring an Eisen module."
  [key decl] (swap! auto-decls assoc key decl))


;; +-------------------------------------------------------------+
;; |                   Translator utilities.                     |
;; +-------------------------------------------------------------+


(defn error
  "Composes an error message."
  [pos fmt & more]
  (let [row (:line pos)
	col (:col pos)
	loc (if (empty? (:src pos))
	      (format "line %d, column %d\n" row col)
	      (format "%s: line %d, column %d\n" (:src pos) row col))]
    (str loc (apply format fmt more))))


(defn clazz?
  "Tests if a symbol refers to a Java class."
  [s]
  (try
    (let [clazz (eval s)] (class? clazz))
    (catch Throwable t false)))


(defn function?
  "Tests if the var is a function or macro. If there's no root
   binding we assume that is a forward-declared function."
  [v] (if (bound? v) (fn? (var-get v)) true))


(defn exp
  "Implements the power-of (**) operator."
  [x n] (reduce * (repeat n x)))


(defn conj-rev
  "A version of conj with the arguments reversed."
  [x coll] (conj coll x))


(defn make-ref
  "Encodes a reference for a local symbol."
  [s] `(if (fn? ~s) (~s) ~s))


;; +-------------------------------------------------------------+
;; |                   The Eisen Translator.                     |
;; +-------------------------------------------------------------+


(declare trans-expr trans-exprs)


(defn trans-mod
  "Translates an AST into a Clojure namespace call."
  [ast]
  (let [sym-name (symbol (:name ast))
	imports  (for [[key val] @auto-decls]
		   (let [filters (for [[f m] val] `(~f ~m))]
		     `(clojure.core/use '[~key ~@(apply concat filters)])))]
    (->right `(do (clojure.core/ns ~sym-name) ~@imports))))


(defn trans-imp
  "Translates an AST into a Clojure use or require call."
  [{:keys [name qualify]}]
  (let [sym-name (symbol name)]
    (if (nil? qualify)
      (->right `(use '~sym-name))
      (let [names (fn [x] (map (comp symbol :value) (:value x)))]
        (case (:token qualify)
	  :as   (->right `(clojure.core/require
			    '[~sym-name :as ~(symbol (:value qualify))]))
	  :only (->right `(clojure.core/use
			    '[~sym-name :only ~(names qualify)]))
	  :hide (->right `(clojure.core/use
			    '[~sym-name :exclude ~(names qualify)])))))))


(defn trans-val
  "Translates an AST into a Clojure var definition."
  [{:keys [name value]}]
  (let [sym-name (symbol name)]
    (monad [val (trans-expr value)]
      (->right `(def ~sym-name ~val)))))


(defn trans-fun
  "Translates an AST into a Clojure function definition."
  [{:keys [name params value]}]
  (let [sym-name (symbol name)]
    (monad [env (trans-exprs params)
	    _   (modify-se into (cons sym-name env))
	    code (trans-expr value)
	    _   (modify-se difference (cons sym-name env))]
      (->right `(blancas.morph.core/defcurry ~sym-name ~env ~code)))))


(defn trans-fwd
  "Translates an AST into a Clojure forward declaration."
  [{:keys [decls]}]
  (let [names (map (comp symbol :value) decls)]
    (->right `(clojure.core/declare ~@names))))


(defn trans-identifier
  "Translates a reference to an identifier."
  [{:keys [value pos]}]
  (let [sym-name (symbol value)]
    (if (clazz? sym-name)
      (->right sym-name)
      (monad [env get-se]
        (if (contains? env sym-name)
          (->right (make-ref sym-name))
	  (if-let [var-inst (resolve sym-name)]
            (if (function? var-inst)
	      (->right `(~sym-name))
	      (->right sym-name))
            (->left (error pos "undeclared identifier: %s" value))))))))


(defn trans-idarg
  "Translates a reference to an identifier as an argument."
  [{:keys [value pos]}]
  (let [sym-name (symbol value)]
    (monad [env get-se]
      (if (contains? env sym-name)
        (->right (make-ref sym-name))
	(if (resolve sym-name)
	  (->right sym-name)
          (->left (error pos "undeclared identifier: %s" value)))))))


(defn trans-funcall
  "Translates a function call."
  [name args]
  (let [value (:value name)
	pos (:pos name)
	sym-name (symbol value)]
    (monad [env get-se
	    lst (trans-exprs args)]
      (if (contains? env sym-name)
        (->right (list* sym-name lst))
	(if-let [var-inst (resolve sym-name)]
          (if (function? var-inst)
	    (->right (list* sym-name lst))
	    (->left (error pos "%s is not a function" value)))
          (->left (error pos "undeclared identifier: %s" value)))))))


(defn trans-macrocall
  "Translates a call to a macro."
  [name args]
  (let [sym-name (symbol (:value name))]
    (monad [lst (trans-exprs args)]
      (->right (list* sym-name lst)))))


(defn trans-binop
  "Translates the application of a binary operator."
  [ast]
  (monad [x (trans-expr (:left ast))
	  y (trans-expr (:right ast))]
    (let [f (-> ast :op :value str symbol)]
      (->right `(~f ~x ~y)))))


(defn trans-uniop
  "Translates the application of a unary operator."
  [ast]
  (monad [y (trans-expr (:right ast))]
    (let [f (-> ast :op :value str symbol)]
      (->right `(~f ~y)))))


(defn trans-cond
  "Translates a conditional expression."
  [ast]
  (let [e (:else ast)]
    (monad [test (trans-expr (:test ast))
	    then (trans-expr (:then ast))
	    else (if e (trans-expr (:else ast)) (->right :empty))]
      (if (= else :empty)
        (->right `(if ~test ~then))
        (->right `(if ~test ~then ~else))))))


(defn val-binding
  "Translates a val binding."
  [ast]
  (let [name (symbol (:name ast))]
    (monad [val (trans-expr (:value ast))]
      (->right [name val]))))


(defn fun-binding
  "Translates a function binding."
  [{:keys [name params value]}]
  (let [sym (symbol name)]
    (monad [env  (trans-exprs params)
	    _    (modify-se into env)
	    code (trans-expr value)
	    _    (modify-se difference env)]
      (->right [sym `(blancas.morph.core/mcf ~env ~code)]))))


(defn trans-binding
  "Parses a val or fun binding."
  [ast]
  (if (= (:token ast) :val)
    (val-binding ast)
    (fun-binding ast)))


(defn trans-bindings
  "Translates a collection of val or fun bindings."
  [coll]
  (if (seq coll)
    (seqm (map trans-binding coll))
    (->right [])))


(defn trans-let
  "Translates a let expression."
  [{:keys [decls exprs]}]
  (let [env (map (comp symbol :name) decls)]
    (monad [_     (modify-se into env)
	    decls (trans-bindings decls)
            exprs (trans-exprs exprs)
	    _     (modify-se difference env)]
      (->right `(let [~@(apply concat decls)] ~@exprs)))))


(defn val-binding-letrec
  "Translates a val binding in a letrec expression."
  [ast]
  (let [name (symbol (:name ast))]
    (monad [val (trans-expr (:value ast))]
      (->right (list name [] val)))))


(defn fun-binding-letrec
  "Translates a function binding in a letrec expression."
  [{:keys [name params value]}]
  (let [sym (symbol name)]
    (monad [env  (trans-exprs params)
	    _    (modify-se into env)
	    code (trans-expr value)
	    _    (modify-se difference env)]
      (->right (list sym env code)))))


(defn trans-binding-letrec
  "Parses a val or fun binding in a letrec expression."
  [ast]
  (if (= (:token ast) :val)
    (val-binding-letrec ast)
    (fun-binding-letrec ast)))


(defn trans-letrec
  "Translates a letrec expression."
  [{:keys [decls exprs]}]
  (let [env (map (comp symbol :name) decls)]
    (monad [_     (modify-se into env)
	    decls (seqm (map trans-binding-letrec decls))
            exprs (trans-exprs exprs)
	    _     (modify-se difference env)]
      (->right `(letfn [~@decls] ~@exprs)))))


(defn trans-funlit
  "Translates an AST into a Clojure anonymous function."
  [{:keys [params value]}]
  (monad [env  (trans-exprs params)
	  _    (modify-se into env)
	  code (trans-expr value)
	  _    (modify-se difference env)]
    (->right `(blancas.morph.core/mcf ~env ~code))))


(defn trans-setqex
  "Translates a setq statement."
  [{:keys [name value]}]
  (monad [source (trans-expr value)]
    (if @using-model
      (->right `(blancas.eisen.core/->m ~(symbol name) ~source))
      (->right `(alter-var-root (var ~(symbol name)) (constantly ~source))))))


(defn trans-setvex
  "Translates a setv statement."
  [{:keys [name value]}]
  (if @using-model
    (->right `(blancas.eisen.core/->m ~(symbol name) (var-get (var ~(symbol value)))))
    (->right `(alter-var-root (var ~(symbol name))
			      (constantly (var-get (var ~(symbol value))))))))


  (defn trans-expr
  "Translates an AST into a Clojure expression."
  [ast]
  (case (:token ast)
    (:new-line :char-lit  :string-lit :dec-lit  :oct-lit
     :hex-lit  :float-lit :bool-lit   :nil-lit  :semi
     :comma    :colon     :dot        :keyword  :re-lit)
                 (->right (:value ast))

    (:id-formal :sym-arg)
		 (->right (-> ast :value symbol))

    :id-arg      (trans-idarg ast)

    :identifier  (trans-identifier ast)

    :fun-call    (let [val (:value ast)]
                   (trans-funcall (first val) (rest val)))

    :macro-call  (let [val (:value ast)]
                   (trans-macrocall (first val) (rest val)))

    :list-range  (monad [vals (trans-exprs (:value ast))]
		   (->right `(clojure.core/list*
			       (clojure.core/range
				 ~(first vals)
				  (clojure.core/inc ~(second vals))))))

    :list-lit    (monad [vals (trans-exprs (:value ast))]
                   (->right `(clojure.core/list ~@vals)))

    :vec-range   (monad [vals (trans-exprs (:value ast))]
		   (->right `(clojure.core/vec
			       (clojure.core/range
				 ~(first vals)
				  (clojure.core/inc ~(second vals))))))

    :vector-lit  (monad [vals (trans-exprs (:value ast))]
                   (->right vals))

    :set-lit     (monad [vals (trans-exprs (:value ast))]
                   (->right (set vals)))

    :map-lit     (monad [vals (trans-exprs (:value ast))]
                   (->right (apply hash-map vals)))

    :seq-expr    (monad [vals (trans-exprs (:value ast))]
                   (->right `(do ~@vals)))

    :BINOP       (trans-binop ast)

    :UNIOP       (trans-uniop ast)

    :cond-expr   (trans-cond ast)

    :let-expr    (trans-let ast)

    :letrec-expr (trans-letrec ast)

    :fun-lit     (trans-funlit ast)

    :setq-expr   (trans-setqex ast)

    :setv-expr   (trans-setvex ast)

    ;; User-defined expression translator.

    (if-let [trans ((:token ast) @expr-trans)]
      (trans ast)
      (->left ast))))


(defn trans-exprs
  "Translates a collection of ASTs into Clojure expressions."
  [coll]
  (if (seq coll)
    (seqm (map trans-expr coll))
    (->right [])))


(defn trans-ast
  "Translates a collection of AST maps into unevaluated Clojure forms."
  [ast]
  (let [token (:token ast)]
    (case (:token ast)  
      :val  (trans-val ast)
      :fun  (trans-fun ast)
      :fwd  (trans-fwd ast)
      :mod  (trans-mod ast)
      :imp  (trans-imp ast)
      ;; User-defined expression translator.
      (if-let [trans (token @decl-trans)]
	(trans ast)
	(trans-expr ast)))))


(defn eval-ast
  "Translates and evaluates an AST; returns a vector with the
   generated code and the result of the evaluation."
  [ast]
  (monad [code (trans-ast ast)]
    (let [code (code-hook code)]
      (try
        (->right [code (eval code)])
        (catch Throwable t
	  (->left [code (str t)]))))))


(defn trans
  "Translates and evaluates a collection of top-level ASTs.
   Returns a map with the following fields:
   :ok     true on success; false otherwise
   :value  if ok, the value of the last form
   :decls  if ok, a vector of Clojure forms
   :error  if not ok, the error or warning message"
  [coll]
  (let [job (monad [v (seqm (map eval-ast coll))] (->right v))]
    (either [res (run-se job predefs)]
      {:ok false :error res}
      {:ok true :decls (map first res) :value (-> res last second)})))
