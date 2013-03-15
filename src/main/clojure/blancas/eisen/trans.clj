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
	[blancas.morph.monads :only (left right either)]
	[blancas.morph.transf :only (->StateT state-t get-st modify-st eval-state-t)]))


(def predefs #{'recur})  ;; Initial state of the environment.


;; +-------------------------------------------------------------+
;; |                       Extensibility.                        |
;; +-------------------------------------------------------------+

(def expr-trans (atom {}))  ;; User-defined table of expression ranslators.
(def decl-trans (atom {}))  ;; User-defined table of declaration translators.


(defn add-expr-trans
  "Adds a translator function from an expression AST to Clojure code,
   assigned to the supplied key."
  [key trans] (swap! expr-trans assoc key trans))


(defn add-decl-trans
  "Adds a translator function from a declaration AST to Clojure code,
   assigned to the supplied key."
  [key trans] (swap! expr-trans assoc key trans))


;; +-------------------------------------------------------------+
;; |                    StateT Either monad.                     |
;; +-------------------------------------------------------------+


(defn make-left
  "Makes a Left value inside a State. This makes possible
   to get a Left off `run-se` whose value is not a pair."
  [x] (->StateT left (fn [_] (left x))))


(defn make-right
  "Makes an Right value inside a State."
  [x] (state-t right x))


(defn run-se
  "Returns the Either inner monad."
  [m s] (eval-state-t m s))


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
  (let [sym-name (symbol (:name ast))]
    (make-right `(ns ~sym-name))))


(defn trans-imp
  "Translates an AST into a Clojure use or require call."
  [{:keys [name qualify]}]
  (let [sym-name (symbol name)]
    (if (nil? qualify)
      (make-right `(use '~sym-name))
      (let [names (fn [x] (map (comp symbol :value) (:value x)))]
        (case (:token qualify)
          :as   (make-right `(require '[~sym-name :as ~(symbol (:value qualify))]))
	  :only (make-right `(use '[~sym-name :only ~(names qualify)]))
	  :hide (make-right `(use '[~sym-name :exclude ~(names qualify)])))))))


(defn trans-val
  "Translates an AST into a Clojure var definition."
  [{:keys [name value]}]
  (let [sym-name (symbol name)]
    (monad [val (trans-expr value)]
      (make-right `(def ~sym-name ~val)))))


(defn trans-fun
  "Translates an AST into a Clojure function definition."
  [{:keys [name params value]}]
  (let [sym-name (symbol name)]
    (monad [env (trans-exprs params)
	    _   (modify-st right into (cons sym-name env))
	    code (trans-expr value)
	    _   (modify-st right difference (cons sym-name env))]
      (make-right `(blancas.morph.core/defcurry ~sym-name ~env ~code)))))


(defn trans-fwd
  "Translates an AST into a Clojure forward declaration."
  [{:keys [decls]}]
  (let [names (map (comp symbol :value) decls)]
    (make-right `(declare ~@names))))


(defn trans-identifier
  "Translates a reference to an identifier."
  [{:keys [value pos]}]
  (let [sym-name (symbol value)]
    (if (clazz? sym-name)
      (make-right sym-name)
      (monad [env (get-st right)]
        (if (contains? env sym-name)
          (make-right (make-ref sym-name))
	  (if-let [var-inst (resolve sym-name)]
            (if (function? var-inst)
	      (make-right `(~sym-name))
	      (make-right sym-name))
            (make-left (error pos "undeclared identifier: %s" value))))))))


(defn trans-idarg
  "Translates a reference to an identifier as an argument."
  [{:keys [value pos]}]
  (let [sym-name (symbol value)]
    (monad [env (get-st right)]
      (if (contains? env sym-name)
        (make-right (make-ref sym-name))
	(if (resolve sym-name)
	  (make-right sym-name)
          (make-left (error pos "undeclared identifier: %s" value)))))))


(defn trans-funcall
  "Translates a function call."
  [name args]
  (let [value (:value name)
	pos (:pos name)
	sym-name (symbol value)]
    (monad [env (get-st right)
	    lst (trans-exprs args)]
      (if (contains? env sym-name)
        (make-right (list* sym-name lst))
	(if-let [var-inst (resolve sym-name)]
          (if (function? var-inst)
	    (make-right (list* sym-name lst))
	    (make-left (error pos "%s is not a function" value)))
          (make-left (error pos "undeclared identifier: %s" value)))))))


(defn trans-macrocall
  "Translates a call to a macro."
  [name args]
  (let [sym-name (symbol (:value name))]
    (monad [lst (trans-exprs args)]
      (make-right (list* sym-name lst)))))


(defn trans-binop
  "Translates the application of a binary operator."
  [ast]
  (monad [x (trans-expr (:left ast))
	  y (trans-expr (:right ast))]
    (let [f (-> ast :op :value str symbol)]
      (make-right `(~f ~x ~y)))))


(defn trans-uniop
  "Translates the application of a unary operator."
  [ast]
  (monad [y (trans-expr (:right ast))]
    (let [f (-> ast :op :value str symbol)]
      (make-right `(~f ~y)))))


(defn trans-cond
  "Translates a conditional expression."
  [ast]
  (let [e (:else ast)]
    (monad [test (trans-expr (:test ast))
	    then (trans-expr (:then ast))
	    else (if e (trans-expr (:else ast)) (make-right :empty))]
      (if (= else :empty)
        (make-right `(if ~test ~then))
        (make-right `(if ~test ~then ~else))))))


(defn val-binding
  "Translates a val binding."
  [ast]
  (let [name (symbol (:name ast))]
    (monad [val (trans-expr (:value ast))]
      (make-right [name val]))))


(defn fun-binding
  "Translates a function binding."
  [{:keys [name params value]}]
  (let [sym (symbol name)]
    (monad [env (trans-exprs params)
	    _   (modify-st right into env)
	    code (trans-expr value)
	    _   (modify-st right difference env)]
      (make-right [sym `(blancas.morph.core/mcf ~env ~code)]))))


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
    (make-right [])))


(defn trans-let
  "Translates a let expression."
  [{:keys [decls exprs]}]
  (let [env (map (comp symbol :name) decls)]
    (monad [_     (modify-st right into env)
	    decls (trans-bindings decls)
            exprs (trans-exprs exprs)
	    _     (modify-st right difference env)]
      (make-right `(let [~@(apply concat decls)] ~@exprs)))))


(defn val-binding-letrec
  "Translates a val binding in a letrec expression."
  [ast]
  (let [name (symbol (:name ast))]
    (monad [val (trans-expr (:value ast))]
      (make-right (list name [] val)))))


(defn fun-binding-letrec
  "Translates a function binding in a letrec expression."
  [{:keys [name params value]}]
  (let [sym (symbol name)]
    (monad [env (trans-exprs params)
	    _   (modify-st right into env)
	    code (trans-expr value)
	    _   (modify-st right difference env)]
      (make-right (list sym env code)))))


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
    (monad [_     (modify-st right into env)
	    decls (seqm (map trans-binding-letrec decls))
            exprs (trans-exprs exprs)
	    _     (modify-st right difference env)]
      (make-right `(letfn [~@decls] ~@exprs)))))


(defn trans-funlit
  "Translates an AST into a Clojure anonymous function."
  [{:keys [params value]}]
  (monad [env (trans-exprs params)
	  _   (modify-st right into env)
	  code (trans-expr value)
	  _   (modify-st right difference env)]
    (make-right `(blancas.morph.core/mcf ~env ~code))))


(defn trans-expr
  "Translates an AST into a Clojure expression."
  [ast]
  (case (:token ast)
    (:new-line :char-lit  :string-lit :dec-lit  :oct-lit
     :hex-lit  :float-lit :bool-lit   :nil-lit  :semi
     :comma    :colon     :dot        :keyword  :re-lit)
                 (make-right (:value ast))

    (:id-formal :sym-arg)
		 (make-right (-> ast :value symbol))

    :id-arg      (trans-idarg ast)

    :identifier  (trans-identifier ast)

    :fun-call    (let [val (:value ast)]
                   (trans-funcall (first val) (rest val)))

    :macro-call  (let [val (:value ast)]
                   (trans-macrocall (first val) (rest val)))

    :list-lit    (monad [vals (trans-exprs (:value ast))]
                   (make-right `(list ~@vals)))

    :vector-lit  (monad [vals (trans-exprs (:value ast))]
                   (make-right vals))

    :set-lit     (monad [vals (trans-exprs (:value ast))]
                   (make-right (set vals)))

    :map-lit     (monad [vals (trans-exprs (:value ast))]
                   (make-right (apply hash-map vals)))

    :seq-expr    (monad [vals (trans-exprs (:value ast))]
                   (make-right `(do ~@vals)))

    :BINOP       (trans-binop ast)

    :UNIOP       (trans-uniop ast)

    :cond-expr   (trans-cond ast)

    :let-expr    (trans-let ast)

    :letrec-expr (trans-letrec ast)

    :fun-lit     (trans-funlit ast)

    ;; User-defined expression translator.

    (if-let [trans ((:token ast) @expr-trans)]
      (trans ast)
      (make-left ast))))


(defn trans-exprs
  "Translates a collection of ASTs into Clojure expressions."
  [coll]
  (if (seq coll)
    (seqm (map trans-expr coll))
    (make-right [])))


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
    (try
      (make-right [code (eval code)])
      (catch Throwable t
	(make-left [code (str t)])))))


(defn trans
  "Translates and evaluates a collection of top-level ASTs.
   Returns a map with the following fields:
   :ok     true on success; false otherwise
   :value  if ok, the value of the last form
   :decls  if ok, a vector of Clojure forms
   :error  if not ok, the error or warning message"
  [coll]
  (let [job (monad [v (seqm (map eval-ast coll))] (make-right v))]
    (either [res (run-se job predefs)]
      {:ok false :error res}
      {:ok true :decls (map first res) :value (-> res last second)})))
