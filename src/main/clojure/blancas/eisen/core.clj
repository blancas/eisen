;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Main module of the Eisen translator."
      :author "Armando Blancas"}
  blancas.eisen.core
  (:require [blancas.eisen.clojure :as cc])
  (:use [blancas.eisen.parser :only (eisen-code)]
	[blancas.eisen.trans :only (trans add-auto-decl)]
        [blancas.kern.core :only (parse run print-error f->s)]))


;; +-------------------------------------------------------------+
;; |                  Working with Eisen Code.                   |
;; +-------------------------------------------------------------+


(declare init-eisen)


(defn parse-eisen
  "Parses the supplied Eisen code; returns an abstract syntax tree (AST)."
  ([text]
   (parse-eisen text ""))
  ([text source]
   (let [st (parse eisen-code text source)]
     (if (:ok st)
       {:ok true :decls (:value st)}
       {:ok false :error (with-out-str (print-error st))}))))
  

(defn eisen
  "Translates the supplied Eisen code into Clojure and evaluates
   the resulting forms. If given an expression, it will evaluate it
   and return the result in the :value field.

   Returns a map with:
   :ok     true on success; false otherwise
   :value  if ok, the value of the last form
   :decls  if ok, a vector of Clojure forms
   :error  if not ok, the error or warning message"
  ([text]
   (eisen text ""))
  ([text source]
    (let [current (symbol (str *ns*))]
      (try
        (let [ast (parse-eisen text source)]
          (if (:ok ast) (trans (:decls ast)) ast))
        (finally
          (in-ns current))))))


(defn eisen=
  "Runs the supplied Eisen code. It returns the :value field of the
   resulting map, or it prints any error messages on failure.
   Intended for evaluating expressions at the REPL."
  ([text]
   (eisen= text ""))
  ([text source]
    (let [e (eisen text source)]
      (if (:ok e)
        (:value e)
        (println (:error e))))))


(defn eisen*
  "Parses the supplied Eisen code and prints the syntax tree. Any
   errors are printed to stdout; intended for testing at the REPL."
  ([text] (eisen* text ""))
  ([text source] (run eisen-code text source)))


(defn eisenf
  "Runs the code from an eisen file; takes an optional encoding value,
   which defaults to UTF-8."
  ([f] (eisenf f "UTF-8"))
  ([f en] (eisen (f->s f en) f)))


(defn eisenf=
  "Parses the code from an eisen file and prints the syntax tree.
   Takes an optional encoding value, which defaults to UTF-8.
   Any errors are printed to stdout; intended for testing at the REPL."
  ([f] (eisenf= f "UTF-8"))
  ([f en] (eisen= (f->s f en) f)))


(defn read-eisen
  "Reads one or more lines of Eisen code; nsp tells whether to print
   the current namespace; p1 is the initial prompt and p2 is the
   line-continuation prompt; p3 is the command to quit reading.
   A single new-line character ends the input."
  [nsp p1 p2 p3]
  (print (str (if nsp (ns-name *ns*) "") p1 \space))
  (.flush *out*)
  (loop [line "" s (read-line)]
    (if (seq s)
      (if (= s p3)
        p3
        (do
          (print (str p2 \space))
          (.flush *out*)
          (recur (str line s \newline) (read-line))))
      (if (= (last line) \newline)
        (apply str (butlast line))
        line))))


(defn eisen-repl
  "Implements a simple Eisen REPL. Uses (read-eisen) to read code
   until the user types the quit command (default //) followed by
   Enter. It evaluates each block of code and prints the result.

   If called with no arguments, it will default to printing the
   current namespace, followed by : and using > as a continuation
   prompt; nsp tells whether to print the current namespace; p1 is
   the initial prompt and p2 is the line-continuation prompt.

   user=> (eisen-repl)
   user: 3 + 4 <Enter>
   > <Enter>
   >
   user: //
   > <Enter>
   user=>"
  ([]
   (eisen-repl true ":" ">" "//"))
  ([nsp p1 p2 p3]
   (init-eisen)
   (let [code (read-eisen nsp p1 p2 p3)]
     (when-not (= code "//")
       (if (seq code) 
         (println (eisen= code)))
       (recur nsp p1 p2 p3)))))


;; +-------------------------------------------------------------+
;; |                      Extending Eisen.                       |
;; +-------------------------------------------------------------+


(defn add-expression
  "Extends Eisen with the ability to parse and translate
   a new type of expression. Parameters:

   token   A keyword that identifies the translator.

   parser  A Kern parser that produces an Eisen AST, which is a
           map with a :token field whose value is token (above).
           Any other fields collect data for the translator.

   trans   A function that translates an AST to Clojure code
           using any data produced by the parser.

   words   Any words to be reserved for this parser's syntax."
  [token parser trans & words]
  (blancas.eisen.parser/add-expr parser)
  (blancas.eisen.trans/add-expr-trans token trans)
  (blancas.eisen.parser/add-reserved words))


(defn add-declaration
  "Extends Eisen with the ability to parse and translate
   a new type of top-level declaration. Parameters:

   token   A keyword that identifies the translator.

   parser  A Kern parser that produces an Eisen AST, which is a
           map with a :token field whose value is token (above).
           Any other fields collect data for the translator.

   trans   A function that translates an AST to Clojure code
           using any data produced by the parser.

   words   Any words to be reserved for this parser's syntax."
  [token parser trans & words]
  (blancas.eisen.parser/add-decl parser)
  (blancas.eisen.trans/add-decl-trans token trans)
  (blancas.eisen.parser/add-reserved words))


;; +-------------------------------------------------------------+
;; |                   Extending Applications.                   |
;; +-------------------------------------------------------------+


(def model (atom {}))


(defmacro ->m
  "Puts on or more key value pairs into the model. A key must be
   an unquoted symbol; a value may be any Clojure object."
  [& args]
  (reset! blancas.eisen.trans/using-model true)
  (let [n (count args)]
    (when (pos? n)
      (when (odd? n)
	(throw (Exception. "Macro ->m takes an even number of arguments")))
      (let [pairs (for [[k v] (partition 2 args)]
		    `('~(clojure.core/symbol (clojure.core/name k)) ~v))]
        `(clojure.core/swap! model clojure.core/assoc
			     ~@(clojure.core/apply concat pairs))))))


(defmacro m->
  "Looks up a key in the model and returns its value.
   A key must be an unquoted symbol."
  [k] `(clojure.core/get @blancas.eisen.core/model
			 '~(clojure.core/symbol (clojure.core/name k))))


(defmacro run->
  "If the name has a value in the host model it gets called along
   with any other supplied arguments."
  [name & more]
  `(if (m-> ~name) ((m-> ~name) ~@more)))


(defmacro host-name
  "Creates a reference from the eisen.user namespace to the
   supplied symbol. The name is preserved unless an alias is
   also given, which is useful for avoiding backquotes if it
   contains characters not accepted in Eisen identifiers."
  ([sym]
   `(host-name ~sym nil))
  ([sym alias]
   (let [name  (symbol (name sym))
         nspc  (symbol (or (namespace sym) (str *ns*)))
         rcmd  (if alias
                 `(refer '~nspc :rename {'~name '~alias})
                 `(refer '~nspc :only '~(list name)))]
     `(do (ns eisen.user) ~rcmd (ns ~nspc)))))


(defmacro host-module
  "Similar to (refer) but acts on the eisen.user namespace. It is
   intended to simplify the host API by adding names in bulk.
   It supports refer filters:

   (host-module x.y.z)
   (host-module x.y.z :exclude '(f2 f3))
   (host-module x.y.z :only '(f2 f3))
   (host-module x.y.z :rename {'f-1 'f1})"
  [tgt & args]
  `(let [ns# (symbol (str *ns*))]
     (ns eisen.user) ~(list* 'refer (list 'quote tgt) args) (in-ns ns#)))


(defn init-eisen
  "Initializes the Eisen library. This is a convenience function that
   a host program may omit or override with custom calls.

   (1) It installs the following commands into the Eisen langauge:

       * asString
       * case
       * cond
       * doseq
       * dosync
       * for
       * io!
       * locking
       * loop
       * sync
       * when
       * whenFirst
       * while
       * withOpen
       * withString

   (2) Installs the following imports, to be applied to any Eisen module
       so that common Clojure names are available automatically and with
       in idiomatic camel case instead of embedded dashes.

   clojure.core
      assocIn
      dropWhile
      fileSeq
      getIn
      groupBy
      hashMap
      hashSet
      lazySeq
      lineSeq
      mergeWith
      notAny?
      notEmpty
      notEvery?
      partitionAll
      partitionBy
      prStr
      printStr
      printlnStr
      reFind
      reGroups
      reMatcher
      reMatches
      rePattern
      reSeq
      readLine
      refSet
      selectKeys
      sortBy
      sortedMap
      sortedMapBy
      sortedSet
      sortedSetBy
      splitAt
      splitWith
      takeLast
      takeNth
      takeWhile
      treeSeq
      updateIn
      xmlSeq

   clojure.java.io
      file 
      reader
      writer
      inputStream
      outputStream

   clojure.string
      blank? 
      join 
      split 
      trim
      splitLines

   clojure.xml
      parse"
  []
  (add-expression  :when-expr    cc/whenex  cc/trans-whenex  "when")
  (add-expression  :while-expr   cc/whileex cc/trans-whileex "while")
  (add-expression  :loop-expr    cc/loopex  cc/trans-loopex  "loop")
  (add-expression  :whenf-expr   cc/whenfex cc/trans-whenfex "whenFirst")
  (add-expression  :cljcond-expr cc/cljcond cc/trans-cljcond "cond")
  (add-expression  :case-expr    cc/caseex  cc/trans-caseex  "case" "of")
  (add-expression  :for-expr     cc/forex   cc/trans-forex   "for" "while" "when")
  (add-expression  :doseq-expr   cc/doseqex cc/trans-doseqex "doseq" "while" "when")
  (add-expression  :wopen-expr   cc/wopenex cc/trans-wopenex "withOpen")
  (add-expression  :str-expr     cc/strex   cc/trans-strex   "asString")
  (add-expression  :wstr-expr    cc/wstrex  cc/trans-wstrex  "withString")
  (add-expression  :trans-expr   cc/transex cc/trans-transex
		   "locking" "io!" "sync" "dosync")

  (add-auto-decl
    'clojure.core
    { :rename {
      'assoc-in       'assocIn
      'drop-while     'dropWhile
      'file-seq       'fileSeq
      'get-in         'getIn
      'group-by       'groupBy
      'hash-map       'hashMap
      'hash-set       'hashSet
      'lazy-seq       'lazySeq
      'line-seq       'lineSeq
      'merge-with     'mergeWith
      'not-any?       'notAny?
      'not-empty      'notEmpty
      'not-every?     'notEvery?
      'partition-all  'partitionAll
      'partition-by   'partitionBy
      'pr-str         'prStr
      'print-str      'printStr
      'println-str    'printlnStr
      're-find        'reFind
      're-groups      'reGroups
      're-matcher     'reMatcher
      're-matches     'reMatches
      're-pattern     'rePattern
      're-seq         'reSeq
      'read-line      'readLine
      'ref-set        'refSet
      'select-keys    'selectKeys
      'sort-by        'sortBy
      'sorted-map     'sortedMap
      'sorted-map-by  'sortedMapBy
      'sorted-set     'sortedSet
      'sorted-set-by  'sortedSetBy
      'split-at       'splitAt
      'split-with     'splitWith
      'take-last      'takeLast
      'take-nth       'takeNth
      'take-while     'takeWhile
      'tree-seq       'treeSeq
      'update-in      'updateIn
      'xml-seq        'xmlSeq }})

  (add-auto-decl
    'clojure.java.io { :only '(file reader writer) })

  (add-auto-decl
    'clojure.java.io
    { :rename {
      'input-stream 'inputStream
      'output-stream 'outputStream }})

  (add-auto-decl
    'clojure.string
    { :only '(blank? join split trim)
      :rename {'split-lines 'splitLines} })

  (add-auto-decl
    'clojure.xml { :only '(parse) }))
