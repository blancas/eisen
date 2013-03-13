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
  (:use [blancas.eisen.parser :only (eisen-code)]
	[blancas.eisen.trans :only (trans)]
        [blancas.kern.core :only (parse run print-error f->s)]))


(defn parse-eisen
  "Parses the supplied Eisen code; returns an abstract syntax tree."
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
   and return the result in the :value field. Returns a map with:
   :ok     true on success; false otherwise
   :value  if ok, the value of the last form
   :decls  if ok, a vector of Clojure forms
   :error  if not ok, the error or warning message"
  ([text]
   (eisen text ""))
  ([text source]
   (let [ast (parse-eisen text source)]
     (if (:ok ast) (trans (:decls ast)) ast))))


(defn eisen=
  "Runs the supplied Eisen code and shows the resulting value or error.
   Intended to evaluating expressions at the REPL."
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
  "Translates an eisen file; takes an optional encoding, which
   defaults to UTF-8."
  ([f] (eisenf f "UTF-8"))
  ([f en] (eisen (f->s f en) f)))


(defn eisenf=
  "Parses an eisen file and prints the syntax tree. Takes an optional
   encoding, which defaults to UTF-8. Any errors are printed to stdout;
   intended for testing at the REPL."
  ([f] (eisenf= f "UTF-8"))
  ([f en] (eisen= (f->s f en) f)))


(defn read-eisen
  "Reads one or more lines of Eisen code; nsp tells whether to print
   the current namespace; p1 is the initial prompt and p2 is the
   line-continuation prompt. A single new-line character ends the input."
  [nsp p1 p2]
  (print (str (if nsp (ns-name *ns*) "") p1 \space))
  (.flush *out*)
  (loop [line "" s (read-line)]
    (if (seq s)
      (do
        (print (str p2 \space))
        (.flush *out*)
        (recur (str line s \newline) (read-line)))
      (if (= (last line) \newline)
        (apply str (butlast line))
        line))))


(defn eisen-repl
  "Implements a simple Eisen REPL. Uses (read-eisen) to read code
   until the user types // followed by Enter. It evaluates each
   block of code and prints the result.

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
   (eisen-repl true ":" ">"))
  ([nsp p1 p2]
   (let [code (read-eisen nsp p1 p2)]
     (when-not (= code "//")
       (if (seq code) 
         (println (eisen= code)))
       (recur nsp p1 p2)))))


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
