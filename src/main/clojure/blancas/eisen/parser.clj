;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "The Eisen Parser.

The Eisen Lexer is configured with the following settings:

comment-start        /*
comment-end          */
comment-line         //
nested-comments      Yes
identifier-start     Lowercase or _
identifier-letter    Alphanumeric or _ 
reserved-names       None
case-sensitive       Yes
line-continuation    Backslash
trim-newline         Yes

Literal values follow the rules of Java and Clojure."
      :author "Armando Blancas"}
  blancas.eisen.parser
  (:use [blancas.kern.core])
  (:require [blancas.kern.lexer :as lex]))


;; +-------------------------------------------------------------+
;; |                      Lexer definitions.                     |
;; +-------------------------------------------------------------+

(def eisen-style
  "Lexical settings for the Eisen language."
  (assoc lex/java-style
    :nested-comments    true
    :identifier-start   (<|> lower (sym* \_))
    :identifier-letter  (<|> alpha-num (sym* \_))
    :reserved-names     []))

(def rec (lex/make-parsers eisen-style))

(def trim       (:trim       rec))
(def lexeme     (:lexeme     rec))
(def sym        (:sym        rec))
(def new-line   (:new-line   rec))
(def one-of     (:one-of     rec))
(def none-of    (:none-of    rec))
(def token      (:token      rec))
(def word       (:word       rec))
(def identifier (:identifier rec))
(def field      (:field      rec))
(def char-lit   (:char-lit   rec))
(def string-lit (:string-lit rec))
(def dec-lit    (:dec-lit    rec))
(def oct-lit    (:oct-lit    rec))
(def hex-lit    (:hex-lit    rec))
(def float-lit  (:float-lit  rec))
(def bool-lit   (:bool-lit   rec))
(def nil-lit    (:nil-lit    rec))
(def parens     (:parens     rec))
(def braces     (:braces     rec))
(def angles     (:angles     rec))
(def brackets   (:brackets   rec))
(def semi       (:semi       rec))
(def comma      (:comma      rec))
(def colon      (:colon      rec))
(def dot        (:dot        rec))
(def semi-sep   (:semi-sep   rec))
(def semi-sep1  (:semi-sep1  rec))
(def comma-sep  (:comma-sep  rec))
(def comma-sep1 (:comma-sep1 rec))


(def sym-name
  "Parses a name for anything that is not a top-level declaration.
   Cannot go at the start of a line, which signals a new declaration."
  (bind [p get-position]
    (if (= (:col p) 1)
      (fail "")
      identifier)))


(def key-name
  "Parses a Clojure keyword."
  (<$> keyword
       (>> (sym* \:)
	   (<+> (lexeme (many1 (none-of* " `~@%^*()[]{};\"\\,")))))))


;; +-------------------------------------------------------------+
;; |                     Parser definitions.                     |
;; +-------------------------------------------------------------+


(declare expr)


(def list-lit
  "Parses a list literal."
  (bind [elems (brackets (comma-sep (fwd expr)))]
    (return `(list ~@elems))))


(def vector-lit
  "Parses a vector literal."
  (<:> (>> (sym* \#)
           (brackets (comma-sep (fwd expr))))))


(def set-lit
  "Parses a set literal."
  (<$> set
       (<:> (>> (sym* \#)
                (braces (comma-sep (fwd expr)))))))


(def map-lit
  "Parses a map literal."
  (<$> (comp (partial apply hash-map) flatten)
       (braces (comma-sep (fwd expr)))))


(def re-lit
  "Parses a regular-expression literal."
  (<:> (bind [_ (sym* \#)  s string-lit]
         (return (read-string (str "#\"" s "\""))))))


(def factor
  "A factor is an operand within an expression."
  (<|> sym-name
       key-name
       char-lit
       string-lit
       dec-lit
       oct-lit
       hex-lit
       float-lit
       bool-lit
       nil-lit
       list-lit
       vector-lit
       set-lit
       map-lit
       re-lit
       (parens (fwd expr))))


(def expr factor)


(def decl
  "Parses a declaration as a named value or funcion. The name should be
   placed at the start of a line to ensure it denotes a declaration."
  (bind [name identifier  parms (many sym-name)  _ (sym \=)  val expr]
    (let [tok (if (seq parms) :defn :def)]
      (return {:tok tok :name name :parms parms :val val}))))


(def decls
  "Parses one or more declarations or naked expressions, which
   are allowed for Java interop or side-effects."
  (>> trim (many (<|> decl expr))))
