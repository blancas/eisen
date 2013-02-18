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
  (:use [blancas.kern core expr])
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
    :reserved-names     ["def"]))


(def- rec (lex/make-parsers eisen-style))


(def trim       (:trim       rec))
(def lexeme     (:lexeme     rec))
(def parens     (:parens     rec))
(def braces     (:braces     rec))
(def angles     (:angles     rec))
(def brackets   (:brackets   rec))
(def semi-sep   (:semi-sep   rec))
(def semi-sep1  (:semi-sep1  rec))
(def comma-sep  (:comma-sep  rec))
(def comma-sep1 (:comma-sep1 rec))


(defn- lexer
  "Wraps a lexer parser to produce a token record with
   the token code, value and position."
  ([tok rec]
   (bind [pos get-position val (tok rec)]
     (return {:tok tok :value val :pos pos})))
  ([tok rec arg]
   (bind [pos get-position val ((tok rec) arg)]
     (return {:tok tok :vallue val :pos pos}))))


(def new-line   (lexer :new-line   rec))
(def identifier (lexer :identifier rec))
(def char-lit   (lexer :char-lit   rec))
(def string-lit (lexer :string-lit rec))
(def dec-lit    (lexer :dec-lit    rec))
(def oct-lit    (lexer :oct-lit    rec))
(def hex-lit    (lexer :hex-lit    rec))
(def float-lit  (lexer :float-lit  rec))
(def bool-lit   (lexer :bool-lit   rec))
(def nil-lit    (lexer :nil-lit    rec))
(def semi       (lexer :semi       rec))
(def comma      (lexer :comma      rec))
(def colon      (lexer :colon      rec))
(def dot        (lexer :dot        rec))

(defn sym     [x] (lexer :sym     rec x))
(defn one-of  [x] (lexer :one-of  rec x))
(defn none-of [x] (lexer :none-of rec x))
(defn token   [x] (lexer :token   rec x))
(defn word    [x] (lexer :word    rec x))
(defn field   [x] (lexer :field   rec x))


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
  (<|> identifier
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


(def unary  (prefix1* :UNIOP  factor uni-op))
(def power  (chainr1* :BINOP  unary  pow-op))
(def term   (chainl1* :BINOP  power  mul-op))
(def sum    (chainl1* :BINOP  term   add-op))
(def relex  (chainl1* :BINOP  sum    rel-op))
(def orex   (chainl1* :BINOP  relex  or-op))
(def expr   (chainl1* :BINOP  orex   and-op))


(def decl
  "Parses a declaration as a named value or funcion. The name should be
   placed at the start of a line to ensure it denotes a declaration."
  (bind [_     (word "def")
	 name  identifier
	 parms (many identifier)
	 _     (sym \=)
	 val   expr]
    (let [tok (if (seq parms) :defn :def)]
      (return {:tok tok :name (:value name) :parms parms :value val}))))


(def decls
  "Parses one or more declarations or expressions."
  (>> trim (many1 (<|> decl expr))))
