;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "The Eisen Parser.

The Eisen Lexer is configured with the following settings:

comment-start        {-
comment-end          -}
comment-line         --
nested-comments      Yes
identifier-start     Lowercase or _
identifier-letter    Alphanumeric or _ ' ? !
reserved-names       None
case-sensitive       Yes
line-continuation    Backslash
trim-newline         Yes

Literal values follow the rules of Java and Clojure."
      :author "Armando Blancas"}
  blancas.eisen.parser
  (:use [blancas.kern core i18n]
	[blancas.kern.expr :only (prefix1* chainl1* chainr1*)])
  (:require [blancas.kern.lexer :as lex]))


;; +-------------------------------------------------------------+
;; |                      Lexer definitions.                     |
;; +-------------------------------------------------------------+

(def eisen-style
  "Lexical settings for the Eisen language."
  (assoc lex/haskell-style
    :identifier-start   (<|> lower (sym* \_))
    :identifier-letter  (<|> alpha-num (one-of* "_'?!"))
    :reserved-names     ["_" "val" "fun" "if" "then" "else"]))


(def rec (lex/make-parsers eisen-style))


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
  ([p]
   (bind [pos get-position val p]
     (return {:token :word :value val :pos pos})))
  ([tok rec]
   (bind [pos get-position val (tok rec)]
     (return {:token tok :value val :pos pos})))
  ([tok rec & args]
   (bind [pos get-position val (apply (tok rec) args)]
     (return {:token tok :value val :pos pos}))))


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
(defn field   [x] (lexer :field   rec x))

(defn token   [x & more] (apply lexer :token rec x more)) 
(defn word    [x & more] (apply lexer :word  rec x more))


(def key-name
  "Parses a Clojure keyword."
  (bind [pos get-position
	 key (<$> keyword
	          (>> (sym* \:)
		      (<+> (lexeme (many1 (none-of* " `~@%^*()[]{};\"\\,"))))))]
    (return {:token :keyword :value key :pos pos})))


(def id-formal
  "Parses an identifier as a formal parameter."
  (bind [pos get-position id (<|> (word "_") identifier)]
    (return (assoc id :token :id-formal))))


(def id-arg
  "Parses an identifier as an argument; as such it won't be called
   if it refers to a function."
  (bind [pos get-position arg identifier]
    (return (assoc arg :token :id-arg))))


(def lisp-id
  "Parses a lisp id with extra characters."
  (let [fst (<|> letter (one-of* "!$*-_+=<>?"))
        rst (<|> fst digit (sym* \'))]
    (<+> fst (many rst))))


(def lisp-name
  "Parses a lisp name between dots to avoid interference with eisen."
  (bind [pos get-position
	 val (lexeme (between (sym* \.) (sym* \.) lisp-id))]
    (return {:token :identifier :value val :pos pos})))


;; Custom parsing of numeric literals for reading function arguments.
;; In these cases the parser must not allow a leading sing as part
;; of the literal, as it interferes with the overall arithmetic.

(def sign (optional (one-of* "+-")))

(def int-suffix (<|> (<< (sym* \N) (not-followed-by letter))
		     (not-followed-by (<|> letter (sym* \.)))))

(def float-suffix (<< (optional (sym* \M)) (not-followed-by letter)))

(def custom-dec-lit
  (<?> (>>= (<:> (lexeme (<+> (many1 digit) int-suffix)))
            (fn [x] (return (read-string x))))
       (i18n :dec-lit)))
	
(def custom-oct-lit
  (<?> (>>= (<:> (lexeme (<+> (sym* \0) (many oct-digit) int-suffix)))
            (fn [x] (return (read-string x))))
       (i18n :oct-lit)))

(def custom-hex-lit
  (<?> (>>= (<:> (lexeme (<+> (token- "0x") (many1 hex-digit) int-suffix)))
            (fn [x] (return (read-string x))))
       (i18n :hex-lit)))

(def custom-flt-lit
  (<?> (>>= (<:> (lexeme
		   (<+> (many1 digit)
	                (option ".0" (<*> (sym* \.) (many1 digit)))
	                (optional (<*> (one-of* "eE") sign (many1 digit)))
			float-suffix)))
            (fn [x] (return (read-string x))))
       (i18n :float-lit)))


(def dec-lit*
  "Parses a decimal literal, with no leading sign."
  (bind [pos get-position val custom-dec-lit]
    (return {:token :dec-lit :value val :pos pos})))


(def oct-lit*
  "Parses an octal literal, with no leading sign."
  (bind [pos get-position val custom-oct-lit]
    (return {:token :oct-lit :value val :pos pos})))


(def hex-lit*
  "Parses a hex literal, with no leading sign."
  (bind [pos get-position val custom-hex-lit]
    (return {:token :hex-lit :value val :pos pos})))


(def flt-lit*
  "Parses a floating-point literal, with no leading sign."
  (bind [pos get-position val custom-flt-lit]
    (return {:token :float-lit :value val :pos pos})))


;; +-------------------------------------------------------------+
;; |                     Parser definitions.                     |
;; +-------------------------------------------------------------+


(declare expr)


(def list-lit
  "Parses a list literal."
  (bind [pos get-position
	 val (brackets (comma-sep (fwd expr)))]
    (return {:token :list-lit :value val :pos pos})))


(def vector-lit
  "Parses a vector literal."
  (<:> (bind [pos get-position
	      val (>> (sym* \#)
		      (brackets (comma-sep (fwd expr))))]
         (return {:token :vector-lit :value val :pos pos}))))     


(def set-lit
  "Parses a set literal."
  (<:> (bind [pos get-position
	      val (>> (sym* \#)
		      (braces (comma-sep (fwd expr))))]
         (return {:token :set-lit :value val :pos pos}))))


(def map-lit
  "Parses a map literal."
  (bind [pos get-position
	 val (braces (comma-sep (fwd expr)))]
    (return {:token :map-lit :value val :pos pos})))


(def re-lit
  "Parses a regular-expression literal."
  (<:> (bind [pos get-position
	      reg (>> (sym* \#) string-lit)]
         (let [val (str "#\"" (:value reg) "\"")]
           (return {:token :re-lit :value (read-string val) :pos pos})))))


(def argument
  "An argument to a function call; this does not include a function call
   directly, but only through an expression in parenthesis."
  (<|> key-name
       char-lit
       string-lit
       dec-lit*
       oct-lit*
       hex-lit*
       flt-lit*
       bool-lit
       nil-lit
       list-lit
       vector-lit
       set-lit
       map-lit
       re-lit
       id-arg
       (parens (fwd expr))))


(def val-call
  "Parses a reference to a value or a function call."
  (bind [name (<|> lisp-name identifier) args (many argument)]
    (if (empty? args)
      (return name)  
      (return {:token :fun-call :value (into [name] args)}))))


(def factor
  "A factor is an operand within an expression."
  (<|> key-name
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
       val-call
       (parens (fwd expr))))


(def pow-op
  "Power-of operator; implemented as a custom funcion exp."
  (>> (token "**") (lexer (return "blancas.eisen.trans/exp"))))


(def uni-op
  "Unary operators not, bitwise not, plus, minus."
  (<|> (>> (sym \!) (lexer (return "not")))
       (>> (sym \~) (lexer (return "bit-not")))
       (one-of  "+-")))


(def back-op
  "Parses the backquoted name of an arity-2 function as a binary
   operator; for eisen or lisp functions."
  (lexer (lexeme (between (sym* \`) (sym* \`) lisp-id))))


(def mul-op
  "Multiplicative operators; a backslash denotes (quot);
   % denotes (mod); * denotes multiplication; and / denotes
   both division and integer ratio."
  (<|> (>> (sym \\) (lexer (return "quot")))
       (>> (sym \%) (lexer (return "mod")))
       (one-of  "*/")))


(def add-op
  "Additive operators plus and minus."
  (<|> (sym \-)
       (<:> (<< (sym \+) (not-followed-by (sym \+))))))


(def cons-op
  "List construction operator."
  (>> (sym \:) (lexer (return "blancas.eisen.trans/conj-rev"))))


(def lcat-op
  "List concatenation operator."
  (>> (token "++") (lexer (return "concat"))))


(def shft-op
  "Bitwise shift operators; << denotes (bit-shift-left);
   >> denotes (bit-shift-right)."
  (<|> (>> (token "<<") (lexer (return "bit-shift-left")))
       (>> (token ">>") (lexer (return "bit-shift-right")))))


(def band-op
  "Operator bitwise and."
  (<:> (>> (sym* \&) (not-followed-by (sym* \&)) trim (lexer (return "bit-and")))))


(def bxor-op
  "Opertor bitwise xor."
  (>> (sym \^) (lexer (return "bit-xor"))))


(def bor-op
  "Opertor bitwise or."
  (<:> (>> (sym* \|) (not-followed-by (sym* \|)) trim (lexer (return "bit-or")))))


(def rel-op
  "Relational operators."
  (token ">=" "<=" ">" "<"))


(def equ-op
  "Equality operators; == denotes (=); != denotes not=."
  (<|> (>> (token "==") (lexer (return "=")))
       (>> (token "!=") (lexer (return "not=")))))


(def and-op
  "Operator logical and."
  (>> (token "&&") (lexer (return "and"))))


(def or-op
  "Operator logical or."
  (>> (token "||") (lexer (return "or"))))


;;
;; Operator chaining, from highest to lowest precedence.
;;

(def power  (chainr1* :BINOP  factor pow-op))
(def unary  (prefix1* :UNIOP  power  uni-op))
(def fbin   (chainl1* :BINOP  unary  back-op))
(def term   (chainl1* :BINOP  fbin   mul-op))
(def sum    (chainl1* :BINOP  term   add-op))
(def const  (chainr1* :BINOP  sum    cons-op))
(def cat    (chainl1* :BINOP  const  lcat-op))
(def shift  (chainl1* :BINOP  cat    shft-op))
(def band   (chainl1* :BINOP  shift  band-op))
(def bxor   (chainl1* :BINOP  band   bxor-op))
(def bor    (chainl1* :BINOP  bxor   bor-op))
(def relex  (chainl1* :BINOP  bor    rel-op))
(def equ    (chainl1* :BINOP  relex  equ-op))
(def andex  (chainl1* :BINOP  equ    and-op))
(def orex   (chainl1* :BINOP  andex  or-op))


(def seqex
  "Parses sequenced expressions: one or more expressions in
   parenthesis and separated by semicolons. The value of the
   compound expression is the value of its last expression."
  (bind [xs (parens (semi-sep orex))]
    (return {:token :seq-expr :value xs})))


(def condex
  "Parses a conditional expression."
  (bind [test (>> (word "if") orex)
	 then (>> (word "then") (fwd expr))
	 else (optional (>> (word "else") (fwd expr)))]
    (return {:token :cond-expr :test test :then then :else else})))
	 

(def expr
  "Parses an Eisen expression."
  (<|> seqex condex orex))


(def def-decl
  "Parses a declaration for a named value."
  (>> (word "val")
      (semi-sep1
        (bind [name identifier  _ (sym \=)  val expr]
          (return {:token :val :name (:value name) :value val})))))


(def fun-decl
  "Parses a function definition."
  (bind [_     (word "fun")
	 name  identifier
	 parm  (many id-formal)
	 _     (sym \=)
	 val   expr]
    (return {:token :fun :name (:value name) :params parm :value val})))


(def eisen-code
  "Parses one or more declarations, or a single expressions."
  (>> trim
      (<|> (<$> flatten (many1 (<|> def-decl fun-decl)))
	   (<$> vector expr))))
