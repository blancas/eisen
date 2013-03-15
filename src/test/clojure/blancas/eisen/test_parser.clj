;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.eisen.test-parser
  (:use [blancas.eisen core parser]
	[clojure.test]
	[midje.sweet :exclude (expect one-of)]))


;; +-------------------------------------------------------------+
;; |                      Lexer definitions.                     |
;; +-------------------------------------------------------------+


(deftest test-0000
  (fact "Decimal literal"
    (let [p1 (parse-eisen "787")]
      (-> p1 :decls first :value)) => 787
    (let [p1 (parse-eisen "10000000000000000000")]
      (-> p1 :decls first :value)) => 10000000000000000000N
    (let [p1 (parse-eisen "100N")]
      (-> p1 :decls first :value)) => 100N))


(deftest test-0005
  (fact "Octal literal"
    (let [p1 (parse-eisen "0644")]
      (-> p1 :decls first :value)) => 0644
    (let [p1 (parse-eisen "07777777777777777777777")]
      (-> p1 :decls first :value)) => 73786976294838206463N
    (let [p1 (parse-eisen "0100N")]
      (-> p1 :decls first :value)) => 0100N))


(deftest test-0010
  (fact "Hex literal"
    (let [p1 (parse-eisen "0xCAFE")]
      (-> p1 :decls first :value)) => 0xCAFE
    (let [p1 (parse-eisen "0xCAFEFFFFFFFFFFFF")]
      (-> p1 :decls first :value)) => 14627410114722660351N
    (let [p1 (parse-eisen "0xCAFEN")]
      (-> p1 :decls first :value)) => 0xCAFEN))


(deftest test-0015
  (fact "Floating-point literal"
    (let [p1 (parse-eisen "3.1415927")]
      (-> p1 :decls first :value)) => 3.1415927
    (let [p1 (parse-eisen "3.1415927e8")]
      (-> p1 :decls first :value)) => 314159270.0
    (let [p1 (parse-eisen "3.1415927M")]
      (-> p1 :decls first :value)) => 3.1415927M))


(deftest test-0020
  (fact "Character literal"
    (let [p1 (parse-eisen "'z'")]
      (-> p1 :decls first :value)) => \z
    (let [p1 (parse-eisen "'\\n'")]
      (-> p1 :decls first :value)) => \newline
    (let [p1 (parse-eisen "'\\''")]
      (-> p1 :decls first :value)) => \'))


(deftest test-0025
  (fact "String literal"
    (let [p1 (parse-eisen "\"z\"")]
      (-> p1 :decls first :value)) => "z"
    (let [p1 (parse-eisen "\"\\n\"")]
      (-> p1 :decls first :value)) => "\n"
    (let [p1 (parse-eisen "\"\\\"\"")]
      (-> p1 :decls first :value)) => "\""))


(deftest test-0030
  (fact "Boolean literal"
    (let [p1 (parse-eisen "true")]
      (-> p1 :decls first :value)) => true?
    (let [p1 (parse-eisen "false")]
      (-> p1 :decls first :value)) => false?))


(deftest test-0035
  (fact "Nil literal"
    (let [p1 (parse-eisen "nil")]
      (-> p1 :decls first :value)) => nil?
    (let [p1 (parse-eisen "null")]
      (-> p1 :decls first :value)) => nil?))


(deftest test-0040
  (fact "Clojure keyword."
    (let [p1 (parse-eisen ":foobar")]
      (-> p1 :decls first :value)) => :foobar))


(deftest test-0045
  (fact "Regular expression literal"
    (let [p1 (parse-eisen "#\"a*b\"")]
      (-> p1 :decls first :value)) => #"a*b"))


(deftest test-0050
  (fact "An eisen identifier."
    (let [p1 (parse-eisen "foobar")]
      (-> p1 :decls first :value)) => "foobar"
    (let [p1 (parse-eisen "_foobar")]
      (-> p1 :decls first :value)) => "_foobar"
    (let [p1 (parse-eisen "foo_bar123")]
      (-> p1 :decls first :value)) => "foo_bar123"
    (let [p1 (parse-eisen "_0123")]
      (-> p1 :decls first :value)) => "_0123"
    (let [p1 (parse-eisen "Foo")] ;; first capital is for data types
      (:ok p1) => false?)))


;; +-------------------------------------------------------------+
;; |                     Parser definitions.                     |
;; +-------------------------------------------------------------+


(deftest test-0100
  (fact "List literal"
    (let [p1 (eisen "[]")]
      (:value p1)) => ()
    (let [p1 (eisen "[737, 747, 757, 767, 777, 787]")]
      (:value p1)) => '(737 747 757 767 777 787)))


(deftest test-0105
  (fact "Vector literal"
    (let [p1 (eisen "#[]")]
      (:value p1)) => []
    (let [p1 (eisen "#[737, 747, 757, 767, 777, 787]")]
      (:value p1)) => [737 747 757 767 777 787]))


(deftest test-0110
  (fact "Set literal"
    (let [p1 (eisen "#{}")]
      (:value p1)) => #{}
    (let [p1 (eisen "#{737, 747, 757, 767, 777, 787}")]
      (:value p1)) => #{737 747 757 767 777 787}))


(deftest test-0115
  (fact "Map literal"
    (let [p1 (eisen "{}")]
      (:value p1)) => {}
    (let [p1 (eisen "{:k1 737, :k2 747, :k3 757, :k4 767, :k5 777, :k6 787}")]
      (:value p1)) => {:k1 737 :k2 747 :k3 757 :k4 767 :k5 777 :k6 787}))


(deftest test-0120
  (fact "Map literal with expressions"
    (let [p1 (eisen "{:k1 (700 + 37), :k2 (740 + 7 ), (str \"k\" 3) 757}")]
      (:value p1)) => {:k1 737 :k2 747 "k3" 757}))


(deftest test-0150
  (fact "List literal with a range"
    (let [p1 (eisen "[1 5]")]
      (:value p1)) => '(1 2 3 4 5)))


(deftest test-0155
  (fact "List range with expressions"
    (let [p1 (eisen "[1+3 5+3]")]
      (:value p1)) => '(4 5 6 7 8)))


(deftest test-0160
  (fact "List literal with a range"
    (let [p1 (eisen "let t160Low = 1; t160High = 5 in [(t160Low) (t160High)] end")]
      (:value p1)) => '(1 2 3 4 5)))


(deftest test-0180
  (fact "Vector literal with a range"
    (let [p1 (eisen "#[100 105]")]
      (:value p1)) => [100 101 102 103 104 105]))


(deftest test-0185
  (fact "Vector range with expressions"
    (let [p1 (eisen "#[100+5 105+5]")]
      (:value p1)) => [105 106 107 108 109 110]))


(deftest test-0190
  (fact "Vector range with expressions"
    (let [p1 (eisen "let t190Low = 100; t190High = 105 in #[(t190Low) (t190High)] end")]
      (:value p1)) => [100 101 102 103 104 105]))


;; +-------------------------------------------------------------+
;; |                          Comments.                          |
;; +-------------------------------------------------------------+

(deftest test-0300
  (eisen "val val1 (* why foo? *) = 787")
  (fact "Comments in (* xxx *)"
    (eisen= "val1") => 787))


(deftest test-0305
  (eisen "val val2 = (4 * (* is this right? *) (3 + 2))")
  (fact "Comments in (* xxx *)"
    (eisen= "val2") => 20))


(deftest test-0310
  (eisen "val val1 (* why (* not too late! *) foo? *) = 787")
  (fact "nested comments: (* x (* x *) x *)"
    (eisen= "val1") => 787))


(deftest test-0350
  (eisen "val val3 = 787 -- why foo?\nval bar = 747")
  (fact "line comments with: --"
	(eisen= "bar") => 747))
