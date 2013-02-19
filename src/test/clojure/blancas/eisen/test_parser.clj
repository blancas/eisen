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
      (-> p1 :decls first :value)) => 787))


(deftest test-0005
  (fact "Octal literal"
    (let [p1 (parse-eisen "0644")]
      (-> p1 :decls first :value)) => 0644))


(deftest test-0010
  (fact "Hex literal"
    (let [p1 (parse-eisen "0xCAFE")]
      (-> p1 :decls first :value)) => 0xCAFE))


(deftest test-0015
  (fact "Floating-point literal"
    (let [p1 (parse-eisen "3.1415927")]
      (-> p1 :decls first :value)) => 3.1415927))


(deftest test-0020
  (fact "Character literal"
    (let [p1 (parse-eisen "'z'")]
      (-> p1 :decls first :value)) => \z))


(deftest test-0025
  (fact "String literal"
    (let [p1 (parse-eisen "\"z\"")]
      (-> p1 :decls first :value)) => "z"))


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
      (-> p1 :decls first :value)) => "foobar"))


;; +-------------------------------------------------------------+
;; |                     Parser definitions.                     |
;; +-------------------------------------------------------------+


(deftest test-0100
  (fact "List literal"
    (let [p1 (trans-eisen "[737, 747, 757, 767, 777, 787]")]
      (-> p1 :decls first eval)) => '(737 747 757 767 777 787)))


(deftest test-0105
  (fact "Vector literal"
    (let [p1 (trans-eisen "#[737, 747, 757, 767, 777, 787]")]
      (-> p1 :decls first eval)) => [737 747 757 767 777 787]))


(deftest test-0110
  (fact "Set literal"
    (let [p1 (trans-eisen "#{737, 747, 757, 767, 777, 787}")]
      (-> p1 :decls first eval)) => #{737 747 757 767 777 787}))


(deftest test-0115
  (fact "Map literal"
    (let [p1 (trans-eisen "{:k1, 737, :k2, 747, :k3, 757, :k4, 767, :k5, 777, :k6, 787}")]
      (-> p1 :decls first eval)) => {:k1 737 :k2 747 :k3 757 :k4 767 :k5 777 :k6 787}))
