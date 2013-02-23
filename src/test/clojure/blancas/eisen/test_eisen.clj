;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns blancas.eisen.test-eisen
  (:use [blancas.eisen.core]
	[clojure.test]
	[midje.sweet :exclude (expect one-of)]))


;; +-------------------------------------------------------------+
;; |                       Eisen Operators.                      |
;; +-------------------------------------------------------------+


(deftest test-0000
  (fact "power of"
    (let [p1 (eisen "2 ** 8")]
      (:value p1)) => 256
    (let [p1 (eisen "5 ** 2 ** 3")]
      (:value p1)) => 390625))


(deftest test-0005
  (fact "logical not"
    (let [p1 (eisen "!true")]
      (:value p1)) => false
    (let [p1 (eisen "!false")]
      (:value p1)) => true
    (let [p1 (eisen "!!true")]
      (:value p1)) => true))


(deftest test-0010
  (fact "bitwise not"
    (let [p1 (eisen "~0xF0F0F0F0 & 0xFFFFFFFF")]
      (:value p1)) => 0xF0F0F0F))


(deftest test-0015
  (fact "unary plus"
    (let [p1 (eisen "+500")]
      (:value p1)) => 500))


(deftest test-0020
  (fact "unary minus"
    (let [p1 (eisen "-500")]
      (:value p1)) => -500))


(deftest test-0025
  (fact "integer division -- quot"
    (let [p1 (eisen "11 \\ 3")]
      (:value p1)) => 3))


(deftest test-0030
  (fact "modulo -- mod"
    (let [p1 (eisen "11 % 3")]
      (:value p1)) => 2))


(deftest test-0035
  (fact "multiplication"
    (let [p1 (eisen "11 * 3")]
      (:value p1)) => 33
    (let [p1 (eisen "11.0 * 3")]
      (:value p1)) => 33.0
    (let [p1 (eisen "6 * (1 /3)")]
      (:value p1)) => 2))


(deftest test-0040
  (fact "division with floating-point numbers"
    (let [p1 (eisen "5.0 / 2.0")]
      (:value p1)) => 2.5
    (let [p1 (eisen "5.0 / 2")]
      (:value p1)) => 2.5
    (let [p1 (eisen "5 / 2.0")]
      (:value p1)) => 2.5))


(deftest test-0045
  (fact "ratio with integer numbers"
    (let [p1 (eisen "11 / 3")]
      (:value p1)) => (/ 11 3)))


(deftest test-0050
  (fact "addition"
    (let [p1 (eisen "3 + 4")]
      (:value p1)) => 7
    (let [p1 (eisen "11.0 + 3.0")]
      (:value p1)) => 14.0
    (let [p1 (eisen "3 / 4 + 2 / 8")]
      (:value p1)) => 1))


(deftest test-0055
  (fact "subtraction"
    (let [p1 (eisen "10 - 4")]
      (:value p1)) => 6
    (let [p1 (eisen "4 - 10")]
      (:value p1)) => -6
    (let [p1 (eisen "4 - -10")]
      (:value p1)) => 14
    (let [p1 (eisen "11.0 - 3.0")]
      (:value p1)) => 8.0
    (let [p1 (eisen "3 / 4 - 2 / 8")]
      (:value p1)) => (/ 1 2)))


(deftest test-0055
  (fact "construct (:) -- flip conj"
    (let [p1 (eisen "0 : [1,2,3]")]
      (:value p1)) => '(0 1 2 3)
    (let [p1 (eisen "0:[1,2,3]")]
      (:value p1)) => '(0 1 2 3)
    (let [p1 (eisen "0 : 1 : 2 : 3 : []")]
      (:value p1)) => '(0 1 2 3)
    (let [p1 (eisen "0:1:2:3:[]")]
      (:value p1)) => '(0 1 2 3)))


(deftest test-0100
  (fact "concat (++)"
    (let [p1 (eisen "[0,1,2] ++ [3,4,5]")]
      (:value p1)) => '(0 1 2 3 4 5)))
