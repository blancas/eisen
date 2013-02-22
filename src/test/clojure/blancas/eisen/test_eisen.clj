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
