;; Copyright (c) 2013 Armando Blancas. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Eisen Expressions for Clojure Core Macros"
      :author "Armando Blancas"}
  blancas.eisen.clojure
  (:use [blancas.kern core i18n]
	[blancas.eisen parser trans]))


(def whenex
  "Parses a when expression."
  (bind [test (>> (word "when") orex)
	 body doex]
    (return {:token :when-expr :test test :body then})))
