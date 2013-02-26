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
  "Runs the supplied Eisen code and shows the resulting value.
   Intended to evaluating expressions at the REPL."
  [text] (:value (eisen text)))


(defn eisen*
  "Parses the supplied Eisen code as a dry run; discards the parsed results.
   Any errors are printed to stdout; intended for testing at the REPL."
  ([text] (eisen* text ""))
  ([text source] (run eisen-code text source)))


(defn eisenf
  "Translates an eisen file; takes an optional encoding, which
   defaults to UTF-8."
  ([f] (eisenf f "UTF-8"))
  ([f en] (eisen (f->s f en) f)))


(defn eisenf*
  "Parses an eisen file as a dry run; discards the parsed results.
   Takes an optional encoding, which defaults to UTF-8.
   Any errors are printed to stdout; intended for testing at the REPL."
  ([f] (eisenf* f "UTF-8"))
  ([f en] (eisen* (f->s f en) f)))
