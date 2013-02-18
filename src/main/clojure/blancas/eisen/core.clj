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
  (:use [blancas.eisen.parser :only (decls)]
	[blancas.eisen.trans :only (trans)]
        [blancas.kern.core :only (parse run print-error f->s)]))


(defn parse-eisen
  "Parses the supplied text; returns a sequence objects to translate,
   each either a top-level declaration or expression."
  ([text]
   (parse-eisen text ""))
  ([text source]
   (let [st (parse decls text source)]
     (if (:ok st)
       {:ok true :decls (:value st) :error nil}
       {:ok false :decls nil :error (with-out-str (print-error st))}))))


(defn trans-eisen
  "Translates the supplied AST. Returns a map with three fields:
   :ok     true on success; false otherwise
   :value  the value of the last expression
   :error  error or warning message; not nil if :ok is false"
  [decls]
  (let [result (trans (first decls))]
    (if (:ok result)
      (assoc result :value (eval (:value result)))
      result)))


  (defn eisen
  "Translates the supplied text. Returns a map with three fields:
   :ok     true on success; false otherwise
   :value  the value of the last expression
   :error  error or warning message; not nil if :ok is false"
  ([text]
   (eisen text ""))
  ([text source]
   (let [st (parse-eisen text source)]
     (if (:ok st)
       (trans-eisen (:decls st))
       st))))


(defn eisen*
  "Parses the supplied text as a dry run; discards the parsed results.
   Any errors are printed to stdout; intended for testing at the REPL."
  ([text] (eisen* text ""))
  ([text source] (run decls text source)))


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
