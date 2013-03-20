(use 'blancas.eisen.core)

;; To run from the REPL:
(comment
  (load-file "src/main/resources/hello/salute.clj")
)

(def greeting "hello, %s!")
(def subject "world")

(def hook1)  ;; Something to do before the greeting.
(def hook2)  ;; Something to do after the greeting.

(defn greet
  "Greets someone or something."
  [g x] (println (format g x)))

(host-module user)
(clojure-core)
(eisen-user "src/main/resources/hello/salute.esn")

;; Run user-defined code.
(when (bound? (var hook1)) (hook1))

(greet greeting subject)

;; Run user-defined code.
(when (bound? (var hook2)) (hook2))
