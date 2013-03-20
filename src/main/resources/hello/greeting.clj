(use 'blancas.eisen.core)

;; To run from the REPL:
(comment
  (load-file "src/main/resources/hello/greeting.clj")
)

(def greeting "hello, %s!")
(def subject "world")

(defn greet
  "Greets someone or something."
  [g x] (println (format g x)))

(host-module user)
(clojure-core)
(eisen-user "src/main/resources/hello/greeting.esn")

(greet greeting subject)
