(use 'blancas.eisen.core)

;; To run from the REPL:
(comment
  (load-file "src/main/resources/hello/greeting.clj")
)

;; Definitions.

(def greeting "hello, %s!")
(def subject "world")

(defn greet
  "Greets someone or something."
  [g x] (println (format g x)))

;; Extensions.

(init-eisen)                                          ;; Initialize Eisen.
(host-module user)                                    ;; Provide names to user code.
(eisen-user "src/main/resources/hello/greeting.esn")  ;; Run user code.

;; Main program

(greet greeting subject)
