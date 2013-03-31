(use 'blancas.eisen.core)

;; To run from the REPL:
(comment
  (load-file "src/main/resources/hello/greeting.clj")
)

;; Definitions.

(def eisen-file "src/main/resources/hello/greeting.esn")

(def greeting "hello, %s!\n")  ;; Default form of the greeting.
(def subject "world")          ;; Default receiver of the greeting.

(defn greet
  "Greets someone or something."
  [g x] (printf g x))

;; Extensions.

(init-eisen)         ;; Initialize Eisen.
(host-module user)   ;; Add local names to eisen.user
(eisenf eisen-file)  ;; Run user code.

;; Main program

(greet greeting subject)
