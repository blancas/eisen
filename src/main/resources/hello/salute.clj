(use 'blancas.eisen.core)

;; To run from the REPL:
(comment
  (load-file "src/main/resources/hello/salute.clj")
)

;; Definitions.

(def eisen-file "src/main/resources/hello/salute.esn")

(def greeting "hello, %s!\n")  ;; Default form of the greeting.
(def subject "world")          ;; Default receiver of the greeting.

(def hook1)                    ;; Something to do before the greeting.

(defn greet
  "Greets someone or something."
  [g x] (printf g x))

;; Extensions.

(init-eisen)             ;; Initialize Eisen.
(host-module user)       ;; Provide names to user code.
(eisen-user eisen-file)  ;; Run user code from the well-known place.

;; Main program.

(when (bound? (var hook1)) (hook1))  ;; Run user-defined code.
(greet greeting subject)             ;; Greets the subject.
