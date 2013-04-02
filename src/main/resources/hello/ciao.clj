(use 'blancas.eisen.core)

;; To run from the REPL:
(comment
  (load-file "src/main/resources/hello/ciao.clj")
)

;; Definitions.

(def eisen-file "src/main/resources/hello/ciao.esn")

;; Host model
(->m greeting "Hello, %s!\n"  ;; Default form of the greeting.
     subject  "world"         ;; Default receiver of the greeting.
     hook     nil)            ;; Something to do before the greeting.

(defn greet
  "Greets someone or something."
  [g x] (printf g x))

;; Extensions.

(init-eisen)         ;; Initialize Eisen.
(eisenf eisen-file)  ;; Run user code from the well-known place.

;; Main program.

(when (m-> hook) ((m-> hook)))        ;; Run user-defined code.
(greet (m-> greeting) (m-> subject))  ;; Greets the subject.
