(use 'blancas.eisen.core)

;; To run from the REPL:
(comment
  (load-file "src/main/resources/hello/ciao.clj")
)

;; Expected location of user code.
(def eisen-file "src/main/resources/hello/ciao.esn")

;; The host model
(->m greeting "Hello, %s!\n"  ;; Default form of the greeting.
     subject  "world"         ;; Default receiver of the greeting.
     hook     nil)            ;; Something to do before the greeting.

(defn greet
  "Greets someone or something."
  [g x] (printf g x))

;; Extensions.
(init-eisen)         ;; Setup the default Eisen configuration.
(eisenf eisen-file)  ;; Run user code from the well-known place.

;; Main program.

(run-> hook)                          ;; Run user-defined code.
(greet (m-> greeting) (m-> subject))  ;; Greets the subject.
