(ql:quickload "unix-opts")
(asdf:load-system :unix-opts)           ; Load the library

(unix-opts:define-opts
  (:name :help
   :description "Print this help text"
   :short #\h
   :long "help"))

(defun main ()
  "Greet someone, or something."
  (multiple-value-bind (options free-args)
      (unix-opts:get-opts)
    (if (or (getf options :help) (not (= (length free-args 1))))
        (unix-opts:describe
         :prefix "A Hello World program."
         :args "WHOM")
        (write (greet (car free-args))))

  (uiop:quit))
