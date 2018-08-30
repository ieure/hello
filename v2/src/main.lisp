(in-package :hello)

(defun main ()
  "Greet someone, or something."
  (write (greet (car (uiop:command-line-arguments))))

  (uiop:quit))
