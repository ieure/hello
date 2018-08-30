(in-package :hello)                     ; We have to tell Lisp what
                                        ; package this is in now.

(defun greet (whom)
  "Create a greeting message for WHOM."
  (format nil "Hello, ~A." whom))
