(defpackage :hello                      ; Define a package and name it HELLO
  (:use :common-lisp)                   ; The package needs Common Lisp
  (:export :greet :main))               ; This package has two public
                                        ; symbols, GREET and MAIN.

(in-package :hello)                     ; DEFPACKAGE only defines the
                                        ; package; we must call
                                        ; IN-PACKAGE to switch to the
                                        ; context of the package we
                                        ; just defined.

(defun greet (whom)
  "Create a greeting message for WHOM."
  (format nil "Hello, ~A." whom))

(defun main ()
  "Greet someone, or something."
  (write (greet (car (uiop:command-line-arguments))))

  (uiop:quit))
