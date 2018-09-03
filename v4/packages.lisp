(defpackage :hello                      ; Define a package and name it HELLO
  (:use :common-lisp)                   ; The package needs Common Lisp
  (:export :greet :main))               ; This package has two public
                                        ; symbols, GREET and MAIN.

(in-package :hello)                     ; DEFPACKAGE only defines the
                                        ; package, it doesn't make it
                                        ; active.  You have to call
                                        ; IN-PACKAGE to switch.
