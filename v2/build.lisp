(load "packages.lisp")                  ; Load package definition
(load "src/greet.lisp")                 ; Load the core
(load "src/main.lisp")                  ; Load the toplevel

(save-lisp-and-die "hello"
 :toplevel 'hello:main
 :executable t)
