(asdf:load-system :hello/bin)

(save-lisp-and-die "hello"
 :toplevel 'hello:main
 :executable t)
