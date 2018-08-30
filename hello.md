<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#org4961f17">Introduction</a></li>
<li><a href="#org0384e93">Setup</a></li>
<li><a href="#org88c5aed">Background: Lisp environments &amp; images</a></li>
<li><a href="#org2894059">Version 1: Quick &amp; dirty</a></li>
<li><a href="#orga257465">The toplevel form</a></li>
<li><a href="#orgc18bc10">Packages</a></li>
<li><a href="#org3cecd79">Tying it all together</a></li>
<li><a href="#org98ce65b">Building an image</a></li>
<li><a href="#org5dc6f7f">Version 2: Structure</a></li>
<li><a href="#org3d7ae9e">Version 3: Systems</a></li>
<li><a href="#org7738672">Defining the system</a></li>
<li><a href="#org58c8952">Conclusion</a></li>
</ul>
</div>
</div>


<a id="org4961f17"></a>

# Introduction

One of the things which has kept Common Lisp out of my day-to-day
toolbox is a lack of clear instructions how to get up and running
with it — not in the REPL, but building programs that can be called
from the shell.  I tend to reach for Python or Emacs Lisp for a lot
of these cases, since I understand it and it’s readily available,
but I’ve always felt that Common Lisp could be a potent tool as
well.

After reading my friend Steve’s [Road to Common Lisp](http://stevelosh.com/blog/2018/08/a-road-to-common-lisp/), I was inspired
to finally understand this.  With some patient help from him, I
believe I’ve finally got a decent understanding.

Building a project in Lisp can be confusing, because Lisp itself
works so differently to other languages, and this affects how builds
work.  While Lisp can be compiled to machine code, it has more in
common with an interpreted language than a traditional ahead-of-time
compiled one like C.

This is not a tutorial on Lisp programming.  It is an attempt to
comprehensively explain the non-obvious practical aspects of
building Common Lisp programs: Where to put your source code, how to
make a binary, and how to use libraries.

Even though it’s not entirely accurate, I’m going to refer to Common
Lisp as "Lisp" throughout.


<a id="org0384e93"></a>

# Setup

If you wish to run the example code yourself, you’ll need to install
[Steel Bank Common Lisp (SBCL)](http://www.sbcl.org/) and [Quicklisp](https://www.quicklisp.org/).  On Debian-like
systems, this will do it:

    sudo apt install sbcl cl-quicklisp

&#x2026;but substitute with whatever the system-specfic method is.  Or
just read along a bit before you invest in a Lisp development setup.

I took a swing at [literate programming](https://en.wikipedia.org/wiki/Literate_programming) using org-babel for this<sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup>, so
all output is the direct result of running those commands on my
computer, and they can be replicated by anyone.  Both the original
Org document and the source extracted from it are on my
GitHub. (FIXME FIX FIX FIX)


<a id="org88c5aed"></a>

# Background: Lisp environments & images

Many compilers operate on a single source file at a time, producing
some kind of output representing the code it contains.  Because of
its roots as an interpreted language, Lisp does not work this way.

When you start Lisp, it initializes a **Lisp environment** in your
computer’s memory, then evaluates a **toplevel form**.  The
environment contains the Lisp language and tools; the standard
toplevel form is the REPL.  If you type code into the REPL, or load
code from a file, it’s added to the environment and can be used
inside it.  So far, this is pretty similar to how other interpreted
languages work.

Lisp differs from other interpreted languages in two important ways:
Firstly, the environment is much more comprehensive than other
languages.  Rather than separate parser, compiler, linker, and
debugger, these are built into the language, loaded into the
environment, and can be used by any Lisp programs, including yours.

This is an important point to understand.  Nearly every other
language is either unusable without multiple binaries which do
different things, or ships with a significant amount of
functionality locked up in programs which have to be run from a
shell.

For example, Python 3 ships with five binaries:

    dpkg -L python3-minimal | grep -c /bin/

    5

OpenJDK has 21:

    dpkg -L openjdk-8-jre-headless | grep -c /bin/

    21

GCC has 16:

    dpkg -L gcc | grep -c /bin/

    16

And in order to actually use GCC, you need binutils, which has
nearly 40 more:

    dpkg -L binutils | grep -c /bin/

    37

Can you guess how many Steel Bank Common Lisp (SBCL) has?

    dpkg -L sbcl | grep -c /bin/

    1

Just one, `/usr/bin/sbcl`.  Everything you can do with Lisp, you do
inside the Lisp environment.

Secondly, the environment can be saved to disk in a **Lisp image** (or
"core"), then restored from it at a later date.  When you save the
image, you can specify a toplevel form other than the REPL which
should be evaluated.

To make an executable Lisp program which you can run from a UNIX
shell, you load your code into the Lisp environment, then create an
image with the toplevel form set to your entry point.


<a id="org2894059"></a>

# Version 1: Quick & dirty

The goal is to make a traditional Hello, World program which will:

1.  Run from a shell.
2.  Use the first argument given to it as the name of the person or
    thing to greet.

Starting from the ground up, a function to create the greeting is
needed:

    (defun greet (whom)
      "Create a greeting message for WHOM."
      (format nil "Hello, ~A." whom))

Trying this in the REPL shows that it works:

    (greet "World")

    Hello, World.


<a id="orga257465"></a>

# The toplevel form

Satisfying the first requirement, running from the shell, means a
toplevel form is needed — this will be evaluated when the image is
restored.

    (defun main ()
      "Greet someone, or something."
      (write (greet (car (uiop:command-line-arguments))))
    
      (uiop:quit))

There are two functions in here that may be new to you,
`UIOP:COMMAND-LINE-ARGUMENTS` and `UIOP:QUIT`.  These are part of
ASDF, which we’ll cover in a bit, and provide a portable interface
to Lisp-implementation-specific behavior.  They pretty much do what they say on
the tin: `COMMAND-LINE-ARGUMENTS` evaluates to a list of arguments
given to the Lisp image, with each list element containing a single
argument; and `QUIT` terminates the Lisp process.


<a id="orgc18bc10"></a>

# Packages

The next piece to get a handle on is packages.  Packages are
containers for symbols — things like `MAIN` and `GREET` which we
defined earlier.

When the Lisp REPL starts, it plops you into the `COMMON-LISP-USER`
package, which is a scratch area you can safely tinker in without
wrecking the whole environment<sup><a id="fnr.2" class="footref" href="#fn.2">2</a></sup>.

For the Hello World program, it needs to be in its own package,
which I’ve creatively called `HELLO`.

    (defpackage :hello                      ; Define a package and name it HELLO
      (:use :common-lisp)                   ; The package needs Common Lisp
      (:export :greet :main))               ; This package has two public
                                            ; symbols, GREET and MAIN.
    
    (in-package :hello)                     ; DEFPACKAGE only defines the
                                            ; package; we must call
                                            ; IN-PACKAGE to switch to the
                                            ; context of the package we
                                            ; just defined.

The setup here is a little weird, because the whole declaration is a
forward reference:  The package has to be defined, and some symbols
inside the package enumerated, before any of them have been loaded
into the Lisp environment.

Starting with the `:USE` form, this tells Lisp that symbols from the
`COMMON-LISP` package should be made visible inside your package.
The form expects a list, so if you need multiple things, you’d do:

    (:use :common-lisp :foo :bar)

This has nothing to do with **loading** those packages — they have to
be loaded already, or you’ll get an error.  We’ll cover this in a
bit.

The entirety of the Common Lisp API exists inside the `COMMON-LISP`
package, and none of those symbols are visible from your package
unless you say you want them<sup><a id="fnr.3" class="footref" href="#fn.3">3</a></sup>, so you’ll want this in every
`DEFPACKAGE`.

Exported symbols are next, this list tells Lisp which things inside
your package should be usable by other packages, similar to `public`
/ `private` in C++ or Java.

You may note thatI’ve given the name of the package as `HELLO`,
which it is, but it’s in the code as `:hello`.  An explanation of
these discrepencies is out of scope, and you’ll just have to trust
that it’s right and I know what I’m doing<sup><a id="fnr.4" class="footref" href="#fn.4">4</a></sup>.


<a id="org3cecd79"></a>

# Tying it all together

The complete source for Hello World now looks like:

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


<a id="org98ce65b"></a>

# Building an image

Because the Lisp toolchain exists inside the Lisp environment, build
scripts for Lisp project are written in, you guessed it, Lisp.

    (load "hello.lisp")                     ; Load the code into the Lisp
                                            ; environment
    
    (save-lisp-and-die "hello"              ; Save a Lisp image
     :toplevel 'hello:main                  ; The toplevel function is
                                            ; MAIN, inside the HELLO
                                            ; package.
     :executable t)                         ; Make an executable.

For this toy example, this could just as easily be put at the end of
`hello.lisp`.  In this example’s sole not to practicality, it’s
going to go in `build.lisp`, which a more reasonable place.  If
`SAVE-LISP-AND-DIE` was in `hello.lisp`, and that file was loaded
into any Lisp environment, it would immediately terminate.  This is
unacceptably antisocial behavior, even for Lisp.

Executing the build script with `sbcl(1)` will produce the binary:

    sbcl --non-interactive --load build.lisp

    This is SBCL 1.3.14.debian, an implementation of ANSI Common Lisp.
    More information about SBCL is available at <http://www.sbcl.org/>.
    
    SBCL is free software, provided as is, with absolutely no warranty.
    It is mostly in the public domain; some portions are provided under
    BSD-style licenses.  See the CREDITS and COPYING files in the
    distribution for more information.
    [undoing binding stack and other enclosing state... done]
    [defragmenting immobile space... done]
    [saving current Lisp image into hello:
    writing 4800 bytes from the read-only space at 0x20000000
    writing 3216 bytes from the static space at 0x20100000
    writing 1245184 bytes from the immobile space at 0x20300000
    writing 13796160 bytes from the immobile space at 0x21b00000
    writing 37584896 bytes from the dynamic space at 0x1000000000
    done]

Running it shows about what we’d expect:

    ./hello World

    Hello, World.

Passing in the name of the current user also seems to work:

    ./hello $(whoami)

    Hello, ieure.

Now that the program works, and you hopefully understand why and
how, it’s time to tear it down and rebuild it.


<a id="org5dc6f7f"></a>

# Version 2: Structure

This is all fine for a toy, but larger programs benefit from more
organization.  If the core functionality is split from the CLI,
other Lisp projects can reuse the greeting without the CLI code.
Having the packages definition out of the way is a good idea, since
as a project grows, it can get unwieldy.  Since all this work will
produce multiple source files, the code making up the main
functionality ought to be separated from the code used to build the
system.

What this should look like is:

-   build.lisp
-   packages.lisp
    -   src/
        -   greet.lisp
        -   main.lisp

Even though the organization is different, the contents of the files
are almost exactly the same.

`build.lisp`

    (load "packages.lisp")                  ; Load package definition
    (load "src/greet.lisp")                 ; Load the core
    (load "src/main.lisp")                  ; Load the toplevel
    
    (save-lisp-and-die "hello"
     :toplevel 'hello:main
     :executable t)

`packages.lisp`

    (defpackage :hello                      ; Define a package and name it HELLO
      (:use :common-lisp)                   ; The package needs Common Lisp
      (:export :greet :main))               ; This package has two public
                                            ; symbols, GREET and MAIN.
    
    (in-package :hello)                     ; DEFPACKAGE only defines the
                                            ; package; we must call
                                            ; IN-PACKAGE to switch to the
                                            ; context of the package we
                                            ; just defined.

`src/greet.lisp`

    (in-package :hello)                     ; We have to tell Lisp what
                                            ; package this is in now.
    
    (defun greet (whom)
      "Create a greeting message for WHOM."
      (format nil "Hello, ~A." whom))

`src/main.lisp`

    (in-package :hello)
    
    (defun main ()
      "Greet someone, or something."
      (write (greet (car (uiop:command-line-arguments))))
    
      (uiop:quit))

Building and running works the same way:

    sbcl --non-interactive --load build.lisp
    ./hello World


<a id="org3d7ae9e"></a>

# Version 3: Systems

The next yak in this recursive shave is **systems**.  Whereas packages
are built into the Lisp language, systems are provided by a library,
[ASDF](https://common-lisp.net/project/asdf/), which means "Another System Definition Facility."

Systems and packages are orthogonal, but in a way that’s confusing,
because they both deal with some of the same things in your project.

A package is **a way of organizing the symbols of your project inside
the Lisp environment**.  Lisp doesn’t care if your package is
split between multiple files, or if a single file contains multiple
packages, it only cares that certain symbols live in certain
packages.

A system is **a description of how to load your project into the
environment**.  Because packages can be split or mixed however you
choose, you need a system to load the pieces in the right order.  In
our example, if you try to load `greet.lisp` before `packages.lisp`,
it will break, because the `HELLO` package hasn’t been defined.  Or
if you load `main.lisp` and not `greet.lisp`, it will break because
the `GREET` function isn’t defined.

Further complicating things, **one project can have multiple
systems**.  If you write unit tests, you’ll want a system for that,
because you need to load different things (your test code, the test
framework) in a different order (your test code, the test
framework).


<a id="org7738672"></a>

# Defining the system

Starting from the ground up again, this is the system which defines
the main `HELLO`, which contains the package definition and `GREET`.

    (defsystem :hello                       ; The system will be named
                                            ; HELLO, same as the project
      :serial t                             ; Load components in the same
                                            ; order they're defined.
      :components ((:file "packages")
                   (:module "src" ; A module is a collection of pieces of
                                  ; your program
                    :components ((:file "greet"))))) ; Load the greet
                                                     ; function from
                                                     ; greet.lisp. The
                                                     ; file extension is
                                                     ; implied, and must
                                                     ; not appear here.

And now a secondary system for the binary:

    (defsystem :hello/bin       ; The name HELLO/BIN indicates that this
                                ; is a secondary system of system HELLO.
      :depends-on (:hello)      ; This system needs the core HELLO system.
      :components ((:module :src
                    :components ((:file "main"))))) ; ...and includes one
                                                    ; additional file.

The whole thing should look like:

    (defsystem :hello                       ; The system will be named
                                            ; HELLO, same as the project
      :serial t                             ; Load components in the same
                                            ; order they're defined.
      :components ((:file "packages")
                   (:module "src" ; A module is a collection of pieces of
                                  ; your program
                    :components ((:file "greet"))))) ; Load the greet
                                                     ; function from
                                                     ; greet.lisp. The
                                                     ; file extension is
                                                     ; implied, and must
                                                     ; not appear here.
    
    (defsystem :hello/bin       ; The name HELLO/BIN indicates that this
                                ; is a secondary system of system HELLO.
      :depends-on (:hello)      ; This system needs the core HELLO system.
      :components ((:module :src
                    :components ((:file "main"))))) ; ...and includes one
                                                    ; additional file.

In the build script, ASDF’s loader can be used instead of loading
the pieces manually:

    (asdf:load-system :hello/bin)
    
    (save-lisp-and-die "hello"
     :toplevel 'hello:main
     :executable t)

In order for ASDF to know where the files for your system live, you
need to make a symlink.  This is easily the grossest thing about
this entire setup.

    ln -sf $PWD/v3 ~/quicklisp/local-projects/hello

The rest of the source is unchanged from v2.

    (defpackage :hello                      ; Define a package and name it HELLO
      (:use :common-lisp)                   ; The package needs Common Lisp
      (:export :greet :main))               ; This package has two public
                                            ; symbols, GREET and MAIN.
    
    (in-package :hello)                     ; DEFPACKAGE only defines the
                                            ; package; we must call
                                            ; IN-PACKAGE to switch to the
                                            ; context of the package we
                                            ; just defined.

    (in-package :hello)                     ; We have to tell Lisp what
                                            ; package this is in now.
    
    (defun greet (whom)
      "Create a greeting message for WHOM."
      (format nil "Hello, ~A." whom))

    (in-package :hello)
    
    (defun main ()
      "Greet someone, or something."
      (write (greet (car (uiop:command-line-arguments))))
    
      (uiop:quit))

    sbcl --non-interactive --load build.lisp
    ./hello World


<a id="org58c8952"></a>

# Conclusion

That is all.  I hope this has been instructive, and many people will
go forth with the desire and ability to use Common Lisp more.


# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> Because of course I did.  Throw another yak on the pile.

<sup><a id="fn.2" href="#fnr.2">2</a></sup> It is **absolutely** possible to wreck the Lisp environment if
your’re not careful, so this is a good thing.  For example, if you
eval:

    (in-package :common-lisp)
    (fmakunbound 'defun)

It will remove the function binding from the `DEFUN` symbol, with the
upshot that you can’t define new functions.  Oops.

<sup><a id="fn.3" href="#fnr.3">3</a></sup> It’s possible to create a package which doesn’t use symbols
from `COMMON-LISP`, but you won’t get much done, since you have no way
to define functions, set variables, or build lists.

<sup><a id="fn.4" href="#fnr.4">4</a></sup> I have absolutely no idea what I’m doing.
