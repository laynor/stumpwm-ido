;;;; stumpwm-ido.asd

(asdf:defsystem #:stumpwm-ido
  :serial t
  :depends-on (#:cl-ppcre
               #:clx
	       #:cl-fad
	       #:pstrings
	       #:pstrings-xlib
               #:alexandria)
  :components ((:file "package")
	       (:file "utils")
	       (:file "completions")
	       (:file "matchers")
               (:file "stumpwm-ido")))

