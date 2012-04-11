;;;; stumpwm-ido.asd

(asdf:defsystem #:stumpwm-ido
  :serial t
  :depends-on (#:cl-ppcre
               #:clx
	       #:cl-fad
	       #:pstrings
               #:alexandria)
  :components ((:file "package")
               (:file "stumpwm-ido")))

