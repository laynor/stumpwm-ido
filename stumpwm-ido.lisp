;;;; stumpwm-ido.lisp

(in-package #:stumpwm-ido)

(defvar *completions-set* nil)
(defvar *ido-match-function* *ido-flex-matcher*)

(declaim (optimize (speed 0) (debug 3)))

(defvar *ido-max-inline-completions* 30
  "Maximum number of completions displayed in the input window. This
should be set reasonably low as it impacts the speed of the completion
formatting function.")

(pstr:defface ido-matches-separator
  :documentation "Face for symbols used when displaying completions matches."
  :foreground "red")

(pstr:defface ido-matches
  :documentation "Face used by ido for displaying normal matches."
  :foreground "Gray80")

(pstr:defface ido-exact-match
  :documentation "Face used by ido for highlighting an exact match."
  :foreground "green")

(pstr:defface ido-first-match
  :documentation "Face used by ido for highlighting the first match in the list."
  :weight "bold")

(pstr:defface ido-matches-alt-1
  :documentation "Face used by ido for highlighting matches having :match-type
property set to 1."
  :foreground "blue")

(pstr:defface ido-matches-alt-2
  :documentation "Face used by ido for highlighting matches having :match-type
property set to 2."
  :foreground "violet")

(pstrings:defface :default
  :family "terminus"
  :pixel-size 12
  :foreground "white"
  :slant "r")

(pstrings:defface :cursor
  :background "magenta"
  :foreground "black")

(pstrings:defface :title1
  :pixel-size 24
  :foreground "red")

(pstrings:defface :title2
  :pixel-size 20
  :inherit '(:title1))

(pstrings:defface :title3
  :pixel-size 16)

(pstrings:defface :emph
  :inherit '(:italic))

(pstrings:defface :input-prompt
  :foreground "green"
  :weight "bold")


;;; FIXME: Completions should not be calculated every time, but only when input changes
;;; TODO: Make it possible to catch changes in the input text
;;;       This can probably be done checking what the last command was, and update the
;;;       completions only in that case. The current completion list should be stored
;;;       somewhere.


;;; Symbols internal in STUMPWM used:
;;; screen-font
;;; screen-input-window
;;; screen-message-gc
;;; screen-width
;;; make-input-string
;;; input-line-string
;;; input-line-position
;;; input-line-password
;;; input-find-completions FIXME
;;; input-insert-string
;;; is-modifier
;;; *input-completions*
;;; read-key-or-selections
;;; code-state->key
;;; *input-last-command*
;;; *input-map*
;;; *input-history*
;;; shutdown-input-window

(defun ido-setup-input-window (screen prompt input)
  "Set the window input up to read input."
  (let ((height (+ (xlib:font-descent (stumpwm::screen-font screen))  ;FIXME: calculate height for input (pstring)
		   (xlib:font-descent (stumpwm::screen-font screen))))
	(win (stumpwm::screen-input-window screen)))
    ;; window dimensions
    (xlib:with-state (win)
      (setf (xlib:window-priority win) :above
	    (xlib:drawable-height win) height))
    ;; Draw the prompt
    (xlib:map-window win)
    (ido-draw-input-bucket screen prompt input)))

(defun propertize-match (match)
  (let ((face (case (pstr:pstring-get-property match :match-type 0)
		(nil :ido-matches)
		(1 :ido-matches-alt-1)
		(2 :ido-matches-alt-2))))
    (pstr:pstring-propertize match :face face)))

(defun format-matches (completions)
  (flet ((csymbol (text)
	   (pstr:pstring-propertize text :face :ido-matches-separator)))
    (reduce #'pstr:pstring-concat
	    (nconc (list (csymbol "{ "))
		   (butlast (mapcan (lambda (match)
				      (list (propertize-match match)
					    (pstr:pstring-propertize " | " :face :ido-matches-separator)))
				    completions))
		   (list (csymbol " }"))))))

(defun ido-inline-completions (prompt input completions screen)
  (let ((gcontext (stumpwm::screen-message-gc screen))
	(maxwidth (- (stumpwm::screen-width screen) (* 2 stumpwm:*message-window-padding*)))
	(n (length completions)))
    (flet ((make-display-string (comps)
	     (pstr:pstring-concat prompt (stumpwm::input-line-string input) " " (format-matches comps) " ... ")))
      (do ((comps (reverse (subseq completions 0 (min *ido-max-inline-completions* n))) (cdr comps)))
	  ((< (pstr:xlib-pstring-extents gcontext (make-display-string comps)) maxwidth)
	   (reverse (append (and (> n *ido-max-inline-completions*) (list "...")) comps)))))))

(defun ido-process-input (screen prompt input code state)
  "Process the key (code and state) given the current input
buffer. Returns a new modified input buffer."
  (labels ((process-key (code state)
	     "Call the appropriate function based on the key
pressed. Return 'done when the user has signalled the finish of his
input (pressing Return), nil otherwise."
	     (let* ((key (stumpwm::code-state->key code state))
		    (command (and key (stumpwm::lookup-key stumpwm::*input-map* key t))))
	       (if command
		   (prog1
		       (funcall command input key)
		     (setf stumpwm::*input-last-command* command))
		   :error)))
	   (completions () (ido-input-completions (stumpwm::input-line-string input)
						  (stumpwm::input-line-position input)
						  *completions-set*
						  *ido-match-function*)))
    (case (process-key code state)
      (:done
       (unless (or (stumpwm::input-line-password input)
		   (and stumpwm:*input-history-ignore-duplicates*
			(string= (stumpwm::input-line-string input) (first stumpwm::*input-history*))))
	 (push (stumpwm::input-line-string input) stumpwm::*input-history*))
       :done)
      (:abort
       (throw :abort t))
      (:error
       ;; FIXME draw inverted text
       (ido-draw-input-bucket screen prompt input (format-matches
						   (ido-inline-completions prompt input (completions)
									   screen)) t)
       nil)
      (t (ido-draw-input-bucket screen prompt input (format-matches
						     (ido-inline-completions prompt input (completions)
									     screen)))
	 nil))))

(defun ido-read-one-line (screen prompt &key (initial-input "") require-match password)
  "Read a line of input through stumpwm and return it. Returns nil if
the user aborted."
  (let ((stumpwm::*input-last-command* nil)
	(input (stumpwm::make-input-line :string (stumpwm::make-input-string initial-input)
					 :position (length initial-input)
					 :history -1
					 :password password)))
    (labels ((match-input ()
	       (let* ((in (string-trim " " (stumpwm::input-line-string input)))
		      (compls (stumpwm::input-find-completions in stumpwm::*input-completions*)))
		 (and (consp compls)
		      (string= in (car compls)))))
	     (key-loop ()
	       (loop for key = (stumpwm::read-key-or-selection) do
		    (let ((completions (ido-input-completions (stumpwm::input-line-string input)
							      (stumpwm::input-line-position input)
							      *completions-set*
							      *ido-match-function*)))
		      (cond ((stringp key)
			     (stumpwm::input-insert-string input key)
			     (ido-draw-input-bucket screen prompt input (format-matches
									 (ido-inline-completions prompt
												    input
												    completions
												    screen))))
			    ;; skip modifiers
			    ((stumpwm::is-modifier (car key)))
			    ((ido-process-input screen prompt input (car key) (cdr key))
			     (if (or (not require-match)
				     (match-input))
				 (return (stumpwm::input-line-string input))
				 (ido-draw-input-bucket screen prompt input "[No match]" t))))))))
	     (ido-setup-input-window screen prompt input)
	     (catch :abort
	       (unwind-protect
		    (stumpwm::with-focus (stumpwm::screen-input-window screen)
		      (key-loop))
		 (stumpwm::shutdown-input-window screen))))))

(defun color-cursor (text point)
  (let ((face (pstr:copy-face
	       (or (pstr:lookup-face
		    (pstr:pstring-get-property text :face point))
		   (pstr:lookup-face :default)))))
    (setf face (pstr::merge-faces (pstr:lookup-face :cursor) face))
    (pstr:pstring-put-property text point (1+ point) :face face)))

(defun ido-draw-input-bucket (screen prompt input &optional (tail "") errorp)
  (declare (ignorable errorp))
  (let* ((text-to-print (pstr:pstring-concat prompt
					     (color-cursor (stumpwm::input-line-string input)
							   (stumpwm::input-line-position input))
					     " "
					     tail))
	 (gcontext (stumpwm::screen-message-gc screen))
	 (win (stumpwm::screen-input-window screen)))
    (multiple-value-bind (w h a)
	(pstr:xlib-pstring-extents gcontext text-to-print)
      (let* ((width (+ w (* 2 stumpwm:*message-window-padding*)))
	     (height (+ h (* 1 stumpwm:*message-window-padding*))))
	(setf (xlib:drawable-width win) width
	      (xlib:drawable-height win) height)
	(stumpwm::setup-win-gravity screen win stumpwm:*input-window-gravity*)
	(pstr:xlib-draw-pstring text-to-print
				win
				gcontext
				stumpwm:*message-window-padding*
				(+ a  stumpwm:*message-window-padding*)
				t)))))

(stumpwm:defcommand prova () ()
  (print (ido-read-one-line (stumpwm:current-screen)
			    (pstr:pstring-propertize "Cmd: " :face :input-prompt))))

(stumpwm:defcommand prova2() ()
  (let ((*completions-set* *pathname-completions*))
    (print (ido-read-one-line (stumpwm:current-screen)
			      (pstr:pstring-propertize "Cmd: " :face :input-prompt)))))
