;;;; stumpwm-ido.lisp

(in-package #:stumpwm-ido)
(declaim (optimize (speed 0) (debug 3)))

(defvar *completions-set* nil)
(defvar *ido-match-function* *ido-flex-matcher*)


(defvar *ido-max-inline-completions* 30
  "Maximum number of completions displayed in the input window. This
should be set reasonably low as it impacts the speed of the completion
formatting function.")

(pstr:defface :default
  :family "terminus"
  :pixel-size 12
  :foreground "white"
  :slant "r")

(pstr:defface :cursor
  :background "magenta"
  :foreground "black")

(pstr:defface :title1
  :pixel-size 24
  :foreground "red")

(pstr:defface :title2
  :pixel-size 20
  :inherit '(:title1))

(pstr:defface :title3
  :pixel-size 16)

(pstr:defface :italic
  :slant "i")
(pstr:defface :oblique
  :slant "o")
(pstr:defface :emph
  :inherit '(:italic))

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
  :foreground "cyan"
  :weight "bold")

(pstr:defface ido-matches-alt-1
  :documentation "Face used by ido for highlighting matches having :match-type
property set to 1."
  :foreground "blue")

(pstr:defface ido-matches-alt-2
  :documentation "Face used by ido for highlighting matches having :match-type
property set to 2."
  :foreground "violet")

(pstr:defface :ido-error
  :documentation "Face used by ido for highlighting inline error messages."
  :foreground "magenta"
  :inherit '(:italic))


(pstr:defface :input-prompt
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
  (cond ((null completions)
	 (pstr:pstring-concat (pstr:pstring-propertize "[" :face :ido-matches-separator)
			      (pstr:pstring-propertize "No match" :face :ido-error)
			      (pstr:pstring-propertize "]" :face :ido-matches-separator)))
	(t (flet ((csymbol (text)
		    (pstr:pstring-propertize text :face :ido-matches-separator))
		  (add-sep (match)
		    (list (propertize-match match)
			  (pstr:pstring-propertize " | " :face :ido-matches-separator))))
	     ;; separated completions
	     (let* ((scmps-1 (butlast (mapcan #'add-sep completions))) 
		    ;; first-match
		    (scmps (cons (pstr:pstring-propertize (first scmps-1) :face :ido-first-match)
				 (cdr scmps-1))))
	       (reduce #'pstr:pstring-concat
		       (nconc (list (csymbol "{ "))
			      scmps
			      (list (csymbol " }")))))))))

(defun ido-inline-completions (prompt input completions screen)
  (let ((gcontext (stumpwm::screen-message-gc screen))
	(maxwidth (- (stumpwm::screen-width screen) (* 2 stumpwm:*message-window-padding*)))
	(n (length completions)))
    (flet ((make-display-string (comps)
	     (pstr:pstring-concat prompt (stumpwm::input-line-string input) " " (format-matches comps) " ... ")))
      (do ((comps (reverse (subseq completions 0 (min *ido-max-inline-completions* n))) (cdr comps)))
	  ((or (null comps) (< (pstr:xlib-pstring-extents gcontext (make-display-string comps)) maxwidth))
	   (reverse (append (and  (> n (length comps)) (list "...")) comps)))))))

(defparameter *ido-current-completions* nil)

(defparameter *ido-input-map*
  (stumpwm::copy-kmap stumpwm::*input-map*))

(defun ido-process-input (input code state)
  "Process the key (code and state) given the current input
buffer. Returns a new modified input buffer."
  (labels ((process-key (code state)
	     "Call the appropriate function based on the key
pressed. Return 'done when the user has signalled the finish of his
input (pressing Return), nil otherwise."
	     (let* ((key (stumpwm::code-state->key code state))
		    (command (and key (stumpwm::lookup-key *ido-input-map* key t))))
	       (if command
		   (prog1
		       (funcall command input key)
		     (setf stumpwm::*input-last-command* command))
		   :error))))
    (case (process-key code state)
      (:done
       (unless (or (stumpwm::input-line-password input)
		   (and stumpwm:*input-history-ignore-duplicates*
			(string= (stumpwm::input-line-string input) (first stumpwm::*input-history*))))
	 (push (stumpwm::input-line-string input) stumpwm::*input-history*))
       :done)
      (:abort (throw :abort t))
      (:error
       ;; FIXME draw inverted text
       :error)
      (t nil))))

(defun process-key-or-selection (key input)
  (cond ((stringp key)
	 (stumpwm::input-insert-string input key))
	((stumpwm::is-modifier (car key)))
	(t (case (ido-process-input input (car key) (cdr key))
	     (:error :error)
	     (:done :done)
	     (t nil)))))

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
	     (input-changed (old-input-string old-input-position input-line)
	       (declare (ignorable old-input-position))
	       (not (string= old-input-string (stumpwm::input-line-string input-line))))
	     (key-loop ()
	       (let ((old-input initial-input)
		     (old-input-pos (length initial-input)))
		 (loop for key = (stumpwm::read-key-or-selection) do
		      (let ((status (process-key-or-selection key input)))
			;; handle end of input
			(when (and (eq status :done)
				   (or (not require-match) (match-input)))
			  (return (stumpwm::input-line-string input)))
			;; handle input change
			(when (input-changed old-input old-input-pos input)
			  (setf old-input (copy-sequence 'string (stumpwm::input-line-string input)))
			  (setf old-input-pos (stumpwm::input-line-position input))
			  (setf *ido-current-completions* (ido-input-completions old-input
										 old-input-pos
										 *completions-set*
										 *ido-match-function*)))
			;; draw 
			(ido-draw-input-bucket screen prompt input
					       (format-matches (ido-inline-completions prompt
										       input
										       *ido-current-completions*
										       screen))
					       (eq status :error)))))))
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
				t)))
      (when errorp
        (stumpwm::invert-rect screen win 0 0 (xlib:drawable-width win) (xlib:drawable-height win))
        (xlib:display-force-output stumpwm::*display*)
        (sleep 0.05)
        (stumpwm::invert-rect screen win 0 0 (xlib:drawable-width win) (xlib:drawable-height win)))
    ))


(stumpwm:defcommand prova () ()
  (print (ido-read-one-line (stumpwm:current-screen)
			    (pstr:pstring-propertize "Cmd: " :face :input-prompt))))

(stumpwm:defcommand prova3 () ()
  (let ((*completions-set* *command-completions*))
    (ido-read-one-line (stumpwm::current-screen)
		       (pstr:pstring-propertize "CMD: " :face :input-prompt))))
(stumpwm:defcommand prova2() ()
  (let ((*completions-set* *pathname-completions*))
    (print (ido-read-one-line (stumpwm:current-screen)
			      (pstr:pstring-propertize "Cmd: " :face :input-prompt)))))


(stumpwm:defcommand ido-rotate-fwd (input key) ()
  (setf *ido-current-completions* (rotate *ido-current-completions* -1)))

(stumpwm:defcommand ido-rotate-bwd (input key) ()
  (setf *ido-current-completions* (rotate *ido-current-completions*)))

(stumpwm:define-key *ido-input-map* (stumpwm:kbd "C-s") 'ido-rotate-fwd)
(stumpwm:define-key *ido-input-map* (stumpwm:kbd "C-r") 'ido-rotate-bwd)


(defun ido-complete-input (input key)
  (declare (ignore key))
  (setf (stumpwm::input-line-string input) (stumpwm::make-input-string (pstr:pstring-string (car *ido-current-completions*)))
	(stumpwm::input-line-position input) (pstr:pstring-length (car *ido-current-completions*))))

(stumpwm:define-key *ido-input-map* (stumpwm:kbd "C-Return") 'ido-complete-input)

(defun ido-expand-input (input key) 
  (declare (ignore key))
  (setf (stumpwm::input-line-string input)
	(stumpwm::make-input-string 
	 (ido-cset-expand-input (stumpwm::input-line-string input)
				(stumpwm::input-line-position input)
				*completions-set*
				*ido-match-function*)))
  (setf (stumpwm::input-line-position input)
	(length (stumpwm::input-line-string input))))

(stumpwm:define-key *ido-input-map* (stumpwm:kbd "Tab") 'ido-expand-input)

