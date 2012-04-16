;;;; completions.lisp --- 
;;; 
;;; Filename: completions.lisp
;;; Description: Completions for the stumpwm IDO module
;;; Author: Alessandro Piras
;;; Maintainer: 
;;; Created: Tue Apr 10 16:24:32 2012 (+0200)
;;; Last-Updated: Mon Apr 16 14:04:41 2012 (+0200)
;;;           By: Alessandro Piras
;;;     Update #: 4
;;; URL: 
;;; Keywords: 
;;; Compatibility: 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;; Commentary:
;;;  Completions Sets for stumpwm IDO module.
;;;
;;;  The sets are implemented as `completions-set' structures (csets).
;;;  These structures have three fields:
;;;  SET: A data structure representing a set of values from which to
;;;       choose the matches, or a function that takes an input string
;;;       and a cursor position and returns such a set.
;;;  FILTER-FN: A function that, given the input string, the cursor
;;;             position, the set data structure stored in or returned
;;;             by the SET field and a MATCH-FN predicate and returns a
;;;             list containing all the elements in set that match the
;;;             input string according to the MATCH-FN predicate.
;;;  SORT-FN: A function that takes the result of FILTER-FN and sorts
;;;           them. Defaults to `identity'. 
;;;
;;;  Input Matchers: The MATCH-FN predicates (defined in matchers.lisp)
;;;  An input matcher is a function that takes the input string, the
;;;  cursor position and a pstring, and returns non nil if the pstring
;;;  matches the input string, nil otherwise.
;;;
;;;  This module offers some helpers to easily create csets for some
;;;  common cases:
;;;  
;;;  1. The cset is a sequence, or a function that returns one:
;;;     this sets can be created using `sequence-cset'.
;;;  2. The cset is the union of other csets:
;;;     this set can be created using `cset-union'.
;;;  3. The cset has the same values as another completions set, but
;;;     the input should have some prefix string to trigger the
;;;     completion: this set can be created using `prefix-cset'.
;;;
;;;  Example: creating a cset *MYSET* that completes comma-prefixed
;;;           stumpwm commands or shell commands:
;;;  Let *commands-set* be the cset of command completions, and
;;;  *shell-commands-set* be the cset of shell commands.
;;;  We can create the set *MYSET* as follows:
;;;  (defparameter *myset* (cset-union (prefix-cset *commands-set*)
;;;                                    *shell-commands-set*))
;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;; Change Log:
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;; Code:

;;; TODO: add documentation
(in-package :stumpwm-ido)
(declaim (optimize (speed 0) (debug 3)))

;;; Set of completions
(defstruct (completions-set (:conc-name cset-))
  filter-fn		; (filter-fn match-fn input-string cursor-position set-data)
  (sort-fn #'identity)	; (sort-fn input-string cursor-position completions-list)
  set)

(defun ido-unsorted-input-completions-1 (input-string cursor-position cset match-fn)
  "Returns the completions in COMPLETIONS-SET matching INPUT-STRING
and CURSOR-POSITION according to the string matching function
MATCH-FN."
  (funcall (cset-filter-fn cset)
	   match-fn
	   input-string
	   cursor-position
	   (let ((set (cset-set cset)))
	     (typecase set
	       (function (funcall set input-string cursor-position))
	       (t set)))))
				  
(defun ido-unsorted-input-completions (input-string cursor-position cset match-fn-or-list)
  (typecase match-fn-or-list
    (function (ido-unsorted-input-completions-1 input-string cursor-position cset match-fn-or-list))
    (list (case (first match-fn-or-list)
	    ;; Try the matching rules in the list until at least
	    ;; one match is found, and return all the matches in the
	    ;; set according to that matching function.
	    (:or (or (ido-unsorted-input-completions input-string cursor-position
						     cset
						     (second match-fn-or-list))
		     (when (first (cddr match-fn-or-list))
		       (ido-unsorted-input-completions input-string cursor-position
						       cset
						       (cons :or (cddr match-fn-or-list))))))
	    ;; Return the completions matching all the matching rules
	    ;; specified.
	    (:and (reduce (lambda (s1 s2) (intersection s1 s2 :test #'pstr:pstring-equal))
			  (mapcar (lambda (match-rule)
				    (ido-unsorted-input-completions input-string cursor-position
								    cset
								    match-rule))
				  (cdr match-fn-or-list))))
				  
	    ;; Return the completions matching according to any of the
	    ;; matching rules specified. The matches for the first
	    ;; predicate will appear first in the completion list.
	    (:union (remove-duplicates (mappend (lambda (match-fn)
						  (ido-unsorted-input-completions input-string
										  cursor-position
										  cset
										  match-fn))
						(cdr match-fn-or-list)))
				       :test #'pstr:pstring-equal
				       :from-end t)))))

(defun ido-input-completions (input-string cursor-position cset match-fn-or-list)
  (let* ((completions (funcall (cset-sort-fn cset)
			       (ido-unsorted-input-completions input-string cursor-position
							       cset
							       match-fn-or-list)))
	 (item (find input-string completions :key #'pstr:pstring-string :test #'string-equal)))
    (if item (cons item (remove item completions))
	completions)))


;;;; Some helpers for completions set definition

;;; Filter function for sets expressed as sequences
(defun sequence-filter (match-fn input-string cursor-position sequence)
  (remove-if-not (curry match-fn input-string cursor-position)
		 sequence))

(defun sequence-cset (sequence-or-function)
  "Takes a sequence or a function that returns one, and returns a
completion set."
  (make-completions-set :filter-fn #'sequence-filter
			:set sequence-or-function))

;;; Prefix input: switch completion set using prefixes
(defun prefix-input-string (plen input-string include-prefix-p)
  (if include-prefix-p
      input-string
      (subseq input-string plen)))
(defun prefix-cursor-pos (plen cursor-pos include-prefix-p)
  (if include-prefix-p
      cursor-pos
      (- cursor-pos plen)))

(defun prefix-filter (prefix cset include-prefix-p)
  (let ((plen (length prefix))
	(filter-fn (cset-filter-fn cset)))
    (lambda (match-fn input-string cursor-position set)
      (let ((p (search prefix input-string)))
	(when (and (numberp p) (zerop p))
	  (funcall filter-fn
		   match-fn
		   (prefix-input-string plen input-string include-prefix-p)
		   (prefix-cursor-pos plen cursor-position include-prefix-p)
		   set))))))

(defun prefix-set (prefix cset include-prefix-p)
  (let ((set (cset-set cset))
	(plen (length prefix)))
    (typecase set
      (function
       (lambda (input-string cursor-position)
	 (funcall set
		  (prefix-input-string plen input-string include-prefix-p)
		  (prefix-cursor-pos plen cursor-position include-prefix-p))))
      (t set))))
		  

(defun prefix-cset (prefix cset &key include-prefix-p)
  (make-completions-set :filter-fn (prefix-filter prefix cset include-prefix-p)
			:set (prefix-set prefix cset include-prefix-p)))

;;; Union cset: match from more than one set
(defun cset-union-filter (csets)
  (let ((filter-fn-list (mapcar #'cset-filter-fn csets)))
    (lambda (match-fn input-string cursor-position setlist)
      (reduce #'append
	      (mapcar (lambda (filter-fn set)
			(funcall filter-fn
				 match-fn input-string cursor-position set))
		      filter-fn-list setlist)))))

(defun cset-union-set (csets)
  (let ((sets (mapcar #'cset-set csets)))
    (lambda (input-string cursor-position)
      (mapcar (lambda (set)
		(typecase set
		  (function (funcall set input-string cursor-position))
		  (t set)))
	      sets))))
    
(defun cset-union (&rest csets)
  (make-completions-set :filter-fn (cset-union-filter csets)
			:set (cset-union-set csets)))
  

;;;; Some predefined sets

;;; Stumpwm Command Set
(defun commands (&rest args)
  (declare (ignore args))
  (let (res)
    (maphash-keys (lambda (k) (push (pstr:pstring-propertize (symbol-name k)
							     :value k)
							     res))
		  stumpwm::*command-hash*)
    (reverse res)))

(defparameter *command-completions*
  (sequence-cset #'commands))

;;; File Path Set
(defun pathname-set-filter (match-fn input-string cursor-position set)
  (remove-if-not (curry match-fn (file-namestring input-string) cursor-position)
		 set))

(defun pathname-set (input-string cursor-position)
  (declare (ignore cursor-position))
  (mapcar #'enough-namestring
	  (fad:list-directory (directory-namestring input-string))))

(defparameter *pathname-completions* 
  (make-completions-set :filter-fn #'pathname-set-filter
			:set #'pathname-set))

;;; TODO Find a way to list only files with a given property (ex. executables)
;;;      a portable way would be better!
;; (defun is-exe (path)
;;   (and (not (fad:directory-exists-p path)) (is-executable path)))
;; (defun is-executable (path) (boole boole-and (sb-posix:stat-mode (sb-posix:stat path)) #o0111)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; completions.lisp ends here
