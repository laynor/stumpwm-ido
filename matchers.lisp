;;;; matchers.lisp --- 
;;; 
;;; Filename: matchers.lisp
;;; Description: Input string matchers for stumpwm-ido
;;; Author: Alessandro Piras
;;; Maintainer: 
;;; Created: Thu Apr 12 14:58:18 2012 (+0200)
;;; Version: 
;;; Last-Updated: Thu Apr 12 14:58:28 2012 (+0200)
;;;           By: Alessandro Piras
;;;     Update #: 1
;;; URL: 
;;; Keywords: 
;;; Compatibility: 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;; Commentary: 
;;; 
;;; 
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

(in-package :stumpwm-ido)
(declaim (optimize (speed 0) (debug 3)))

;; (defun make-fuzzy-matcher-regex (string case-sensitive-p)
;;   (ppcre:create-scanner (format nil "\\A.*~{~c.*~}\\z"
;; 				(coerce string 'list))
;; 			:case-insensitive-mode (not case-sensitive-p)))

;; (defun make-fuzzy-matcher-regex (string case-sensitive-p)
;;   (let ((csensitive-regex (format nil "\\A.*~{~c.*~}\\z"
;; 				  (coerce string 'list))))
;;     (if case-sensitive-p
;; 	csensitive-regex
;; 	(concatenate 'string "(?i)"
;; 		     csensitive-regex))))

(defun ido-fuzzy-match (input-string cursor-pos string &key case-sensitive-p)
  (declare (ignore cursor-pos))
  (let ((j 0)
	(string (pstr:pstring-string string))
	(n (length input-string)))
    (catch :match 
      (dotimes (i (length string))
	(let ((is-elt (elt input-string j))
	      (s-elt (elt string i)))
	  (when (or (and case-sensitive-p (char-equal is-elt s-elt))
		     (and (not case-sensitive-p) (char= is-elt s-elt)))
		 (incf j))
	  (when (>= j n) (throw :match t))))
      nil)))

;; (defun ido-fuzzy-match (input-string cursor-pos string &key case-sensitive-p)
;;   (declare (ignorable cursor-pos))
;;   (ppcre:scan (make-fuzzy-matcher-regex input-string case-sensitive-p)
;; 	      (pstr:pstring-string string)))

(defun ido-subseq-match (input-string cursor-pos string &key case-sensitive-p)
  (declare (ignore cursor-pos))
  (search input-string (pstr:pstring-string string) :test (if case-sensitive-p #'eql #'char-equal)))

(defparameter *ido-flex-matcher* (list :or #'ido-subseq-match #'ido-fuzzy-match))

(defparameter *ido-fuzzy-matcher-substring-match-first* (list :union #'ido-subseq-match #'ido-fuzzy-match))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; matchers.lisp ends here
