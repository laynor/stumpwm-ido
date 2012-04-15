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

(defun make-fuzzy-matcher-regex (string case-sensitive-p)
  (ppcre:create-scanner (format nil "\\A.*~{~c.*~}\\z"
				(coerce string 'list))
			:case-insensitive-mode (not case-sensitive-p)))
			
(defun ido-fuzzy-match (input-string cursor-pos string &key case-sensitive-p)
  (declare (ignorable cursor-pos))
  (ppcre:scan (make-fuzzy-matcher-regex input-string case-sensitive-p)
	      (pstr:pstring-string string)))

(defun ido-subseq-match (input-string cursor-pos string &key case-sensitive-p)
  (declare (ignore cursor-pos))
  (search input-string (pstr:pstring-string string) :test (if case-sensitive-p #'eql #'char-equal)))

(defparameter ido-flex-matcher (list :or #'ido-subseq-match #'ido-fuzzy-match))
(defparameter ido-fuzzy-matcher-substring-match-first (list :union #'ido-subseq-match))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; matchers.lisp ends here
