(in-package :stumpwm-ido)
(defun dsubstring (string start &optional end (substring (make-array 0 :element-type 'character
								     :displaced-to ""
								     :displaced-index-offset 0)))
  "Same as subseq, but uses a displaced array. The displaced array can
be passed as the SUBSTRING parameter."
  (adjust-array substring (- end start)
		:displaced-to string
		:displaced-index-offset start))

