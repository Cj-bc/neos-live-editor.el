;;; neos-live-editor.el --- Emacs client for rhenium's Neos live editor  -*- lexical-binding: t; -*-
;; Copyright (C) 2023  

;; Author:  Cj-bc
;; Keywords: processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'web-server)

(defvar neos-live-editor/server-instance nil
  "Contains `ws-server' object running neos-live-editor.
If it's nil, no server is running.")

(defun neos-live-editor/handler/v1/neos (request)
  "handler function for default neos-live-editor endpoint"
  (with-slots (process headers) request
    (ws-response-header process 200 '("Content-type" . "text/plain"))
    (process-send-string process "This is test\n"))
  )

(defun neos-live-editor/format (original-window begin end)
  "Format using neos' rich text format.
For format information, please look at

<https://wiki.neos.com/Text_(Component)#Rich_text>"
  (let* ((original-buffer (window-buffer original-window))
	 (text (with-current-buffer original-buffer (buffer-substring begin end)))
	 (cursor-pos-marker (make-marker)))
    (with-temp-buffer
      ;; Add markers before modifying original text
      (insert text)
      (set-marker cursor-pos-marker (+ (- (window-point original-window)
					  (window-start original-window))
				       1)) ;; Adjust so that first position is 1.

      (neos-live-editor/format/delete-invisible-text)
      (neos-live-editor/format/insert-cursor cursor-pos-marker)
      (neos-live-editor/format/append-line-number
       (with-current-buffer original-buffer
      	 (line-number-at-pos (window-start (get-buffer-window original-buffer)))))
      (neos-live-editor/format/apply-tags)

      ;; Release every markers
      (set-marker cursor-pos-marker nil)
      (buffer-string))))

(defun neos-live-editor/format/insert-cursor (cursor-pos &optional buffer)
  "Insert cursor text at CURSOR-POS (MARKER) on BUFFER (or `current-buffer' when it's nil)"
  (with-current-buffer (or buffer (current-buffer))
    (goto-char cursor-pos)
    (insert "<$cursor />")))

(defun neos-live-editor/format/apply-tags (&optional buffer)
  "Insert neos' rich text tags based on face."
  (let ((buf (or buffer (current-buffer))))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(let* ((next-change (next-single-property-change (point) 'face buf (point-max)))
      	       (next-change-marker (set-marker (make-marker) next-change))
      	       (current-face (get-text-property (point) 'face))
      	       )
      	  (if (eq next-change nil)
      	      (goto-char (point-max))
      	    (if (not (eq current-face nil))
		(goto-char (neos-live-editor/format/surround-with (point)
						       next-change
						       "color"
						       (neos-live-editor/format/retrive-fgcolor current-face)))
      	      (goto-char next-change-marker)))
      	  (set-marker next-change-marker nil))))))

(defun neos-live-editor/format/retrive-fgcolor (face)
  "Return text representation of foreground color of given face."
  (cond ((listp face) (car (seq-filter 'stringp (seq-map (lambda (s) (face-attribute s :foreground nil t)) face))))
	((symbolp face) (face-attribute face :foreground nil t))
	((stringp face) nil) ;; TODO: How can I convert string name to face symbol?
	(t nil)))

(defun neos-live-editor/format/surround-with (beg end tag-name &optional parameter buffer)
  "Surround text between BEG and END in BUFFER with proper Neos's rich text tag based on TAG-NAME and PARAMETER.
Return the position of last of sorrounded. (??????????????????????????????????????????????????????)
If BUFFER is `nil', it will use `current-buffer'.
BEG, END should be integer (marker isn't allowed). TAG-NAME, PARAMETER should be string."
  (save-excursion
    (with-current-buffer (or buffer (current-buffer))
      (let ((end-marker (make-marker)));; (if (markerp end) end (set-marker (make-marker) end))))
	(set-marker end-marker end)
	(goto-char beg)
	(if parameter
    	    (insert (format "<%s=%s>" tag-name parameter))
	  (insert (format "<%s>" tag-name)))
	(goto-char end-marker)
	(insert (format "</%s>" tag-name))
	(set-marker end-marker nil))
      (point))))

(defun neos-live-editor/format/append-line-number (window-start-line-number &optional buffer)
  "Insert line number at the beginning of each line in BUFFER (or `current-buffer' when it's nil)
First line will be `window-start-line-number'"
  (let* ((buf (or buffer (current-buffer)))
	 (offset (- window-start-line-number 1)))
    (save-excursion
      (with-current-buffer buf
	(goto-char (point-min))
	(while (not (eobp))
      	  (insert (seq-subseq (format "     %s" (+ offset (line-number-at-pos))) 
			      -4)
		  "    ")
      	  (vertical-motion 1)
      	  )))))

(defun neos-live-editor/format/delete-invisible-text (&optional buffer)
  "Delete all texts that have any sort of 'invisible' text property in
given BUFFER (or `current-buffer' when it's nil"
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      ;; 1. Make sure while loop start with `point' being placed at
      ;; text that have `invisible' enabled.
      (when (equal (get-text-property (point) 'invisible) nil)
	(goto-char (next-single-property-change (point) 'invisible nil (point-max))))
      ;; For each iteration, it will do:
      ;; 1. Remove invisible text
      ;; 2. Find next invisible text beggining point and move point to there
      (while (pcase (next-single-property-change (point) 'invisible)
  	       ('nil (delete-region (point) (point-max)) nil)
  	       (visible-start
  		(delete-region (point) visible-start)
  		(pcase (next-single-property-change (point) 'invisible)
  		  ('nil nil)
		  (invisible-start
  		   (goto-char invisible-start)
  		   t))))))))

(defun neos-live-editor/format/buffer-visible-substring (beg end &optional buffer)
  "`buffer-substring' but without strings with `invisible' text property/overlay"
  (let ((result-buf (generate-new-buffer " *temp-handwritten*")))
    (unwind-protect
	(with-current-buffer (or buffer (current-buffer))
	  (save-excursion
      	    (goto-char (point-min))
      	    ;; 1. Make sure while loop start with `point' being placed at
      	    ;; text that is visible.
      	    (unless (equal (get-text-property (point) 'invisible) nil)
      	      (goto-char (next-single-char-property-change (point) 'invisible nil (point-max))))
      	    ;; For each iteration, it will do:
      	    ;; 1. Copy & insert visible text into `result-buf'
      	    ;; 2. Find next visible text beggining point and move point to there
      	    (while (or (> (point) end)
      		       (pcase (next-single-char-property-change (point) 'invisible)
			 ;; When 'invisible does not change till buffer end
      			 ((pred (lambda (p) (eq p (point-max))))
      			  (let ((visible-text (buffer-substring (point) (point-max))))
      			    (with-current-buffer result-buf (insert visible-text)))
			  nil)
      			 (invisible-start
      			  (let ((visible-text (buffer-substring (point) invisible-start)))
      			    (with-current-buffer result-buf (insert visible-text)))
			  (goto-char invisible-start)
      	       		  (pcase (next-single-char-property-change (point) 'invisible)
			    ;; When 'invisible does not change till buffer end
      	       		    ((pred (lambda (p) (eq p (point-max)))) nil)
      	       		    (visible-start
      	       		     (goto-char visible-start)
      			     t)))))))
	  (with-current-buffer result-buf (buffer-string)))
      (kill-buffer result-buf))))

(defun neos-live-editor/server (request)
  "Server program for neos-live-editor. It should be used with `ws-start'"
  (with-slots (process headers) request
  		  (ws-response-header process 200 '("Content-type" . "text/plain"))
  		  (let* ((window (frame-selected-window))
  			 (start (window-start window))
  			 (end (window-end window))
  			 )
  		    (process-send-string process (neos-live-editor/format window start end))
  		    )))

(defun neos-live-editor/run (port-number &optional log-buffer)
  "Run neos-live-editor server and store that server instance into
`neos-live-editor/server-instance' variable.
If server is running at any port, it won't run again.
"
  (if neos-live-editor/server-instance
      (message "Server is already running.")
    (pcase (ws-start 'neos-live-editor/server
  			   port-number (or log-buffer "neos-live-editor-log")
			   :host "0.0.0.0")
      ('nil (message "failed to run neos-live-editor server"))
      (server (setq neos-live-editor/server-instance server)
	      (message "neos-live-editor is running at port %d" port-number)))))

(defun neos-live-editor/stop ()
  "Stop neos-live-editor server stored in `neos-live-editor/server-instance'"
  (when neos-live-editor/server-instance
    (ws-stop neos-live-editor/server-instance)
    (setq neos-live-editor/server-instance nil)))

(provide 'neos-live-editor)
;;; neos-live-editor.el ends here
