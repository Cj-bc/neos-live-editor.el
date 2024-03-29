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



(defun neos-live-editor/format (original-window begin formatters)
  "Format buffer and return.
For format information, please look at

<https://wiki.neos.com/Text_(Component)#Rich_text>"
  (let* ((original-buffer (window-buffer original-window))
	 (text (with-current-buffer original-buffer (buffer-substring begin (point-max))))
	 ;; Sort overlays by its start position so that we can start to apply them from latest one
	 (original-overlays (seq-sort '(lambda (a b) (> (overlay-start a) (overlay-start b)))
				      (flatten-list (with-current-buffer original-buffer (overlay-lists)))))
	 (cursor-pos-marker (make-marker)))
    (with-temp-buffer
      ;; Add markers before modifying original text
      (insert text)

      ;; In order to remove read-only text property, I have to set `inhibit-read-only' to non-nil
      ;; according to [info:elisp#Special Properties]
      (let ((inhibit-read-only t))
	(remove-text-properties (point-min) (point-max) '(read-only nil)))

      (set-marker cursor-pos-marker (+ (- (window-point original-window)
					  (window-start original-window))
				       1)) ;; Adjust so that first position is 1.

      (dolist (formatter formatters)
	(funcall formatter
		 :cursor cursor-pos-marker
		 :begin begin
		 :original-buffer original-buffer
		 :original-window original-window))

      ;; Release every markers
      (set-marker cursor-pos-marker nil)
      (buffer-string))))

(defun neos-live-editor/format/retrive-fgcolor (face)
  "Return text representation of foreground color of given face.
It follows all inheritance if given FACE does not specify foreground
color, but it does not merge `default' face to reduce amount of text
to send to neos.
"
  (cond ((and (listp face) (facep (car face)))
	 (car (seq-filter 'stringp (seq-map 'neos-live-editor/format/retrive-fgcolor face))))
	((and (listp face) (plist-get face :foreground)) (plist-get face :foreground))
	((symbolp face) (face-attribute face :foreground nil t))
	((stringp face) nil) ;; TODO: How can I convert string name to face symbol?
	(t nil)))

(defun neos-live-editor/format/surround-with (beg end tag-name &optional parameter)
  "THIS MODIFIES THE BUFFER DIRECTLY.
Surround text between BEG and END in `current-buffer' with proper Neos's rich text tag based on TAG-NAME and PARAMETER.
Return the position of last of sorrounded. (閉じタグの最後の文字の位置を返します)
BEG, END should be integer (marker isn't allowed). TAG-NAME, PARAMETER should be string."
  (save-excursion
    (let ((end-marker (make-marker)));; (if (markerp end) end (set-marker (make-marker) end))))
      (set-marker end-marker end)
      (goto-char beg)
      (if parameter
    	  (insert (format "<%s=%s>" tag-name parameter))
	(insert (format "<%s>" tag-name)))
      (goto-char end-marker)
      (insert (format "</%s>" tag-name))
      (set-marker end-marker nil))
    (point)))




(cl-defun neos-live-editor/formatter/bake-prefixes (&allow-other-keys)
  "THIS MODIFIES THE BUFFER DIRECTLY.
Bake `line-prefix' and `wrap-prefix' into buffer. That means, those values
will be directly inserted into the buffer.
"
  (goto-char (point-min))
  (while (not (eobp))
    (let ((pref (get-text-property (point) 'line-prefix)))
      (if pref (insert pref)))
    (forward-line 1)))

(cl-defun neos-live-editor/formatter/insert-cursor (&key cursor &allow-other-keys)
    "THIS MODIFIES THE BUFFER DIRECTLY.
Insert cursor text at CURSOR (MARKER or INTEGER)"
    (goto-char cursor)
    (insert "<$cursor />"))

(cl-defun neos-live-editor/formatter/apply-tags (&allow-other-keys)
  "THIS MODIFIES THE BUFFER DIRECTLY.
Insert neos' rich text tags based on face."
  (let ((buf (current-buffer)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(let* ((next-change (next-single-property-change (point) 'face buf (point-max)))
      	       (next-change-marker (set-marker (make-marker) next-change))
      	       (current-face (get-text-property (point) 'face))
	       (fgcolor (when current-face (neos-live-editor/format/retrive-fgcolor current-face)))
      	       )
      	  (if (eq next-change nil)
      	      (goto-char (point-max))
      	    (if fgcolor
		(goto-char (neos-live-editor/format/surround-with (point)
								  next-change
								  "color"
								  fgcolor))
      	      (goto-char next-change-marker)))
      	  (set-marker next-change-marker nil))))))

(cl-defun neos-live-editor/formatter/append-line-number (&key original-window &allow-other-keys)
  "THIS MODIFIES THE BUFFER DIRECTLY.
Insert line number at the beginning of each line in `current-buffer'"
  (let* ((window-start-line-number (line-number-at-pos (window-start original-window)))
	 (offset (- window-start-line-number 1)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
      	(insert (seq-subseq (format "     %s" (+ offset (line-number-at-pos)))
			    -4)
		"    ")
      	(vertical-motion 1)
      	))))

(cl-defun neos-live-editor/formatter/delete-invisible-text (&key original-buffer begin &allow-other-keys)
  "THIS MODIFIES THE BUFFER DIRECTLY.
Delete all texts that have any sort of 'invisible' text property in
`current-buffer'"
  (save-excursion
    ;; -- Delete texts covered by overlays with invisible proprety set
    (dolist (ovl (seq-sort '(lambda (a b) (> (overlay-start a) (overlay-start b)))
			   (flatten-list (with-current-buffer original-buffer (overlay-lists)))))
      (let ((inv (overlay-get ovl 'invisible))
	    ;; We have to offset overlay start/end because we inserted _only part of original buffer_
	    ;; into current temporary buffer
	    (start (1+ (- (overlay-start ovl) begin)))
	    (end (1+ (- (overlay-end ovl) begin))))
	;; Do not apply out-of-buffer overlays
	(when (and inv (< 0 start)) (progn (delete-region start end)))))

    ;; -- Delete texts with invisible text properties
    (goto-char (point-min))
    ;; 1. Make sure while loop start with `point' being placed at
    ;; text that have `invisible' enabled.
    (unless (get-text-property (point) 'invisible)
      (goto-char (next-single-property-change (point) 'invisible nil (point-max))))
    ;; For each iteration, it will do:
    ;; 1. Remove invisible text
    ;; 2. Find next invisible text beggining point and move point to there
    (while (pcase (next-single-property-change (point) 'invisible)
  	     ('nil (delete-region (point) (point-max)) nil)
  	     (visible-start
	      (let ((invisible-type (get-text-property (point) 'invisible)))
		(delete-region (point) visible-start)
		(when (eq invisible-type 'org-fold-outline)
		  (insert "...")))
  	      (pcase (next-single-property-change (point) 'invisible)
  		('nil nil)
		(invisible-start
  		 (goto-char invisible-start)
  		 t)))))))

(defun neos-live-editor/handler/current-buffer (request)
  "Server program for neos-live-editor. It should be used with `ws-start'"
  (with-slots (process headers) request
  		  (ws-response-header process 200 '("Content-type" . "text/plain"))
  		  (let* ((window (frame-selected-window))
  			 (start (window-start window))
  			 )
  		    (process-send-string
		     process
		     (neos-live-editor/format window start
					      '(neos-live-editor/formatter/delete-invisible-text
						neos-live-editor/formatter/insert-cursor
						neos-live-editor/formatter/bake-prefixes
						neos-live-editor/formatter/append-line-number
						neos-live-editor/formatter/apply-tags
						)))
  		    )))

(defun neos-live-editor/handler/minibuffer (request)
  (with-slots (process headers) request
    (ws-response-header process 200 '("Content-type" . "text/plain"))
    (process-send-string process (neos-live-editor/minibuffer)))
  )

(defun neos-live-editor/run (port-number &optional log-buffer)
  "Run neos-live-editor server and store that server instance into
`neos-live-editor/server-instance' variable.
If server is running at any port, it won't run again.
"
  (if neos-live-editor/server-instance
      (message "Server is already running.")
    (pcase (ws-start '(((:GET . "^/v2/minibuffer") . neos-live-editor/handler/minibuffer)
		       ((:GET . ".*") . neos-live-editor/handler/current-buffer)
		       ;; Fallback
		       ((lambda (_) t)
			. (lambda (request)
			    (with-slots (process) request
			      (ws-response-header process 404)))))
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


(defun neos-live-editor/minibuffer ()
  "Returns content of minibuffer with neos rich text format.
It'll return empty string if there's no active minibuffer."
  (let ((win (active-minibuffer-window)))
    (if win
	(with-current-buffer (window-buffer win)
	  (neos-live-editor/format win (point-min)))
      "")))
(provide 'neos-live-editor)
;;; neos-live-editor.el ends here
