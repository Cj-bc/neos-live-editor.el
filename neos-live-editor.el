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

(defun neos-live-editor/handler/v1/neos (request)
  "handler function for default neos-live-editor endpoint"
  (with-slots (process headers) request
    (ws-response-header process 200 '("Content-type" . "text/plain"))
    (process-send-string process "This is test\n"))
  )

(defun neos-live-editor/format (text)
  "Format using neos' rich text format.
For format information, please look at

<https://wiki.neos.com/Text_(Component)#Rich_text>"
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((next-change (next-single-property-change (point) 'face (current-buffer) (point-max)))
	    (current-face (get-text-property (point) 'face))
	    )
	(if (eq next-change nil)
	    (goto-char (point-max))
	  (if (not (eq current-face nil))
	      (progn (insert (format "<color=%s>" (face-attribute current-face :foreground)))
	    	     (forward-char (- next-change 1)) ;; `next-change' の位置にあるのは、既にfaceが切り替わった後のCharで、その手前に挿入したい
	    	     (insert "</color>")
	    	     (forward-char (length "</color>")))
	    (forward-char (min next-change (- (point-max) (point)))))
	  )
	))
    (buffer-string)))

;; (ws-start
;;  '(((:GET . "/v1/live/neos") . neos-live-editor/handler/v1/neos))
;;  39451)
;; (ws-start (lambda (request)
;; 	    (with-slots (process headers) request
;; 	      (ws-response-header process 200 '("Content-type" . "text/plain"))
;; 	      (process-send-string process (neos-live-editor/format (buffer-string)))
;; 	      ))  3126
;; 		  "ws-server-log" :host "0.0.0.0")

(provide 'neos-live-editor)
;;; neos-live-editor.el ends here
