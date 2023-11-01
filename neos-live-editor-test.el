;;; neos-live-editor-test.el --- Tests for neos-live-editor  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Cj-bc a.k.a. 陽鞠莉桜

;; Author:  Cj-bc <cj.bc-sd at outlook.jp>

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

;;; Code:
(require 'ert)
(require 'neos-live-editor)

(ert-deftest neos-live-editor-test/formatter/insert-cursor ()
  "neos-live-editor/formatter/insert-cursor shuold insert cursor tag
at given marker position"
  (let ((c (make-marker)))
    (with-temp-buffer
      (insert "Lorem ipsum")
      (set-marker c 10)
      (should (string= (progn (neos-live-editor/formatter/insert-cursor :cursor c) (buffer-string))
		       "Lorem ips<$cursor />um")))))

(ert-deftest neos-live-editor-test/formatter/bake-prefixes/line-prefix-by-text-property ()
  "bake-prefixes should bake line-prefix"
  (with-temp-buffer
    (insert #("Lorem ipsum" 0 11 (line-prefix "LINE_PREFIX")))
    (should (string= (progn (neos-live-editor/formatter/bake-prefixes) (buffer-string))
		     "LINE_PREFIXLorem ipsum"))))

(ert-deftest neos-live-editor-test/formatter/bake-prefixes/line-prefix-by-variable ()
  "bake-prefixes should bake line-prefix"
  (with-temp-buffer
    (setq-local line-prefix "LINE_PREFIX")
    (insert "Lorem ipsum")
    (should (string= (progn (neos-live-editor/formatter/bake-prefixes) (buffer-string))
		     "LINE_PREFIXLorem ipsum"))))

(ert-deftest neos-live-editor-test/formatter/bake-prefixes/wrap-prefix-by-text-property ()
  "bake-prefixes should bake wrap-prefix."
  (with-temp-buffer
    (insert #("Lorem ipsum" 0 11 (wrap-prefix "WRAP_PREFIX")))
    (should (string= (progn (neos-live-editor/formatter/bake-prefixes) (buffer-string))
		     "WRAP_PREFIXLorem ipsum"))))

(ert-deftest neos-live-editor-test/formatter/bake-prefixes/wrap-prefix-by-variable ()
  "bake-prefixes should bake wrap-prefix."
  (with-temp-buffer
    (setq-local wrap-prefix "WRAP_PREFIX")
    (insert "Lorem ipsum")
    (should (string= (progn (neos-live-editor/formatter/bake-prefixes) (buffer-string))
		     "WRAP_PREFIXLorem ipsum"))))

(provide 'neos-live-editor-test)
;;; neos-live-editor-test.el ends here
