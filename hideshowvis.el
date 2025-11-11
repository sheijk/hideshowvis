;;; hideshowvis.el --- Fringe markers for regions foldable by hideshow.el -*- lexical-binding: t; -*-

;; Copyright 2008-2018 Jan Rehders

;; Author: Jan Rehders <jan@sheijk.net>
;; URL: https://github.com/sheijk/hideshowvis
;; Version: 0.8
;; Package-Requires: ((emacs "27.1"))

;; Contributions and bug fixes by Bryan Waite, Michael Heerdegen, John Yates,
;; Matthew Fidler, Eyal Soha, and Philip Kaludercic.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This minor mode will add little +/- displays to foldable regions in the
;; buffer and to folded regions.  It is intended to be used in conjunction with
;; hideshow.el.

;;; Installation:

;; Add the following to your .emacs:
;;
;; (autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
;;
;; (autoload 'hideshowvis-minor-mode
;;   "hideshowvis"
;;   "Will indicate regions foldable with hideshow in the fringe."
;;   'interactive)
;;
;; (dolist (hook (list 'emacs-lisp-mode-hook
;;                     'c++-mode-hook))
;;   (add-hook hook 'hideshowvis-enable))

;; If enabling hideshowvis-minor-mode is slow on your machine use M-x,
;; customize-option, hideshowvis-ignore-same-line and set it to nil.  This will
;; then display - icons for foldable regions of one line, too but is faster

;; To enable displaying a + symbol in the fringe for folded regions,
;; use:

;;    (hideshowvis-symbols)

;; in your ~/.emacs

;; It is not enabled by default because it might interfere with custom
;; hs-set-up-overlay functions

;;; TODO

;; - global-hideshowvis-minor-mode
;; - defcustom for hideshowvis-max-file-size needs to offer setting to nil
;; - add fringe icons lazily, only for visible region (check lazy font-lock to
;;   see if it can help with this)

;;; Changelog

;; - hideshowvis-minor-mode can now be turned on during hs-minor-mode-hook
;;
;; v0.8, 2024-05-28
;; - Fixed interaction with auto-revert-mode and indent-region
;; - Fixed performance issue in some cases due to leaking overlays
;; - Factoring out hideshowvis-symbols a bit more
;;
;; v0.7, 2018-09-21
;; - Fixed issues found using flycheck-package, package-lint, checkdoc
;;
;; v0.6, 2013-03-28
;; - Running hideshowvis-enable will not enable minor mode if buffer is larger
;;   than `hideshowvis-max-file-size' to avoid slow loading of large files.
;;
;; v0.5, 2012-09-11
;; - Made ELPA compliant and added function `hideshowvis-symbols'
;;
;; v0.4, 2012-03-13
;; - Fixed bug causing transpose-words to be broken as well as causing problems
;;   when auto-fill-mode was enabled
;;
;; v0.3, 2010-08-26
;; - Added autoload cookies.
;; - Fixed bug causing major mode menu to disappear, among other things.
;;
;; v0.2, 2009-08-09
;; - The '-' symbol in fringe is clickable.
;; - Don't show '-' in fringe if the foldable region ends on the same line.
;;
;;; Code:

(require 'hideshow)

(defgroup hideshowvis ()
  "Fringe markers for regions foldable by hideshow."
  :group 'hideshow)

(define-fringe-bitmap 'hideshowvis-hideable-marker [0 0 0 126 126 0 0 0])

(defconst hideshowvis-version "v0.8" "Version of hideshowvis minor mode.")

(defface hideshowvis-hidable-face
  '((t (:foreground "#ccc" :box t)))
  "Face to highlight foldable regions.")

(defcustom hideshowvis-ignore-same-line t
  "No + for single line regions.
Do not display foldable regions in the fringe if the matching closing
parenthesis is on the same line.  Set this to nil if enabling the minor mode is
slow on your machine"
  :type 'boolean)

(defcustom hideshowvis-max-file-size (* 1024 100)
  "No highlighting in files larger than this number of bytes.

‘hideshowvis-enable’ will not enable hideshowvis-mode if file is larger than
this value (in bytes).  The minor mode can still be forced to be enabled using
`(hideshowvis-mode 1)'.  Set this variable to nil to disable restriction."
  :type 'natnum)

(defun hideshowvis-highlight-hs-regions-in-fringe (&optional start end _old-text-length)
  "Will update the fringe indicators for all foldable regions in the buffer.
This can be slow for large buffers.  Adjust `hideshowvis-max-file-size' when
this happens for you.

`START', `END', and `OLD-TEXT-LENGTH' are the same as other
functions used with `after-change-functions'."
  (when hs-minor-mode
    (save-excursion
      (save-restriction
        (when (and start end)
          (narrow-to-region start end))
        (goto-char (point-min))
        (remove-overlays (point-min) (point-max) 'hideshowvis-hs t)
        (while (search-forward-regexp hs-block-start-regexp nil t)
          (when (if hideshowvis-ignore-same-line
                    (let ((begin-line (save-excursion
                                        (goto-char (match-beginning 0))
                                        (line-number-at-pos (point)))))
                      (save-excursion
                        (goto-char (match-beginning 0))
                        (ignore-errors
                          (progn
                            (funcall hs-forward-sexp-func 1)
                            (> (line-number-at-pos (point)) begin-line)))))
                  t)
            (let* ((ovl (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put ovl 'before-string
                           (propertize
                            "*hideshowvis*"
                            'display
                            (list 'left-fringe
                                  'hideshowvis-hideable-marker
                                  'hideshowvis-hidable-face)))
              (overlay-put ovl 'hideshowvis-hs t))))))))

;;;###autoload
(defun hideshowvis-click-fringe (event)
  "Handler function for mouse click.

`EVENT' mouse event"
  (interactive "e")
  (mouse-set-point event)
  (end-of-line)
  (if (save-excursion
        (end-of-line 1)
        (or (hs-already-hidden-p)
            (progn
              (forward-char 1)
              (hs-already-hidden-p))))
      (hs-show-block)
    (hs-hide-block)
    (beginning-of-line)))

(define-obsolete-variable-alias 'hideshowvis-mode-map
  'hideshowvis-minor-mode-map
  "0.9")

(defvar hideshowvis-minor-mode-map
  (let ((hideshowvis-mode-map (make-sparse-keymap)))
    (define-key hideshowvis-mode-map [left-fringe mouse-1]
      'hideshowvis-click-fringe)
    hideshowvis-mode-map)
  "Keymap for hideshowvis minor mode.")

;;;###autoload
(define-minor-mode hideshowvis-minor-mode
  "Will indicate regions foldable with hideshow in the fringe."
  :global nil
  (condition-case err
      (if hideshowvis-minor-mode
          (progn
            (unless hs-minor-mode
              (hs-minor-mode 1))
            (hideshowvis-highlight-hs-regions-in-fringe (point-min) (point-max) 0)
            (add-hook 'after-change-functions
                      'hideshowvis-highlight-hs-regions-in-fringe))
        (remove-overlays (point-min) (point-max) 'hideshowvis-hs t)
        (remove-hook 'after-change-functions
                     #'hideshowvis-highlight-hs-regions-in-fringe))
    (error
     (message "Failed to toggle `hideshowvis-minor-mode': %S" err))))

;;;###autoload
(defun hideshowvis-enable ()
  "Will enable hideshowvis minor mode."
  (interactive)
  (when (or (null hideshowvis-max-file-size)
            (<= (point-max) hideshowvis-max-file-size))
    (hideshowvis-minor-mode 1)))

(define-fringe-bitmap 'hideshowvis-hidden-marker [0 24 24 126 126 24 24 0])

(defcustom hideshowvis-hidden-fringe-face 'hideshowvis-hidden-fringe-face
  "*Specify face used to highlight the fringe on hidden regions."
  :type 'face)

(defface hideshowvis-hidden-fringe-face
  '((t (:foreground "#888" :box (:line-width 2 :color "grey75" :style released-button))))
  "Face used to highlight the fringe on folded regions.")

(defcustom hideshowvis-hidden-region-face 'hideshowvis-hidden-region-face
  "*Specify the face to to use for the hidden region indicator."
  :type 'face)

(defface hideshowvis-hidden-region-face
  '((t (:background "#ff8" :box t)))
  "Face to hightlight the ... area of hidden regions.")

(defun hideshowvis-display-code-line-counts (ov)
  "Extend overlay OV to show number of lines hidden for `hideshowvis-symbols'."
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'help-echo "Hidden text. C-c,= to show")
    (overlay-put ov 'before-string
                 (propertize "*fringe-dummy*"
                             'display
                             '(left-fringe
                               hideshowvis-hidden-marker
                               hideshowvis-hidden-fringe-face)))
    (overlay-put ov 'after-string
                 (propertize
                  (format "%d lines" (count-lines (overlay-start ov) (overlay-end ov)))
                  'face 'hideshowvis-hidden-region-face))))

;;;###autoload
(defun hideshowvis-symbols ()
  "Enhance function `hs-minor-mode' with better highlighting for hidden regions.

Defines the things necessary to get a + symbol in the fringe and a yellow marker
indicating the number of hidden lines at the end of the line for hidden regions.

This will change the value of `hs-set-up-overlay' so it will
overwrite anything you've set there."
  (interactive)
  (if (boundp 'hs-display-lines-hidden) ; Emacs 31
      (setq hs-display-lines-hidden t)
    (setq hs-set-up-overlay #'hideshowvis-display-code-line-counts)
    ;; These won't get removed, again. Revert hooks are global and making them
    ;; buffer local might be risky. Instead checking whether showing symbols is
    ;; turned on in the hook functions
    (add-hook 'before-revert-hook #'hideshowvis-remove-overlays)
    (add-hook 'after-revert-hook #'hideshowvis-update-all-overlays)
    (hideshowvis-update-all-overlays)))

;;;###autoload
(defun hideshowvis-symbols-off ()
  "Disable enhanced highlighting of hidden regions."
  (interactive)
  (if (boundp 'hs-display-lines-hidden)
      (setq hs-display-lines-hidden nil)
    (hideshowvis-remove-overlays)
    (setq hs-set-up-overlay 'ignore)))

(defun hideshowvis-remove-overlays ()
  "Will remove all overlays added after calling `hideshowvis-symbols'."
  (when (eq hs-set-up-overlay #'hideshowvis-display-code-line-counts)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'hs)
        (overlay-put ov 'after-string nil)))))

(defun hideshowvis-update-all-overlays ()
  "Will update all overlays added after calling `hideshowvis-symbols'."
  (when (eq hs-set-up-overlay 'hideshowvis-display-code-line-counts)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'hs)
        (if (= (overlay-start ov) (overlay-end ov))
            (delete-overlay ov)
          (hideshowvis-display-code-line-counts ov))))))

(provide 'hideshowvis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hideshowvis.el ends here
