;;; hideshowvis.el --- Fringe markers for regions foldable by hideshow.el -*- lexical-binding: t; -*-
;;
;; Copyright 2008-2018 Jan Rehders
;;
;; Author: Jan Rehders <jan@sheijk.net>
;; URL: https://github.com/sheijk/hideshowvis
;; Version: 0.7
;; Package-Requires: ((emacs "24"))
;;
;; Contributions and bug fixes by Bryan Waite, Michael Heerdegen, John Yates and
;; Matthew Fidler.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;
;;; Commentary:
;;
;; This minor mode will add little +/- displays to foldable regions in the
;; buffer and to folded regions.  It is indented to be used in conjunction with
;; hideshow.el which is a part of GNU Emacs since version 20.
;;
;; Currently it works for me but is not tested heavily.  Please report any bugs
;; to the above email address
;;
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
;;
;; (dolist (hook (list 'emacs-lisp-mode-hook
;;                     'c++-mode-hook))
;;   (add-hook hook 'hideshowvis-enable))
;;
;; If enabling hideshowvis-minor-mode is slow on your machine use M-x,
;; customize-option, hideshowvis-ignore-same-line and set it to nil.  This will
;; then display - icons for foldable regions of one line, too but is faster
;;
;; To enable displaying a + symbol in the fringe for folded regions,
;; use:
;;
;;    (hideshowvis-symbols)
;;
;; in your ~/.emacs
;;
;; It is not enabled by default because it might interfere with custom
;; hs-set-up-overlay functions
;;
;;; TODO
;;
;; - global-hideshowvis-minor-mode
;; - defcustom for hideshowvis-max-file-size needs to offer setting to nil
;; - add fringe icons lazily, only for visible region (check lazy font-lock to
;;   see if it can help with this)
;;
;;; Changelog
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

(define-fringe-bitmap 'hideshowvis-hideable-marker [0 0 0 126 126 0 0 0])

(defconst hideshowvis-version "v0.7" "Version of hideshowvis minor mode.")

(defface hideshowvis-hidable-face
  '((t (:foreground "#ccc" :box t)))
  "Face to highlight foldable regions"
  :group 'hideshow)

(defcustom hideshowvis-ignore-same-line t
  "No + for single line regions.
Do not display foldable regions in the fringe if the matching closing
parenthesis is on the same line.  Set this to nil if enabling the minor mode is
slow on your machine"
  :group 'hideshow
  :type 'bool)

(defcustom hideshowvis-max-file-size (* 1024 100)
  "No highlighting in files larger than this number of bytes.

‘hideshowvis-enable’ will not enable hideshowvis-mode if file is larger than
this value (in bytes).  The minor mode can still be forced to be enabled using
`(hideshowvis-mode 1)'.  Set this variable to nil to disable restriction."
  :group 'hideshow
  :type 'integer)

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
          (let* ((ovl (make-overlay (match-beginning 0) (match-end 0)))
                 (marker-string "*hideshowvis*")
                 (doit
                  (if hideshowvis-ignore-same-line
                      (let (begin-line)
                        (setq begin-line
                              (save-excursion
                                (goto-char (match-beginning 0))
                                (line-number-at-pos (point))))
                        (save-excursion
                          (goto-char (match-beginning 0))
                          (ignore-errors
                            (progn
                              (funcall hs-forward-sexp-func 1)
                              (> (line-number-at-pos (point)) begin-line)))))
                    t)))
            (when doit
              (put-text-property 0
                                 (length marker-string)
                                 'display
                                 (list 'left-fringe
                                       'hideshowvis-hideable-marker
                                       'hideshowvis-hidable-face)
                                 marker-string)
              (overlay-put ovl 'before-string marker-string)
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

(defvar hideshowvis-mode-map
  (let ((hideshowvis-mode-map (make-sparse-keymap)))
    (define-key hideshowvis-mode-map [left-fringe mouse-1]
      'hideshowvis-click-fringe)
    hideshowvis-mode-map)
  "Keymap for hideshowvis mode.")

;;;###autoload
(define-minor-mode hideshowvis-minor-mode ()
  "Will indicate regions foldable with hideshow in the fringe."
  :init-value nil
  :require 'hideshow
  :group 'hideshow
  :keymap hideshowvis-mode-map
  (condition-case nil
      (if hideshowvis-minor-mode
          (progn
            (hs-minor-mode 1)
            (hideshowvis-highlight-hs-regions-in-fringe (point-min) (point-max) 0)
            (add-to-list 'after-change-functions
                         'hideshowvis-highlight-hs-regions-in-fringe))
        (remove-overlays (point-min) (point-max) 'hideshowvis-hs t)
        (setq after-change-functions
              (remove 'hideshowvis-highlight-hs-regions-in-fringe
                      after-change-functions)))
    (error
     (message "Failed to toggle hideshowvis-minor-mode")
     )))

;;;###autoload
(defun hideshowvis-enable ()
  "Will enable hideshowvis minor mode."
  (interactive)
  (when (or (null hideshowvis-max-file-size)
            (<= (point-max) hideshowvis-max-file-size))
    (hideshowvis-minor-mode 1)))

;;;###autoload
(defun hideshowvis-symbols ()
  "Enhance function `hs-minor-mode' with better highlighting for hidden regions.

Defines the things necessary to get a + symbol in the fringe and a yellow marker
indicating the number of hidden lines at the end of the line for hidden regions.

This will change the value of `hs-set-up-overlay' so it will
overwrite anything you've set there."
  (interactive)
  
  (define-fringe-bitmap 'hideshowvis-hidden-marker [0 24 24 126 126 24 24 0])
  
  (defcustom hideshowvis-hidden-fringe-face 'hideshowvis-hidden-fringe-face
    "*Specify face used to highlight the fringe on hidden regions."
    :type 'face
    :group 'hideshow)
  
  (defface hideshowvis-hidden-fringe-face
    '((t (:foreground "#888" :box (:line-width 2 :color "grey75" :style released-button))))
    "Face used to highlight the fringe on folded regions"
    :group 'hideshow)
  
  (defcustom hideshowvis-hidden-region-face 'hideshowvis-hidden-region-face
    "*Specify the face to to use for the hidden region indicator"
    :type 'face
    :group 'hideshow)
  
  (defface hideshowvis-hidden-region-face
    '((t (:background "#ff8" :box t)))
    "Face to hightlight the ... area of hidden regions"
    :group 'hideshow)

  (defun hideshowvis-display-code-line-counts (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((marker-string "*fringe-dummy*")
             (marker-length (length marker-string))
             (display-string (format "(%d)..." (count-lines (overlay-start ov) (overlay-end ov))))
             )
        (overlay-put ov 'help-echo "Hiddent text. C-c,= to show")
        (put-text-property 0 marker-length 'display
                           (list 'left-fringe 'hideshowvis-hidden-marker 'hideshowvis-hidden-fringe-face)
                           marker-string)
        (overlay-put ov 'before-string marker-string)
        (put-text-property 0 (length display-string) 'face 'hideshowvis-hidden-region-face display-string)
        (overlay-put ov 'display display-string))))
  
  (setq hs-set-up-overlay 'hideshowvis-display-code-line-counts))

(provide 'hideshowvis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hideshowvis.el ends here
