;;; footnotes.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Brad Stewart
;;
;; Author: Brad Stewart <brad@bradstewart.ca>
;; Maintainer: Brad Stewart <brad@bradstewart.ca>
;; Created: décembre 06, 2025
;; Modified: décembre 06, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/brad/footnotes
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; Face for rendered paragraph-end footnotes
(defface org-inline-fn-overlay-face
  '((t :height 0.8 :slant normal :inherit shadow))
  "Face used for paragraph-end inline footnote overlays.")

(defvar org-inline-fn--overlays nil)

(defun org-inline-fn-clear ()
  "Remove all paragraph-end inline footnote overlays."
  (interactive)
  (mapc #'delete-overlay org-inline-fn--overlays)
  (setq org-inline-fn--overlays nil))

(defface org-inline-fn-overlay-face
  '((t :height 0.8 :inherit shadow))
  "Face for paragraph-end inline footnote content.")

(defvar org-inline-fn--overlays nil)


(defun org-inline-fn-clear ()
  "Remove all paragraph-end inline footnote/citation overlays and restore original text properties."
  (interactive)
  ;; delete overlays
  (mapc #'delete-overlay org-inline-fn--overlays)
  (setq org-inline-fn--overlays nil)
  ;; remove read-only from the original text
  (save-excursion
    (goto-char (point-min))
    ;(while (re-search-forward "\\[\\(?:fn::\\|cite:\\)[^]]+\\]" nil t)
      ;(put-text-property (match-beginning 0) (match-end 0)
                         ;'read-only nil ))
        (let ((inhibit-read-only t))
          (remove-text-properties (point-min) (point-max) '(read-only nil rear-nonsticky nil))
    )))


(defun org-inline-fn--superscript (n)
  "Convert arbitrary number N to superscript string."
  (let ((digits ["⁰" "¹" "²" "³" "⁴" "⁵" "⁶" "⁷" "⁸" "⁹"])
        (res ""))
    (while (> n 0)
      (setq res (concat (aref digits (% n 10)) res))
      (setq n (/ n 10)))
    (if (string-empty-p res) "⁰" res)))


(defun org-inline-fn-visualize ()
  "Hide [fn::...] and [cite:...] inline references and show numbered overlays at paragraph ends."
  (interactive)
  (org-inline-fn-clear)
  (save-excursion
    (goto-char (point-min))
    (let ((fn-index 1)) ;; global counter
      ;; search the whole buffer
      (while (re-search-forward
              "\\[\\(?:fn::\\|cite:\\)\\(\\(?:[^][]\\|\\[[^]]*\\]\\)+\\)\\]" nil t)

        (let* ((content (match-string 1))
               (beg (match-beginning 0))
               (end (match-end 0))
               (sup (org-inline-fn--superscript fn-index))
               ;; markers for the hidden original text
               (beg-marker (copy-marker beg))
               (end-marker (copy-marker end))
               ;; overlays
               (hide-ov (make-overlay beg end))
               (sup-ov (make-overlay beg end))
               (para-end (save-excursion (goto-char beg) (forward-paragraph) (point)))
               (end-ov (make-overlay (1- para-end) para-end)))
          
          ;; hide original
          (overlay-put hide-ov 'invisible t)
          ;; Apply read-only as a text property to the original inline footnote/citation
          (put-text-property beg end 'read-only t)

          ;; inline superscript
          (overlay-put sup-ov 'after-string sup)
          (overlay-put sup-ov 'read-only t)

          ;; footnote/citation at paragraph end: keymap attached to the after-string itself
          ;; 
          (overlay-put end-ov 'org-inline-fn-beg beg-marker)
          (overlay-put end-ov 'org-inline-fn-end end-marker)
          (overlay-put end-ov 'after-string
                       (propertize
                        (concat " " sup " " content "\n")
                        'face 'org-inline-fn-overlay-face
                        'mouse-face 'highlight
                        'keymap (let ((map (make-sparse-keymap)))
                                  (define-key map [mouse-1]
                                              (lambda (event)
                                                (interactive "e")
                                                (org-inline-fn-edit-marker
                                                 beg-marker
                                                 end-marker)))
                                  map)))

          

          ;; store overlays
          (push hide-ov org-inline-fn--overlays)
          (push sup-ov org-inline-fn--overlays)
          (push end-ov org-inline-fn--overlays)

          ;; increment global counter
          (setq fn-index (1+ fn-index)))))))

(defun org-inline-fn-edit-marker (beg-marker end-marker)
  "Edit the footnote/citation text at markers in a popup buffer."
  (interactive)
  (let ((content (buffer-substring-no-properties beg-marker end-marker)))
    (let ((edited (read-from-minibuffer "Edit footnote: " content)))
      (when edited
        (let ((inhibit-read-only t))
          (delete-region beg-marker end-marker)
          (goto-char beg-marker)
          (insert edited))
        ;; refresh overlays
        (org-inline-fn-visualize)))))


;; Usage:
;; M-x org-inline-fn-visualize   → inline superscripts + text at paragraph end
;; M-x org-inline-fn-clear       → restore original appearance



(provide 'footnotes)
;;; footnotes.el ends here
