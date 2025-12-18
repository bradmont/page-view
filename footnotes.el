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



(define-minor-mode org-inline-footnote-mode
  "Toggle inline footnotes visualization."
  :lighter " FN"
  (if org-inline-footnote-mode
      (org-inline-fn-visualize)
    (org-inline-fn-clear)))


;; Face for rendered paragraph-end footnotes
(defface org-inline-fn-overlay-face
  '((t :height 0.8 :slant normal :inherit shadow))
  "Face used for paragraph-end inline footnote overlays.")

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
  "Convert arbitrary natural number N to superscript string."
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
               (content-beg-marker (copy-marker (match-beginning 1)))
               (content-end-marker (copy-marker (match-end 1)))
               (screen-lines (count-screen-lines (match-beginning 1) (match-end 1)) )
               ;; overlays
               (hide-ov (make-overlay beg end)) ;; hide footnote definition
               (sup-ov (make-overlay beg end)) ;; superscript for reference
               (para-end (save-excursion (goto-char beg) (forward-paragraph) (point)))
               (end-string ))
          ;; TODO: saving overlays as a string, not as an overlay.
          ;; TODO: so instead of using move-overlay, prepend them to the after-string
          ;; in the pagebreak ov (save the original string in the pagebreak of as a cookie)
          
          (setq end-string
                (propertize
                 (concat " " sup " " content )
                 'face 'org-inline-fn-overlay-face
                 'line-height 1.0
                 'mouse-face 'highlight
                 'keymap (let ((map (make-sparse-keymap)))
                           (define-key map [mouse-1]
                                       (lambda (event)
                                         (interactive "e")
                                         (org-inline-fn-edit-marker
                                          content-beg-marker
                                          content-end-marker)))
                           map)))
          ;; hide original
          (overlay-put hide-ov 'invisible t)
           
          ;; Apply read-only as a text property to the original inline footnote/citation
          (put-text-property beg end 'read-only t)

          ;; inline superscript
          (overlay-put sup-ov 'after-string sup)
          (overlay-put sup-ov 'read-only t)
          (overlay-put sup-ov 'fn-index fn-index)
          (overlay-put sup-ov 'footnote t)
          (overlay-put sup-ov 'end-string end-string)
          (overlay-put sup-ov 'end-string-cookie end-string)

          ;; store overlays
          (push hide-ov org-inline-fn--overlays)
          (push sup-ov org-inline-fn--overlays)

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

(defun org-inline-fn-get-in-region (beg end)
  "Return a list of footnotes in region BEG to END."
  (seq-filter (lambda (ov)
                (eq (overlay-get ov 'footnote) t))
              (overlays-in beg end)))




(provide 'org-inline-fn)
;;; footnotes.el ends here
