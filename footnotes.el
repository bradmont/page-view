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

(defvar-local org-inline-fn--last-valid-index 0
  "Last footnote index assigned in this buffer.")

(defvar-local org-inline-fn--last-valid-line nil
  "Physical line number of the last indexed footnote.")



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
  "Hide inline fn:: and cite: notes and replace them with overlays."
  (interactive)
  (org-inline-fn-clear)
  (setq org-inline-fn--last-valid-index 0
        org-inline-fn--last-valid-line nil)
  (org-inline-fn--process-region (point-min) (point-max)))

(defun org-inline-fn--process-region (beg end)
  "Process inline fn:: and cite: notes between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward
            "\\[\\(?:fn::\\|cite:\\)\\(\\(?:[^][]\\|\\[[^]]*\\]\\)+\\)\\]"
            end t)
      (let* ((fn-index (1+ org-inline-fn--last-valid-index))
             (content (match-string 1))
             (match-beg (match-beginning 0))
             (match-end (match-end 0))
             (sup (org-inline-fn--superscript fn-index))

             ;; markers
             (content-beg-marker (copy-marker (match-beginning 1)))
             (content-end-marker (copy-marker (match-end 1)))

             ;; overlays
             (hide-ov (make-overlay match-beg match-end))
             (sup-ov  (make-overlay match-beg match-end))

             ;; bookkeeping
             (line (line-number-at-pos match-beg))
             end-string)

        ;; build end-string
        (setq end-string
              (propertize
               (concat " " sup " " content)
               'face 'org-inline-fn-overlay-face
               'line-height 1.0
               'mouse-face 'highlight
               'keymap
               (let ((map (make-sparse-keymap)))
                 (define-key map [mouse-1]
                   (lambda (_event)
                     (interactive)
                     (org-inline-fn-edit-marker
                      content-beg-marker
                      content-end-marker)))
                 map)))

        ;; hide original
        (overlay-put hide-ov 'invisible t)
        (put-text-property match-beg match-end 'read-only t)

        ;; superscript overlay
        (overlay-put sup-ov 'after-string sup)
        (overlay-put sup-ov 'read-only t)
        (overlay-put sup-ov 'footnote t)
        (overlay-put sup-ov 'fn-index fn-index)
        (overlay-put sup-ov 'end-string end-string)
        (overlay-put sup-ov 'end-string-cookie end-string)

        ;; track overlays
        (push hide-ov org-inline-fn--overlays)
        (push sup-ov  org-inline-fn--overlays)

        ;; update buffer-local state
        (setq org-inline-fn--last-valid-index fn-index)
        (setq org-inline-fn--last-valid-line line)))))

(defun org-inline-fn--invalidate-from-line (line)
  "Invalidate inline-footnote cache starting from LINE (exclusive).
Only footnotes strictly before LINE are considered valid."
  (let ((last-valid-ov nil)
        (last-valid-line 0)
        (last-valid-index 0))
    ;; find the last valid footnote strictly before LINE
    (dolist (ov org-inline-fn--overlays)
      (when (and (overlay-get ov 'footnote)
                 (< (line-number-at-pos (overlay-start ov)) line))
        (when (or (not last-valid-ov)
                  (> (overlay-start ov)
                     (overlay-start last-valid-ov)))
          (setq last-valid-ov ov
                last-valid-line (line-number-at-pos (overlay-start ov))
                last-valid-index (overlay-get ov 'fn-index)))))
    ;; delete overlays after the last valid line
    (setq org-inline-fn--overlays
          (cl-delete-if
           (lambda (ov)
             (> (line-number-at-pos (overlay-start ov))
                last-valid-line))
           org-inline-fn--overlays))
    ;; update cache
    (setq org-inline-fn--last-valid-line last-valid-line
          org-inline-fn--last-valid-index last-valid-index)))


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
