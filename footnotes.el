;;; footnotes.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Brad Stewart
;;
;; Author: Brad Stewart <brad@bradstewart.ca> Maintainer: Brad Stewart
;; <brad@bradstewart.ca> Created: décembre 06, 2025 Modified: décembre 06, 2025
;; Version: 0.0.1 Keywords: abbrev bib c calendar comm convenience data docs
;; emulations extensions faces files frames games hardware help hypermedia i18n
;; internal languages lisp local maint mail matching mouse multimedia news
;; outlines processes terminals tex text tools unix vc wp Homepage:
;; https://github.com/brad/footnotes Package-Requires: ((emacs "24.3"))
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
      (ignore)
      ;(org-inline-fn-visualize)
    (org-inline-fn-clear)))


;; Face for rendered paragraph-end footnotes
(defface org-inline-fn-overlay-face
  `((t :inherit page-view-body-face
       :height 0.8
       :slant normal
       :foreground ,(face-foreground 'default nil 'default)
       :background ,(face-background 'default nil 'default)
       :underline nil
       :box nil
       :overline nil
       :strike-through nil
       :inverse-video nil
       :weight normal
       :family unspecified
       :inherit-shadow t)) ;; optional, may need :foreground "gray40" for subtle shadow
  "Face for inline footnote overlays, scaled and slanted, but copying page-view-body-face attributes.")


(defvar org-inline-fn-regex
  "\\[\\(?:fn::\\|cite:\\)\\(\\(?:[^][]\\|\\[[^]]*\\]\\)+\\)\\]"
  "Regex matching inline footnotes or citations in org buffers.")



(defun org-inline-fn-clear ()
  "Remove all paragraph-end inline footnote/citation overlays and restore original text properties."
  (interactive)
  ;; delete overlays

  (let ((inhibit-read-only t))
    (remove-overlays (point-min) (point-max) 'footnote t))

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
  (org-inline-fn--process-region (point-min) (point-max)))

(defun org-inline-fn--process-line (&optional last-index)
  "Process inline fn:: and cite: notes in current line"
  (save-excursion
    (beginning-of-line)
    (org-inline-fn--process-region (point) (line-end-position) last-index)))

(defun org-inline-fn--process-region (beg end &optional last-index)
  "Process inline fn:: and cite: notes between BEG and END."

    ;(org-inline-fn--invalidate-from-line (line-number-at-pos beg))
  (org-inline-fn--delete-in-region beg end)
  (setq last-index (or last-index 0))
  (save-excursion
    (goto-char beg)
    (while (re-search-forward
            org-inline-fn-regex
            end t)
      (let* ((fn-index (1+ last-index ))
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
             fn-string fn-height)

        (setq last-index (1+ last-index))
        ;; build fn-string
        (setq fn-string
              (propertize
               (concat " " sup " " content)
               'face 'org-inline-fn-overlay-face
               'line-height 1.0
               'mouse-face 'highlight
               'keymap
               (let ((map (make-sparse-keymap)))
                 (define-key map [mouse-1]
                   (lambda ()
                     (interactive)
                     (org-inline-fn-edit-marker
                      content-beg-marker
                      content-end-marker)))
                 map)))

        ;; approximate, but that's ok.
        ;; 
        (let ((inhibit-read-only t))
          (setq fn-height (let* ((pixels (string-pixel-width fn-string))
                                 (line-width (window-body-width nil t)))
                            (ceiling (/ (float pixels) line-width)))))
        ;; hide original
        (overlay-put hide-ov 'invisible t)
        ;(put-text-property match-beg match-end 'read-only t)
        (overlay-put hide-ov 'footnote t)
        (overlay-put hide-ov 'sup-ov sup-ov)

        ;; superscript overlay
        (overlay-put sup-ov 'after-string sup)
        ;(overlay-put sup-ov 'read-only t)
        (overlay-put sup-ov 'footnote t)
        (overlay-put sup-ov 'fn-index fn-index)
        (overlay-put sup-ov 'fn-string fn-string)
        (overlay-put sup-ov 'fn-string-cookie fn-string)
        (overlay-put sup-ov 'fn-height fn-height)
        (message "fontnote %d height %d last-index %d" fn-index fn-height last-index))))
    (page-view--set-line-metadata 'org-inline-fn-last-index last-index))

(defun org-inline-fn--invalidate-from-line (line)
  "Invalidate inline-footnote cache starting from LINE (exclusive).
Only footnotes strictly before LINE are considered valid."

  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (beginning-of-line)
    (org-inline-fn--delete-in-region ((point) (point-max)))))


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

(defun org-inline-fn-get-in-line ()

  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (beginning-of-line)
    (org-inline-fn--get-in-region ((point) (line-end-position)))))
  
(defun org-inline-fn-get-in-region (beg end)
  "Return a list of footnotes in region BEG to END that have a 'fn-string property."
  (seq-filter (lambda (ov)
                (and (eq (overlay-get ov 'footnote) t)
                     (overlay-get ov 'fn-string)))
              (overlays-in beg end)))

(defun org-inline-fn-get-overlays-in-region (beg end)
  "Return a list of footnotes in region BEG to END that have a 'fn-string property."
  (seq-filter (lambda (ov)
                (eq (overlay-get ov 'footnote) t)
                     )
              (overlays-in beg end)))

(defun org-inline-fn--delete-in-region (beg end)

  (let ((inhibit-read-only t))
    (mapc (lambda (ov)
            (delete-overlay ov))
            (org-inline-fn-get-overlays-in-region beg end))))



(provide 'org-inline-fn)
;;; footnotes.el ends here
