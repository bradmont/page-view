;;; page-view.el --- Simple page view -*- lexical-binding: t; -*-

;; Author: Brad Stewart
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (olivetti "0"))
;; Keywords: convenience, editing
;; URL: https://example.com/page-view

;;; Commentary:

;; This is a package meant to emulate the visual, page-based layout of a word
;; processor like LibreOffice Write or Microsoft Word in org-mode. Mainly, it
;; divides your document into pages, divided by a visual pagebreak/footer,
;; implemented as an overlay. It currently applies styling and page sizes
;; that approximate a letter-sized page using Times New Roman 12 point, at 1.5
;; spacing. The package is built to work on top of olivetti-mode, but it should
;; be able to function without it with minimal changes.

;;; Code:
;;;

(require 'olivetti)

(defvar page-view-debug-flag nil
  "If non-nil, show debug overlays for line heights and cumulative heights.")

(defvar page-view-lines-per-page 36
  "Number of lines per page for page breakinsertion. Approximates a word
processer Times New Roman 12 point, 1.5 spacing.")
;; TODO: calculate this based on face styles; make face styles configurable

(defun page-view-debug-overlay (height cumulative-height)
  "Display a small overlay in the left fringe showing HEIGHT and
CUMULATIVE-HEIGHT for the current line."
  (message "line %d: height: %d ch: %d" (line-number-at-pos) height cumulative-height)
  (let* ((start (save-excursion (beginning-of-line) (point)))
         (ov (make-overlay start (max (1+ start) (point-max))))
         (label (format "%d/%d" height cumulative-height)))

    (remove-overlays (max (1- start) (point-min))
                     (point-max)
                     'page-view-debug t)
    ;; store overlay so it doesn't get GC'd, optionally buffer-local list
    (overlay-put ov 'page-view-debug t)
    ;; after-string can be displayed in the left fringe
    (overlay-put ov 'before-string
                 (propertize label
                             'display '((margin left-margin))
                             'face '(:foreground "red" :weight bold)))))


(defun page-view-remove-debug-overlays()
  "Clear overlays from page-view-debug-mode"
  (interactive)
    (remove-overlays (point-min)
                     (point-max)
                     'page-view-debug t))

(defun page-view-maybe-debug (height cumulative)
  "Emit a debug overlay call if `page-view-debug-flag` is non-nil."
  (when page-view-debug-flag
     (page-view-debug-overlay height cumulative)))


(defun page-view-setup()
  "Setup the display visuals. We assume olivetti mode, but it shouldn't be
necessary. We haven't tested with visual-fill-column or other packages
though. Overrid or advise this function to adjust."
  (interactive)
  ;; use margins and  fringes
  (setq olivetti-style 'fancy)
  (olivetti-mode)

  ;; we're emulating Times New Roman 12 at 1.5 spacing in a word processor.
  ;; eventually these ought to be configurable
  (setq-local olivetti-body-width 69)

  (setq my-line-spacing 8)

  (setq-local default-text-properties
              `(line-spacing ,my-line-spacing
                wrap-prefix (space . (:height (,(+ (default-font-height) my-line-spacing))
                                      :width (0)))))
  
  (face-remap-add-relative 'default `(:family "Times New Roman"))
  
 (setup-olivetti-fringes) 
  
  (org-indent-mode -1)
  (diff-hl-mode -1)
  (hl-line-mode -1)
  (redraw-display))



(defun setup-olivetti-fringes ()
  "Set up fringe backgrounds for other modes that use them to follow
the Olivetti fringe style."
  (interactive)
  (when olivetti-mode
    ;; Olivetti fringe itself
    ;; update the root face
    (set-face-attribute 'olivetti-fringe nil :background (face-background 'tab-bar nil t))
    ;; Flycheck and diff-hl fringes
    (dolist (face '(flycheck-fringe-error
                    flycheck-fringe-warning
                    flycheck-fringe-info
                    diff-hl-insert
                    diff-hl-delete
                    diff-hl-change))
      (set-face-attribute face nil :inherit 'olivetti-fringe ))))


(define-minor-mode page-view-debug-mode
  "page-view-mode display debug info like cached line heights"
  :init-value nil
  :lighter "PVDebug"
  (if page-view-debug-mode
      (progn
        (setq page-view-debug-flag t)
        (page-view-mode 1)
        )
    (progn
      (page-view-mode -1)
      (setq page-view-debug-flag nil)
      )))

(define-minor-mode page-view-mode
  "Word-processor-like view for Org."
  :init-value nil
  :lighter " PageView"
  (if page-view-mode
      (progn
        ;; Register the jit-lock function buffer-locally
        (jit-lock-register #'page-view-jit-reflow)
        (add-hook 'window-scroll-functions #'page-view--on-scroll nil t)

        (add-hook 'after-change-functions #'page-view-handle-change nil t)
        (page-view-setup)
        (page-view-reset)
        (page-view-jit-reflow  (window-start) (window-end))
        )
    (progn
      (remove-hook 'after-change-functions #'page-view-handle-change t)
      (remove-hook 'window-scroll-functions #'page-view--on-scroll t)
      (jit-lock-unregister #'page-view-jit-reflow)

      (if page-view-debug-flag 
          (page-view-remove-debug-overlays)
        )

      (page-view-reset))))


(defun page-view-jit-reflow (start end)
  "Reflow pages for lines in the redisplay region."
  (page-view--reflow-screen start end)
  )


(defun page-view-reflow-screen ()
  "Apply pagebreaks on region currently visible in window"
  (interactive)
  (page-view--reflow-screen
   (window-start)
   (window-end (selected-window) t)))



(defun page-view--reflow-screen (start end)
  "Apply pagebreaks for the region between START and END buffer positions."
  ;; with our cache code adding pagebreaks as it goes, we shoud just be
  ;; able to call the cache for end line...
  (page-view-get-cumulative-height (line-number-at-pos end)))

(defun page-view-goto-visual-line (visual-line)
  "Use the page-view cache to jump to a visual line. DO NOT USE
in functions that get called repeatedly, it'll be inefficient. Works
best if (point) is already relatively near the target visual-line."
  ;; go forward by physical lines until we pass visual-line
  (while (and
          (< (line-number-at-pos) (line-number-at-pos (point-max)) )
          (< (page-view-get-cumulative-height) visual-line))
    (forward-line 1))

  ;; go back to the physical line at or before visual-line
  (while (and
          (> (line-number-at-pos) (line-number-at-pos (point-min)) )
          (> (page-view-get-cumulative-height) visual-line))
    (forward-line -1))
  (line-move-visual (- visual-line (page-view-get-cumulative-height) 1 )))

(defun page-view--on-scroll (window display-start)
  "Hook function for `window-scroll-functions` to apply pagebreaks."
  (page-view--reflow-screen display-start (window-end window t)))

;; A buffer-local hash: page-number -> overlay
(defvar-local page-view-overlays (make-hash-table))


(defun page-view-maybe-apply-pagebreak (cumulative-height line-height)
  "Check if we need to apply a pagebreak here, between CUMULATIVE-HEIGHT and
CUMULATIVE-HEIGHT + LINE-HEIGHT. TODO make into a macro"

  ;; logic: if the end of our previous physical line is on a different page
  ;; than the end of this phyical line, there's a page break here
  (let* ((previous-visual-line cumulative-height ) 
         (last-line (+ cumulative-height line-height)) 
         (previous-page (/ previous-visual-line page-view-lines-per-page))
         (this-page (/ last-line page-view-lines-per-page)))
    (if (> this-page previous-page)
        (let* ((target-visual-line (* this-page page-view-lines-per-page)))

          
          (save-excursion
            (line-move-visual (- target-visual-line cumulative-height) )
            (page-view-apply-pagebreak this-page target-visual-line))))))


(defun page-view-apply-pagebreak (page-number &optional target-line)
  "Insert a visual page break below the current line.
PAGE-NUMBER is displayed. HEIGHT is the number of empty lines for spacing (default 3)."
  ; TODO use a configurable function for formatting page-break text
  ; TODO use a configurable function for formatting page-break style
  (interactive "nPage number: \nP")
  (beginning-of-visual-line)
  (let* ((height 3)
         (ov-pair (gethash page-number page-view-overlays))
         (ov (or (car ov-pair) nil))
         (ov-margin (or (cdr ov-pair) nil))
         (label
          (if page-view-debug-flag
              (format "Page %d; line %d; visual-line %d" page-number (line-number-at-pos) target-line)
            (format "Page %d" page-number )))
         (pad  (/ (- (or olivetti-body-width fill-column) (length label) ) 2) ))

    (if page-view-debug-flag
        (message "page-break : %s" label))

    (if ov
        (move-overlay ov (point) (point)))

    (if ov-margin
        (move-overlay ov-margin (point) (point)))

    (unless ov

      (setq ov (make-overlay (point) (point)))
      (setq ov-pair nil)

      (overlay-put ov 'pagebreak t)  ;; <--- mark it
      (overlay-put ov 'after-string
                   (concat "\n"
                           (propertize
                            (concat

                             ;;(propertize " " 'display `((space :width , (+ 1 (window-text-width)) :height ,height)))
                             ;; shorten for now until I figure out how to make ov-margin tlaler...
                             (make-string pad ?\s)
                             label
                             (propertize " " 'display `((space :width , (+ 1 (window-text-width)) :height ,height)))
                             )
                            'face `(:family "monospace" :background ,(face-background 'tab-bar)
                                    :foreground ,(face-foreground 'default)
                                    :weight bold
                                    :underline nil
                                    :slant normal )
                            ))))
    (unless ov-margin
      (setq ov-margin (make-overlay (point) (point)))
      (setq ov-pair nil)
      (overlay-put ov-margin 'pagebreak t)  ;; <--- mark it

      (overlay-put ov-margin 'before-string
                   (propertize " "
                   'display
                   `((margin left-margin) ,(propertize (make-string olivetti-margin-width ?\s)
                        'face `(:family "monospace"
                                ;:width ,olivetti-margin-width
                                :background ,(face-background 'tab-bar)
                                :foreground "white"
                                :underline nil)))
                               )
                            )

      (overlay-put ov-margin 'after-string
                   (propertize " "
                   'display
                   `((margin right-margin) ,(propertize (make-string olivetti-margin-width ?\s)
                        'face `(:family "monospace"
                                ;:width ,olivetti-margin-width
                                :background ,(face-background 'tab-bar)
                                :foreground "white"
                                :underline nil)))
                               )
                            )
      )
    (unless ov-pair

      (puthash page-number (cons ov ov-margin) page-view-overlays)
      )
    ))


(defun page-view-clear (&optional start end)
  "Remove all page-break overlays created by `page-view-apply-pagebreak`.
START and END specify the region to clear; defaults to the whole buffer."
  (interactive)
  (remove-overlays (or start (point-min))
                   (or end   (point-max))
                   'pagebreak t))


(defun page-view-reset()
  "Hard reset: wipe overlays, metadata"
  (interactive)
  (setq page-view-overlays (make-hash-table))
  (page-view-clear-all-line-metadata)
  (page-view-clear)
)


;;;;;; line-height caching module starts here ;;;;;;;
;;;;;; mostly working, but reflowing while typing is broken

(defun page-view-compute-line-height (&optional line)
  "Compute the visual height of a physical line.
LINE is the 1-based line number (defaults to the current line)."
  (save-excursion
    (let* ((start (if line ;; if we don't need to move, don't waste oerations
                      (progn
                        (goto-char (point-min))
                        (forward-line (1- line))
                        (point))
                    (beginning-of-line)
                    (point)))
           (end (line-end-position)))
      ;; count-screen-lines returns the number of screen lines
      ;; the region occupies, including wrapping
      (if (= start end) 1 (count-screen-lines start end)) ; every line 
      )))

(defun page-view-set-line-height (&optional line)
  "Compute and store the visual height of a physical LINE as a text property.
LINE is 1-based and defaults to the current line."
  (save-excursion
    ;; move to the line if LINE is provided, otherwise just beginning-of-line
    (if line
        (progn
          (goto-char (point-min))
          (forward-line (1- line)))
      (beginning-of-line))
    ;; compute height from current line without moving point elsewhere
    (let ((height (page-view-compute-line-height)))
      ;; store the height on the first character
      ;; 
      (put-text-property (point) (min (1+ (point)) (point-max)) ;; see comment
                         ;; page-view-get-cumulative-height
                         'page-view-line-height height)
      height
      )))

(defvar-local page-view-cache-invalid-from nil
  "First physical line number with invalid cached line height.
Set by `page-view-handle-change` and used for incremental recomputation.")

(defun page-view-handle-change (beg end _len)
  "Invalidate cached line-height properties for lines touched by the change."
  ;; Remove the 'line-height property from the changed region
  ;; 
  ;(message "(page-view-handle-change %d %d %d)" beg end _len) 
  (remove-text-properties beg (min (1+ end) (point-max)) '(page-view-line-height nil))
  ;; Optionally track the first invalidated line for incremental recalculation
  (let ((line (line-number-at-pos beg)))
    (setq page-view-cache-invalid-from
          (min (or page-view-cache-invalid-from 1) line)
          ))
  (page-view-set-line-height)
  (page-view-get-cumulative-height (line-number-at-pos (window-end))))


(defun page-view-get-line-height (&optional line)
  "Return the cached visual height of a physical line.
If LINE is provided (1-based), move to that line first.
If no cached value exists, compute and store it for the current line."
  (save-excursion
    ;; navigate if line number is given
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))
    ;; ensure we are at beginning of line
    (beginning-of-line)
    (let ((start (point))
          (height (get-text-property (point) 'page-view-line-height)))
      (if height
          height
        ;; compute, store, and return
        (page-view-set-line-height)))))

(defun page-view--goto-end-of-cache()
  "Go to the last physical line with valid cached values. If there is no
cache, goto line 1"

  (if (= (line-number-at-pos) page-view-cache-invalid-from)
          ;; Only traverse buffer if we're not already on the first invalid line.
      (if (> (line-number-at-pos) 1) ; move back to last valid line
          (forward-line -1)
        (progn ;; at line 1; calculate & store its height
          (page-view-set-line-height)
          )
        )
    (progn
      ;; otherwise move forward from the beginning
      (goto-char (point-min))
      (forward-line (- page-view-cache-invalid-from 2))) ; last valid line
    )
  (line-number-at-pos)
  )

(defun page-view-get-cumulative-height (&optional line)
  "Return the cumulative visual height up to LINE.
LINE defaults to the current line. Uses and updates cached
'page-view-cumulative-height text properties."
  (unless line
    (setq line (line-number-at-pos)))

  (unless page-view-cache-invalid-from
    (setq page-view-cache-invalid-from 1))

  (save-excursion
    (if (>= line page-view-cache-invalid-from ) ; cache invalid here
        ;; compute from the first invalid line up to LINE.
        (progn
          (page-view--goto-end-of-cache)
          (beginning-of-line)

          ;; Starting cumulative height.
          (let ((cumulative-height (or (get-text-property (point) 'page-view-cumulative-height)
                                       0 ))) ;; we are on a cached line;

            ;; if no cumulative-height is stored, we're on line 1
            ;; Add this line's height if necessary.


            ;; Walk forward until reaching the requested line.
            (while (< (line-number-at-pos) line)
              (forward-line 1)
              (page-view-maybe-apply-pagebreak cumulative-height (page-view-get-line-height))
              (setq cumulative-height (+ cumulative-height (page-view-get-line-height)))
              (page-view-maybe-debug (page-view-get-line-height) cumulative-height)

                                        ;(put-text-property (point) (1+ (point))
              (put-text-property (point) (min (1+ (point)) (point-max)) ;; if we're on a
                                 ;; final, empty line, we're at point-max and can't set
                                 ;; the property past the end of the buffer. In this case
                                 ;; put-text-property will silently do nothing. We won't
                                 ;; cache anything for this line, so our getter will call
                                 ;; down to page-view-compute-line-height which will return
                                 ;; a default value of 1.
                                 'page-view-cumulative-height cumulative-height)

              ;; Update invalid-from pointer.
            (setq page-view-cache-invalid-from (1+ line)))
            cumulative-height))

      ;; Cache already valid for this line: just fetch.
      (progn
        (unless (= (line-number-at-pos) line);; goto line unless there already
          (goto-char (point-min))
          (forward-line (1- line))) 

        (beginning-of-line)
        (or (get-text-property (point) 'page-view-cumulative-height)
            0)))))

(defun page-view-print-line-heights-and-cumulative ()
  "Print a list of (line-height . cumulative-height) for all lines in the buffer.
Lines without cached heights show nil for that component."
  (interactive)
  (message "Cache invalid from: %d" page-view-cache-invalid-from)
  (let (results cumulative)
    (setq cumulative 0)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((lh (get-text-property (point) 'page-view-line-height))
              (ch (get-text-property (point) 'page-view-cumulative-height)))
          (push (cons lh ch) results)
          ;; update running cumulative if cumulative-height is nil
          (setq cumulative (+ cumulative (or lh 0))))
        (forward-line 1)))
    (setq results (nreverse results))
    (message "%S" results)
    results))

(defun page-view-clear-all-line-metadata ()
  "Remove all page-view line height and cumulative height properties from the buffer."
  (interactive)
  (setq page-view-cache-invalid-from 1)
  (remove-text-properties (point-min) (point-max)
                          '(page-view-line-height nil
                            page-view-cumulative-height nil)))


(provide 'page-view)



;;; page-view.el ends here

