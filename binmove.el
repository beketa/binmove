;;; binmove.el --- Minor mode to move cursor up / down like binary search.

;;
;; Add setting to .emacs.el as follows:
;;
;; (require 'binmove)
;; (define-key global-map [M-down] 'binary-move-down)
;; (define-key global-map [M-up] 'binary-move-up)
;;

(defvar binary-move-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [M-down] 'binary-move-down)
    (define-key map [M-up] 'binary-move-up)
    (define-key map [t] 'binary-move-exit)
    map))

(defvar binary-move-top 0)
(defvar binary-move-bottom 0)

(defvar binary-move-overlay-1 nil)
(defvar binary-move-overlay-2 nil)

(defvar binary-move-overlay-1-color "LightGoldenrod2")
(defvar binary-move-overlay-2-color "PaleGreen1")

(define-minor-mode binary-move-mode
  "Move cursor up / down like binary search."
  :lighter " bm"
  (if binary-move-mode
      (progn
	(setq overriding-terminal-local-map binary-move-mode-map)

	(setq binary-move-overlay-1 (make-overlay 0 0))
	(setq binary-move-overlay-2 (make-overlay 0 0))

	(overlay-put binary-move-overlay-1
		     'face
		     `((background-color . ,binary-move-overlay-1-color)))
	(overlay-put binary-move-overlay-2
		     'face
		     `((background-color . ,binary-move-overlay-2-color)))
	)))

(defun binary-move-get-current-line ()
  (let (beg end line)
    (save-excursion
      (vertical-motion 0)
      (setq end (point))
      (move-to-window-line 0)
      (setq beg (point))
      (setq line (count-screen-lines beg end)))
    line))

(defun binary-move-get-bottom-line ()
  (let (line)
    (save-excursion
      ;; (window-height) - 2 (status line and mini buffer) is the number
      ;; of lines in window.
      (setq line (move-to-window-line (- (window-height) 2))))
    ;; Return line + 1 to include the end of bottom line.
    (+ line 1)))

(defun binary-move-down ()
  (interactive)
  (if binary-move-mode
      (if (= (+ binary-move-top 1) binary-move-bottom)
	  (setq binary-move-top (+ binary-move-top 1))
	(setq binary-move-top (/ (+ binary-move-top binary-move-bottom) 2)))
    ;; If not in binary-move-mode, enter binary-move-mode and initialize.
    (binary-move-mode t)
    (setq binary-move-top (binary-move-get-current-line))
    (setq binary-move-bottom (binary-move-get-bottom-line)))
  (if (/= binary-move-top binary-move-bottom)
      (binary-move-move)
    (binary-move-next-or-previous-line 1)))

(defun binary-move-up ()
  (interactive)
  (if binary-move-mode
      (setq binary-move-bottom (/ (+ binary-move-top binary-move-bottom) 2))
    ;; If not in binary-move-mode, enter binary-move-mode and initialize.
    (binary-move-mode t)
    (setq binary-move-top 0)
    (setq binary-move-bottom (binary-move-get-current-line)))
  (if (/= binary-move-top binary-move-bottom)
      (binary-move-move)
    (binary-move-next-or-previous-line -1)))

(defun binary-move-goto-center ()
  ;; Go to center of the line.
  ;; It may reduce distance from the target to cursor on average (^_^;).
  (let (beg end line-end screen-end)
    (save-excursion
      (vertical-motion 0)
      (setq beg (point))
      ;; Calculate line end position on screen.
      (setq line-end (line-end-position))
      (setq screen-end
	    (car (compute-motion beg '(0 . 0)
				 line-end (cons (window-width) 0)
				 (window-width) nil nil)))
      (setq end (min line-end screen-end)))
    (goto-char (/ (+ beg end) 2))
    ))

(defun binary-move-move ()
  (let (top mid bottom)
    (save-excursion
      (move-to-window-line binary-move-top)
      (setq top (point))
      (move-to-window-line (/ (+ binary-move-top binary-move-bottom) 2))
      (setq mid (point))
      (move-to-window-line binary-move-bottom)
      (setq bottom (point)))

    (move-overlay binary-move-overlay-1 top mid)
    (move-overlay binary-move-overlay-2 mid bottom)

    (goto-char mid)
    (binary-move-goto-center)
    ))

(defun binary-move-next-or-previous-line (arg)
  ;; Hide overlay.
  (move-overlay binary-move-overlay-1 0 0)
  (move-overlay binary-move-overlay-2 0 0)

  (vertical-motion arg)
  (binary-move-goto-center))

(defun binary-move-exit ()
  (interactive)
  (setq overriding-terminal-local-map nil)

  ;; Unread this command keys.
  (setq keys (this-command-keys-vector))
  (setq unread-command-events (append keys unread-command-events))

  (delete-overlay binary-move-overlay-1)
  (delete-overlay binary-move-overlay-2)

  (binary-move-mode 0))

(provide 'binmove)
