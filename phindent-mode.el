;; phindent-mode.el  Shows phantom indentation in empty lines

;;;###autoload
(define-minor-mode phindent-mode
  "Toggle Phantom-indent mode, shows indentation in empty lines."
  :lighter    " Phindent"
  :init-value nil
  (if phindent-mode
      (phindent-turn-on)
    (phindent-turn-off)))

(defun phindent-turn-on ()
  "Turn phindent-mode on"
  (add-hook 'after-change-functions 'phindent-update-between nil t)
  (add-hook 'pre-command-hook 'phindent-before-region-change nil t)
  (add-hook 'post-command-hook 'phindent-after-region-change nil t)
  (phindent-update-between (point-min) (point-max) 0))

(defun phindent-turn-off ()
  "Turn phindent-mode off"
  (remove-hook 'after-change-functions 'phindent-update-between t)
  (remove-hook 'pre-command-hook 'phindent-before-region-change)
  (remove-hook 'post-command-hook 'phindent-after-region-change)
  (phindent-clean-buffer (point-min) (point-max)))

(defun phindent-update-between (min max unused)
  "To be called after each change in the buffer"
  (let ((new-min (phindent-get-min min))
        (new-max (phindent-get-max max)))
    (phindent-clean-buffer new-min new-max)
    (phindent-buffer new-min new-max)))

(defun phindent-clean-buffer (min max)
  "Remove all display text properties on new lines between min and max"
  (save-excursion
    (goto-char min)
    (save-match-data
      (while (re-search-forward "\n" (1+ max) t)
        (with-silent-modifications
          (remove-text-properties (match-beginning 0) (match-end 0) '(display)))))))

(defun phindent-before-region-change ()
  (when mark-active
    (with-silent-modifications
      (phindent-update-between (region-beginning) (region-end) 0))))

(defun phindent-after-region-change ()
  ;; There seems to be a bug in emacs which makes it so that after
  ;; `kill-ring-save` the mark is considered active even though it is not.
  (when (and mark-active (not (memq this-command '(kill-ring-save))))
    (with-silent-modifications
      (phindent-clean-buffer (region-beginning) (1- (region-end)))))
  ;; Similar issue with `comment-dwim`, except that we need to regenerate
  ;; the text properties in the region
  (when (memq this-command '(comment-dwim ultimate-js--comment-dwim))
    (with-silent-modifications
      (phindent-update-between (region-beginning) (region-end) 0))))

(defun phindent-buffer (min max)
  "Indent all empty lines in the buffer between min and max"
  (save-excursion
    (goto-char min)
    (while (phindent-next max))))

(defun phindent-next (max)
  "Indent the next empty line, return t if it worked, nil if we reached max"
  (save-match-data
    (when (re-search-forward "\n\\(\n+\\)\\(\t+\\)" max t)
      (let* ((initial-pos (match-beginning 1))
             (number-lines (- (match-end 1) initial-pos))
             (size-indent (- (match-end 2) (match-beginning 2))))
        (dotimes (pos number-lines)
          (phindent-by-at size-indent (+ initial-pos pos)))
        't))))

(defun phindent-by-at (size position)
  "Display [size] tabs at the empty line at position [position]"
  (with-silent-modifications
    (put-text-property position
                       (+ position 1)
                       'display
                       (concat (propertize (make-string size ?\t)
                                           'font-lock-face
                                           'whitespace-tab)
                               "\n"))))

(defun phindent-get-min (min)
  (save-excursion
    (save-match-data
      (goto-char min)
      (re-search-backward "[^\n\t]" nil 1)
      (point))))

(defun phindent-get-max (max)
  (save-excursion
    (save-match-data
      (goto-char max)
      (re-search-forward "[^\n\t]" nil 1)
      (point))))

(provide 'phindent-mode)
