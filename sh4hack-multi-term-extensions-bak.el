(defun remove-next-line (str)
  (interactive)
  (replace-regexp-in-string "\n" "" str))

(defun trim-whitespace-in-vain (string)
  (interactive)
  (let ((substr string))
    (when (string-prefix-p "\s\s" substr)
      (setq substr (string-trim-left substr)))
    (when (string-suffix-p "\s\s" substr)
      (setq substr (string-trim-right substr)))
    substr))

(defun term-extract-command (beg end)
  (interactive)
  (trim-whitespace-in-vain (remove-next-line (buffer-substring-no-properties beg end))))

(defun term-extract-backward-command ()
  (interactive)
  "It is assumed that PROMPT_ALTERNATIVE variable is equal to `oneline` in .zshrc."
  (let* ((substr (buffer-substring-no-properties (point) (point-min)))
	 (cmd (car (last (split-string substr (format "%s@%s:.+\$ " sh4h-luser sh4h-lhostname))))))
    (trim-whitespace-in-vain (remove-next-line cmd))))

(defun term-extract-current-path (separator)
  (interactive)
  "It is assumed that `PROMPT_ALTERNATIVE variable is equal to `oneline` in .zshrc."
  (let* ((substr1 (buffer-substring-no-properties (point) (point-min)))
	 (substr2 (car (last (split-string substr1 separator))))
	 (path (car (split-string substr2 "$ "))))
    (remove-next-line path)))

(defun term-find-file ()
  (interactive)
  (let* ((sep (format "%s@%s:" sh4h-luser sh4h-lhostname))
	 (dir (concat (term-extract-current-path sep) "/")))
    (find-file (read-file-name "Find File: " dir))))

(defun term-kill (str cmd-symbol)
  (interactive)
  (message "killed string: %s" str)
  (kill-new str)
  (if (> (length kill-ring) kill-ring-max)
      (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil))
  (setq this-command cmd-symbol)
  (setq kill-ring-yank-pointer kill-ring))

(defun term-kill-line ()
  (interactive)
  "`kill-line` is bound to C-k in zsh by default."
  (let ((cmd (term-extract-command (point) (point-max))))
    (term-kill cmd 'term-kill-line)
    (term-send-raw)))

(defun term-backward-kill-line ()
  (interactive)
  "`backward-kill-line` is bound to C-u in zsh by default."
  (let ((cmd (term-extract-backward-command)))
    (term-kill cmd 'term-backward-kill-line)
    (term-send-raw)))

(defun term-kill-region ()
  (interactive)
  "`kill-region` needs to be bound to whatever keys you use in zsh. 
`forward-word` is assigned to C-f, `backward-word` to C-b, and `set-mark-command` to C-SPC by default."
  (let* ((beg (mark))
	 (end (point))
         (key (if (> end beg) "\C-b" "\C-f"))
	 (cmd (term-extract-command beg end))
	 (tlist (string-to-list cmd)))
    (term-kill cmd 'term-kill-region)
    (term-send-raw-string "\C- ")
    (while tlist
      (term-send-raw-string key)
      (setq tlist (cdr tlist)))
    (term-send-raw)))

(defun term-kill-ring-save (beg end)
  (interactive "r")
  (let ((cmd (term-extract-command beg end)))
    (unless (zerop (length cmd))
      (term-kill cmd 'term-kill-ring-save))))

(defun send-to-term ()
  (interactive)  
  (define-key term-raw-map (kbd "C-x") 'term-send-raw)
  (define-key term-raw-map (kbd "C-z") 'term-send-raw)
  (message "key: raw"))

(defun send-to-emacs ()
  (interactive)
  (let ((buf (current-buffer)))    
    (define-key term-raw-map (kbd "C-x") 'Control-X-prefix)
    (define-key term-raw-map (kbd "C-z") 'suspend-frame)
    (kill-buffer (multi-term))
    (switch-to-buffer buf)
    (message "key: emacs")))

(defun term-keystroke-p ()
  (interactive)
  (eq 'term-send-raw (lookup-key term-raw-map (kbd "C-x"))))

(defun toggle-keystrokes ()
  (interactive)
  (if (not (term-keystroke-p))
      (send-to-term)
    (send-to-emacs)))

(add-hook 'term-mode-hook '(lambda ()
			     (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)
			     (define-key term-raw-map (kbd "C-c C-k") 'term-char-mode)
			     (substitute-key-definition 'find-file 'term-find-file term-raw-map global-map)))

(advice-add 'term-line-mode :after '(lambda () (message "term-line-mode: Move back and forth in the terminal!")))
(advice-add 'term-char-mode :after '(lambda () (message "term-char-mode: Get back to the normal terminal.")))
(advice-add 'multi-term :after #'set-term-info)

;;(define-key term-raw-map (kbd "s-n") 'scroll-up)
;;(define-key term-raw-map (kbd "s-p") 'scroll-down)
(define-key term-raw-map (kbd "s-n") 'scroll-up-command)
(define-key term-raw-map (kbd "s-p") 'scroll-down-command)
(define-key term-raw-map (kbd "C-k") 'term-kill-line)
(define-key term-raw-map (kbd "C-u") 'term-backward-kill-line)
(define-key term-raw-map (kbd "M-w") 'term-kill-ring-save)
(define-key term-raw-map (kbd "C-w") 'term-kill-region)
(define-key term-raw-map (kbd "C-;") 'toggle-keystrokes)

(provide 'sh4hack-multi-term-extensions)
