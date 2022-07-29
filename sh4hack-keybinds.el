(defun remove-next-line (str)
  (interactive)
  (replace-regexp-in-string "\n" "" str))

(defun term-extract-substring (beg end)
  (interactive)
  (let* ((substr1 (filter-buffer-substring beg end))
         (substr2 (car (last (split-string substr1 "\n")))))
    (cadr (split-string substr2 "@kali:"))))

(defun term-extract-command (beg end)
  (interactive)
  (let ((substr (filter-buffer-substring beg end)))
    (remove-next-line (car (split-string substr "  ")))))

(defun term-extract-backward-command (beg end)
  (interactive)
  (let* ((substr (term-extract-substring beg end))
	 (cmd-line (cadr (split-string substr "$ ")))
	 (cmd (car (split-string cmd-line "  "))))
    (remove-next-line cmd)))

(defun term-extract-current-path (beg end)
  (interactive)
  (let* ((substr (term-extract-substring beg end))
	 (path (car (split-string substr "$ "))))
    (remove-next-line path)))

(defun term-find-file ()
  (interactive)
  (let ((dir (concat (term-extract-current-path (point) (point-min)) "/")))
    (find-file (read-file-name "Find File: " dir))))

(defun term-kill (str cmd-symbol)
  (interactive)
  (message "killed str: %s" str)
  (kill-new str)
  (if (> (length kill-ring) kill-ring-max)
      (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil))
  (setq this-command cmd-symbol)
  (setq kill-ring-yank-pointer kill-ring))

(defun term-kill-line ()
  (interactive)
  (message "term-kill-line")
  (let ((cmd (term-extract-command (point) (point-max))))
    (term-kill cmd 'term-kill-line)
    (term-send-raw)))

(defun term-backward-kill-line ()
  (interactive)
  (message "term-backward-kill-line")
  (let ((cmd (term-extract-backward-command (point) (point-min))))
    (term-kill cmd 'term-backward-kill-line)
    (term-send-raw)))

(defun term-kill-region ()
  (interactive)
  (message "term-kill-region")
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
    (term-send-raw-string "\C-h")))

(defun term-kill-ring-save (beg end)
  (interactive "r")
  (message "term-kill-ring-save")
  (let ((cmd (term-extract-command beg end)))
    (when (not (string= "" cmd))
      (term-kill cmd 'term-kill-ring-save))))

(defun send-to-term ()
  (interactive)
  (message "emacs-keys: raw")
  (define-key term-raw-map (kbd "C-x") 'term-send-raw)
  (define-key term-raw-map (kbd "C-z") 'term-send-raw))

(defun send-to-emacs ()
  (interactive)
  (let ((buf (current-buffer)))
    (message "emacs-keys: default")
    (define-key term-raw-map (kbd "C-x") 'Control-X-prefix)
    (define-key term-raw-map (kbd "C-z") 'suspend-frame)
    (kill-buffer (multi-term))
    (switch-to-buffer buf)))

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
(advice-add 'term-line-mode :before '(lambda () (message "term-line-mode")))
(advice-add 'term-char-mode :before '(lambda () (message "term-char-mode")))
;;(define-key term-raw-map (kbd "s-n") 'scroll-up)
;;(define-key term-raw-map (kbd "s-p") 'scroll-down)
(define-key term-raw-map (kbd "s-n") 'scroll-up-command)
(define-key term-raw-map (kbd "s-p") 'scroll-down-command)
(define-key term-raw-map (kbd "C-k") 'term-kill-line)
(define-key term-raw-map (kbd "C-u") 'term-backward-kill-line)
(define-key term-raw-map (kbd "M-w") 'term-kill-ring-save)
(define-key term-raw-map (kbd "C-w") 'term-kill-region)
(define-key term-raw-map (kbd "C-;") 'toggle-keystrokes)

;; Newline(\n) after the below first quotation is necessary.
;; "Not enough arguments for format string" error occurs otherwise.
(defhydra sh4hack-menu (:exit t :color pink :hint nil)
  "

 ######  ##     ## ##        ##     ##    ###     ######  ##    ## 
##    ## ##     ## ##    ##  ##     ##   ## ##   ##    ## ##   ##  
##       ##     ## ##    ##  ##     ##  ##   ##  ##       ##  ##   
 ######  ######### ##    ##  ######### ##     ## ##       #####    
      ## ##     ## ######### ##     ## ######### ##       ##  ##   
##    ## ##     ##       ##  ##     ## ##     ## ##    ## ##   ##  
 ######  ##     ##       ##  ##     ## ##     ##  ######  ##    ## 

               ^Metasploit^               |              ^Shell^               |      ^Others        
^^^^^^^^----------------------------------------------------------------------------------------------
       ^Linux^       |       ^Windows^      |       ^SSH^      |     ^Normal^      |      ******
^^^^^^^^----------------------------------------------------------------------------------------------
 _l_: x64-staged     |  _w_: x64-staged     |  _p_: password   |  _n_: netcat      |  _i_: info
 _L_: x64-stageless  |  _W_: x64-stageless  |  _P_: publickey  |  _s_: socat       |  _I_: inspect
 _m_: x86-staged     |  _x_: x86-staged     | ^ ^              | ^ ^               |  _r_: register
 _M_: x86-stageless  |  _X_: x86-stageless  | ^ ^              | ^ ^               |  _e_: edit
 ^ ^                 | ^ ^                  | ^ ^              | ^ ^               |  _q_: quit

"
  ("l" (msf-linux "x64" "staged"))
  ("L" (msf-linux "x64" "stageless"))
  ("m" (msf-linux "x86" "staged"))
  ("M" (msf-linux "x86" "stageless"))
  ("w" nil)
  ("W" nil)
  ("x" nil)
  ("X" nil)
  ("p" auth-with-password)
  ("P" auth-with-public-key)
  ("n" upgrade-netcat)
  ("s" connect-with-socat)
  ("i" get-info)
  ("I" inspect-server)
  ("r" register-info)
  ("e" edit-remote-file)
  ("q" nil))

(global-set-key (kbd "C-c s h") 'sh4hack-menu/body)

(provide 'sh4hack-keybinds)
