(defhydra sh4hack-util-menu (:exit t :color pink :hint nil)
  "

 _e_: edit remote file
 _f_: fetch remote file
 _s_: show info
 _i_: inspect remote server
 _u_: update info
 _r_: run script remotely

"
  ("e" edit-remote-file)
  ("f" fetch-remote-file)
  ("s" get-info)
  ("i" inspect-server)
  ("u" register-info)
  ("r" run-script-remotely)
  ("h" sh4hack-menu/body)
  ("q" nil))

(defhydra sh4hack-command-menu (:exit t :color pink :hint nil)
  "

 _g_: gobuster-dir | _G_: gobuster-vhost | _r_: rustscan-all | _R_: rustscan-ports
 _y_: hydra-normal | _Y_: hydra-post

"
  ("g" (run-from-snippet 'gobuster 'dir 'single))
  ("G" (run-from-snippet 'gobuster 'vhost 'single))
  ("r" (run-from-snippet 'rustscan 'all))
  ("R" (run-from-snippet 'rustscan 'ports))
  ("y" (run-from-snippet 'hydra 'get 'multi))
  ("Y" (run-from-snippet 'hydra 'post 'multi))
  ("h" sh4hack-menu/body)
  ("q" nil))

(defhydra sh4hack-shell-menu (:exit t :color pink :hint nil)
  "

     ^MSF-linux^     |    ^MSF-Windows^    |      ^SSH^      |      ^Others^     
^^^^^^^^--------------------------------------------------------------------------
 _l_: x64-staged     | _w_: x64-staged     | _p_: password   | _n_: netcat      
 _L_: x64-stageless  | _W_: x64-stageless  | _P_: public key | _s_: socat       
 _m_: x86-staged     | _x_: x86-staged     | ^ ^             | ^ ^                 
 _M_: x86-stageless  | _X_: x86-stageless  | ^ ^             | ^ ^                 

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
  ("h" sh4hack-menu/body)
  ("q" nil))

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

 _s_: shell | _c_: command | _u_: util 

"
  ("s" sh4hack-shell-menu/body)
  ("c" sh4hack-command-menu/body)
  ("u" sh4hack-util-menu/body)
  ("q" nil))

(global-set-key (kbd "C-c h") 'sh4hack-menu/body)
(provide 'sh4hack-keybindings)
