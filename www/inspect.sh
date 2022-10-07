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
	printf "\033[0;35m			         Shell utilities for Emacs hackers\033[0m\n\n"
}

homeDir() {
    if [ ! -d "$HOME" ]; then return 1; fi
    return 0;
}

sshDir() {
    if [ ! -d "$HOME/.ssh" ]; then return 1; fi
    return 0;
}

inspect() {
    if [ $# -lt 1 ]; then
	return 1;
    fi

    case "$1" in
	"ssh")
	    echo "[+] You can SSH with public key.";
            echo "[+] Generate key pair on your PC and transfer public key via Netcat.";
	    ;;
	"upgrade")
            echo "[-] You CANNOT SSH with public key.";
            echo "[-] $(whoami)'s home directory does not exist...";
            echo "[+] Recommend using socat or upgrading current shell.";
	    ;;
	*)	    
	    return 1;
	    ;;
    esac
}

banner;
if homeDir; then
    echo "[+] $(whoami)'s home directory exists.";
    
    if ! sshDir; then
	mkdir "$HOME/.ssh";
        echo "[+] New SSH directory is created.";
    fi

    inspect ssh;
else
    inspect upgrade;
fi
