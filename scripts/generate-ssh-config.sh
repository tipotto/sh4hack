USER="$1"
HOST="$2"
PORT="${3:-22}"
SSH_CONF="$HOME/.ssh/config"
ID_FILE="$HOME/.ssh/id_rsa"
readonly USER HOST PORT SSH_CONF ID_FILE

#if [ -z "$USER" ] || [ -z "$HOST" ]; then
if [[ -z "$USER" || -z "$HOST" ]]; then
    echo "Username and host are required.";
    echo "arg1: User, arg2: Host, arg3: Port(default: 22)";
    exit 1;
fi

cat > $SSH_CONF <<- EOF
Host hack
    User     $USER
    Port     $PORT
    Hostname $HOST
    ServerAliveInterval 60
    ServerAliveCountMax 3
    IdentityFile $ID_FILE
EOF
