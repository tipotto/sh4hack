USER="$1"
HOST="$2"
PORT="${3:-22}"
FILE="${HOME}/.ssh/config"

if [ -z "$USER" ] || [ -z "$HOST" ]; then
    echo "Username and host are required.";
    echo "arg1: User, arg2: Host, arg3: Port(default: 22)";
    exit 1;
fi

cat > $FILE <<- EOM
Host hack
    User     $USER
    Port     $PORT
    Hostname $HOST
    ServerAliveInterval 60
    ServerAliveCountMax 3
    IdentityFile ~/.ssh/id_rsa
EOM
