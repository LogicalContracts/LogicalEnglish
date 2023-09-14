#/bin/sh

docker build --tag cclawdev/logicalenglish:local -f swish/Dockerfile .
docker run -it -p 3050:3050  -e LOAD_KB=true -e SWISH_DAEMON_USER=root  --memory="300m" cclawdev/logicalenglish:local