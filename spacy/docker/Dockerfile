# English
# docker build -f Dockerfile_en -t logicalcontracts/spacyapiplus:en_v2 .
# docker run -p "127.0.0.1:8080:80" logicalcontracts/spacyapiplus:en_v2
# or: docker run -d --restart always -p "demo.logicalcontracts.com:8080:80" logicalcontracts/spacyapiplus:en_v2
#   ...or use your server's internal IP address
#TODO: clean up the above with Docker Composer or other
# docker push logicalcontracts/spacyapiplus:en_v2
FROM jgontrum/spacyapi:en_v2
LABEL maintainer="mc@interprolog.com"
LABEL version="0.1"
LABEL description="English, REST service altered to return also token lemmas and offsets."
COPY parse.py /app/displacy_service
CMD ["bash", "/app/start.sh"]