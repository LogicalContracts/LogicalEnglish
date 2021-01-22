# docker build -t logicalcontracts/spacyapiplus:all_v2 .
# docker run -p "127.0.0.1:8080:80" logicalcontracts/spacyapiplus:all_v2
# or: docker run -d --restart always -p "172.31.6.64:8080:80" logicalcontracts/spacyapiplus:all_v2
# docker push logicalcontracts/spacyapiplus:all_v2
FROM jgontrum/spacyapi:all_v2
LABEL maintainer="mc@interprolog.com"
LABEL version="0.1"
LABEL description="English and 6 other languages, REST service altered to return also token lemmas and offsets."
COPY parse.py /app/displacy_service
CMD ["bash", "/app/start.sh"]