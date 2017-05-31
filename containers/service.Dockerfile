# FROM alpine
# RUN apk update && apk add gmp

FROM debian
RUN apt-get update && \
  apt-get install -y libgmp-dev && \
  rm -rf /var/lib/apt/lists/*
ADD ./obj/dist/build/Matching/Matching /opt/service/
ADD ./style /opt/service/style
ADD ./js /opt/service/js
EXPOSE 80
WORKDIR /opt/service
CMD /opt/service/Matching
