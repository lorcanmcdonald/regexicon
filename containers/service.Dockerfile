# FROM alpine
# RUN apk update && apk add gmp

FROM debian
RUN apt-get update && \
  apt-get install -y libgmp-dev && \
  rm -rf /var/lib/apt/lists/*
ADD ./obj/dist-newstyle/build/x86_64-linux/ghc-8.6.5/Matching-0.1.0.0/x/Matching /opt/service/
ADD ./style /opt/service/style
ADD ./font /opt/service/font
ADD ./js/dist /opt/service/js
EXPOSE 80
WORKDIR /opt/service
CMD /opt/service/Matching
