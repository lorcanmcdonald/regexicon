FROM haskell:8.0
RUN apt-get update && \
  apt-get install -y libpcre3-dev && \
  rm -rf /var/lib/apt/lists/*
