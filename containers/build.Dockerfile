FROM haskell:8.6

ARG regex_user_id=999
ARG regex_group_id=999
RUN groupadd -g $regex_group_id appuser && \
    useradd -m -r -u $regex_user_id -g appuser appuser
USER appuser

RUN apt-get update && \
  apt-get install -y libpcre3-dev && \
  rm -rf /var/lib/apt/lists/*
