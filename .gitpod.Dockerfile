FROM ubuntu

RUN apt-get update && \
    apt-get install -y gcc build-essential git

RUN git clone https://github.com/rusimody/gofer.git && \
    cd /gofer/src && \
    make

ENV PATH=/gofer/src:$PATH
ENV PUGOFER=/gofer/pusimple.pre