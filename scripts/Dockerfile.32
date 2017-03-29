FROM erlang/ubuntu-build:32bit

ADD ./otp.tar.gz /buildroot/

WORKDIR /buildroot/otp/

ENV MAKEFLAGS -j4

CMD ./scripts/build-otp
