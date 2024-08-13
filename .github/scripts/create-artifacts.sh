#!/bin/sh

DIR=${1}
TAG=${2}
VSN=${TAG#OTP-}

mkdir ${DIR}
tar -xzf otp_src.tar.gz
mv otp otp_src_${VSN}
tar -czf ${DIR}/otp_src_${VSN}.tar.gz otp_src_${VSN}
if [ -f otp_doc_man.tar.gz ]; then
    mv otp_doc_man.tar.gz ${DIR}/otp_doc_man_${VSN}.tar.gz
fi
mv otp_doc_html.tar.gz ${DIR}/otp_doc_html_${VSN}.tar.gz
