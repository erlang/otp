#!/bin/bash

release=${1}
if [[ ${release} < 27 ]]; then
    docker run -v $PWD/:/github otp "make release docs release_docs && make release_docs DOC_TARGETS='man html pdf' RELEASE_ROOT=/github/docs"
else
    docker run -v $PWD/:/github otp "./otp_build download_ex_doc && make release docs release_docs && make release_docs DOC_TARGETS=html RELEASE_ROOT=/github/docs"
fi
sudo chown -R `whoami` docs
cd docs
if test -x man; then
    tar czf ../otp_doc_man.tar.gz man
    rm -rf man
fi
tar czf ../otp_doc_html.tar.gz *
