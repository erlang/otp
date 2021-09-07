#!/bin/sh

docker run -v $PWD/:/github otp "make release docs release_docs && make release_docs DOC_TARGETS='man html pdf' RELEASE_ROOT=/github/docs"
sudo chown -R `whoami` docs
cd docs
tar czf ../otp_doc_man.tar.gz man
rm -rf man
tar czf ../otp_doc_html.tar.gz *
