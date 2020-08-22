FROM daewok/sbcl:alpine
# Update all Alpine packages
RUN apk update && apk upgrade
# Set working directory
WORKDIR /opt/test-runner
# Pull down the latest Quicklisp
ADD https://beta.quicklisp.org/quicklisp.lisp .
# Install Quicklisp and load FiveAM
RUN sbcl --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval "(ql:quickload '(:fiveam :st-json))" --quit
# Copy over the test runner
COPY bin/run.lisp bin/run.sh bin/
# Pull down the tooling connector binary
RUN wget -O /usr/local/bin/exercism_local_tooling_webserver https://github.com/exercism/local-tooling-webserver/releases/latest/download/exercism_local_tooling_webserver
# Make the binary executable
RUN chmod +x /usr/local/bin/exercism_local_tooling_webserver
# Set test runner as the ENTRYPOINT
ENTRYPOINT ["sh", "bin/run.sh"]
