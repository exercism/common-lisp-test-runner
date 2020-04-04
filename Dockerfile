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
         --eval "(ql:quickload '(:fiveam :dissect :st-json))" --quit
# Copy over the test runner
COPY bin/ bin/
# Set test runner as the ENTRYPOINT
ENTRYPOINT ["/opt/test-runner/bin/run.sh"]
