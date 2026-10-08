FROM clfoundation/sbcl:2.6.1-alpine3.22 AS build
RUN apk --no-cache add curl

# Set working directory
WORKDIR /opt/test-runner
ENV HOME=/opt/test-runner

# Pull down the latest Quicklisp
RUN mkdir build && curl https://beta.quicklisp.org/quicklisp.lisp -o build/quicklisp.lisp

# Install quicklisp
COPY build/install-quicklisp.lisp build/
RUN sbcl --script build/install-quicklisp.lisp

# Build the application
COPY build/build.lisp build/
COPY src quicklisp/local-projects/test-runner
RUN sbcl --script ./build/build.lisp

# Build the runtime image
FROM alpine:3.24.2@sha256:294b683cb724975bec92580e1e685676bd4b50bda910ddb8c51d4cabeaec77e6
WORKDIR /opt/test-runner

# Copy over the test-runner code
COPY --from=build /opt/test-runner/bin/ bin/
COPY --from=build /opt/test-runner/.cache/common-lisp/ .cache/common-lisp/
COPY bin/run.sh bin/

# Set test-runner script as the ENTRYPOINT
ENTRYPOINT ["bin/run.sh"]
