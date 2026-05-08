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
FROM alpine:3.23.4@sha256:5b10f432ef3da1b8d4c7eb6c487f2f5a8f096bc91145e68878dd4a5019afde11
WORKDIR /opt/test-runner

# Copy over the test-runner code
COPY --from=build /opt/test-runner/bin/ bin/
COPY --from=build /opt/test-runner/.cache/common-lisp/ .cache/common-lisp/
COPY bin/run.sh bin/

# Set test-runner script as the ENTRYPOINT
ENTRYPOINT ["bin/run.sh"]
