FROM daewok/sbcl:alpine AS build
RUN apk update && apk upgrade

# Set working directory
WORKDIR /opt/test-runner
ENV HOME /opt/test-runner

# Pull down the latest Quicklisp
ADD https://beta.quicklisp.org/quicklisp.lisp quicklisp/

# install quicklisp
COPY build/install-quicklisp.lisp build/
RUN sbcl --script build/install-quicklisp.lisp

COPY build/build.lisp build/
COPY src quicklisp/local-projects/test-runner
RUN sbcl --script ./build/build.lisp

# Built the runtime image
FROM alpine
WORKDIR /opt/test-runner

# Copy over the test-runner code
COPY --from=build /opt/test-runner/test-runner bin/
COPY --from=build /opt/test-runner/.cache/common-lisp/ .cache/common-lisp/
COPY bin/run.sh bin/

# Set test runner as the ENTRYPOINT
ENTRYPOINT ["sh", "bin/run.sh"]
