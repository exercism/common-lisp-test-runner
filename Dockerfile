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

# Copy over the representer code
COPY --from=build /opt/test-runner/test-runner bin/
COPY bin/run.sh bin/

# Pull down the tooling connector binary and make it executable.
ADD https://github.com/exercism/tooling-webserver/releases/latest/download/tooling_webserver /usr/local/bin
RUN chmod +x /usr/local/bin/tooling_webserver

# Set test runner as the ENTRYPOINT
ENTRYPOINT ["sh", "bin/run.sh"]
