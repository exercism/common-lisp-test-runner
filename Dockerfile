FROM daewok/sbcl:alpine AS build
# Update all Alpine packages
RUN apk update && apk upgrade

# Set working directory
WORKDIR /opt/test-runner

# to control where quicklisp puts its files
ENV HOME=/opt/test-runner

# Build the builder
ADD https://beta.quicklisp.org/quicklisp.lisp .
COPY build/ build/
RUN ./build/bootstrap.sh ./build/builder

# Build the test-runner
COPY src/ src/
RUN ./build/build.sh ./build/builder ./test-runner

# Minimal image for running the test-runner.
FROM daewok/sbcl:alpine
WORKDIR /opt/test-runner
# controling where quicklisp looks
ENV HOME=/opt/test-runner
#RUN mkdir bin
COPY --from=build /opt/test-runner/.cache ./.cache/
COPY --from=build /opt/test-runner/quicklisp ./quicklisp/
COPY --from=build /opt/test-runner/test-runner bin/
COPY bin/run.sh bin/

ENTRYPOINT ["sh", "bin/run.sh"]
