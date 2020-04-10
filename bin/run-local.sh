#!/bin/sh -e

slug=$1
local_input=`realpath $2`
local_output=`realpath $3`

image=`docker build -q .`

docker run \
       --mount type=bind,source=${local_input},target=/input \
       --mount type=bind,source=${local_output},target=/output \
       --rm \
       -it $image $slug /input/ /output/

docker image rm $image
