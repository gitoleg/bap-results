#!/bin/bash

repo=binaryanalysisplatform/bap-artifacts

for tag in `docker images $repo  --format "{{.Tag}}" | grep -v none`; do

    echo docker run -ti -v `pwd`:/mydrive $repo:$tag cp /artifact /mydrive

    docker run -ti -v `pwd`:/mydrive $repo:$tag cp /artifact /mydrive
    mkdir $tag
    mv artifact $tag

done
