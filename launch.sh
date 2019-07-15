#!/bin/bash

artifact=$1
recipe=$2
drive=/mydrive

script=`mktemp -p .`
filename=`mktemp -u -p .`

cat > $script <<EOF
#!/usr/bin/env sh

bap $drive/$filename --recipe=$recipe

if [ -f incidents ]; then
   cp incidents $drive
fi
EOF

chmod +x $script

docker run -ti -v `pwd`:$drive binaryanalysisplatform/bap-artifacts:$artifact cp /artifact $drive/$filename
docker run -ti -v `pwd`:$drive --entrypoint $drive/$script binaryanalysisplatform/bap-toolkit

rm -f $script
rm -f $filename
