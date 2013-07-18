#!/bin/sh
cd `dirname $0`

export HEART_COMMAND="./start.sh"
exec erl -heart -detached -pa $(pwd)/ebin $(find $(pwd)/deps -type d -name ebin | xargs) -s dtgcaa
