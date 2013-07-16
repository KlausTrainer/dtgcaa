#!/bin/sh
cd `dirname $0`

exec erl -pa erl -pa $(pwd)/ebin $(find $(pwd)/deps -type d -name ebin | xargs) -s dtgcaa
