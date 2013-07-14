#!/bin/sh
cd `dirname $0`

exec erl -heart -detached -pa $(pwd)/ebin $(pwd)/deps/*/ebin -s dtgcaa
