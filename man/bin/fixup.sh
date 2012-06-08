#!/bin/bash

set -e

if [ $# -ne 1 ]; then
    echo" usage: $0 <file>"
    exit 1
fi

file=$1

if [ ! -f ${file} ]; then
    echo ${file}: not found
    exit 1
fi

if [ -f ${file}.tmp ]; then
    echo ${file}.tmp: in use
    exit 1
fi

version=$(cd .. && runghc Version)

mv ${file} ${file}.tmp

cat ${file}.tmp | sed -e 's/\[FIXME: manual\]/User Commands/' -e "s/\\[FIXME: source\\]/Hub ${version}/" >${file}

rm  ${file}.tmp
