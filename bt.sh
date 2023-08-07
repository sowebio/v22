#!/usr/bin/bash

if [[ $# -eq 0 ]]; then
	echo "Usage: $0 <addresses>"
fi

addr2line -e example/bin/application $*
