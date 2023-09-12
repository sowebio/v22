#!/bin/bash

if [[ $# -ne 1 ]]; then
	echo "Usage: $0 <number of windows>"
	exit
fi

sleep 1

for ((i=0; i<=$1; i++)); do
	/opt/firefox/firefox http://localhost:8080 &
	echo "Launched Firefox $i"
done;
