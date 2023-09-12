#!/bin/bash

color="#ffffff"
size=1024

while getopts ":c:f:s:" opt; do
	case $opt in
		c) color="$OPTARG"
		;;
		f) file="$OPTARG"
		;;
		s) size=$OPTARG
		;;
		\?) echo "Invalid option -$OPTARG" >& 2
		exit 1
		;;
	esac
done

echo $file
echo $color
echo $size

output=$(echo $file | cut -d "." -f1)

convert -background none -density 1000 -resize "$size"x $file output.png
echo "Changed size..."
convert output.png +level-colors $color, output.png
convert output.png -fill $color -opaque white $output.png
echo "Changed color."
