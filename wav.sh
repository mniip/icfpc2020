#!/bin/sh
# Convert an array of signed 16 bit ints into a WAV
sox --type raw --encoding signed-integer --bits 16 --rate 44100 --endian little - --type wav -
