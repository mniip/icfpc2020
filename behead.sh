#!/bin/sh
# Strip RIFF/WAV header from a .wav file, leaving only an array of 16-bit signed ints
sox - --type raw --encoding signed-integer --bits 16 --rate 44100 --endian little -
