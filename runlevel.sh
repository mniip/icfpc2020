#!/bin/bash

readarray -t levels < levels.txt
./meta/app/HsInteract ${levels[$1]}
