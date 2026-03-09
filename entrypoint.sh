#!/bin/bash

if [ -d /home/opam/app ]; then
	cd /home/opam/app
	opam-2.4 depext
fi

exec opam exec -- "$@"
