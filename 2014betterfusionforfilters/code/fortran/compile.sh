#!/bin/bash
ifort Normalise2.f90 -O3 -c -save-temps -fsource-asm -FaNormalise2.s
ifort dumb.f90 -O3 -c -save-temps -fsource-asm -Fadumb.s
