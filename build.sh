#!/bin/bash

mkdir bin
mkdir obj

rm bin/*
rm obj/*

cd obj

set -x

gcc ../example/send_byte.c ../src/spi_driver.c -I../include -o ../bin/c_send_byte

g++ -c ../src/spi_driver.c -I../include
gnatmake -g ../example/send_byte.adb -aI../spec -aI../src -largs spi_driver.o
mv send_byte ../bin/ada_send_byte
