# This file is part of the cl-libxml2 library, released under Lisp-LGPL.
# See file COPYING for details.
#
# Author: Moskvitin Andrey <archimag@gmail.com>

CC = gcc
PREFIX = /usr

INSTALL = install
INSOPTS = -s -m 755 -o 0 -g 0

all: cllibxml2.so

cllibxml2.so: cllibxml2.c
	$(CC) -I/usr/include/libxml2 -shared -fPIC cllibxml2.c -o cllibxml2.so

clean:
	rm -f cllibxml2.so

install: all
	 $(INSTALL) $(INSOPTS) cllibxml2.so $(PREFIX)/lib