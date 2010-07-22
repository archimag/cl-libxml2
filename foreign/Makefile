# This file is part of the cl-libxml2 library, released under Lisp-LGPL.
# See file COPYING for details.
#
# Author: Moskvitin Andrey <archimag@gmail.com>,  Stelian Ionescu <stelian.ionescu-zeus@poste.it>

CC = gcc
PREFIX = /usr

INSTALL = install
INSOPTS = -s -m 755 -o 0 -g 0

all: cllibxml2.so

cllibxml2.so: cllibxml2.c
	$(CC) $(CFLAGS) $(LDFLAGS) -shared -fPIC $< -o $@

clean:
	rm -f cllibxml2.so

install: all
	 $(INSTALL) $(INSOPTS) cllibxml2.so $(PREFIX)/lib
