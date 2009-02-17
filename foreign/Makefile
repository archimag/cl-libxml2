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