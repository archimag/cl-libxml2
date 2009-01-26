CC = gcc
PREFIX = /usr

INSTALL = install
INSOPTS = -s -m 755 -o 0 -g 0

all: cl_libxml2.so

cl_libxml2.so: cl_libxml2.c
	$(CC) -shared -fPIC cl_libxml2.c -o cl_libxml2.so

clean:
	rm -f cl_libxml2.so

install: all
	 $(INSTALL) $(INSOPTS) cl_libxml2.so $(PREFIX)/lib