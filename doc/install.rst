Dowload and Installation
========================

Implementation-specific notes
-----------------------------

`cl-libxml2`_ tested with sbcl and clisp on Gentoo Linux (with use gentoo-lisp-overlay).

Requirements
------------

`cl-libxml2`_ needs:

* `libxml2`_
* `libxslt`_ (optional, necessary for xslt support)
* `cffi`_ (=> 0.10.2)
* `lift`_ (optional, for tests)
* `puri`_
* `iterate`_
* `flexi-streams`_
* `metabang-bind`_
* `garbage-pools`_ (=> 0.1.1)

Download
--------

Download `tarballs`_ or get it from git:
::

  $ git clone git://github.com/archimag/cl-libxml2.git

Build C helper library
----------------------

libxslt for error handling require callback functions that called with a varying
number of arguments of varying types (see xsltSetGenericErrorFunc). cffi is do not
support it. Thus, C helper library are required for translate libxslt errors to lisp
conditions. The helper library reside in the directory foreign.
::

  $ cd /path/to/cl-libxml2
  $ make -C foreign
  $ make -C foreign install

Compilation and loading
-----------------------

Register the .asd file, e.g. by symlinking it:
::

  $ ln -sf `pwd`/cl-libxml2/cl-libxml2.asd /path/to/your/registry/
  $ ln -sf `pwd`/cl-libxml2/cl-libxslt.asd /path/to/your/registry/
  $ ln -sf `pwd`/cl-libxml2/xfactory.asd /path/to/your/registry/

The compile cl-libxlm2 without xslt using:

.. code-block:: common-lisp

  (asdf:operate 'asdf:load-op :cl-libxml2)

Or with xslt using

.. code-block:: common-lisp

  (asdf:operate 'asdf:load-op :cl-libxslt)

The compile xfactory system:

.. code-block:: common-lisp

  (asdf:operate 'asdf:load-op :xfactory)

Run test suite (optional)
-------------------------

The test suite can be executed using the asdf test-op operator. If cl-libxml2
has not been loaded with asdf:load-op, the asdf:test-op operator will automatically
load cl-libxml2.

Run tests without xslt:

.. code-block:: common-lisp

  (asdf:operate 'asdf:test-op :cl-libxml2)

With xslt:

.. code-block:: common-lisp

  (asdf:operate 'asdf:test-op :cl-libxslt)

Run tests for xfactory system:

.. code-block:: common-lisp

  (asdf:operate 'asdf:test-op :xfactory)

.. _cl-libxml2: http://code.google.com/p/cl-libxml2/

.. _libxml2: http://www.xmlsoft.org/
.. _libxslt: http://www.xmlsoft.org/XSLT/
.. _cxml-stp: http://www.lichteblau.com/cxml-stp/
.. _iterate: http://common-lisp.net/project/iterate/
.. _cffi: http://common-lisp.net/project/cffi/
.. _lift: http://common-lisp.net/project/lift/
.. _puri: http://puri.b9.com/
.. _flexi-streams: http://www.weitz.de/flexi-streams/
.. _metabang-bind: http://common-lisp.net/project/metabang-bind
.. _garbage-pools: http://code.google.com/p/garbage-pools/

.. _tarballs: http://cl-libxml2.googlecode.com/files/cl-libxml2-latest.tar.bz2
