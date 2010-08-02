Dowload and Installation
========================

Implementation-specific notes
-----------------------------

`cl-libxml2`_ is known to work with SBCL and CLISP on Gentoo Linux using gentoo-lisp-overlay.

Requirements
------------

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

libxslt requires functions with variable arguments list for error
handling callbacks (see xsltSetGenericErrorFunc). CFFI does not
support such functions, and, thus, C helper library is required for
libxslt errors translation into lisp conditions. The helper library
resides in the directory foreign.  ::

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

To compile cl-libxlm2 with XSLT support use:

.. code-block:: common-lisp

  (asdf:operate 'asdf:load-op :cl-libxslt)

Or without XSLT:

.. code-block:: common-lisp

  (asdf:operate 'asdf:load-op :cl-libxml2)

To compile XFactory system:

.. code-block:: common-lisp

  (asdf:operate 'asdf:load-op :xfactory)

Run test suite (optional)
-------------------------
Compile cl-libxlm2 with XSLT support as described above.

Test suite can be launched by asdf's test-op operator. If cl-libxml2
has not been yet loaded with asdf:load-op, asdf:test-op will do it
automatically.

Run tests with XSLT:

.. code-block:: common-lisp

  (asdf:operate 'asdf:test-op :cl-libxslt)

Without XSLT:

.. code-block:: common-lisp

  (asdf:operate 'asdf:test-op :cl-libxml2)

And for XFactory system:

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
