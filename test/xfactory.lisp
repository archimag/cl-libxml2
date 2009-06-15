;;; test.lisp
;;;
;;; This file is part of the cl-libxml2 library, released under Lisp-LGPL.
;;; See file COPYING for details.
;;;
;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defpackage :xfactory.test
  (:use :cl :iter :lift :xfactory :xtree)
  (:export :xfactory-test
           :run-xfactory-tests))

(in-package #:xfactory.test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xfactory-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite xfactory-test () ())

(addtest (xfactory-test)
         create-elements-1
         (ensure-same '(:xml-element-node "root" nil nil)
                      (with-object (el (with-element-factory ((E))
                                                 (E :root)))
                        (list (node-type el)
                              (local-name el)
                              (namespace-uri el)
                              (namespace-prefix el)))))

(addtest (xfactory-test)
         create-elements-2
         (ensure-same '(:xml-element-node "root" "www.sample.org" nil)
                      (with-object (el (with-element-factory ((E "www.sample.org"))
                                                 (E :root)))
                        (list (node-type el)
                              (local-name el)
                              (namespace-uri el)
                              (namespace-prefix el)))))

(addtest (xfactory-test)
         create-elements-3
         (ensure-same '(:xml-element-node "root" "www.sample.org" "my")
                      (with-object (el (with-element-factory ((E "www.sample.org" "my"))
                                                 (E :root)))
                        (list (node-type el)
                              (local-name el)
                              (namespace-uri el)
                              (namespace-prefix el)))))

(addtest (xfactory-test)
         create-elements-4
         (ensure-same '("a" "b" "c")
                      (with-object (el (with-element-factory ((E))
                                                 (E :root
                                                    (E :a)
                                                    (E :b)
                                                    (E :c))))
                        (iter (for node in-child-nodes el)
                              (collect (xtree:local-name node))))))
                        
(addtest (xfactory-test)
         create-elements-5
         (ensure-same '("a1" "a2" "a3")
                      (with-object (el (with-element-factory ((p))
                                                 (p :root
                                                    (iter (for i from 1 to 3)
                                                          (p (format nil "a~A" i))))))
                        (iter (for node in-child-nodes el)
                              (collect (xtree:local-name node))))))

(addtest (xfactory-test)
         text-content-1
         (ensure-same "Hello world"
                      (with-object (el (with-element-factory ((E))
                                         (E "root"
                                            "Hello world")))
                        (xtree:text-content el))))

(addtest (xfactory-test)
         text-content-2
         (ensure-same '("foo" "bar")
                      (with-object (el (with-element-factory ((E))
                                         (E "root"
                                            (E "a" "foo")
                                            (E "b" "bar"))))
                        (iter (for node in-child-nodes el)
                              (collect (xtree:text-content node))))))

(addtest (xfactory-test)
         add-namespace-1
         (ensure-same '("http://www.w3.org/1999/xlink" "xlink")
                      (with-object (el (with-element-factory ((E))
                                         (E "root"
                                            (namespace "http://www.w3.org/1999/xlink" "xlink"))))
                        (let ((ns (xtree::search-ns-by-prefix el "xlink")))
                          (list (xtree:namespace-uri ns)
                                (xtree:namespace-prefix ns))))))

(addtest (xfactory-test)
         add-attributes-1
         (ensure-same '("val1" "1" "1.0")
                      (with-object (el (with-element-factory ((E))
                                         (E "root"
                                            (attributes "attr1" "val1"
                                                        :attr2 1
                                                        "attr3" 1.0))))
                        (list (xtree:attribute-value el "attr1")
                              (xtree:attribute-value el "attr2")
                              (xtree:attribute-value el "attr3")))))

(addtest (xfactory-test)
         add-attributes-2
         (ensure-same '("http://www.sample.org" "Title text")
                      (with-object (el (with-element-factory ((E))
                                         (E "root"
                                            (namespace "http://www.w3.org/1999/xlink" "xlink")
                                            (attributes "xlink:title" "Title text"
                                                        "xlink:href" "http://www.sample.org"))))
                        (list (xtree:attribute-value el "href" "http://www.w3.org/1999/xlink")
                              (xtree:attribute-value el "title" "http://www.w3.org/1999/xlink")))))
                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-xfactory-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-xfactory-tests (&optional (test 'xfactory-test))
  (run-tests :suite test :report-pathname nil))
