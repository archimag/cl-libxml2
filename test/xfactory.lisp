;; test.lisp

(defpackage :xfactory.test
  (:use :cl :iter :libxml2.tree :lift :libxml2.xpath :metabang.bind)
  (:export :xfactory-test
           :run-xfactory-tests))

(in-package #:xfactory.test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xfactory-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftestsuite xfactory-test () ())

(addtest (xfactory-test)
  build-element-1
  (ensure-same '(:xml-element-node "root" nil nil)
               (with-libxml2-object (el (xfactory:build-element () (:root)))
                 (list (node-type el)
                       (local-name el)
                       (namespace-uri el)
                       (namespace-prefix el)))))

(addtest (xfactory-test)
  build-element-2
  (ensure-same '(:xml-element-node "root" "www.sample.org" nil)
               (with-libxml2-object (el (xfactory:build-element ((p "www.sample.org"))
                                                                (:p.root)))
                 (list (node-type el)
                       (local-name el)
                       (namespace-uri el)
                       (namespace-prefix el)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-xfactory-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-xfactory-tests (&optional (test 'xfactory-test))
  (run-tests :suite test :report-pathname nil))
