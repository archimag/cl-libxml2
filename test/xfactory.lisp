;; test.lisp

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
         xfactory-1
         (ensure-same '(:xml-element-node "root" nil nil)
                      (with-object (el (with-element-factory ((E))
                                                 (E :root)))
                        (list (node-type el)
                              (local-name el)
                              (namespace-uri el)
                              (namespace-prefix el)))))

(addtest (xfactory-test)
         xfactory-2
         (ensure-same '(:xml-element-node "root" "www.sample.org" nil)
                      (with-object (el (with-element-factory ((E "www.sample.org"))
                                                 (E :root)))
                        (list (node-type el)
                              (local-name el)
                              (namespace-uri el)
                              (namespace-prefix el)))))

(addtest (xfactory-test)
         xfactory-3
         (ensure-same '(:xml-element-node "root" "www.sample.org" "my")
                      (with-object (el (with-element-factory ((E "www.sample.org" "my"))
                                                 (E :root)))
                        (list (node-type el)
                              (local-name el)
                              (namespace-uri el)
                              (namespace-prefix el)))))

(addtest (xfactory-test)
         xfactory-4
         (ensure-same '("a" "b" "c")
                      (with-object (el (with-element-factory ((E))
                                                 (E :root
                                                    (E :a)
                                                    (E :b)
                                                    (E :c))))
                        (iter (for node in-child-nodes el)
                              (collect (xtree:local-name node))))))
                        
(addtest (xfactory-test)
         xfactory-4
         (ensure-same '("a1" "a2" "a3")
                      (with-object (el (with-element-factory ((p))
                                                 (p :root
                                                    (iter (for i from 1 to 3)
                                                          (p (format nil "a~A" i))))))
                        (iter (for node in-child-nodes el)
                              (collect (xtree:local-name node))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run-xfactory-test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-xfactory-tests (&optional (test 'xfactory-test))
  (run-tests :suite test :report-pathname nil))
