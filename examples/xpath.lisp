;; xpath.lisp

(in-package #:libxml2.xpath)

(define-xpath-function hello-world ()
  "Hello world!")

(define-xpath-function echo (msg)
  msg)

(define-xpath-function join (delimiter &rest strs)
  (iter (for str in strs)
        (reducing str
                  by (lambda (s x) (concatenate 'string s delimiter x)))))

(with-xpath-functions ((hello-world "hello-world")
                       (echo "echo")
                       (join "join"))
  (with-parse-document (doc "<root />")
    (find-string doc "join('//', hello-world(), '---', echo('Buy!'))")))


