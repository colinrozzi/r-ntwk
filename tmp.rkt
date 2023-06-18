#lang racket

(require racket/date)

(print 5)

(parameterize ([current-output-port (open-output-file "tmp.txt" #:exists 'replace)])
  (print 1)
  (flush-output))

(print 6)