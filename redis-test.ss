#lang scheme

(require (planet schematics/schemeunit:3))
(require (planet schematics/schemeunit:3/text-ui))
(require "redis.ss")
(require "redis-internal.ss")

(define (mock-connection-returning str)
  (make-redis:connection "localhost"
                         1234
                         (open-input-string str)
                         (open-output-string)
                         null))
(define (get-connection-output conn)
  (get-output-string (redis:connection-out conn)))

(define/provide-test-suite test-redis-commands
  (test-case 
   "Test successful PING command"
   (letrec [(conn (mock-connection-returning "+PONG\r\n"))
            (result (redis:ping conn))
            (output (get-connection-output conn))]
     (check-equal? output "PING\r\n")
     (check-equal? result #t)))
 )
(define/provide-test-suite test-redis-command-processing
 (test-case "Test redis:send-command"
            (let [(conn (mock-connection-returning ""))]
              (redis:send-command conn "cmd" '("foo" 1 2))
              (check-equal? (get-connection-output conn)
                            "cmd foo 1 2\r\n")))
  (test-case "Test redis:parse-response with string response"
             (letrec [(conn (mock-connection-returning "+PONG\r\n"))
                      (result (redis:parse-response conn))]
               (check-equal? result "PONG")))
  (test-case
   "Test redis:parse-response with error response"
   (letrec [(conn (mock-connection-returning "-Error message\r\n"))
            (result (redis:parse-response conn))]
     (check-equal? result '(error "Error message"))))
  (test-case
   "Test redis:parse-response with integer response"
   (letrec [(conn (mock-connection-returning ":12345\r\n"))
            (result (redis:parse-response conn))]
     (check-equal? result 12345)))
  (test-case
   "Test redis:parse-response with bulk response"
   (letrec [(conn (mock-connection-returning "$6\r\nfoobar\r\n"))
            (result (redis:parse-response conn))]
     (check-equal? result (string->bytes/utf-8 "foobar"))))  
  (test-case
   "Test redis:parse-response with null bulk response"
   (letrec [(conn (mock-connection-returning "$-1\r\n"))
            (result (redis:parse-response conn))]
     (check-equal? result null)))   
  (test-case
   "Test redis:parse-response with multi-bulk response"
   (letrec [(conn (mock-connection-returning "*3\r\n$3\r\nboo\r\n$4\r\nwoah\r\n$6\r\nfoobar\r\n"))
            (result (redis:parse-response conn))]
     (check-equal? result (list (string->bytes/utf-8 "boo")
                                (string->bytes/utf-8 "woah")
                                (string->bytes/utf-8 "foobar")))))  
  (test-case
   "Test redis:parse-response with multi-bulk response containing nulls"
   (letrec [(conn (mock-connection-returning "*3\r\n$3\r\nboo\r\n$-1\r\n$6\r\nfoobar\r\n"))
            (result (redis:parse-response conn))]
     (check-equal? result (list (string->bytes/utf-8 "boo")
                                null
                                (string->bytes/utf-8 "foobar")))))  
  (test-case
   "Test redis:parse-response with null multi-bulk response"
   (letrec [(conn (mock-connection-returning "*-1\r\n"))
            (result (redis:parse-response conn))]
     (check-equal? result null)))  

  )
(define/provide-test-suite redis-tests
  test-redis-command-processing
  test-redis-commands)

(run-tests redis-tests)