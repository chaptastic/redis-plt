(module redis-internal scheme
  (define-struct redis:connection (host port in out (state #:mutable)))
  (define-struct exn:redis:protocol-error (msg))
 
  (define (redis:send-command conn cmd args)
    (letrec [(outp (redis:connection-out conn))
             (argstrs (map (lambda (a)
                             (cond 
                               ((string? a) a)
                               ((number? a) (number->string a))
                               (else "")))
                           args))
             (cmdlst (cons cmd argstrs))
             (cmdstr (string-join cmdlst " "))]
      (write-string (string-append cmdstr "\r\n") outp)
      (flush-output outp)
      #t))
  
  (define (redis:send-bulk-command conn cmd args data)
    null)
  
  (define (redis:send-multi-bulk-command conn cmd args data)
    null)
  
   (define (redis:parse-error-response conn)
    null)
  
  (define (redis:parse-integer-response conn)
    (let [(in (redis:connection-in conn))]
      (if (equal? (read-char in) #\:)
          (let [(line (read-line in 'return-linefeed))]
            (string->number line))
          (raise (make-exn:redis:protocol-error "Expected : to begin integer response")))))
  (define (redis:parse-single-line-response conn)
    (let [(in (redis:connection-in conn))]
      (if (equal? (read-char in) #\+)
          (read-line in 'return-linefeed)
          (raise (make-exn:redis:protocol-error "Expected + to begin single-line response")))))
  
  (define (read-bulk-response in)
    (if (equal? (read-char in) #\$)
          (letrec [(lenstr (read-line in 'return-linefeed))
                   (len (string->number lenstr))]
            (if (equal? len -1)
                null
                (let [(data (read-bytes len in))]
                  (read-bytes 2 in)
                  data)))
          (raise (make-exn:redis:protocol-error "Expected $ to begin bulk response"))))
  
  (define (redis:parse-bulk-response conn)
    (let [(in (redis:connection-in conn))]
      (read-bulk-response in)))
  
  (define (redis:parse-multi-bulk-response conn)
    (letrec [(in (redis:connection-in conn))
             (read-int (lambda (num lst)
                         (if (> num 0)
                             (read-int (- num 1) (cons (read-bulk-response in) lst))
                             (reverse lst))))]
      (if (equal? (read-char in) #\*)
          (letrec [(lenstr (read-line in 'return-linefeed))
                   (len (string->number lenstr))]
            (if (equal? len -1)
                null
                (read-int len null)))
          (raise (make-exn:redis:protocol-error "Expected $ to begin bulk response")))))
  
  (define (redis:parse-response conn)
    (let ((c (peek-char (redis:connection-in conn))))
      (cond ((equal? c #\-) (redis:parse-error-response conn))
            ((equal? c #\:) (redis:parse-integer-response conn))
            ((equal? c #\+) (redis:parse-single-line-response conn))
            ((equal? c #\$) (redis:parse-bulk-response conn))
            ((equal? c #\*) (redis:parse-multi-bulk-response conn))
            (else (raise (make-exn:redis:protocol-error "Unknown response signifier"))))))

  (provide/contract (struct redis:connection
                            ((host string?)
                             (port exact-positive-integer?)
                             (in input-port?)
                             (out output-port?)
                             (state any/c)))
                    (struct exn:redis:protocol-error 
                            ((msg string?)))
                    (redis:send-command (-> redis:connection?
                                            string?
                                            (or/c (listof (or/c string?
                                                                number?))
                                                  null?)
                                            #t))
                    (redis:parse-response (-> redis:connection?
                                              any/c))
   )

) ;; end of module redis-internal