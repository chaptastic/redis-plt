(module redis scheme
  
  (require scheme/contract)
  (require "redis-internal.ss")
  
    
  (define (redis:connect (host "localhost") (port 6379))
    (let-values ([(in out) (tcp-connect host port)])
      (make-redis:connection host port in out null)))
  
  (define (redis:close connection)
    (let [(in (redis:connection-in connection))
          (out (redis:connection-out connection))]
      (tcp-abandon-port out)
      (close-input-port in)))
  
  (define (redis:ping connection)
    (redis:send-command connection "PING" null)
    (let [(response (redis:parse-response connection))]
      (equal? response "PONG")))
  
  (define (redis:lastsave connection)
    (redis:send-command connection "LASTSAVE" null)
    (redis:parse-response connection))
  
  (define (redis:exists connection key)
    (redis:send-command connection "EXISTS" '(key))
    (redis:parse-response connection))
  
  
  
  (define redis-arg/c
    (or/c string?
          integer?))
  
  (define redis-data-arg/c
    (or/c redis-arg/c
          bytes?))
  
 
  (provide/contract (redis:connect (-> string?
                                       (and/c exact-nonnegative-integer?
                                              (integer-in 1 65535))
                                       any/c))
                    (redis:close (-> redis:connection? null))
                    (redis:ping (-> redis:connection? boolean?))
                    (redis:lastsave (-> redis:connection? (or/c integer? #f)))
                    (redis:exists (-> redis:connection? string? integer?))
                    )
  ) ; end of module