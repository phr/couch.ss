#lang scheme

(provide get-uuids
         info
         make-database
         next-doc-url
         next-uuid
         put-doc
         current-server-url
         with-current-server)

(require scheme/base
         net/url
         (planet dherman/json:3:0))

(define current-server-url
  (make-parameter (string->url "http://127.0.0.1:5984/")))

(define-syntax-rule (with-current-server new-server body ...)
  (parameterize ((current-server-url (string->url new-server)))
    body ...))

(define (info)
  (call/input-url (current-server-url) get-pure-port read-json))

(define (make-database name)
  (read-json (put-pure-port (combine-url/relative (current-server-url) name) #f)))

(define uuid-stash '())

(define (get-uuids (count 100))
  (hash-ref (call/input-url 
             (combine-url/relative (current-server-url)
                                   (string-append
                                    "_uuids"
                                    (format "?count=~a" count)))
             get-pure-port
             read-json)
            'uuids))

(define (next-uuid)
  (begin
    (when (null? uuid-stash)
      (set! uuid-stash (append (get-uuids) uuid-stash)))
    (let ((u (car uuid-stash)))
      (set! uuid-stash (cdr uuid-stash))
      u)))

(define (next-doc-url database-name)
  (combine-url/relative (current-server-url)
                        (string-append
                         database-name
                         "/"
                         (next-uuid))))

(define (put-doc database-name doc-as-jsexpr)
  (let* ((port (put-pure-port (next-doc-url database-name)
                              (string->bytes/utf-8 (jsexpr->json doc-as-jsexpr))))
         (result (read-json port)))
    (close-input-port port)
    result))
