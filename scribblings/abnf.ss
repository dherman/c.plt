#lang at-exp scheme/base

(require scheme/match
         scheme/list
         scribble/manual
         scribble/struct
         scribble/decode)
(require (for-syntax scheme/base))

(provide BNF BNF-seq BNF-alt BNF-alt/close BNF-etc nonterm
         term BNF-var
         ABNF attr-decl attr-sel node-var attr-label
         optional BNF-group kleenestar kleeneplus kleenerange)

(define spacer (make-element 'hspace (list " ")))
(define equals (make-element 'tt (list spacer "::=" spacer)))
(define set (make-element #f (list (make-element 'tt (list spacer)) 'larr (make-element 'tt (list spacer)))))
(define alt (make-element 'tt (list spacer spacer "|" spacer spacer)))
(define attr (make-element 'tt (list spacer spacer spacer spacer spacer)))

(define (as-flow i) (make-flow (list (make-paragraph (list i)))))

(define (interleave l spacer)
  (make-element #f (cons (car l)
                         (apply append
                                (map (lambda (i)
                                       (list spacer i))
                                     (cdr l))))))

(define (column lhs op rhs)
  (list (as-flow spacer) (as-flow lhs) (as-flow op) (as-flow rhs)))

(define (BNF . defns)
  (make-table
   #f
   (apply
    append
    (for/list ([defn defns])
      (match defn
        [(list A p1 ps ...)
         (cons (column A equals p1)
               (for/list ([p ps])
                 (column " " alt p)))])))))

(define (ABNF . defns)
  (make-table
   #f
   (apply
    append
    (for/list ([defn defns])
      (match defn
        [(list A (list p1 as1 ...) aps ...)
         (cons (column A equals p1)
               (append
                (for/list ([a1 as1])
                  (column " " attr a1))
                (append-map (match-lambda
                              [(list p as ...)
                               (cons (column " " alt p)
                                     (for/list ([a as])
                                       (column " " attr a)))])
                            aps)))])))))

(define-for-syntax (node-var? x)
  (let ([x (syntax->datum x)])
    (or (integer? x) (symbol? x))))

(define-syntax (node-var stx)
  (syntax-case stx ()
    [(_ name)
     (integer? (syntax->datum #'name))
     (with-syntax ([var (datum->syntax #'name (string->symbol (format "$~a" (syntax->datum #'name))))])
       #'(node-var var))]
    [(_ name)
     (identifier? #'name)
     (with-syntax ([var-name (symbol->string (syntax->datum #'name))])
       #'(schemeidfont var-name))]))

(define-syntax (attr-label stx)
  (syntax-case stx ()
    [(_ attribute)
     (identifier? #'attribute)
     (with-syntax ([attribute-name (symbol->string (syntax->datum #'attribute))])
       #'(make-element 'sf (list attribute-name)))]))

(define-syntax (attr-sel stx)
  (syntax-case stx ()
    [(_ name attribute)
     (and (node-var? #'name) (identifier? #'attribute))
     #'(make-element #f (append (list (node-var name))
                                (list (schemekeywordfont "."))
                                (list (attr-label attribute))))]))

(define-syntax (attr-decl stx)
  (syntax-case stx ()
    [(_ name attribute expr)
     (and (node-var? #'name) (identifier? #'attribute))
     #'(make-element #f (append (list (make-element 'tt (list spacer spacer spacer)))
                                (list (attr-sel name attribute))
                                (list set)
                                (list expr)))]))

(define (BNF-seq . l)
  (if (null? l)
      ""
      (interleave l spacer)))

(define (BNF-alt . l)
  (interleave l alt))

(define (BNF-alt/close . l)
  (interleave l " | "))

(define BNF-etc "...")

(define (BNF-var . s)
  (make-element 'italic (decode-content s)))

(define (nonterm #:sub [arg #f] . s)
  (if arg
      (make-element #f (append (list 'lang)
                               (list (make-element 'italic (decode-content s)))
                               (list 'rang)
                               (list (make-element 'subscript (list arg)))))
      (make-element #f (append (list 'lang)
                               (list (make-element 'italic (decode-content s)))
                               (list 'rang)))))

(define (term s)
  (schemevalfont (format "~v" s)))

(define (optional . s)
  (make-element #f (append (list "[") (decode-content s) (list "]"))))

(define (BNF-group . s)
  (make-element #f (append (list "{") 
                           (list (apply BNF-seq (decode-content s)))
                           (list "}"))))

(define (kleenestar . s)
  (make-element #f (append (decode-content s) (list "*"))))

(define (kleeneplus . s)
  (make-element #f (append (decode-content s) (list (make-element 'superscript (list "+"))))))

(define (kleenerange a b . s)
  (make-element #f (append (decode-content s) 
                           (list (make-element 'superscript 
                                               (list (format "{~a,~a}" a b)))))))
