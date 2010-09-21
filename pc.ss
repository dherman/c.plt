#lang scheme/base

(require "ast.ss")
(require (for-syntax scheme/base))

(provide typedef struct union enum array pointer)

(define-for-syntax (symbolic-identifier=? id1 id2)
  (eq? (syntax->datum id1) (syntax->datum id2)))

(define-syntax (@type stx)
  (syntax-case* stx (void char short int long float double signed unsigned _Bool _Complex) symbolic-identifier=?
    [(@type void)     #'(make-type:primitive #f 'void)]
    [(@type char)     #'(make-type:primitive #f 'char)]
    [(@type short)    #'(make-type:primitive #f 'short)]
    [(@type int)      #'(make-type:primitive #f 'int)]
    [(@type long)     #'(make-type:primitive #f 'long)]
    [(@type float)    #'(make-type:primitive #f 'float)]
    [(@type double)   #'(make-type:primitive #f 'double)]
    [(@type signed)   #'(make-type:primitive #f 'signed)]
    [(@type unsigned) #'(make-type:primitive #f 'unsigned)]
    [(@type _Bool)    #'(make-type:primitive #f '_Bool)]
    [(@type _Complex) #'(make-type:primitive #f '_Complex)]
    [(@type id)
     (identifier? #'id)
     #'(make-type:ref #f 'id)]
    [(@type t)
     #'(let ([tmp t])
         (if (decl:vars? tmp)
             (decl:vars-type tmp)
             tmp))]))

(define-syntax (typedef stx)
  (syntax-case stx ()
    [(_ t name)
     #'(make-decl:typedef #f (@type t) (list (make-decl:declarator #f (make-id:var #f 'name) #f #f)))]));'((name . #f)))]))

(define-syntax (struct stx)
  (syntax-case stx ()
    [(_ tag)
     (identifier? #'tag)
     #'(make-decl:vars #f #f (make-type:struct #f (make-id:label #f 'tag) #f) '())]
;     #'(make-decl:type:tagged (make-type:struct 'tag #f))]
    [(_ tag (field ...))
     (identifier? #'tag)
;     #'(make-decl:type:tagged (make-type:struct 'tag (list (struct-field field) ...)))]
     #'(make-decl:vars #f #f (make-type:struct #f (make-id:label #f 'tag) (list (struct-field field) ...)) '())]
    [(_ (field ...))
     #'(make-decl:vars #f #f (make-type:struct #f #f (list (struct-field field) ...)) '())]))
;     #'(make-decl:type:tagged (make-type:struct #f (list (struct-field field) ...)))]))

(define-syntax (struct-field stx)
  (syntax-case stx ()
    [(_ [t name])
     #'(make-decl:member #f (@type t) (list (make-decl:member-declarator #f (make-id:label #f 'name) #f #f #f)))]
;     #'`(name . ,(@type t))]
    [(_ name)
     ;; XXX: what is this silly case?
     #'(make-decl:member #f #f (list (make-decl:member-declarator #f (make-id:label #f 'name) #f #f)))]))
;     #'`(name . #f)]))

(define-syntax (union stx)
  (syntax-case stx ()
    [(_ tag)
     (identifier? #'tag)
     #'(make-decl:vars #f #f (make-type:union #f (make-id:label #f 'tag) #f) '())]
;     #'(make-decl:type:tagged (make-type:union 'tag #f))]
    [(_ tag (variant ...))
     (identifier? #'tag)
     #'(make-decl:vars #f #f (make-type:union #f (make-id:label #f 'tag) #f) (list (union-variant variant) ...))]
;     #'(make-decl:type:tagged (make-type:union 'tag (list (union-variant variant) ...)))]
    [(_ (variant ...))
     #'(make-decl:vars #f #f (make-type:union #f #f (list (union-variant variant) ...)))]))
;     #'(make-decl:type:tagged (make-type:union #f (list (union-variant variant) ...)))]))

(define-syntax (union-variant stx)
  (syntax-case stx ()
    [(_ [t name])
     #'(make-decl:member #f (@type t) (list (make-decl:member-declarator #f (@type t) #f #f)))]
;     #'`(name . ,(@type t))]
    [(_ name)
     ;; XXX: what is this silly case?
     #'(make-decl:member #f #f (list (make-decl:member-declarator #f (make-id:label #f 'name) #f #f)))]))
;     #'`(name . #f)]))

(define-syntax (enum stx)
  (syntax-case stx ()
    [(_ tag)
     (identifier? #'tag)
     #'(make-decl:type:tagged (make-type:enum 'tag #f))]
    [(_ tag (variant ...))
     (identifier? #'tag)
     #'(make-decl:type:tagged (make-type:enum 'tag (list (enum-variant variant) ...)))]
    [(_ (variant ...))
     #'(make-decl:type:tagged (make-type:enum #f (list (enum-variant variant) ...)))]))

(define-syntax (enum-variant stx)
  (syntax-case stx ()
    [(_ [name expr])
     #'`(name . ,(make-expr:lit 'int expr))]
    [(_ name)
     #'`(name . #f)]))

(define array (procedure-rename make-type:array 'array))
(define pointer (procedure-rename make-type:pointer 'pointer))
