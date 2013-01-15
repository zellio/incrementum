
;;;;
;;
;;  <EXPR> -> <Imm>
;;          | (prim <EXPR)
;;  <Imm>  -> fixnum | boolean | char | null


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Constants
;;
(define wordsize 4)

(define fixnum-shift 2)
(define fixnum-mask #x03)
(define fixnum-tag #x00)

(define char-shift 8)
(define char-tag #x0F)
(define char-mask #x3F)

(define boolean-t #x6F)
(define boolean-f #x2F)
(define boolean-bit #x06)
(define boolean-mask #xBF)

(define nil #x3F)


(define fixnum-bits
  (- (* wordsize 8) fixnum-shift))

(define fixnum-lower-bound
  (- (expt 2 (- fixnum-bits 1))))

(define fixnum-upper-bound
  (sub1 (expt 2 (- fixnum-bits 1))))

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fixnum-lower-bound x fixnum-upper-bound)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Immediate handlers
;;
(define (immediate? x)
  (or (fixnum? x) (boolean? x) (null? x) (char? x)))

(define (immediate-rep x)
  (cond
   ((fixnum? x) (ash x fixnum-shift))
   ((boolean? x) (if x boolean-t boolean-f))
   ((null? x) nil)
   ((char? x) (logor (ash (char->integer x) char-shift) char-tag))
   (else #f)))

(define (emit-immediate x)
  (unless (immediate? x) (error 'emit-program "value must be an immediate"))
  (emit "	movl	$~s,	%eax" (immediate-rep x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Primirive Handlers
;;
(define-syntax define-primitive
  (syntax-rules ()
    ((_ (prim-name arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count* (length '(arg* ...)))
       (putprop 'prim-name '*emitter* (lambda (arg* ...) b b* ...))))
    ))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*)
      (error 'primitive-emitter (format "primitive ~s has no emitter" x))))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (check-primcall-args prim args)
  (= (getprop prim '*arg-count*) (length args)))

(define (emit-primcall expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) args)))

(define (emit-predicate)
  (emit "  sete %al")
  (emit "  movzbl %al, %eax")
  (emit "  sal $~s, %al" boolean-bit)
  (emit "  or $~s, %al" boolean-f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Primitve definitions
;;
(define-primitive ($fxadd1 arg)
  (emit-expr arg)
  (emit "	addl $~s,	%eax" (immediate-rep 1)))

(define-primitive ($fxsub1 arg)
  (emit-expr arg)
  (emit "	subl $~s,	%eax" (immediate-rep 1)))

(define-primitive ($fixnum->char arg)
  (emit-expr arg)
  (emit "	shll	$~s,	%eax" (- char-shift fixnum-shift))
  (emit "	orl	$~s,	%eax" char-tag))

(define-primitive ($char->fixnum arg)
  (emit-expr arg)
  (emit "	shrl	$~s,	%eax" (- char-shift fixnum-shift)))

(define-primitive ($fxlognot arg)
  (emit-expr arg)
  (emit "	shrl	$~s,	%eax" fixnum-shift)
  (emit "	not	%eax")
  (emit "	shll	$~s,	%eax" fixnum-shift))

(define-primitive ($fxzero? arg)
  (emit-expr arg)
  (emit "  cmp $~s, %al" fixnum-tag)
  (emit-predicate))

(define-primitive (fixnum? arg)
  (emit-expr arg)
  (emit "  and $~s, %al" fixnum-mask)
  (emit "  cmp $~s, %al" fixnum-tag)
  (emit-predicate))

(define-primitive (null? arg)
  (emit-expr arg)
  (emit "	cmp $~s, %al" nil)
  (emit-predicate))

(define-primitive (boolean? arg)
  (emit-expr arg)
  (emit "	and $~s, %al" boolean-mask)
  (emit "	cmp $~s, %al" boolean-f)
  (emit-predicate))

(define-primitive (char? arg)
  (emit-expr arg)
  (emit "  and $~s, %al" char-mask)
  (emit "  cmp $~s, %al" char-tag)
  (emit-predicate))

(define-primitive (not arg)
  (emit-expr arg)
  (emit "  cmp $~s, %al" boolean-f)
  (emit-predicate))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  (if test then else)
;;
(define unique-label
  (let ((count 0))
    (lambda ()
      (let ((label (format "L_~s" count)))
        (set! count (add1 count))
        label))))

(define (if? expr)
  (and (list? expr) (eq? (car expr) 'if) (= 4 (length expr))))

(define (if-test expr)
  (cadr expr))

(define (if-conseq expr)
  (caddr expr))

(define (if-altern expr)
  (cadddr expr))

(define (emit-if expr)
  (let ((alt-label (unique-label))
        (end-label (unique-label)))
    (emit-expr (if-test expr))
    (emit "	cmp $~s,	%al" boolean-f)
    (emit "	je ~a" alt-label)
    (emit-expr (if-conseq expr))
    (emit "	jmp ~a" end-label)
    (emit "~a:" alt-label)
    (emit-expr (if-altern expr))
    (emit "~a:" end-label)))



(define (emit-expr expr)
  (cond
   ((immediate? expr) (emit-immediate expr))
   ((primcall? expr)  (emit-primcall expr))
   ((if? expr)        (emit-if expr))
   (else (error 'emit-expr (format "~s is not a valid expression" expr)))))

(define (emit-function-header f)
  (emit "	.text")
  (emit "	.globl	~a" f)
  (emit "	.type	~a,	@function" f)
  (emit "~a:" f))

(define (emit-program x)
  (emit-function-header "scheme_entry")
  (emit-expr x)
  (emit "	ret"))
