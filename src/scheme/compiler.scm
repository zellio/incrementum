
;;
;;  <EXPR> -> <Imm>
;;          | (prim <Expr>)
;;          | (prim <Expr> <Expr>)
;;          | (if <Expr> <Expr> <Expr>)
;;          | (and <Expr>* ...)
;;          | (or <Expr>* ...)
;;          | var
;;          | (let ((var <Expr) ...) <Expr>)
;;          | (let* ((var <Expr) ...) <Expr>)
;;  <Imm>  -> fixnum | boolean | char | null
;;

;;
;;  Constants
;;
(define wordsize 8)

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


;;
;; 1.1 Integers
;; 1.2 Immediate Constants
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
  (emit "	mov	$~s,	%rax" (immediate-rep x)))


;;
;;  1.3 Unary Primitives
;;
(define-syntax define-primitive
  (syntax-rules ()
    ((_ (prim-name si env arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count* (length '(arg* ...)))
       (putprop 'prim-name '*emitter* (lambda (si env arg* ...) b b* ...))))
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

(define (emit-primcall si env expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si env args)))

(define (emit-predicate . args)
  (emit "	~a	%al" (if (null? args) 'sete (car args)))
  (emit "	movzb	%al,	%rax")
  (emit "	sal	$~s,	%al" boolean-bit)
  (emit "	or	$~s,	%al" boolean-f))

(define-primitive ($fxadd1 si env arg)
  (emit-expr si env arg)
  (emit "	addl	$~s,	%eax" (immediate-rep 1)))

(define-primitive ($fxsub1 si env arg)
  (emit-expr si env arg)
  (emit "	subl	$~s,	%eax" (immediate-rep 1)))

(define-primitive ($fixnum->char si env arg)
  (emit-expr si env arg)
  (emit "	shll	$~s,	%eax" (- char-shift fixnum-shift))
  (emit "	orl	$~s,	%eax" char-tag))

(define-primitive ($char->fixnum si env arg)
  (emit-expr si env arg)
  (emit "	shrl	$~s,	%eax" (- char-shift fixnum-shift)))

(define-primitive ($fxlognot si env arg)
  (emit-expr si env arg)
  (emit "	shrl	$~s,	%eax" fixnum-shift)
  (emit "	not	%eax")
  (emit "	shll	$~s,	%eax" fixnum-shift))

(define-primitive ($fxzero? si env arg)
  (emit-expr si env arg)
  (emit "	cmp	$~s,	%al" fixnum-tag)
  (emit-predicate))

(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
  (emit "	and	$~s,	%al" fixnum-mask)
  (emit "	cmp	$~s,	%al" fixnum-tag)
  (emit-predicate))

(define-primitive (null? si env arg)
  (emit-expr si env arg)
  (emit "	cmp	$~s,	%al" nil)
  (emit-predicate))

(define-primitive (boolean? si env arg)
  (emit-expr si env arg)
  (emit "	and	$~s,	%al" boolean-mask)
  (emit "	cmp	$~s,	%al" boolean-f)
  (emit-predicate))

(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (emit "	and	$~s,	%al" char-mask)
  (emit "	cmp	$~s,	%al" char-tag)
  (emit-predicate))

(define-primitive (not si env arg)
  (emit-expr si env arg)
  (emit "	cmp	$~s,	%al" boolean-f)
  (emit-predicate))


;;
;;  1.4 Conditional Expressions
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

(define (emit-if si env expr)
  (let ((alt-label (unique-label))
        (end-label (unique-label)))
    (emit-expr si env (if-test expr))
    (emit "	cmp	$~s,	%al" boolean-f)
    (emit "	je	~a" alt-label)
    (emit-expr si env (if-conseq expr))
    (emit "	jmp	~a" end-label)
    (emit-label alt-label)
    (emit-expr si env (if-altern expr))
    (emit-label end-label)))

(define (emit-jump-block si env expr jump label)
  (let ((head (car expr)) (rest (cdr expr)))
    (emit-expr si env head)
    (emit "	cmp	$~s,	%al" boolean-f)
    (emit "	~a	~a" jump label)
    (unless (null? rest)
      (emit-jump-block si env rest jump label))))

(define (emit-conditional-block default jump)
  (lambda (si env expr)
    (case (length expr)
      ((1) (emit-immediate default))
      ((2) (emit-expr si env (cadr expr)))
      (else
       (let ((end-label (unique-label)))
         (emit-jump-block si env (cdr expr) jump end-label)
         (emit-label end-label))))))

(define (and? expr)
  (and (list? expr) (eq? (car expr) 'and)))

(define emit-and
  (emit-conditional-block #t "je"))

(define (or? expr)
  (and (list? expr) (eq? (car expr) 'or)))

(define emit-or
  (emit-conditional-block #f "jne"))


;;
;; 1.5 Binary Primitives
;;
(define (emit-label f)
  (emit "~a:" f))


(define (emit-binary-operator si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "	mov	%rax,	~s(%rsp)" si)
  (emit-expr (- si wordsize) env arg2))

(define-primitive (fx+ si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  (emit "	add	~s(%rsp), %rax" si))

(define-primitive (fx- si env arg1 arg2)
  (emit-binary-operator si env arg2 arg1)
  (emit "	sub	~s(%rsp),	%rax" si))

(define-primitive (fx* si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  (emit "	shr	$2,	%rax")
  (emit "	imull	~s(%rsp)" si))

(define-primitive (fxlogor si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  (emit "	or	~s(%rsp), %rax" si))

(define-primitive (fxlognot si env arg1)
  (emit-expr si env arg1)
  (emit "	shr	$~s,	%rax" fixnum-shift)
  (emit "	not	%rax")
  (emit "	shl	$~s,	%rax" fixnum-shift))

(define-primitive (fxlogand si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  (emit "	and	~s(%rsp), %rax" si))

(define (define-binary-predicate op si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  (emit "	cmp	%rax,	~s(%rsp)" si)
  (emit-predicate op))

(define-primitive (fx= si env arg1 arg2)
  (define-binary-predicate 'sete si env arg1 arg2))

(define-primitive (fx< si env arg1 arg2)
  (define-binary-predicate 'setl si env arg1 arg2))

(define-primitive (fx<= si env arg1 arg2)
  (define-binary-predicate 'setle si env arg1 arg2))

(define-primitive (fx> si env arg1 arg2)
  (define-binary-predicate 'setg si env arg1 arg2))

(define-primitive (fx>= si env arg1 arg2)
  (define-binary-predicate 'setge si env arg1 arg2))

;;
;; 1.6 Local Variables
;;
(define variable? symbol?)

(define (emit-stack-load si)
  (emit "	mov	~s(%rsp),	%rax" si))

(define (emit-variable-ref env expr)
  (let ((pair (assoc expr env)))
    (if pair (emit-stack-load (cdr pair))
        (error 'emit-variable-ref (format "undefined variable ~s" expr)))))

(define (let? expr)
  (and (list? expr) (eq? (car expr) 'let) (= (length expr) 3)))

(define let-bindings cadr)
(define let-body caddr)

(define empty? null?)

(define (emit-stack-save si)
  (emit "	mov	%rax,	~s(%rsp)" si))

(define (next-stack-index si)
  (- si wordsize))

(define (extend-env var si new-env)
  (cons (cons var si) new-env))

(define (emit-let si env expr)
  (define (process-let bindings si new-env)
    (cond
     ((empty? bindings)
      (emit-expr si new-env (let-body expr)))
     (else
      (let ((b (car bindings)))
        (format "~a" env)
        (emit-expr si (if (let*? expr) new-env env) (cadr b))
        (emit-stack-save si)
        (process-let (cdr bindings)
           (next-stack-index si)
           (extend-env (car b) si new-env))))))
  (process-let (let-bindings expr) si env))


;;
;; 1.6 Supplemental -- let*
;;
(define (let*? expr)
  (and (list? expr) (eq? (car expr) 'let*) (= 3 (length expr))))

(define (emit-let* si env expr)
  (define (process-let bindings si new-env)
    (cond
     ((empty? bindings)
      (emit-expr si new-env (let-body expr)))
     (else
      (let ((b (car bindings)))
        (emit-expr si new-env (cadr b))
        (emit-stack-save si)
        (process-let (cdr bindings)
           (next-stack-index si)
           (extend-env (car b) si new-env))))))
  (process-let (let-bindings expr) si env))


;;
;; 1.7 Procedures
;;



;;
;;  Compiler
;;
(define (emit-expr si env expr)
  (cond
   ((immediate? expr) (emit-immediate expr))
   ((variable? expr)  (emit-variable-ref env expr))
   ((if? expr)        (emit-if si env expr))
   ((and? expr)       (emit-and si env expr))
   ((or? expr)        (emit-or si env expr))
   ((or (let? expr)
        (let*? expr)) (emit-let si env expr))
   ((app? expr)       (emit-app si env expr))
   ((primcall? expr)  (emit-primcall si env expr))
   (else (error 'emit-expr (format "~s is not a valid expression" expr)))))

(define (emit-function-header f)
  (emit "	.text")
  (emit "	.globl	~a" f)
  (emit "	.type	~a,	@function" f)
  (emit-label f))

(define (emit-program expr)
  (emit-function-header "scheme_entry")
  (emit "	mov	%rsp,	%rcx")
  (emit "	sub	$~s,	%rsp" wordsize)
  (emit "	call	L_scheme_entry")
  (emit "	mov	%rcx,	%rsp")
  (emit "	ret")
  (emit-label "L_scheme_entry")
  (emit-expr (- wordsize) '() expr)
  (emit "	ret"))
