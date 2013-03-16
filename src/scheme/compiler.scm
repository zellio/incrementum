
;;; compiler.scm --- Scheme to x86 compiler

;; Copyright (C) 2012,2013 Zachary Elliott
;; See LICENSE for more information

;;; Commentary:

;; this file assumes that x86asm-dsl.scm has been loaded

;;; Code:

;;
;; <Program> -> <Expr>
;;            | (letrec ((lvar <Lambda>) ...) <Expr>)
;;  <Lambda> -> (lambda (var ...) <Expr>)
;;    <Expr> -> <Imm>
;;            | var
;;            | (if <Expr> <Expr> <Expr>)
;;            | (let ((var <Expr>) ...) <Expr>)
;;            | (app lvar <Expr> ... )
;;            | (prim <Expr>)
;;     <Imm> -> fixnum | boolean | char | null
;;

(define wordsize 8)

(define nil-tag #x3F)

(define boolean-mask #xBF)
(define boolean-bit  #x06)
(define true-tag     #x6F)
(define false-tag    #x2F)

(define fixnum-shift 2)
(define fixnum-mask  #x03)
(define fixnum-tag   #x00)

(define char-shift 8)
(define char-mask  #x3F)
(define char-tag   #x0F)

(define object-mask #x07)
(define cons-tag    #x01)
(define vector-tag  #x05)
(define string-tag  #x06)

(define heap-align-mask #xF8)


(define fixnum?
  (let* ((bit-length (- (* wordsize 8) fixnum-shift))
         (lower-bound (- (expt 2 (- bit-length 1))))
         (upper-bound (sub1 (expt 2 (- bit-length 1)))))
    (lambda (x)
      (and (integer? x) (exact? x) (<= lower-bound x upper-bound)))))

;;
;; 1.1 Integers
;; 1.2 Immediate Constants
;;
(define (immediate? expr)
  (or (null? expr) (fixnum? expr) (boolean? expr) (char? expr)))

(define (immediate-rep x)
  (cond
   ((fixnum? x) (ash x fixnum-shift))
   ((boolean? x) (if x true-tag false-tag))
   ((null? x) nil-tag)
   ((char? x) (logor (ash (char->integer x) char-shift) char-tag))
   (else #f)))

(define (emit-immediate value . args)
  (emit-mov (immediate-rep value) (if (null? args) ax (car args))))


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

(define (primitive-call? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (primitive-arg-count sym)
  (or (getprop sym '*arg-count*)
      (error 'primitive-arg-count
             (format "primitive ~s has no arg count" sym))))

(define (primitive-emitter sym)
  (or (getprop sym '*emitter*)
      (error 'primitive-emitter
             (format "primitive ~s has no emitter" sym))))

(define (check-primitive-call-args sym args)
  (=  (primitive-arg-count sym) (length args)))

(define (emit-primitive-call si env expr)
  (let ((sym (car expr))
        (args (cdr expr)))
    (check-primitive-call-args sym args)
    (apply (primitive-emitter sym) si env args)))


(define (emit-boolean-transform . args)
  (let* ((arg-length (length args))
         (target (if (> arg-length 0) (car args) ax))
         (asm-op (if (> arg-length 1) (cadr args) 'sete)))
    (emit "	~a	%al" asm-op)
    (emit-and #x7 target)
    (emit-shl boolean-bit target)
    (emit-or false-tag target)))

(define (mask-primitive primitive label)
  (putprop label '*is-prim* #t)
  (putprop label '*arg-count* (primitive-arg-count primitive))
  (putprop label '*emitter* (primitive-emitter primitive)))


(define-primitive ($fxadd1 si env arg)
  (emit-expr si env arg)
  (emit "	add	$~s,	%rax" (immediate-rep 1)))

(define-primitive ($fxsub1 si env arg)
  (emit-expr si env arg)
  (emit "	sub	$~s,	%rax" (immediate-rep 1)))

(define-primitive ($fixnum->char si env arg)
  (emit-expr si env arg)
  (emit "	shl	$~s,	%rax" (- char-shift fixnum-shift))
  (emit "	or	$~s,	%rax" char-tag))

(define-primitive ($char->fixnum si env arg)
  (emit-expr si env arg)
  (emit "	shr	$~s,	%rax" (- char-shift fixnum-shift)))

(define-primitive ($fxlognot si env arg)
  (emit-expr si env arg)
  (emit "	shr	$~s,	%rax" fixnum-shift)
  (emit "	not	%rax")
  (emit "	shl	$~s,	%rax" fixnum-shift))

(define-primitive ($fxzero? si env arg)
  (emit-expr si env arg)
  (emit "	cmp	$~s,	%rax" fixnum-tag)
  (emit-boolean-transform))

(map
 mask-primitive
 '($fxadd1 $fxsub1 $fixnum->char $char->fixnum $fxlognot $fxzero?)
 '( fxadd1  fxsub1  fixnum->char  char->fixnum  fxlognot  fxzero?))

(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
  (emit "	and	$~s,	%al" fixnum-mask)
  (emit "	cmp	$~s,	%al" fixnum-tag)
  (emit-boolean-transform))

(define-primitive (null? si env arg)
  (emit-expr si env arg)
  (emit "	cmp	$~s,	%al" nil-tag)
  (emit-boolean-transform))

(define-primitive (boolean? si env arg)
  (emit-expr si env arg)
  (emit "	and	$~s,	%al" boolean-mask)
  (emit "	cmp	$~s,	%al" false-tag)
  (emit-boolean-transform))

(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (emit "	and	$~s,	%al" char-mask)
  (emit "	cmp	$~s,	%al" char-tag)
  (emit-boolean-transform))

(define-primitive (not si env arg)
  (emit-expr si env arg)
  (emit "	cmp	$~s,	%al" false-tag)
  (emit-boolean-transform))


;;
;;  1.4 Conditional Expressions
;;
(define unique-label
  (let ((count 0))
    (lambda ()
      (let ((label (format "sym_~s" count)))
        (set! count (add1 count))
        label))))

(define (list-expr? sym expr)
  (and (list? expr) (not (null? expr)) (eq? sym (car expr))))

(define (emit-label label)
  (emit "~a:" label))

(define (emit-jmp label)
  (emit "	jmp	~a" label))

(define (if? expr)
  (and (list-expr? 'if expr) (= 4 (length expr))))

(define if-predicate  cadr)
(define if-consequent caddr)
(define if-alternate  cadddr)

(define (emit-if si env tail expr)
  (let ((alternate-label (unique-label))
        (terminal-label (unique-label)))
    (emit-expr si env (if-predicate expr))
    (emit "	cmp	$~s,	%al" false-tag)
    (emit "	je	~a" alternate-label)
    (emit-generic-expr si env tail (if-consequent expr))
    (emit-jmp terminal-label)
    (emit-label alternate-label)
    (emit-generic-expr si env tail (if-alternate expr))
    (emit-label terminal-label)))

(define (emit-jump-block si env expr jump label)
  (let ((head (car expr)) (rest (cdr expr)))
    (emit-expr si env head)
    (emit "	cmp	$~s,	%al" false-tag)
    (emit "	~a	~a" jump label)
    (unless (null? rest)
      (emit-jump-block si env rest jump label))))

(define (emit-conditional-block default jump)
  (lambda (si env tail expr)
    (case (length expr)
      ((1) (emit-immediate default))
      ((2) (emit-expr si env (cadr expr)))
      (else
       (let ((end-label (unique-label)))
         (emit-jump-block si env (cdr expr) jump end-label)
         (emit-label end-label))))))

(define (and? expr)
  (list-expr? 'and expr))

(define emit-and-expr
  (emit-conditional-block #t "je"))

(define (or? expr)
  (list-expr? 'or expr))

(define emit-or-expr
  (emit-conditional-block #f "jne"))


;;
;; 1.5 Binary Primitives
;;
(define (next-stack-index si)
  (- si wordsize))

(define (prev-stack-index si)
  (+ si wordsize))

(define (emit-stack-save si)
  (emit "	mov	%rax,	~s(%rsp)" si))

(define (emit-stack-load si)
  (emit "	mov	~s(%rsp),	%rax" si))

(define (emit-binary-operator si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-stack-save si)
  (emit-expr (next-stack-index si) env arg2))

(define (define-binary-predicate op si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  (emit "	cmp	%rax,	~s(%rsp)" si)
  (emit-boolean-transform ax op))

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

(map
 mask-primitive
 '(fx=   fx<   fx<=   fx>   fx>=  )
 '(char= char< char<= char> char>=))

;;
;; 1.6 Local Variables
;;
(define variable? symbol?)

(define (emit-variable-ref env expr)
  (let ((table-entry (assoc expr env)))
    (if table-entry (emit-stack-load (cdr table-entry))
        (error 'emit-variable-ref (format "Undefined variable ~s" expr)))
    ))

(define (let? expr)
  (list-expr? 'let expr))

(define (let*? expr)
  (list-expr? 'let* expr))

(define (let!? expr)
  (or (let? expr) (let*? expr)))

(define let-bindings cadr)
(define (let-body expr)
  (if (> (length expr) 3) (cons 'begin (cddr expr)) (caddr expr)))

(define (extend-env var si new-env)
  (cons (cons var si) new-env))

(define (emit-let si env tail expr)
  (define (process-let bindings si new-env)
    (cond
     ((null? bindings)
      (emit-generic-expr si new-env tail (let-body expr)))
     (else
      (let ((binding (car bindings)))
        (emit-expr si (if (let*? expr) new-env env) (cadr binding))
        (emit-stack-save si)
        (process-let (cdr bindings)
                     (next-stack-index si)
                     (extend-env (car binding) si new-env))))))
  (process-let (let-bindings expr) si env))


;;
;; 1.7 Procedures
;;
(define (emit-function-header label)
  (emit "	.globl	~a" label)
  (emit "	.type	~a,	@function" label)
  (emit-label label))

(define (emit-call label)
  (emit "	call	~a" label))

(define (emit-ret)
  (emit "	ret"))

(define lambda-fmls cadr)
(define lambda-body caddr)

(define (emit-lambda env)
  (lambda (expr label)
    (emit-function-header label)
    (let ((fmls (lambda-fmls expr))
          (body (lambda-body expr)))
      (let fn ((fmls fmls) (si (next-stack-index 0)) (env env))
        (cond
         ((null? fmls) (emit-tail-expr si env body) (emit-ret))
         (else
          (fn (cdr fmls) (next-stack-index si) (extend-env (car fmls) si env))))
        ))
    ))

(define call-target car)
(define call-args cdr)

(define (apply? expr env)
  (and (list? expr) (not (null? expr)) (assoc (call-target expr) env)))

(define (emit-adjust-base si)
  (unless (zero? si) (emit "	add	$~s,	%rsp" si)))

(define (assoc-val key alist)
  (cdr (assoc key alist)))

(define (emit-apply si env tail expr)
  (define (emit-apply-arguments si args)
    (unless (null? args)
      (emit-expr si env (car args))
      (emit-stack-save si)
      (emit-apply-arguments (next-stack-index si) (cdr args))))
  (define (emit-collapse-stack-frame si frame-size args)
    (unless (or (= frame-size 0) (null? args))
      (emit-stack-load si)
      (emit-stack-save (+ si frame-size))
      (emit-collapse-stack-frame (- si wordsize) frame-size (cdr args))))
  (if tail
    (begin
      (emit-apply-arguments si (call-args expr))
      (emit-collapse-stack-frame si (- (+ si wordsize)) (call-args expr))
      (emit-jmp (assoc-val (call-target expr) env)))
    (begin
      (emit-apply-arguments (next-stack-index si) (call-args expr))
      (emit-adjust-base (prev-stack-index si))
      (emit-call (assoc-val (call-target expr) env))
      (emit-adjust-base (- (prev-stack-index si))))
    ))

(define (letrec? expr)
  (list-expr? 'letrec expr))

(define letrec-bindings cadr)
(define letrec-body caddr)

(define (letrec-labels lvars)
  (map (lambda (lvar) (format "fn_~s" lvar)) lvars))

(define (make-initial-env lvars labels)
  (map cons lvars labels))

(define (emit-scheme-entry expr env)
  (emit-function-header "l_scheme_entry")
  (emit-tail-expr (- wordsize) env expr)
  (emit-ret))

(define (emit-letrec expr)
  (let* ((bindings (letrec-bindings expr))
         (lvars (map car bindings))
         (lambdas (map cadr bindings))
         (labels (letrec-labels lvars))
         (env (make-initial-env lvars labels)))
    (for-each (emit-lambda env) lambdas labels)
    (emit-scheme-entry (letrec-body expr) env)))


;;
;; 1.9 Heap
;;
(define heap-align-mask #xF8)
(define heap-mask #x07)

(define pair-tag  #x01)

(define-primitive (pair? si env arg)
  (emit-expr si env arg)
  (emit "	and	$~s,	%al" heap-mask)
  (emit "	cmp	$~s,	%al" pair-tag)
  (emit-boolean-transform))

(define (cons? expr)
  (list-expr? 'cons expr))

(define car-offset 0)
(define cdr-offset wordsize)
(define pair-size 2)

(define (emit-heap-store hi)
  (emit "	mov	%rax,	~s(%rbp)" hi))

(define-primitive (cons si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  (emit "	mov	%rax,	~s(%rbp)" cdr-offset)
  (emit-stack-load si)
  (emit "	mov	%rax,	~s(%rbp)" car-offset)
  (emit "	mov	%rbp,	%rax")
  (emit "	or	$~s,	%rax" pair-tag)
  (emit "	add	$~s,	%rbp" (* 8 pair-size)))

(define-primitive (car si env arg)
  (emit-expr si env arg)
  (emit "	and	$~s,	%al" heap-align-mask)
  (emit "	mov	~s(%rax),	%rax" car-offset))

(define-primitive (cdr si env arg)
  (emit-expr si env arg)
  (emit "	and	$~s,	%al" heap-align-mask)
  (emit "	mov	~s(%rax),	%rax" cdr-offset))

(define (begin? expr)
  (list-expr? 'begin expr))

(define (emit-begin si env tail expr)
  (if
   (null? expr) '()
   (let ((head (car expr))
         (rest (cdr expr)))
     (emit-expr si env head)
     (emit-begin si env tail rest))))

(define-primitive (set-cdr! si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  (emit "	mov	~s(%rsp),	%rcx" si)
  (emit "	and	$~s,	%cl" heap-align-mask)
  (emit "	mov	%rax,	~s(%rcx)" cdr-offset))

(define-primitive (set-car! si env arg1 arg2)
  (emit-binary-operator si env arg1 arg2)
  (emit "	mov	~s(%rsp),	%rcx" si)
  (emit "	and	$~s,	%cl" heap-align-mask)
  (emit "	mov	%rax,	~s(%rcx)" car-offset))

(define-primitive (eq? si env arg1 arg2)
  (define-binary-predicate 'sete si env arg1 arg2))

(define vector-tag #x05)

(define-primitive (vector? si env arg)
  (emit-expr si env arg)
  (emit "	and	$~s,	%al" heap-mask)
  (emit "	cmp	$~s,	%al" vector-tag)
  (emit-boolean-transform))

(define vector-length-offset  0)
(define vector-content-offset wordsize)

(define-primitive (make-vector si env arg)
  (emit-expr si env arg)
  (emit "	mov	%rbp,	%rcx")
  (emit "	mov	%rax,	~s(%rbp)" vector-length-offset)
  (emit "	add	$~s,	%rbp" wordsize)
  (emit "	mov	$~s,	%rdx" nil-tag)
  (emit "	shr	$~s,	%rax" fixnum-shift)
  (let ((memset-start (unique-label))
        (memset-end   (unique-label)))
    (emit-label memset-start)
    (emit "	cmp	$0x0,	%rax")
    (emit "	je	~a" memset-end)
    (emit "	mov	%rdx,	0(%rbp)")
    (emit "	sub	$0x1,	%rax")
    (emit "	add	$~s,	%rbp" wordsize)
    (emit-jmp memset-start)
    (emit-label memset-end))
  (emit "	mov	%rcx,	%rax")
  (emit "	or	$~s,	%rax" vector-tag))

(define-primitive (vector-length si env arg)
  (emit-expr si env arg)
  (emit "	and	$~s,	%al" heap-align-mask)
  (emit "	mov	~s(%rax),	%rax" vector-length-offset))

(define-primitive (vector-set! si env arg1 arg2 arg3)
  (emit-expr si env arg1)
  (emit-stack-save si)
  (emit-expr (next-stack-index si) env arg2)
  (emit-stack-save (next-stack-index si))
  (emit-expr (next-stack-index (next-stack-index si)) env arg3)
  (emit "	mov	%rax,	%rdx")
  (emit "	mov	~s(%rsp),	%rcx" (next-stack-index si))
  (emit "	mov	~s(%rsp),	%rax" si)
  (emit "	and	$~s,	%al" heap-align-mask)
  (emit "	shl	$0x1,	%rcx")
  (emit "	add	$~s,	%rax" vector-content-offset)
  (emit "	add	%rcx,	%rax")
  (emit "	mov	%rdx,	(%rax)"))

(define-primitive (vector-ref si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-stack-save si)
  (emit-expr (next-stack-index si) env arg2)
  (emit "	mov	%rax,	%rcx")
  (emit-stack-load si)
  (emit "	and	$~s,	%al" heap-align-mask)
  (emit "	shl	$0x1,	%rcx")
  (emit "	add	$~s,	%rax" vector-content-offset)
  (emit "	add	%rcx,	%rax")
  (emit "	mov	(%rax),	%rax"))

(define string-tag #x06)

(define-primitive (string? si env arg)
  (emit-expr si env arg)
  (emit "	and	$~s,	%al" heap-mask)
  (emit "	cmp	$~s,	%al" string-tag)
  (emit-boolean-transform))

(define-primitive (make-string si env arg)
  (emit-expr si env arg)
  (emit "	mov	%rbp,	%rcx")
  (emit "	mov	%rax,	~s(%rbp)" vector-length-offset)
  (emit "	add	$~s,	%rbp" wordsize)
  (emit "	mov	$~s,	%rdx" char-tag)
  (emit "	shr	$~s,	%rax" fixnum-shift)
  (let ((memset-start (unique-label))
        (memset-end   (unique-label)))
    (emit-label memset-start)
    (emit "	cmp	$0x0,	%rax")
    (emit "	je	~a" memset-end)
    (emit "	mov	%rdx,	0(%rbp)")
    (emit "	sub	$0x1,	%rax")
    (emit "	add	$~s,	%rbp" wordsize)
    (emit-jmp memset-start)
    (emit-label memset-end))
  (emit "	mov	%rcx,	%rax")
  (emit "	or	$~s,	%rax" string-tag))

(map
 mask-primitive
 '(vector-set! vector-ref vector-length)
 '(string-set! string-ref string-length))


;;
;;  Compiler
;;
(define (emit-generic-expr si env tail expr)
  (cond
   ((immediate? expr) (emit-immediate expr) (when tail (emit-ret)))
   ((variable? expr) (emit-variable-ref env expr) (when tail (emit-ret)))
   ((if? expr) (emit-if si env tail expr))
   ((and? expr) (emit-and-expr si env tail expr))
   ((or? expr) (emit-or-expr si env tail expr))
   ((let!? expr) (emit-let si env tail expr))
   ((apply? expr env) (emit-apply si env tail expr))
   ((begin? expr) (emit-begin si env tail (cdr expr)))
   ((primitive-call? expr)
    (emit-primitive-call si env expr) (when tail (emit-ret)))
   (else (error 'emit-expr (format "~s is not a valid expression" expr)))))

(define (emit-expr si env expr)
  (emit-generic-expr si env #f expr))

(define (emit-tail-expr si env expr)
  (emit-generic-expr si env #t expr))

(define (emit-program-header)
  (emit "	.text")
  (emit-function-header "scheme_entry")

  ;; store context ( base ptr in rax )
  (emit "	mov	%rbx,	0x8(%rax)")
  (emit "	mov	%rsi,	0x20(%rax)")
  (emit "	mov	%rdi,	0x28(%rax)")
  (emit "	mov	%rbp,	0x30(%rax)")
  (emit "	mov	%rsp,	0x38(%rax)")

  ;; load stack
  (emit "	mov	%rcx,	%rsp")

  ;; load heap
  (emit "	mov	%rdx,	%rbp")

  ;; store context
  (emit "	mov	%rax,	%rbx")

  (emit-call "l_scheme_entry")

  (emit "	mov	0x38(%rbx),	%rsp")
  (emit "	mov	0x30(%rbx),	%rbp")
  (emit "	mov	0x28(%rbx),	%rdi")
  (emit "	mov	0x20(%rbx),	%rsi")
  (emit "	mov	0x8(%rbx),	%rbx")

  (emit-ret))

(define (emit-program program)
  (emit-program-header)
  (if (letrec? program) (emit-letrec program)
      (emit-scheme-entry program (make-initial-env '() '()))))

;; end of compiler.scm
