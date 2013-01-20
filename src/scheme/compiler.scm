
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

(define fixnum-shift 2)
(define fixnum-mask #x03)
(define fixnum-tag #x00)

(define boolean-t #x6F)
(define boolean-f #x2F)
(define boolean-bit #x06)
(define boolean-mask #xBF)

(define char-shift 8)
(define char-tag #x0F)
(define char-mask #x3F)


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
(define (immediate? expr)
  (or (null? expr) (fixnum? expr) (boolean? expr) (char? expr)))

(define (immediate-rep x)
  (cond
   ((fixnum? x) (ash x fixnum-shift))
   ((boolean? x) (if x boolean-t boolean-f))
   ((null? x) nil-tag)
   ((char? x) (logor (ash (char->integer x) char-shift) char-tag))
   (else #f)))

(define (emit-immediate x)
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
  (emit "	~a	%al" (if (null? args) 'sete (car args)))
  (emit "	movzb	%al,	%rax")
  (emit "	sal	$~s,	%al" boolean-bit)
  (emit "	or	$~s,	%al" boolean-f))

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
  (emit "	not	%eax")
  (emit "	shl	$~s,	%rax" fixnum-shift))

(define-primitive ($fxzero? si env arg)
  (emit-expr si env arg)
  (emit "	cmp	$~s,	%al" fixnum-tag)
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
  (emit "	cmp	$~s,	%al" boolean-f)
  (emit-boolean-transform))

(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (emit "	and	$~s,	%al" char-mask)
  (emit "	cmp	$~s,	%al" char-tag)
  (emit-boolean-transform))

(define-primitive (not si env arg)
  (emit-expr si env arg)
  (emit "	cmp	$~s,	%al" boolean-f)
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

(define (emit-if si env expr)
  (let ((alternate-label (unique-label))
        (terminal-label (unique-label)))
    (emit-expr si env (if-predicate expr))
    (emit "	cmp	$~s,	%al" boolean-f)
    (emit "	je	~a" alternate-label)
    (emit-expr si env (if-consequent expr))
    (emit-jmp terminal-label)
    (emit-label alternate-label)
    (emit-expr si env (if-alternate expr))
    (emit-label terminal-label)))

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
  (list-expr? 'and expr))

(define emit-and
  (emit-conditional-block #t "je"))

(define (or? expr)
  (list-expr? 'or expr))

(define emit-or
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
  (emit-boolean-transform op))

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

;;
;; 1.6 Local Variables
;;
(define variable? symbol?)

(define (emit-variable-ref env expr)
  (let ((table-entry (assoc expr env)))
    (if table-entry (emit-stack-load (cdr table-entry))
        (error 'emit-variable-ref (format "Undefined variable ~s" expr)))))

(define (let? expr)
  (list-expr? 'let expr))

(define (let*? expr)
  (list-expr? 'let* expr))

(define (let!? expr)
  (or (let? expr) (let*? expr)))

(define let-bindings cadr)
(define let-body caddr)

(define (extend-env var si new-env)
  (cons (cons var si) new-env))

(define (emit-let si env expr)
  (define (process-let bindings si new-env)
    (cond
     ((null? bindings) (emit-expr si new-env (let-body expr)))
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
         ((null? fmls) (emit-expr si env body) (emit-ret))
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

(define (emit-apply si env expr)
  (define (emit-apply-arguments si args)
    (unless (null? args)
      (emit-expr si env (car args))
      (emit-stack-save si)
      (emit-apply-arguments (next-stack-index si) (cdr args))))
  (emit-apply-arguments (next-stack-index si) (call-args expr))
  (emit-adjust-base (prev-stack-index si))
  (emit-call (assoc-val (call-target expr) env))
  (emit-adjust-base (- (prev-stack-index si))))

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
  (emit-label "l_scheme_entry")
  (emit-expr (- wordsize) env expr)
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
;;  Compiler
;;
(define (emit-expr si env expr)
  (cond
   ((immediate? expr)      (emit-immediate expr))
   ((variable? expr)       (emit-variable-ref env expr))
   ((if? expr)             (emit-if si env expr))
   ((and? expr)            (emit-and si env expr))
   ((or? expr)             (emit-or si env expr))
   ((let!? expr)           (emit-let si env expr))
   ((apply? expr env)      (emit-apply si env expr))
   ((primitive-call? expr) (emit-primitive-call si env expr))
   (else (error 'emit-expr (format "~s is not a valid expression" expr)))))

(define (emit-program-header)
  (emit "	.text")
  (emit-function-header "scheme_entry")
  (emit "	mov	%rsp,	%rcx")
  (emit "	mov	0x10(%rsp), %rsp")
  (emit-call "l_scheme_entry")
  (emit "	mov	%rcx,	%rsp")
  (emit-ret))

(define (emit-program program)
  (emit-program-header)
  (if (letrec? program) (emit-letrec program)
      (emit-scheme-entry program (make-initial-env '() '()))))
