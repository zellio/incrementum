
;;;;
;;
;;  < EXPR > -> fixnum | boolean | char | null
;;


(define fixnum-shift 2)
(define fixnum-mask #x03)

(define wordsize 4) ;; bytes

(define fixnum-bits
  (- (* wordsize 8) fixnum-shift))

(define fixnum-lower-bound
  (- (expt 2 (- fixnum-bits 1))))

(define fixnum-upper-bound
  (sub1 (expt 2 (- fixnum-bits 1))))

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fixnum-lower-bound x fixnum-upper-bound)))


(define boolean_f #x2F)

(define boolean_t #x6F)

(define nil #x3F)

(define char #x0F)


(define (immediate? x)
  (or (fixnum? x) (boolean? x) (null? x) (char? x)))

(define (immediate-rep x)
  (cond
   ((fixnum? x) (ash x fixnum-shift))
   ((boolean? x) (if x boolean_t boolean_f))
   ((null? x) nil)
   ((char? x) (logor (ash (char->integer x) 8) char))
   (else #f)))


(define (emit-program x)
  (unless (immediate? x) (error 'emit-program "value must be an immediate"))
  (emit "	.text")
  (emit "	.globl scheme_entry")
  (emit "	.type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit "	movl $~s, %eax" (immediate-rep x))
  (emit "	ret"))
