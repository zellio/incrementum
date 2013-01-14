
;;;;
;;
;;  < EXPR > -> fixnum
;;
(define (emit-program x)
  (unless (integer? x) (error 'emit-program "value must be an integer"))
  (emit "	.text")
  (emit "	.globl scheme_entry")
  (emit "	.type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit "	movl $~s, %eax" x)
  (emit "	ret"))
