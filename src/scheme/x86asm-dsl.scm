
;;; x86asm-dsl.scm --- dsl for generating x86 assembly

;; Copyright (C) 2012,2013 Zachary Elliott
;; See LICENSE for more information

;;; Commentary:

;;

;;; Code:


;;; Helper functions and symbols


;; x86 registers of use - (there are many more)

(define ax  'rax)
(define bx  'rbx)
(define cx  'rcx)
(define dx  'rdx)
(define si  'rsi)
(define di  'rdi)
(define bp  'rbp)
(define sp  'rsp)
(define r8  'r8)
(define r9  'r9)
(define r10 'r10)
(define r11 'r11)
(define r12 'r12)
(define r13 'r13)
(define r14 'r14)
(define r15 'r15)


;; pseudo-dsl for asm instructions

(define (emit-instruction instr src dst)
  (emit "	~s	~s~s,	%~s" instr (if (symbol? src) '% '$) src dst))

(define (emit-save-instruction instr offset src dst)
  (emit "	~s	~s~s,	~s(%~s)" instr (if (symbol? src) '% '$) src offset dst))

(define (emit-load-instruction instr offset src dst)
  (emit "	~s	~s(%~s),	%~s" instr offset src dst))

(define (emit-mov src dst)
  (emit-instruction 'mov src dst))

(define (emit-save offset src dst)
  (emit-save-instruction 'mov offset src dst))

(define (emit-load offset src dst)
  (emit-load-instruction 'mov offset src dst))

;; end of x86asm-dsl.scm
