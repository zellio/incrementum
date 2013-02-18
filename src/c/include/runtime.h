
#ifndef __CBTF_RUNTIME_H__
#define __CBTF_RUNTIME_H__ 1


#include <stdint.h>


#ifdef __cplusplus
extern "C" {
#endif
#if 0
}
#endif


#define FIXNUM_MASK 0x03
#define FIXNUM_SHIFT 2

#define CHAR_MASK  0x0F
#define CHAR_SHIFT 8

#define OBJECT_MASK 0x07

typedef unsigned long ptr;

typedef enum {
    LANG_T_BOOLEAN_F = 0x2F,
    LANG_T_BOOLEAN_T = 0x6F,
    LANG_T_FIXNUM    = 0x00,
    LANG_T_NIL       = 0x3F,
    LANG_T_CHAR      = 0x0F,
    LANG_T_CONS      = 0x01
} type_e;

typedef struct context context_t;
struct context {
    void* rax;
    void* rbx;
    void* rcx;
    void* rdx;
    void* rsi;
    void* rdi;
    void* rbp;
    void* rsp;
};

typedef struct cons cons_t;
struct cons {
    ptr car;
    ptr cdr;
};

ptr scheme_entry(context_t* context, char* stack_base, char* heap);

int print_ptr(ptr x);
int _print_ptr(ptr x, char in_list);
int main(void);


#ifdef __cplusplus
};
#endif

#endif
