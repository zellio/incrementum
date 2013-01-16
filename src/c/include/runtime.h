
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


typedef unsigned int ptr;

typedef enum {
    LANG_T_BOOLEAN_F = 0x2F,
    LANG_T_BOOLEAN_T = 0x6F,
    LANG_T_FIXNUM    = 0x00,
    LANG_T_NIL       = 0x3F,
    LANG_T_CHAR      = 0x0F
} type_e;


ptr scheme_entry(uint8_t* stack_base);

int print_ptr(ptr x);
int main(void);


#ifdef __cplusplus
};
#endif

#endif
