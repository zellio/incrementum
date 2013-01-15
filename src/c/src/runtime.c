

#include "runtime.h"

#include <stdio.h>


int print_ptr(ptr x) {
    if ((x & FIXNUM_MASK) == LANG_T_FIXNUM) {
        printf( "%d", ((int)x) >> FIXNUM_SHIFT );
    }
    else if ( x == LANG_T_BOOLEAN_T ) {
        printf( "#t" );
    }
    else if ( x == LANG_T_BOOLEAN_F ) {
        printf( "#f" );
    }
    else if ( x == LANG_T_NIL ) {
        printf( "()" );
    }
    else if ((x & CHAR_MASK) == LANG_T_CHAR) {
        char c;
        switch ((c = (char) (x >> CHAR_SHIFT))) {
        case '\t': printf( "#\\tab" );     break;
        case '\n': printf( "#\\newline" ); break;
        case '\r': printf( "#\\return" );  break;
        case ' ':  printf( "#\\space" );   break;
        default:   printf( "#\\%c", c );   break;
        }
    }
    else {
        printf("#<Unknown 0x%08x>", x);
    }
    printf( "\n" );

    fflush(stdout);
    return 0;
}


int main(void) {
    print_ptr(scheme_entry());
    return 0;
}
