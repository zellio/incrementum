

#include "runtime.h"

#include <sys/mman.h>
#include <unistd.h>

#include <stdio.h>



static uint8_t* alloc_protected_space( size_t size ) {
    uint64_t page = (uint64_t)sysconf(_SC_PAGESIZE);
    uint64_t aligned_size = ((size + page - 1) / page) * page;

    uint8_t* smem = mmap(0, aligned_size + 2 * page, PROT_READ | PROT_WRITE,
                         MAP_ANONYMOUS | MAP_PRIVATE, 0, 0);

    if ( smem == MAP_FAILED )
        fprintf( stderr, "MAP FAILED\r\n");

    if ( mprotect(smem, page, PROT_NONE) != 0 )
        fprintf( stderr, "PROTECT FAILED\r\n" );

    if ( mprotect(smem + aligned_size + page, page, PROT_NONE) != 0 )
        fprintf( stderr, "PROTECT FAILED\r\n" );

    return (smem + page);
}

static void free_protected_space( uint8_t* space, size_t size ) {
    uint64_t page = (uint64_t)sysconf(_SC_PAGESIZE);
    uint64_t aligned_size = ((size + page - 1) / page) * page;

    if ( munmap( space - page, aligned_size + 2 * page ) != 0 )
        fprintf( stderr, "FREE FAILED\r\n" );
}


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
    size_t stack_size = 16 * 4096;
    uint8_t* stack_top = alloc_protected_space( stack_size );

    uint8_t* stack_base = stack_top + stack_size;

    print_ptr(scheme_entry(stack_base));
    free_protected_space( stack_top, stack_size );
    return 0;
}
