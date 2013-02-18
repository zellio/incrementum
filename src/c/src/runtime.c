

#include "runtime.h"

#include <sys/mman.h>
#include <unistd.h>

#include <stdio.h>
#include <stdlib.h>


static char* alloc_protected_space( size_t size ) {
    uint64_t page = (uint64_t)sysconf(_SC_PAGESIZE);
    uint64_t aligned_size = ((size + page - 1) / page) * page;

    char* smem = mmap(0, aligned_size + 2 * page, PROT_READ | PROT_WRITE,
                         MAP_ANONYMOUS | MAP_PRIVATE, 0, 0);

    if ( smem == MAP_FAILED )
        fprintf( stderr, "MAP FAILED\r\n");

    int status = 0;
    if ((status = mprotect(smem, page, PROT_NONE)) != 0 ) {
        perror( "mprotect failed" );
        exit(status);
    }
    if ((status = mprotect(smem + aligned_size + page, page, PROT_NONE)) != 0) {
        perror( "mprotect failed" );
        exit(status);
    }
    return (smem + page);
}

static void free_protected_space( char* space, size_t size ) {
    uint64_t page = (uint64_t)sysconf(_SC_PAGESIZE);
    uint64_t aligned_size = ((size + page - 1) / page) * page;

    int status = munmap( space - page, aligned_size + 2 * page );
    if ( status != 0 ) {
        perror( "munmap failed" );
        exit(status);
    }
}

int _print_ptr(ptr x, char in_list) {
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
    else if ((x & OBJECT_MASK) == LANG_T_CONS) {
        if (in_list == 0)
            printf("(");
        _print_ptr(((cons_t*)(x - 1))->car, 0);

        ptr cdr = ((cons_t*)(x - 1))->cdr;
        if (cdr != LANG_T_NIL) {
            if ((cdr & OBJECT_MASK) == LANG_T_CONS) {
                printf(" ");
                _print_ptr(cdr, 1);
            }
            else {
                printf(" . ");
                _print_ptr(cdr, 0);
            }
        }
        if (in_list == 0)
            printf(")");
    }
    else {
        printf("#<Unknown 0x%08lx>", x);
    }
    fflush(stdout);
    return 0;
}

int print_ptr(ptr x) {
    _print_ptr(x, 0);
    printf( "\n" );
    return 0;
}

int main(void) {
    size_t stack_size = 16 * 4096;
    size_t heap_size = 16 * 4096;

    char* stack_top = alloc_protected_space( stack_size );
    char* stack_base = stack_top + stack_size;

    char* heap = alloc_protected_space( heap_size );

    context_t context;
    print_ptr(scheme_entry(&context, stack_base, heap));

    free_protected_space( stack_top, stack_size );
    free_protected_space( heap, heap_size );
    return 0;
}
