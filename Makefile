
CC = gcc
RM = rm -Rvf

CFLAGS = -ggdb -x c -std=c99 -c -Wall -Wextra -Werror -pedantic-errors -Wdouble-promotion -Wformat=2 -Winit-self -Wmissing-include-dirs -Wparentheses -Wswitch-default -Wswitch-enum -Wsync-nand -Wuninitialized -Wunknown-pragmas -Wstrict-overflow=2 -Wfloat-equal -Wundef -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wconversion -Wlogical-op -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wmissing-format-attribute -Wpacked -Wpacked-bitfield-compat -Wdouble-promotion -Wformat=2 -Winit-self -Wmissing-include-dirs -Wparentheses -Wswitch-default -Wswitch-enum -Wsync-nand -Wuninitialized -Wunknown-pragmas -Wstrict-overflow=2 -Wfloat-equal -Wundef -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wconversion -Wlogical-op -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wmissing-format-attribute -Wpacked -Wpacked-bitfield-compat -Wredundant-decls -Wnested-externs -Winline -Woverlength-strings -Wcomments -Wundef -Wunused-macros -Wendif-labels
LDFLAGS =

SRCROOT = src/c/src
INCROOT = src/c/include
TSTROOT = test/c

OBJROOT = build

VPATH=$(SRCROOT):$(INCROOT):$(TSTROOT):$(OBJROOT)


.PHONY: all

all:

# pointer.o: lang/types/atom/pointer.c lang/types/atom/pointer.h
# 	$(CC) $(CFLAGS) -I$(INCROOT) -o $(OBJROOT)/$(@) $(<)

# repl: top.o atom.o symbol.o boolean.o number.o cons.o elementary.o auxiliary.o evalquote.o lexer.o repl.o
# 	$(CC) -o $(@) $(OBJROOT)/top.o $(OBJROOT)/atom.o $(OBJROOT)/symbol.o $(OBJROOT)/boolean.o $(OBJROOT)/number.o $(OBJROOT)/cons.o $(OBJROOT)/elementary.o $(OBJROOT)/auxiliary.o $(OBJROOT)/evalquote.o $(OBJROOT)/lexer.o $(OBJROOT)/repl.o


.PHONY: clean

clean:
	$(RM) $(OBJROOT)/*.o


.PHONY: rebuild

rebuild: clean all
