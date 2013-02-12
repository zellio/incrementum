
CC = gcc
RM = rm -Rf

CFLAGS = -m64 -ggdb -x c -std=gnu99 -c -Wall -Wextra -Werror -pedantic-errors -Wdouble-promotion -Wformat=2 -Winit-self -Wmissing-include-dirs -Wparentheses -Wswitch-default -Wswitch-enum -Wsync-nand -Wuninitialized -Wunknown-pragmas -Wstrict-overflow=2 -Wfloat-equal -Wundef -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wconversion -Wlogical-op -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wmissing-format-attribute -Wpacked -Wpacked-bitfield-compat -Wdouble-promotion -Wformat=2 -Winit-self -Wmissing-include-dirs -Wparentheses -Wswitch-default -Wswitch-enum -Wsync-nand -Wuninitialized -Wunknown-pragmas -Wstrict-overflow=2 -Wfloat-equal -Wundef -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wconversion -Wlogical-op -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wmissing-format-attribute -Wpacked -Wpacked-bitfield-compat -Wredundant-decls -Wnested-externs -Winline -Woverlength-strings -Wcomments -Wundef -Wunused-macros -Wendif-labels
LDFLAGS =

SRCROOT = src/c/src
INCROOT = src/c/include
TSTROOT = test/c

OBJROOT = build
BINROOT = bin

VPATH=$(SRCROOT):$(INCROOT):$(TSTROOT):$(OBJROOT)

.PHONY: all clean test_clean

all: runtime.o stst.o stst

runtime.o: runtime.c runtime.h
	$(CC) $(CFLAGS) -I$(INCROOT) -o $(OBJROOT)/$(@) $(<)

stst.o: stst.s
	$(CC) -m64 -c -o $(OBJROOT)/$(@) $(<)

stst: runtime.o stst.o
	$(CC) -o $(BINROOT)/$(@) $(OBJROOT)/runtime.o $(OBJROOT)/stst.o

clean test_clean:
	$(RM) $(OBJROOT)/*.s
	$(RM) $(OBJROOT)/*.o
	$(RM) $(BINROOT)/*
	$(RM) stst.out

.SILENT: stst.o stst test_clean
