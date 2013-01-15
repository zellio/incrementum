
CC = gcc
RM = rm -Rf

CFLAGS = -ggdb -x c -std=gnu99 -c -Wall -Wextra -Werror -pedantic-errors -Wdouble-promotion -Wformat=2 -Winit-self -Wmissing-include-dirs -Wparentheses -Wswitch-default -Wswitch-enum -Wsync-nand -Wuninitialized -Wunknown-pragmas -Wstrict-overflow=2 -Wfloat-equal -Wundef -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wconversion -Wlogical-op -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wmissing-format-attribute -Wpacked -Wpacked-bitfield-compat -Wdouble-promotion -Wformat=2 -Winit-self -Wmissing-include-dirs -Wparentheses -Wswitch-default -Wswitch-enum -Wsync-nand -Wuninitialized -Wunknown-pragmas -Wstrict-overflow=2 -Wfloat-equal -Wundef -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wconversion -Wlogical-op -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wmissing-format-attribute -Wpacked -Wpacked-bitfield-compat -Wredundant-decls -Wnested-externs -Winline -Woverlength-strings -Wcomments -Wundef -Wunused-macros -Wendif-labels
LDFLAGS =

SRCROOT = src/c/src
INCROOT = src/c/include
TSTROOT = test/c

OBJROOT = build
BINROOT = bin

VPATH=$(SRCROOT):$(INCROOT):$(TSTROOT):$(OBJROOT)


.PHONY: all

all: stst

.PHONY: clean

clean:
	$(RM) $(OBJROOT)/*.s
	$(RM) $(OBJROOT)/*.o
	$(RM) $(OBJROOT)/runtime
	$(RM) $(BINROOT)/*


ctest.o: ctest.c ctest.h
	$(CC) $(CFLAGS) -I$(INCROOT) -o $(OBJROOT)/$(@) $(<)

runtime.o: runtime.c runtime.h
	$(CC) $(CFLAGS) -I$(INCROOT) -o $(OBJROOT)/$(@) $(<)

runtime: runtime.o ctest.o
	$(CC) -o $(BINROOT)/$(@) $(OBJROOT)/runtime.o $(OBJROOT)/ctest.o

stst.o: stst.s
	$(CC) -c -o $(OBJROOT)/$(@) $(<)

stst: stst.o runtime.o
	$(CC) -o $(BINROOT)/$(@) $(OBJROOT)/runtime.o $(OBJROOT)/stst.o

.PHONY: test_clean

test_clean:
	$(RM) $(OBJROOT)/stst.s
	$(RM) $(OBJROOT)/stst.o
	$(RM) $(BINROOT)/*
	$(RM) stst.out

.SILENT: stst
.SILENT: stst.o
.SILENT: test_clean
