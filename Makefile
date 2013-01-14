
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

all: runtime

ctest.o: ctest.c ctest.h
	$(CC) $(CFLAGS) -I$(INCROOT) -o $(OBJROOT)/$(@) $(<)

runtime.o: runtime.c runtime.h
	$(CC) $(CFLAGS) -I$(INCROOT) -o $(OBJROOT)/$(@) $(<)

runtime: runtime.o ctest.o
	$(CC) -o $(OBJROOT)/$(@) $(OBJROOT)/runtime.o $(OBJROOT)/ctest.o


.PHONY: clean

clean:
	$(RM) $(OBJROOT)/*.o
	$(RM) $(OBJROOT)/runtime


.PHONY: rebuild

rebuild: clean all
