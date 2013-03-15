

# Incrementum

An incrementally implemented compiler for scheme targeting x86 assembly.

## Credits

The work is largely based on the 2006
[paper](doc/An Incremental Approach to Compiler Construction.pdf) and
[tutorial](Compilers: Backend to Frontend and Back to Front Again.pdf)
by Abdulaziz Ghuloum.

The implementation language is currently Chez Scheme v8.4

## Why?

The plan for [Incrementum](https://github.com/zellio/incrementum) is
large. First a largely complete compiler is to be implemented. Then, on top of
it, the [W](http://en.wikipedia.org/wiki/Hindley%E2%80%93Milner) type-inference
algorithm. Followed by a [miniKanren](http://kanren.sourceforge.net/)
system. This project is largely for personal enrichment.

For now, the commit log will serve as the play by play of my
progress. Hopefully, time permitting, I will write more useful notes on various
design choices. The goal therein is to flesh out the tutorial where it is vague
or dated and to extend it far beyond its original scope.

## Copyright

Copyright (c) 2013 Zachary Elliott. See LICENSE for further details.
