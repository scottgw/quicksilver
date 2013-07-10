# QuickSilver

This project is a new runtime for active-object type languages. It is
being built to be efficient and as a test-bed for new ideas. In
particular we target a light version of the SCOOP language, and provide a
compiler for this language.

## Runtime

The runtime is written in C and uses a few external libraries, most notably
  * libffi (to encapsulate calls)
  * TBB (just the concurrent queues)
  * ConcurrencyKit (again, just for the queues)

I often shorten the name of the project to qs to symbolize the central role
that concurrent queues have in the runtime. The runtime focuses on the
concurrent aspects of active-objects and does not (yet) include a garbage
collector.

The runtime is basically a lightweight thread implementation, a scheduler,
and a communication system built on the concurrent queues.

## Compiler

The compiler is written in Haskell and is relatively small (about 4700
lines).  It uses LLVM as a compilation target and to do the lower
level optimizations.  It generates code that calls the runtime, and
many tests/benchmarks are contained in the tests directory. It also
includes a very minimal base library that grows as needed.