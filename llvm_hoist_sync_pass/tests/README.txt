These tests use the `lit' tool from LLVM to perform the testing. For the moment
they require the shared library for the pass be in a specific location, although
this should probably be resolved at some point (via llvm-config maybe?).

Any test can just be put in this directory and it will be run.

The tests can be run individually with `lit <test>.ll' or they can all be run
with `lit *.ll'.
