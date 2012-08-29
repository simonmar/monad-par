
[2012.08.29]

This directory contains regression tests.

What's the distinction between this directory and `examples/`?  Well,
examples should contain *meaningful programs*, whereas this directory
may include many nonsense programs and program fragments.  But both
sets of programs are used in validation and testing.

Note also that there are other `tests/` directories within individual
modules inside this repository.  The reason the tests in this
directory live "out here" is because they are meant to test *multiple*
Par scheduler implementations, just as the examples are meant to run
with multiple implementations.  We follow the same convention of using 

