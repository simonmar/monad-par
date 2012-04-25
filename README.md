
The Par Monad and Friends
=========================

Please read the package descriptions in './*.cabal'

If you've acquired this source package from github you should have the
examples/ and tests/ subdirectories.  Here are some commands you may
be interested in.  First, install everything:

    make install

That will actually run some unit tests in the process of installing.
If you want to build the examples and test further, try this:

    cd examples
    cabal configure --enable-tests
    cabal test

Also, some of the individual packages have test-suites:

    cd monad-par; cabal test

Or for more detailed output:

    ./monad-par/dist/build/test-monad-par/test-monad-par

