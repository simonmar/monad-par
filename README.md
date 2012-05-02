
The Par Monad and Friends
=========================

Please read the package descriptions in './*.cabal'.  If you've
acquired this source package from github you should have the examples/
subdirectory.  Look at examples/README to get started.

Here are some commands you may be interested in.  First, install
to *everything* (except for GPU-dependent packages):

    git submodule update --init --recursive
    make mega-install

That will install certain dependencies as well (Deques and
accelerate), "make install" just installs the packages directly
contained within this repo.  See the Makefile for other things you can
do (e.g. "make mega-install-gpu").

Some of the individual packages have test-suites:

    cd monad-par; cabal test

Or for more detailed output:

    ./monad-par/dist/build/test-monad-par/test-monad-par
