#!/bin/bash

# do one level of submodules the right way
git submodule update --init

# now go in and manually clone subsubmodules to get around broken windows git

pushd Deques
rm -rf DequeTester/BenchScaling
git clone https://github.com/rrnewton/HSBenchScaling.git DequeTester/BenchScaling
rm -rf BenchThroughput
git clone https://github.com/rrnewton/HSBenchThroughput.git BenchThroughput
popd

pushd bench_results
rm -rf full_logs
git clone https://github.com/rrnewton/monad-par-results-full-logs.git full_logs
popd

pushd accelerate
rm -rf accelerate-io
git clone https://github.com/AccelerateHS/accelerate-io.git accelerate-io
rm -rf accelerate-buildbot
git clone https://github.com/AccelerateHS/accelerate-buildbot.git accelerate-buildbot
rm -rf accelerate-examples
git clone https://github.com/AccelerateHS/accelerate-examples.git accelerate-examples
rm -rf accelerate-cuda
git clone https://github.com/AccelerateHS/accelerate-cuda.git accelerate-cuda
rm -rf accelerate-backend-kit
git clone https://github.com/AccelerateHS/accelerate-backend-kit.git accelerate-backend-kit
popd