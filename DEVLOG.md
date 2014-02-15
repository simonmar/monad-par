

This is an UGLY file full of development notes and debugging notes.
It's not meant for external consumption.

[2011.03.15] 
------------------------------------------------------------

I'm seeing some awful weird behavior with OpenLists's async_test.
(First, runParAsync doesn't return immediately as it should.  That's a
bug.)  But the real problem is that I'm seeing different behavior when
I make OpenList into a main module and compile it vs. when I compile
it as a library.

In particular, when compiled as a library, I get "thread blocked
indefinitely" errors.  

The other behavior is that it freezes while taking the head under
GHCI.  I think this is just the indefinite MVar block because it
doesn't burn CPU.

And the last behavior is that it will complete successfully (but the
chaintest still fails to return before the full chain is constructed).
(But now I'm having trouble reproducing this behavior.)

The underlying lazy_chaintest works fine when called from ghci!!

Hmm... this is quite finicky... printing the whole list instead of
taking the first three elements seems to avoid a freeze!?  

Ok... the problems seem to relate to whether its actually loaded
compiled.  After all, if compiled versions are around, ghci will use
them.


[2011.10.18] {Trying on SIF cluster}
------------------------------------------------------------

These are older Intel "Core" architecture Xeon's.  And they are pretty
slow.  For example building our example programs with "make -j" on
basalt takes 9.7 seconds and doing it on one SIF machine takes 45
seconds.


[2011.10.27] {Running full benchmark suite on smoketree}
------------------------------------------------------------

It did 624 configurations in 471 minutes:

    real    471m52.122s
    user    4450m51.630s
    sys     700m4.714s



[2011.12.07] {Debugging DistDirect.hs scheduler}
------------------------------------------------------------

RRN:

Currently seeing lockups (with no error) as well as some error
messages.  Here's one:

    parfib_dist: too few bytes. Failed reading at byte position 8

On my Mac I'm getting this erorr when I start up the worker:

    2011-12-08 06:18:29.156392 EST 2 pid://RyanMacbookAir:56105/8/ SYS
    Process got unhandled exception ConfigException "Can't talk to local
    node registry: QteNetworkError \"getAddrInfo: does not exist (nodename
    nor servname provided, or not known)\""

Regarding the non-deterministic freeze-up:

If I turn worker idling off then both the master and the slave
processes will converge on a state where they are using a minimal
amount of CPU (~3% each).

It looks like a lost continuation problem perhaps?  

I was able to reproduce it on parfib_dist invocations as small as 5
previously (no idling), now with idling turned on I'm having a bit of
trouble doing that...  But I can still get it to freeze at larger
sizes such as 15.



[2012.01.02] {Confirmed real-deque cabal installation under 7.4 on RHEL}
------------------------------------------------------------------------

  carray seems to have been updated.

[2012.01.02] {Reviving tests/}
------------------------------

I disabled the runParAsync test for now.  But ever after that I'm
seeing the following:

  * All tests pass when under -N1
  * at -N>=2: 'case_test_diamond' can fail with indefinitely blocked threads
  * at -N>=2: can fail by simply diverging

That was under under GHC 7.2.1.  But I then observed the same thing
under GHC 7.4.

It's difficult to tell for certain where it is hanging.  For example,
it appeared to hang just now after completing "oneIVar" which would be
in the test "forkNFill".  But on the other hand it could be that the
output is not being flushed by test-framework (though I sure hope it
is) such that my text console is stale.

Also, if the output IS up to date that means it is diverging in
SEVERAL of the tests: forkNFill, test diamond, async test1...


[2012.01.27] {Bug somewhere, divergance in ContFree / quicksort}
----------------------------------------------------------------

If this is a ContFree bug we don't care, but it's worth looking into
incase it's a bug in something else (like GHC):

    --------------------------------------------------------------------------------
      Running Config 32 of 47: quicksort/parquicksort_monad (args "") scheduler ContFree  threads 1
    --------------------------------------------------------------------------------

    (In directory /var/lib/jenkins/jobs/monad-par_github_master/workspace/JENKINS_GHC/ghc-7.4.0.20111219/examples)
    Next run who, reporting users other than the current user.  This may help with detectivework.
    Who_Output: rrnewton rrnewton rrnewton rrnewton
    Executing ./ntimes_minmedmax 1 ./quicksort/parquicksort_monad_ContFree_threaded.exe  +RTS -N1 -RTS
     # ERROR, benchmark.hs: test command "./ntimes_minmedmax 1 ./quicksort/parquicksort_monad_ContFree_threaded.exe  +RTS -N1 -RTS" failed with code ExitFailure 15
     #      Return code 143 Params:  -rtsopts, RTS  
    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
     >>> MIN/MEDIAN/MAX TIMES TIMEOUT TIMEOUT TIMEOUT




[2012.03.06] {}
------------------------------------------------------------

Seeing this on tank.  Funny thing is that its spewing out of my stdout
on a completely unrelated terminal.  Maybe it goes to all tty's?

    -bash: fork: retry: Resource temporarily unavailable
    -bash: fork: retry: Resource temporarily unavailable
    -bash: fork: Resource temporarily unavailable
    -bash: fork: retry: Resource temporarily unavailable

Wow, here I was SERIOUSLY bitten by the fact that forkIO threads do
not propagate exceptions.  createProcess was throwing a process
repeatedly but it just kept going.

Also, I need to try to kill the ssh processes!

Is readProcess is problematic?  It SHOULD be waiting until the process
exits (hence the exit code).

AH!  Maybe the problem is my idle polling... THAT still uses HSH's "run".



[2012.05.02] {Overhead from Accelerate/CUDA invocation}
------------------------------------------------------------

What I'm currently seeing is .75 - 1.5 seconds extra runtime as a
result of calling CUDA.run at all.  The time doesn't appear to
actually be IN the CUDA.run though.


[2012.05.14] {Jenkins freezing in SAME place on Windows and Linux}
------------------------------------------------------------------

A random subset of both my recent windows and linux builds froze while
compiling Control.Monad.Par:

    [5 of 5] Compiling Control.Monad.Par ( Control/Monad/Par.hs, dist/build/Control/Monad/Par.o )

Thate's just weird


[2012.08.27] {Errors with 'make validate' in examples}
------------------------------------------------------

I'm running into this:

    ERROR: echo thread: Got exception inside forked thread: bench_basalt.log: openFile: resource busy (file is locked)

It may be some spurious NFS problem.


[2012.08.31] {Debugging the notorious Issue21}
----------------------------------------------

I'm doing this in the context of Direct right now.  Hmm, I thought 
before that disabling Idling disabled the bug.  Seems not so presently.

If I disable idling, the system live locks and memory leaks when run
on issue21.hs with +RTS -N3.  Great.

But I could swear that previously it at least behaved properly when
NESTED_SCHEDS was disabled.  Yet now it exhibits the same live lock
without nesting...  One variable is that I recently turned ON PARPUTS.
But turning it off still live locks and still leaks memory while
IDLING is on...

Is the basic worker loop LEAKING memory?  Maybe we should do some heap profiling.

Turning idling back on makes it work much better.  (BUT this is in a
config with PARPUTS off, FORKPARENT off, and NESTED off.)  Running
issue21 100 times... worked fine.

Turning FORKPARENT back on... the program is largely getting
serialized, I'm not seeing much more than 100% CPU.  But... then again
this is a serial test, isn't it?  The (^) operator presumably does a
serial sequence of multiplications, so the parallelism inside the
multiply operator doesn't really help (no balance).


    FORKPARENT   NESTED   IDLING  PARPUTS
    1            0        1       1         Works

    1            1        1       1         BlockIndefinitely
    1            1        1       0         BlockIndefinitely
    0            1        1       0         BlockIndefinitely

    1            1        0       0         LiveLock and Leak
    0            0        0       0         LiveLock and Leak   # NOW INVALID...
    1            1        0       1         LiveLock and Leak


The heap residency profile in a livelock run looks like this:

    MAIN    912
    (164)GHC.IO.Encoding.CAF        768
    (206)GHC.Integer.Logarithm...   288
    (186)GHC.Event.Thread.CAF       296
    (172)GHC.IO.Encoding.Iconv.CAF  528
    (243)uniform2.x/uniform2/u...   176
    (241)nextIndex/uniform2.j/...   224
    (216)initialize/create/*/f...   100440
    (214)*/fib.(...)/fib/main/...   21424
    (238)nextIndex/uniform2/un...   144
    (231)uniformRange/uniformR...   3456
    (236)uniform/uniformRange....   1640
    (237)uniform2/uniform/unif...   7464
    (229)*/fib.(...)/fib/main       60365872
    END_SAMPLE 8.15

So the explosion is in fib/main itself.

Ok, let's see, I think with idling off there was an obvious leak in
the counter in the "go" loop.  what if we tweak idling so that it does
evaluate tha counter?  For now still leaving profiling and "-xc -h"
ON.

    FORKPARENT   NESTED   IDLING  PARPUTS  PROFILING
    1            1        0       1        1           LiveLock, NO LEAK

And then if it evaluate's the counter AND yields:

    1            1        0       1        1           LiveLock, NO LEAK

Without profiling:

    1            1        0       1        0           LiveLock, NO LEAK

Ok, so that fixed the leak at least.  Note that now -N3 uses 200% CPU.  What does that say?
Now that we fixed that obvious bug in IDLING, let's turn NESTED back off:

    1            0        0       1        0           Worked SLOWLY.

And NOW it used 300% CPU as well.  It took about 80 seconds to run
issue21.hs though!  (Which does 10M iterations by default and takes
about a second at its best.  In fact the above config takes about a
second with +RTS -N1!!)  80X parallel slowdown.  

But why does it do so well with -N1?  Direct very well COULD as an
optimization NOT fork worker threads in -N1.  But it DOESN'T do that
right now it looks like.

Ok, one more change.  This shouln't matter.  I'm just refactoring how
the NESTED_SCHEDS ifdef is handled, pushing the ifdef to the top of
the file and changing only the helper defs (amINested, registerWorker,
unregisterWorker).  Expecting the same behavior and getting it for
this config.  Now I'll check in.

    1            0        0       1        0           Worked SLOWLY.

----------------

Ok, checked in.

    1            1        0       1        0           Works, 200% cpu,  -N2
    1            1        0       1        0           LiveLock, no leak -N3 

Wait a sec... this idling off mode is still fishy.  It CANNOT stay in
the steal loop.  It needs to return to rescheduleR so as to check
killflag.  If I fix that I get a 200% cpu livelock on -N3.  Is there
some OTHER way that threads are failing to see the killflag?

Hmm... rerunning with apparently unrelated changes makes that a 300%
cpu livelock.  Ah it's nondeterministic.  The 200% behavior happens
comparatively less often.  

Turning on busyTakeMVar seems to ELIMINATE the 200% behavior.  But
this is odd.  Yes, the main thread should be waiting, fine.  But there
should still be THREE worker threads, not TWO.

The 200% must be from hitting some kind of blackhole to disable the worker?


[2012.11.26] {More debugging for Issue 21}
------------------------------------------

Alright, my new nested support in Direct.hs is still failing on
issue21, though the "shallow nesting" hack seems to work.

Coming back to the code now after an absence I am especially
suspicious of this business with sessionFinished -- using a single
boolean variable flag, and then allocating a fresh one for nested
sessions?  I don't trust the protocol aruond that.  Might we need
something like a mutable dictionary allocated at the outset instead?

If I do an "issue21.exe +RTS -N3" exection, then right now I see more
instances of the "existing worker" (start of nested session) message,
than of the "RETURN from nested" message (30 vs. 27).

I don't *think* that's just a matter of continuations not getting called.


 HMM... it is indeed dropping a continuation.  We get these two
messages and not the one in the middle:

     [2 ThreadId 6] Starting Par computation on main thread.
     *** Out of entire runContT user computation on main thread.

That means that work died in the middle of userComp'.


What about idling?  Seems obvious... but turning nesting on and idling
OFF seems to make the problem go away.  Are the extra continuations
dying because nested sessions are started on worker threads that then
go idle?

NO!  Scratch that... it took dozens of iterations but it DID hit the
bug even with idling off.  UGH.  The triad of
existing/Continuation/RETURN messages actually MATCH in this run (30
of each).  So that's not a reliable diagnostic either.

But it DOES drop the top-level continuation midway through userComp'
(and therefore the call to trivialCont).

Ok, well next I'm going to log into several machines with cssh and
stress test the non-nested version.

  * Confirmed: the problem crops up w/ nested even with DEBUG off.
  * Non-nested no-idling: many runs with and without debug... no
    observation of the bug yet (~80 runs x 16 machines).


[2012.11.26] {Totally new kind of problem for Direct.hs}
--------------------------------------------------------

This a new leg of the same debugging saga.  Ok, so if I make the
runPar originator thread explictly wait on all workers... that breaks
things right off the bat even WITHOUT nesting enabled (<<loop>>,
indefinitely blocked on MVar exceptions).  Why!?


[2012.11.27] {Continued debugging}
----------------------------------

My last hack yesterday was to make rescheduleR actually EXECUTE its
continuation when its ready to exit.  (And I turned off the business
with the originator waiting for all workers.)

I also went in an hacked "forkOn" to "asyncOn" but I haven't done
anything else with that yet.

Ok, what's up with this.  After that hack the busyTakeMVar hack seems
to be causing infinite loops on issue21 with:

    debug:on, nested:off, forkparent:off, idling:off

But why should that be!?  It includes a yield?  Is it simply that all
the extra wasted cycles bring it to a crawl and it seems like its
diverged (when without the busy wait with 100K input it takes 270ms)?

But to emphasize that it DOES work without the busyTakeMVar, I stress
tested it.  All these work:

 * 16 machines * 100 reps with debug
 * 16 machines * 50 reps with debug piped to /dev/null
 * 16 machines * 100 reps without debug

Then if we ALSO turn on the new "wait for workers to complete"
thing...

Ok, when I do that I immediately get "thread blocked indefinitely".

If I wait on the Asyncs instead of the MVars... well that's just
worse, then I don't get the exception and it just spins in reschedule.
HERE'S THE WEIRD PART.  The killflag IS set and the DROP-out's do
happen.  Yet it still spins.

It loosk like some nested invocations are reaching the killflag and
others are not, leaving their workers spinning.  In fact, exactly ONE
"set killflag" message gets through....  And I already moved the set
killflag UP into userComp'.

It looks like we're losing real haskell continuations all over the
place.  We're not getting this message:

    Exited scheduling loop.  FINISHED

Could this somehow be an effect of kill the originator threads waiting
on MVars inside an unsafePerformIO?  Also (OOPS) runPar was not set to
NOINLINE...  

    (NOINLINE by itself didn't fix the problem)

Could reschedule be a NAUGHTY loop with no allocation?  Steal
currently has a yield but not rescheduleR...

     (Adding a yield didn't fix things either...)
  
WEIRD COMBINATION
-----------------

I just switched a couple knobs back (forkOn, use MVars to signal
currently has a yield but not rescheduleR.worker completion).

Then I made the wait-for-workers feature *gentle*, it just tries to
take those MVars to check if the workers are done.  This triggers a
behavior that hasn't happened till now:

  * issue21.exe: Error cont: this closure shouldn't be used
  * issue21.exe: DEBUGME -- Why is this not happening??

We weren't using the withAsync feature that would have killed the
child threads before these happened.... so why weren't we reaching
these continuations before but we are now?

Nevertheless, it IS forkOn vs. async that was the trigger, even
turning the gentler waiting strategy off has nothing to do with it.
Was I somehow routing the exceptions to a different thread and
actually losing them under the async method (precisely NOT what it's
for)?

Hmm... unlike async, my own forkWithExceptions seems to propagate them
properly.  That results in a tidier, earlier death for the whole
process.

Ok, in that configuration, if I remove the two intentional errors
(well, full disclosure, originally intentional, but semi-forgotten in
all the other mess)... things seem to work, and run much faster.
Direct.hs WITHOUT nesting can knock out issue21 on 10M in 1.3 seconds.
Barely slower than sparks.

EGAD!  Waiting for the workers to complete introduces a MASSIVE
slowdown.  It goes from 1.3 seconds to 5 seconds.


REACTIVATING Nested -- Now the problem is <<loop>>
--------------------------------------------------

I don't see it when debugging is on...  Is it possible that GHC is
having a FALSE POSITIVE on its loop detection inside the reschedule
loop?  No... it couldn't possibly apply to IO loops, could it?
It happens inside the workers though:

    Exception inside child thread "(worker of originator ThreadId 3)": <<loop>>
    Exception inside child thread "(worker of originator ThreadId 3)": <<loop>>
    issue21.exe: thread blocked indefinitely in an MVar operation
    Exception inside child thread "(worker of originator ThreadId 3)": <<loop>>
    Exception inside child thread "(worker of originator ThreadId 3)": <<loop>>

Oh, wait... on some runs it looks like I'm seeing this instead:

    Beginning benchmark on: ThreadId 3, numCapabilities 4
    Exception inside child thread "(worker 3 of originator ThreadId 3)": this should never be touched
    Exception inside child thread "(worker 1 of originator ThreadId 3)": this should never be touched
    Exception inside child thread "(worker 2 of originator ThreadId 3)": this should never be touched
    issue21.exe: this should never be touched

That would imply that the continuation for the nested session's
runCont is never called...  Still, this is tough because it now fails
a smaller percentage of the time.

This has all been without optimization... do things change if I turn
on -O2?  Nope, same behavior; I see both kinds of failures.

Ah, ok I CAN produce the "this should never be touched" error from
DEBUG mode.  It is just rare.   

Here's a relevant snippet.  These are most definitely out of order,
RETURN before "Continuation":

    [2 ThreadId 6] RETURN from nested (sessFin False, kflag False) runContT (1007) active set fromList [1000,1004,1007]
    [1]  | stole work (unit 64) from cpu 2
    [2 ThreadId 5] Continuation for nested session called, finishing it up (1007)...
    Exception inside child thread "(worker 2 of originator ThreadId 3)": this should never be touched (sid 1007, ThreadId 6)
    Exception inside child thread "(worker 0 of originator ThreadId 3)": this should never be touched (sid 1007, ThreadId 6)
     [1 ThreadId 5]  - Reschedule... kill False sessfin False
    Exception inside child thread "(worker 3 of originator ThreadId 3)": this should never be touched (sid 1007, ThreadId 6)


[2012.11.28] {I hope this is the last debugging round!}
-------------------------------------------------------

Ok, the extra "bounce" policy for going back into reschedule to ensure
LIFO exiting of nested sesisons...  that seems to have fixed at least
the premature RETURN bug.  I *thought* I still saw a <<loop>>
bug... but now I can't reproduce it over 1600 runs so I may have just
been mixing things up and that was before the latest change.

 * 100 reps * 16 machines, nested + wait-for-workers -- passed [spuriously!]

Aha!! There it is... after doing that many reps I FINALLY hit the
<<loop>> bug on two worker machines.

Ok, I'm going to turn PARPUTS back on.  I had turned "everything off".
But Parputs off is really wrong, it could be introducing deadlock and
that may be exactly the problem we are seeing, ok, here we go, running
on all the 4-core MINE machines:

 * 100 reps * 17 machines, nested + wait-for-workers + parputs (-N4) -- failed on 2 machines

Darn, no such luck.  OOPS!!!! WAIT ... ugh, I hate uncertainty in my
procedure.  I was using a different executable to run the batch of
machines, than in my edit-compile-run cycle.  I think I may have rerun
without updating that executable.  Let's try again.

 * w/busyTakeMVar: 100 reps * 17 machines, nested + wait-for-workers + parputs (-N4) -- FAILED
 * +busyTakeMVar, -debug: 100 reps * 17 machines, ditto (-N4) -- FAIL much more often
 * +busyTakeMVar, -debug, -waitForWorkers: 100 reps * 17 machines, ditto (-N4) -- 
    FAIL quickly with "not getting anywhere", OR with "<<loop>>"

So it seems like sometimes it was blocking on the waitForWorkers
takeMVars (why? dead worker, worker stuck in loop?).  But it still
fails blocking on the main result.

On my laptop I don't get the loop detection, but I do get a divergent
loop in reschedule.  Same as earlier, the continuation for finishing
some sessions seems to be lost.

!! NOTE: I've been using GHC 7.4.2 on linux, but 7.6.1 on my laptop:

     [4 ThreadId 8]  - Reschedule... sessions [(1006,False),(1000,False)], pool empty True
     [4 ThreadId 8]  - Reschedule... sessions [(1006,False),(1000,False)], pool empty True
     [4 ThreadId 8]  - Reschedule... sessions [(1006,False),(1000,False)], pool empty True
     [4 ThreadId 8]  - Reschedule... sessions [(1006,False),(1000,False)], pool empty True
     [4 ThreadId 8]  - Reschedule... sessions [(1006,False),(1000,False)], pool empty True
     [4 ThreadId 8]  - Reschedule... sessions [(1006,False),(1000,False)], pool empty True
     [4 ThreadId 8]  - Reschedule... sessions [(1006,False),(1000,False)], pool empty True
     [4 ThreadId 8]  - Reschedule... sessions [(1006,False),(1000,False)], pool empty True
     [4 ThreadId 8]  - Reschedule... sessions [(1006,False),(1000,False)], pool empty True
     [4 ThreadId 8]  - Reschedule... sessions [(1006,False),(1000,False)], pool empty True
     [4 ThreadId 8]  - Reschedule... sessions [(1006,False),(1000,False)], pool empty True
     [4 ThreadId 8]  - Reschedule... sessions [(1006,False),(1000,False)], pool empty True
     [4 ThreadId 8]  - Reschedule... sessions [(1006,False),(1000,False)], pool empty True
     [4 ThreadId 8]  - Reschedule... sessions [(1006,False),(1000,False)], pool empty True

Ack, with waitForWorkers off + busyTakeMVar, I seem to have a really
hard time getting my error on the linux machines...  I'm trying -N5 to
mix it up some.  In the following PARPUTS is implicitly ON from now on.

Also switching to GHC 7.6.1 for now:

 * 7.6.1, +busyTakeMVar, +debug, -waitForWorkers: 100 reps * 17 machines (-N5) -- failed with lots of loops


Ok, let's hop back and make sure our non-nested version is still working with recent changes:

 * 7.6.1, -nested, -busyTakeMVar, -debug, -waitForWorkers, -idling, -forkparent: 100 reps * 17 machines (-N4) -- 

Or, more concisely, if we only list the activated flags (except
PARPUTS, which is now implicit):

 * 7.6.1: 100 reps * 16 machines (-N5) -- 
 * 7.4.2: 100 reps * 16 machines (-N5) --  




[2012.10.06] {Strange GHC bug?}
-------------------------------

I got into a bad state with ghc-7.4.1 where I would try to import my
module, and it would appear to succeed, but nothing would be bound.

But then it looks like I could import any old (capitalized) nonsense
string and it would also appear to succeed!!  

    Prelude> import Control.Monad.Par.aoetu
    <interactive>:1:8: parse error on input `Control.Monad.Par.aoetu'
    Prelude> import Control.Monad.Par.I
    Prelude> import Control.Monad.Par.IO
    Prelude> import Control.Monad.Par.A
    Prelude> import Control.Monad.Par.BB
    Prelude> import Control.Monad.Par.BBBBB

Strange.  Is it because I just installed a new GHC alongside and am
now using this old one by appending the extension?

Note, this works even for top-level nonsense modules:

    Prelude> import ENOTHU

This problem is unique to my 7.4.1 install.  It doesn't happen under
7.0 or 7.6.

-------------

Also, don't forget to list "OtherModules" to avoid this:

    Failed to load interface for `Control.Monad.Par.Scheds.DirectInternal'
    There are files missing in the `monad-par-0.3' package,
    try running 'ghc-pkg check'.
    Use -v to see a list of the files searched for.



[2013.05] {Simon Marlow's Notes about possible nested implementation}
---------------------------------------------------------------------

Problem scenarios:

runPar $ do
  let x = runPar $ ...
  fork (.. x ..)
  .. x ..

In "let x = runPar $ ...", while the runPar is executing, x is a
blackhole.  We must ensure that there is a thread making progress on x
at all times, otherwise we may get a deadlock, because other siblings
of this computation may depend on x.

So, the thread that evaluates runPar gains a new constraint: it can
only work on stuff below this runPar.  The other workers can continue
to work on any work items they like.

How do we know what is "below this runPar"?  A linear depth measure
won't do, because there might be a tree of runPars.  The easy solution
is just to assign each runPar a UID, and say that the leader can only
work on items from this UID.

In general, we have a forest of runPars:

           A       F
          / \     / \
         B   C   G   H
        / \
       D   E

Where A gave rise to B and C, B gave rise to D and E.  F was a
completely separate runPar called by a different thread.

The worker that starts on A can work on anything from [A-E], but not
[F-H].  All of [A-E] are required by A.  Similarly the leader for B
can only work on [B,D,E], and the leader for F can only work on
[F,G,H].

Should the leader for A only work on A itself?  That would be bad,
because A might run out of work while all the action is in its
children, and we want to be able to use all our cores there.


We want to have a fixed number of workers at all times.  So we have a
fixed number of Scheds:

data Sched = Sched
    { no       :: Int,
      thread   :: ThreadId,
      workpool :: IORef (Map UID [Trace]),
      idle     :: IORef [MVar Bool],
      scheds   :: [Sched] -- Global list of all per-thread workers.
    }

We need a global containing the current UID.

When stealing, if the current worker is a leader, it steals only from
its UID.  Otherwise, it can steal from any UID.

When a thread enters runPar, either:

  - it is already a worker, in which case we want to create a new UID
    for this runPar, and dive directly into the scheduler, as a leader
    on this UID.

  - it is not a worker. What do we do here?  Can we hand off to one of
    the existing workers?  But then the target worker must become a
    leader.  What if it was already a leader? Then we can't hand off
    to it, because it has an important job to do.

    Plan:
      - grab a new UID for this runPar
      - make an MVar to hold the result
      - make a work item containing the whole runPar, that puts its result
        in the MVar when done
      - put the work item on one of the work queues.
      - wait on the MVar.

    A leader won't take it up, because it is a different UID.  As soon
    as there are free resources it will be executed, and will
    eventually wake up the original thread that called runPar.



[2013.09.16] {Finishing overhaul of benchmarks for HSBencher}
--------------------------------------------------------------------------------


      Compiling Config 23 of 40:  (args "") confID 
	   "src_matmult_disabledocumentation_disablelibraryprofiling_disableexecutableprofiling_ftrace"

     [hsbencher] Found 1 methods that can handle src/matmult/: ["cabal"]
     [cabalMethod]  Switched to src/matmult/, clearing binary target dir... 
     * Executing command: rm -rf ./bin/*
     * Command completed with 0 lines of output.
     [cabalMethod] Running cabal command: cabal install --bindir=./bin/ ./ --program-suffix=_src_matmult_disabledocumentation_disablelibraryprofiling_disableexecutableprofiling_ftrace --disable-documentation --disable-library-profiling --disable-executable-profiling -ftrace
     * Executing command: cabal install --bindir=./bin/ ./ --program-suffix=_src_matmult_disabledocumentation_disablelibraryprofiling_disableexecutableprofiling_ftrace --disable-documentation --disable-library-profiling --disable-executable-profiling -ftrace
     [cabal] Resolving dependencies...
     [cabal] Configuring matmult-0.3.1...
     [cabal] Building matmult-0.3.1...
     [cabal] Preprocessing executable 'monad-par-test-matmult' for matmult-0.3.1...
     [cabal] [stderr] cabal: can't find source for matmult in .
     [cabal] Failed to install matmult-0.3.1
     [cabal] [stderr] cabal: Error: some packages failed to install:
     [cabal] [stderr] matmult-0.3.1 failed during the building phase. The exception was:
     [cabal] [stderr] ExitFailure 1
     [cabal] 
     [cabal] [stderr] 
     * Command completed with 11 lines of output.
    run_benchmark: expected this command to succeed! But it exited with code 1:
      cabal install --bindir=./bin/ ./ --program-suffix=_src_matmult_disabledocumentation_disablelibraryprofiling_disableexecutableprofiling_ftrace --disable-documentation --disable-library-profiling --disable-executable-profiling -ftrace


[2013.11.21] {Examining fusion table benchmark data}
------------------------------------------------------------

Looking through this data yields some interesting discrepancies.
For example, just narrowing to th eruns on 2013-05-28:

  * coins - all 3 (direct,trace,sparks) track closely and are fine
  * nbody, blackscholes, mandel, sumeuler - ditto
  * mergesort - missing some data, what's there looks fine.
  * MatMult - Direct/Trace are fine, but Sparks falls apart.

Oh, was matmult the weird one that was written with unsafePerformIO,
or no, that was cholesky?

Much later on, 2013-11-14, 

  * runID d007_1384410470 looks suspicious for sparks/coins.
 
[2013.11.12] {Found a nondeterministic failure in sorting after switching to LVish}
-----------------------------------------------------------------------------------

Namely this:

    monad-par-test-mergesort: thread blocked indefinitely in an MVar operation

Running 2^23 through on my laptop gives this behavior with lvish -N4:

    SELFTIMED 1.799088
      12,471,040,200 bytes allocated in the heap
	  18,856,984 bytes copied during GC
	 331,762,496 bytes maximum residency (21 sample(s))
	 113,264,520 bytes maximum slop       
       
Maximum residency goes up with parallelism substantially.
Here's the same thing running with the trace scheduler (-N4):

    SELFTIMED 0.846114
       5,539,016,888 bytes allocated in the heap
	  14,921,576 bytes copied during GC
	 100,744,416 bytes maximum residency (30 sample(s))
	  42,264,096 bytes maximum slop
		 258 MB total memory in use (27 MB lost due to fragmentation)

Egad!  And -N1 for trace has much similar maximum residency:

    SELFTIMED 2.111836
       5,538,817,120 bytes allocated in the heap
	  16,573,512 bytes copied during GC
	  96,322,080 bytes maximum residency (29 sample(s))
	  40,677,680 bytes maximum slop


[2013.11.13] {Testing -A20 systematically}
----------------------------------

This is build #62 where we're turning on -A20M:

  http://tester-lin.soic.indiana.edu:8080/view/Benchmark/job/Benchmark_monad-par_Delta/62/

I added that option, which now shows up in the RUNTIME_FLAGS column.
I'm comparing these run IDs:

    hive_1384348805 (-A20M)
    hive_1384289972 (default 512K nursery)

The -A20M does this on trace/parfib(33):

   http://goo.gl/6zIQ7w   

It's pretty bad.  Both high variance and not smoothly speeding up
(jags).  But that's just parfib on a four socket machine.

And then here's the default one.. It's also pretty bad.  Flat but
zaggy, hovering around the 3 second line:

   http://goo.gl/M8ho2t

But the MINIMUM doesn't go as low as -A20M, and it's USUALLY worse
than the -A20M version.





TEMP / SCRAP:
--------------------------------------------------------------------------------








