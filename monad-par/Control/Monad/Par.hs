
{-|

  The @monad-par@ package provides a family of @Par@ monads, for
  speeding up pure computations using parallel processors.  (for a similar
  programming model for use with @IO@, see "Control.Monad.Par.IO".)

  The result of a given @Par@ computation is always the same - i.e. it
  is deterministic, but the computation may be performed more quickly
  if there are processors available to share the work.

  For example, the following program fragment computes the values of
  @(f x)@ and @(g x)@ in parallel, and returns a pair of their results:

  >  runPar $ do
  >      fx <- spawnP (f x)  -- start evaluating (f x)
  >      gx <- spawnP (g x)  -- start evaluating (g x)
  >      a  <- get fx        -- wait for fx
  >      b  <- get gx        -- wait for gx
  >      return (a,b)        -- return results

  @Par@ can be used for specifying pure parallel computations in
  which the order of the computation is not known beforehand.
  The programmer specifies how information flows from one
  part of the computation to another, but not the order in which
  computations will be evaluated at runtime.  Information flow is
  described using "variables" called @IVar@s, which support 'put' and
  'get' operations.  For example, suppose you have a problem that
  can be expressed as a network with four nodes, where @b@ and @c@
  require the value of @a@, and @d@ requires the value of @b@ and @c@:

  >                       a
  >                      / \               
  >                     b   c             
  >                      \ /  
  >                       d

  Then you could express this in the @Par@ monad like this:

  >   runPar $ do
  >       [a,b,c,d] <- sequence [new,new,new,new]
  >       fork $ do x <- get a; put b (x+1)
  >       fork $ do x <- get a; put c (x+2)
  >       fork $ do x <- get b; y <- get c; put d (x+y)
  >       fork $ do put a (3 :: Int)
  >       get d

  The result of the above computation is always 9.  The 'get' operation
  waits until its input is available; multiple 'put's to the same
  @IVar@ are not allowed, and result in a runtime error.  Values
  stored in @IVar@s are usually fully evaluated (although there are
  ways provided to pass lazy values if necessary).

  In the above example, @b@ and @c@ will be evaluated in parallel.
  In practice the work involved at each node is too small here to see
  the benefits of parallelism though: typically each node should
  involve much more work.  The granularity is completely under your
  control - too small and the overhead of the @Par@ monad will
  outweigh any parallelism benefits, whereas if the nodes are too
  large then there might not be enough parallelism to use all the
  available processors.

  Unlike @Control.Parallel@, in @Control.Monad.Par@ parallelism is
  not combined with laziness, so sharing and granularity are
  completely under the control of the programmer.  New units of
  parallel work are only created by @fork@ and a few other
  combinators.

  The default implementation is based on a work-stealing scheduler
  that divides the work as evenly as possible between the available
  processors at runtime.  Other schedulers are available that are
  based on different policies and have different performance
  characteristics.  To use one of these other schedulers, just import
  its module instead of "Control.Monad.Par":

  * "Control.Monad.Par.Scheds.Trace"

  * "Control.Monad.Par.Scheds.Sparks"

  For more information on the programming model, please see these sources:

      * The wiki\/tutorial (<http://www.haskell.org/haskellwiki/Par_Monad:_A_Parallelism_Tutorial>)

      * The original paper ( [S. Marlow,R. Newton,and S. P. Jones, "A Monad for Deterministic Parallelism,"in Proceedings of the Fourth ACM SIGPLAN Symposium on Haskell, Tokyo, Japan, ACM, 2011, pp. 71-82](http://simonmar.github.io/bib/papers/monad-par.pdf) )

      * Tutorial slides (<https://www.cse.unt.edu/~tarau/teaching/parpro/papers/CUFP.pdf>)

      * Other slides: (<http://www.cs.ox.ac.uk/ralf.hinze/WG2.8/28/slides/simon.pdf>)

 -}

module Control.Monad.Par 
 (
  -- * The Par Monad
  Par, 
  runPar, runParIO,

  fork,
  -- | Forks a computation to happen in parallel.  The forked
  -- computation may exchange values with other computations using
  -- @IVar@s.

  -- * Communication: IVars
  IVar,

  new, 
  -- creates a new @IVar@

  newFull, 
  -- creates a new @IVar@ that contains a value

  newFull_, 
  -- creates a new @IVar@ that contains a value (head-strict only)

  get, 
  -- read the value in an @IVar@.  'get' can only return when the
  -- value has been written by a prior or parallel @put@ to the same
  -- @IVar@.

  put, 
  -- put a value into an @IVar@.  Multiple 'put's to the same @IVar@
  -- are not allowed, and result in a runtime error.
  --
  -- 'put' fully evaluates its argument, which therefore must be an
  -- instance of 'NFData'.  The idea is that this forces the work to
  -- happen when we expect it, rather than being passed to the consumer
  -- of the @IVar@ and performed later, which often results in less
  -- parallelism than expected.
  --
  -- Sometimes partial strictness is more appropriate: see 'put_'.
  --

  put_,
  -- like 'put', but only head-strict rather than fully-strict.

  -- * Operations
  spawn,
  -- | Like 'fork', but returns an @IVar@ that can be used to query the
  -- result of the forked computataion.  Therefore @spawn@ provides /futures/ or /promises/.
  --
  -- >  spawn p = do
  -- >    r <- new
  -- >    fork (p >>= put r)
  -- >    return r
  --

  spawn_,
  -- | Like 'spawn', but the result is only head-strict, not fully-strict.

  spawnP,
  -- | Spawn a pure (rather than monadic) computation.  Fully-strict.
  -- 
  -- >  spawnP = spawn . return

  module Control.Monad.Par.Combinator,
  -- | This module also reexports the Combinator library for backwards
  --   compatibility with version 0.1.

  NFData()
  -- | /(0.3)/ Reexport 'NFData' for fully-strict operators.

 )
where 

import Control.Monad.Par.Class hiding ( spawn, spawn_, spawnP, put, put_
                                      , get, newFull, new, fork, newFull_ )
import Control.Monad.Par.Scheds.Trace
import Control.Monad.Par.Combinator
