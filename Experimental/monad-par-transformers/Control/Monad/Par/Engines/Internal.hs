{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | Parallel Engines (integrated engines and futures)

-- TODO: The ideal would probably be for each child thread to have a state that the
-- parent can check upon sync and decide whether to cancel or continue that child
-- engine in the next round.

-- TODO: A memoized Par computation would be safe and perhaps useful here.

--------------------------------------------------------------------------------
module Control.Monad.Par.Engines.Internal
       (Fuel, ParEng,
        runEng, contEng, 
        spawnEng, getE, fuelLeft, tick
       )
       where

-- import Data.Word
-- import Data.IntMap as M
import qualified Control.Monad.Par as P

import qualified Control.Monad.Par.Class as PC
import Control.Monad.State.Strict

import Debug.Trace

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Co-routines built on IVars communitate by passing IVars back and forth.  For the
-- parallel engine abstraction, we communication is between parent and child
-- threads. Each time an engine checks in with its parent it gets both more fuel and
-- another IVar to use for the next round.

type Fuel = Int

-- | A `ParEng` computation has both parallelism and finite fuel.
--
-- type ParEng a = StateT (EngState) P.Par a
newtype ParEng a = ParEng { unEng :: StateT (EngState) P.Par a } 
  deriving (Monad)
           
data EngState =
  EngState
  { startFuel :: Fuel
  , curFuel   :: Fuel
  , parent    :: OutStanding 
    -- ^ The parent computation to which we yield control.
  , children  :: [OutStanding] 
    -- ^ A list of children that have been forked but may or may
    -- not have been synced.
  }

-- | An engine that is all synced up has no more outstanding threads to wait on, only
-- stalled threads waiting to be restarted, plus a placeholder for the final result.
data EngSynced a = EngSynced [P.IVar Restart] (P.IVar a)

-- | A message to restart a stalled thread contains new fuel and a fresh IVar to
-- communicate with the parent in the next round.  This is used in the "downcall"
-- from parent to child.
data Restart = Restart Fuel (P.IVar EngCheckin)

-- | Check in with the parent thread by sending one of these.  The "upcall".
data EngCheckin = Finished [OutStanding]
                  -- ^ Completed successfully with an answer.  But the answer is not
                  --   here.  Rather, the payload contains any children that have
                  --   been created by the engine and may not have been synced.
                | FuelExhausted (P.IVar Restart) [OutStanding]
                 -- ^ Need to do more work, blocking on this IVar waiting for more
                 -- fuel.  Still return any children.

-- | Outstanding threads, on the other hand, may or may not be finished with their
-- work; we find out when they checkin.
newtype OutStanding = OutStanding (P.IVar EngCheckin)

-- | An engine result may or may not be ready yet.  Either way it returns handles for
-- any downstream child-engines that were created.
--
-- Here we need an extra IVar if it IS ready, for reasons of internal system
-- architecture.
type EngResult a = Either (EngStalled a) (P.IVar a, [OutStanding])

-- | An engine whose main thread has run out of fuel, but still may have outstanding
-- child computations.  It contains an extra IVar for the result.
data EngStalled a =
  EngStalled
  { restarter :: !(P.IVar Restart)    
  , finalVal  :: !(P.IVar a)
  , outstanding :: [OutStanding]
  }
-- But, of course,
-- the engine may run out of fuel again, so *that* result is an "Either" too.
-- data EngStalled a = EngStalled !(P.IVar Restart) !(P.IVar (EngResult a))

-- | Blocking on a future in an enginized context must not prevent all
-- threads/child-engines from checking in when the engine is synced.
newtype EngFuture a = EngFuture (P.IVar (FutureResult a))
-- RRN: NOTE: I don't yet know how to handle arbitrary IVar programs, rather than
-- just futures.  If we didn't need "EngSynced" then perhaps we could do it.

-- | If a child computation blocks repeatedly we will end up with a CHAIN of IVars
-- (several EngStalled objects) that we must chase to find the final answer.  This
-- O(N) space usage is required due to the single-assignment nature of IVars.
data FutureResult a = FutureReady   !(P.IVar a)
                    | FutureStalled !(P.IVar (FutureResult a))

--------------------------------------------------------------------------------
-- Implementation
--------------------------------------------------------------------------------

instance PC.ParFuture EngFuture ParEng where
  spawn_ (x::ParEng a) = do
    curFuel <- fuelLeft
    let (q,r) = curFuel `quotRem` 2
        fuel  = q+r
    fut <- spawnEng fuel x
    tick fuel
    return fut
  get = getE 
--     undefined
  

-- | Running an engine either completes or returns a new engine with the remaining work.
--   The returned engine may represent not just one, but many threads suspended.
runEng :: Fuel -> ParEng a -> P.Par (EngResult a)
runEng fuel (ParEng eng) = do
  iv  <- P.new
  res <- P.new
  let wrapped = do z <- eng;
                   EngState{parent=OutStanding ivp2,children} <- get
                   lift$ P.put_ ivp2 (Finished children)
                   lift$ P.put_ res z;
                   return ()
  -- In this case the 'runEng' itself is a sort of parent to the engine:
  ((), _finState) <- runStateT wrapped
                              (EngState fuel fuel (OutStanding iv) [])
  trace (" ... Done with runStateT, next wait for child checkin..") $ return ()
  x <- P.get iv
  trace (" ... Got child checkin..") $ return ()  
  case x of
    Finished newchld       -> return (Right (res, newchld))
    FuelExhausted iv2 chld -> return (Left$ EngStalled iv2 res chld)

-- | Counterpart to `runEng`.  Continue running an engine once it is stalled.
contEng :: Fuel -> EngStalled a -> P.Par (EngResult a)
contEng newfuel (EngStalled mainchld res allchlds) = do
  nxt <- P.new

  -- actives <- forM allchlds $ \ (OutStanding iv) -> do
  --   chkn <- P.get iv
  --   case chkn of
  --     Finished outs          -> return outs
  --     FuelExhausted ths oths -> return (ths : oths)
  -- let allActive = concat actives

  let 
      loop [] acc = return acc
      loop (OutStanding iv : tl) acc = do
        chkn <- P.get iv
        case chkn of
          Finished outs          -> loop (outs ++ tl) acc
          FuelExhausted ths oths -> loop (oths ++ tl) (ths : acc)
  activeChlds <- loop allchlds []

  -- TODO: Restart ALL children here.  Fairness and Liveness.
  -- SIMPLEST POLICY, everyone gets at least ONE tick:
  let len = length activeChlds
      (q,r) = newfuel `quotRem` (len + 1)
      loop2 _  [] = return ()
      loop2 !ix (hd : tl) = do
        let fuel = max 1 (q + if ix < r then 1 else 0)
        P.put_ hd (Restart fuel nxt)
        loop2 (ix+1) tl
  loop2 0 activeChlds

  
  -- TODO: Parallel wakeup?
  
  P.put_ mainchld (Restart q nxt)
  -- allchlds
  
  x <- P.get nxt
  case x of
   Finished newchld      -> return (Right (res, newchld))
   FuelExhausted iv chld -> return (Left$ EngStalled iv res chld)
          

-- | How much fuel is remaining for use by the current thread.
fuelLeft :: ParEng Fuel
fuelLeft = ParEng$ fmap curFuel get

-- | Fork a parallel child-engine.
-- 
-- We don't explicitly handle the child computation running out of fuel here, rather,
-- we are implicitly blocked either when we run out of fuel or when we block on the
-- IVar-result of the child computation.
--
-- Spawning a child engine subtracts that amount of fuel from our own budget.  If the
-- current fuel level is less than the requested amount, all the remaining fuel is
-- used, but no exception is thrown.
spawnEng :: forall a . Fuel -> ParEng a -> ParEng (EngFuture a)
spawnEng fuel (ParEng eng) = ParEng $ do
  EngState{curFuel=ours, children} <- get
  let ours2  = max 0 (ours - fuel)
      theirs = ours - ours2

  -- TODO!  Tick here to stall out if we are exhausted of fuel!?
  ans    <- lift P.new
  rawans <- lift P.new  
  ret    <- lift P.new
--  newchildren <- lift P.new
  lift$ P.fork $ evalStateT (do
    val <- eng
    -- At this point the engine has completed successfully... but may have spawned children.
    -- lift$ P.put_ ans (Right val) -- DANGER!  Multiple PUTS.
    lift$ P.put_ rawans val
    EngState{parent=OutStanding ivp, children=chld2} <- get
    -- TODO/FIXME: write out the CHILDREN to add to the parents outstanding list:
    -- lift$ P.put_ newchildren children
    lift$ P.put_ ivp (Finished chld2)
    return ()
    ) (EngState theirs theirs (OutStanding ret) [])

  -- Register the new child engine in our state:
  modify $ \ st -> st{curFuel=ours2, children= OutStanding ret : children}


  -- This thread checks on the 'ret' ivar and translates its result to a result for
  -- the 'ans' IVar (if needed).  The two IVars can't be merged mainly because of
  -- typing issues.  (Without sealing, we can't refer to the type var 'a' in the
  -- engine state.)
  lift$ P.fork $ do 
        -- We wait for the child to finish or yield.  Note we are NOT the only thread
        -- that will be getting this IVar.
        childWaiting <- P.get ret
        case childWaiting of
          FuelExhausted _ _ ->  do 
            -- Here we create a placeholder for the final result.  However we DO NOT
            -- concern ourselves with restarting the child.  That's not our job.
            iv <- P.new
            P.put_ ans (FutureStalled iv)

          -- FIXME: What if the child stalls MULTIPLE times?
          Finished _ -> P.put_ ans (FutureReady rawans)
        
  return (EngFuture ans)

-- | Use N units of fuel.  This may cause us to run out and terminate.  The engine
-- will not, however, go into \"debt\".  If the argument to `tick` is greater than
-- the remaining fuel, the fuel goes to zero.
tick :: Fuel -> ParEng ()
tick decr = ParEng $ do
  st0 <- get
  let newfl = max 0 (curFuel st0 - decr)
  put $ st0{curFuel=newfl}
  when (newfl <= 0) $ 
     unEng yield

-- | Report to the parent computation that we could not continue.  
yield :: ParEng ()
yield = ParEng$ do
  EngState {parent, children} <- get  
  -- Here we bounce control back to the whomever is running the engine or waiting
  -- on the result.  We only resume when we are given more fuel.  
  iv <- lift$ P.new
  let OutStanding ivp = parent
  lift$ P.put_ ivp (FuelExhausted iv children)
  Restart fl iv2 <- lift$ P.get iv
  -- When we wake up we inject the new fuel, and CLEAR the children.
  modify $ \ strec -> strec{curFuel=fl, parent=OutStanding iv2, children=[]}


-- | Get the final result of a spawned engine.  Block until the child computation has
-- either completed or run out of fuel and stalled.
getE :: forall a . EngFuture a -> ParEng a
getE (EngFuture iv) = ParEng $ do
  -- If we use a raw P.get here, then we will block without reporting in to our
  -- parent.  
  res <- lift$ P.get iv
  case res of
    FutureReady ans -> lift$ P.get ans
    -- What we get back in this case is the rest of the work for the *child*
    -- computation.  We COULD spend our fuel trying to complete it, before ourselves
    -- yielding.  But we might be racing with someone else to restart it, so for now
    -- we just yield ourselves, discarding what remains of our fuel.
    FutureStalled iv2 -> do unEng yield; unEng$ getE (EngFuture iv2)

--------------------------------------------------------------------------------
-- Tests:

