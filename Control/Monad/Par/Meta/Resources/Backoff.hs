{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

-- | This module implements exponential backoff so as to prevent
--   spamming of steal actions.  This is always a good idea, and
--   especially so in the distributed case where steal attempts send
--   actual messages.

--   Normally backoff functionality is baked into the scheduler loop.
--   One nice aspect of the Meta scheduler design is that backoff can
--   become "just another resource".  Most schedulers (compositions)
--   should include this at tho bottom of their stack.

module Control.Monad.Par.Meta.Resources.Backoff
  ( initAction, mkStealAction )
where 

import Data.IORef (readIORef)
import Data.Word (Word64)
import Control.Monad.Par.Meta hiding (dbg, stealAction)
import Control.Monad.Par.Meta.Resources.Debugging (dbgTaggedMsg)
import Control.Concurrent     (myThreadId, threadDelay)
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Char8 as BS


initAction :: InitAction
initAction = IA (\ sa _ -> return () )

-- | To construct a StealAction we need to know the minimum and
-- maximum amount of time (nanoseconds) to sleep.  The exponential
-- backoff policy is always the same: it starts at 1ns and doubles.
-- 
-- The thing that changes over time is whether sleeping actually
-- *occurs*.  For example, `mkStealAction 1000 100000` will not sleep
-- for the first ten invocations (until 1024), and then will sleep an
-- amount that doubles each time until it surpasses the maximum, at
-- which point each sleep will be for the maximum: 100ms.
mkStealAction :: Word64 -> Word64 -> StealAction
mkStealAction shortest longest = SA sa 
  where 
    sa Sched{consecutiveFailures} _ = do
      failCount <- readIORef consecutiveFailures
      let nanos  = if failCount >= 64
                   then longest 
                   else 2 ^ failCount
      if nanos > shortest then do 
         let capped = min longest nanos
         dbgTaggedMsg 3 $ "Backoff: Sleeping, nanoseconds = " `BS.append` BS.pack (show capped)
         threadDelay (fromIntegral capped)
       else do 
         dbgTaggedMsg 4 $ "Backoff: NOT yet sleeping, nanoseconds = " `BS.append` BS.pack (show nanos)
         -- QUESTION - do we want to yield here?  Probably not.
         -- Maybe for some ranges of nanosecond intervals we could yield instead...
         -- yield
      return Nothing

