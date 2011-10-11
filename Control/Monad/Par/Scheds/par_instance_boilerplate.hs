
-- Boilerplate that crops up repeatedly when defining ParIVar instances:

spawn p  = do r <- new;  fork (p >>= put r);   return r
spawn_ p = do r <- new;  fork (p >>= put_ r);  return r
