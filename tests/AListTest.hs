
import Control.Monad.Par.AList

--------------------------------------------------------------------------------
-- Testing

-- For testing:
bintree 0 x = x
bintree n x = Append sub sub
 where sub = bintree (n-1) x

showDbg ANil         = "_"
showDbg (ASing x)    = show x
showDbg (Append l r) = "("++showDbg l++" | "++showDbg r++")"
showDbg (AList  l)   = show l

alist_tests :: Test
alist_tests = 
  TestList 
    [

      8   ~=? (length$ tail$ tail$ fromList [1..10])
    , 1   ~=? (length$ tail$tail$  cons 1$ cons 2$ cons 3 empty)

    , 253 ~=? (length$ tail$tail$tail$ bintree 8 $ singleton 'a')
    , 0   ~=? (length$ bintree 8 $ empty)

    , "((1 | 1) | (1 | 1))" ~=? (showDbg$            bintree 2 $ singleton 1)
    , "((_ | 1) | (1 | 1))" ~=? (showDbg$ tail$      bintree 2 $ singleton 1)
    , "(_ | (1 | 1))"       ~=? (showDbg$ tail$tail$ bintree 2 $ singleton 1)

    ]

t = runTestTT alist_tests


-- TODO: Quickcheck.
