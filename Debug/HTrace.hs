{-# LANGUAGE BangPatterns #-}
-- | Like Debug.Trace.trace, but with indentation corresponding to the
-- level of nesting in the evaluation tree of expressions under htrace.
-- WARNING: Currently only works in single-threaded programs.
-- 
-- Example:
-- 
-- 
-- > xs = map (\x -> htrace (show x) x) [1..10]
-- >
-- > s = foldl (\a b -> htrace "+" (a+b)) 0 xs
-- > s2 = foldl' (\a b -> htrace "+" (a+b)) 0 xs
-- >
-- > b = htrace "b" 2
-- > c = htrace "c" 3
-- > a = htrace "a" $ b + c
-- > x = htrace "x" $ b + c
-- 
--
-- >>> a
-- a
--     b
--     c
-- 5
-- 
-- >>> x
-- x
-- 5
-- 
-- >>> s
-- +
--     +
--         +
--             +
--                 +
--                     +
--                         +
--                             +
--                                 +
--                                     +
--                                         1
--                                     2
--                                 3
--                             4
--                         5
--                     6
--                 7
--             8
--         9
--     10
-- 55
-- 
-- (reload)
-- 
-- >>> s2
-- +
--     1
-- +
--     2
-- +
--     3
-- +
--     4
-- +
--     5
-- +
--     6
-- +
--     7
-- +
--     8
-- +
--     9
-- +
--     10
-- 55
-- 
module Debug.HTrace (htrace, htraceId, htraceShow, htraceShowId, htraceM, htraceShowM, htraceIndent) where

import Data.IORef
import System.IO.Unsafe

level :: IORef Int
level = unsafePerformIO $ newIORef 0

-- | Trace "str" on a separate line, and increase indentation of subsequent
--   traces while x is being evaluated. 
htrace :: String -> a -> a
htrace str x = unsafePerformIO $ do
  lvl <- readIORef level
  writeIORef level (lvl+1)
  rnf str `seq` putStrLn (replicate (2*lvl) ' ' ++ str)
  let !vx = x
  writeIORef level lvl
  return vx

-- | Like 'htrace' but returns the message instead of a third value.
--   (This doesn't really make sense if evaluation of the message itself invokes
--   further htraces: in this case the shown string will come /after/ the indented
--   sub-traces.)
htraceId :: String -> String
htraceId a = htrace a a

-- | Like 'trace', but uses 'show' on the argument to convert it to a 'String'.
htraceShow :: Show a => a -> b -> b
htraceShow i = htrace $ show i

-- | Like 'traceShow', but returns the shown value instead of a third value.
--   As with 'traceShowId`, this should be at the deepest level of recursion
--   you need tracing in.
htraceShowId :: Show a => a -> a
htraceShowId a = htrace (show a) a

-- | Have any `htrace` messages that are issued during evalution of the
--   argument indented (by two spaces).
htraceIndent :: a -> a
htraceIndent x = unsafePerformIO $ do
  lvl <- readIORef level
  writeIORef level (lvl+1)
  let !vx = x
  writeIORef level lvl
  return vx

-- | Like 'trace' but returning unit in an arbitrary monad. Allows for convenient
--   use in do-notation.
htraceM :: Monad m => String -> m ()
htraceM str = htrace str $ return ()

-- | Like 'htraceM', but uses 'show' on the argument to convert it to a 'String'.
htraceShowM :: Monad m => String -> m ()
htraceShowM = htraceM . show


rnf :: String -> Int
rnf = sum . map fromEnum
