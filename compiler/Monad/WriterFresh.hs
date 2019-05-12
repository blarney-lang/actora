module Monad.WriterFresh where

import Control.Monad

data WriterFresh w a = WF { runWF :: String -> Int -> (Int, [w], a) }

instance Monad (WriterFresh w) where
  return a = WF $ \s i -> (i, [], a)
  m >>= f = WF $ \s i ->
    case runWF m s i of
      (i0, w0, a0) ->
        case runWF (f a0) s i0 of
          (i1, w1, a1) -> (i1, w0 ++ w1, a1)

instance Functor (WriterFresh w) where
  fmap = liftM

instance Applicative (WriterFresh w) where
  pure = return
  (<*>) = ap

write :: w -> WriterFresh w ()
write w = WF $ \s i -> (i, [w], ())

writeMany :: [w] -> WriterFresh w ()
writeMany ws = WF $ \s i -> (i, ws, ())

fresh :: WriterFresh w String
fresh = WF (\s i -> (i+1, [], s ++ show i))
