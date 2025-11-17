module Sec where
import           Lattice

-- Sec monad definition
newtype Sec s a = MkSec a

instance Functor (Sec s) where
  fmap f (MkSec a) = MkSec (f a)

instance Applicative (Sec s) where
  pure = MkSec
  (MkSec f) <*> (MkSec a) = MkSec (f a)

instance Monad (Sec s) where
    return = pure
    (MkSec a) >>= k = k a

sec :: a -> Sec s a
sec = MkSec

open :: Sec s a -> s -> a
open (MkSec a) s = s `seq` a

up :: Less s s' => Sec s a -> Sec s' a
up (MkSec x) = MkSec x

-- only for trusted code!
reveal :: Sec s a -> a
reveal (MkSec a) = a

