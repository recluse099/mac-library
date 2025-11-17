module SecIO where
import           Lattice
import           Sec

-- SecIO
newtype SecIO s a = MkSecIO (IO (Sec s a))

instance Functor (SecIO s) where
  fmap :: (a -> b) -> SecIO s a -> SecIO s b -- dangerous, can cause security breach
  fmap f (MkSecIO m) = MkSecIO $ fmap (fmap f) m

instance Applicative (SecIO s) where
  pure :: a -> SecIO s a
  pure x = MkSecIO $ pure (pure x)
  (<*>) :: SecIO s (a->b) -> SecIO s a -> SecIO s b -- dangerous, can cause security breach
  (MkSecIO mf) <*> (MkSecIO mx) = MkSecIO $ (fmap (<*>) mf) <*> mx

instance Monad (SecIO s) where
  return = pure
  (>>=) :: SecIO s a -> (a -> SecIO s b) -> SecIO s b
  MkSecIO mx >>= f = MkSecIO $ do
    sx <- mx
    let MkSecIO result = f (reveal sx)
     in result

-- SecIO functions
value :: Sec s a -> SecIO s a
value sa = MkSecIO (return sa)

run :: SecIO s a -> IO (Sec s a)
run (MkSecIO m) = m

plug :: Less sl sh => SecIO sh a -> SecIO sl (Sec sh a)
plug (MkSecIO m) = MkSecIO $ do
  sha <- m
  let result = sec sha
  return result

s_read :: Less s' s => File s' -> SecIO s String
s_read file =do
  ss <- readSecIO file
  value (up ss)

s_write :: Less s' s => File s -> String -> SecIO s' (Sec s ())
s_write file str = plug (writeSecIO file str)

-- File IO
data File s = MkFile FilePath
readSecIO :: File s' -> SecIO s (Sec s' String)
readSecIO (MkFile file) = MkSecIO ((sec . sec) `fmap` readFile file)
writeSecIO :: File s' -> String -> SecIO s ()
writeSecIO (MkFile file) s = MkSecIO (sec `fmap` writeFile file s)

