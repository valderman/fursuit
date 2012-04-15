-- | Various kinds of sinks,
module FRP.Fursuit.Sink (Sink (..), perform) where
import Data.IORef
import FRP.Fursuit.Signal

-- | Perform the IO action returned by a signal whenever triggered.
perform :: Signal (IO ()) -> IO ()
perform = sink id

-- | Bind a signal to a value of some type. Examples of instances would be
--   IORef, where ref << sig would store the value of sig in ref whenever
--   triggered, or Pipe, where p << sig would write the value of sig to p.
class Sink s where
  (<<) :: s a -> Signal a -> IO ()

instance Sink Pipe where
  p << s = sink (write p) s

instance Sink IORef where
  ref << p = sink (writeIORef ref) p
