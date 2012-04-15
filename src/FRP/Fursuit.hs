-- | Oversimplified FRP library, using no concurrency primitives.
--   As it's primarily intended to be used with haste-compiler, compiled to
--   Javascript, this library is NOT thread-safe, though it could be made so
--   by simply slamming a global lock around the write and newSinkID
--   operations.
module FRP.Fursuit (module Sink, module Pipe, Signal, sink, new, union, accumS,
                    filterS) where
import FRP.Fursuit.Signal
import FRP.Fursuit.Pipe as Pipe
import FRP.Fursuit.Sink as Sink
import System.IO.Unsafe

-- | Execute the specified IO action to obtain a new signal when registering
--   signals. This is handy when you're creating a signal from an external
--   event for use with a single sink:
-- @
--   clicked <- buttonSig "my_button"
--   sink (_ -> putStrLn "Button clicked!") clicked
--   -- ...can be rewritten as:
--   sink (_ -> putStrLn "Button clicked!") (new $ buttonSig "my_button")
-- @
new :: IO (Signal a) -> Signal a
new = New . unsafePerformIO

-- | Create a signal that has the value of whichever parent signal fired last.
union :: Signal a -> Signal a -> Signal a
union = Union

-- | Behaves pretty much like scanl on signals. Initialize the accumulator with
--   a default value; every time the function signal triggers, apply the
--   function to the accumulator and pass on the result.
accumS :: a -> Signal (a -> a) -> Signal a
accumS = Accum

-- | Filter out events. filterS pred sig only lets the signal sig through if
--   it fulfills the predicate pred. For example:
-- @
--   (pa, a) <- pipe (0 :: Int)
--   (pb, b) <- pipe (0 :: Int)
--   let plus = (+) <$> filterS (< 10) a <*> b
--   sink (putStrLn . show) plus
--   write pa 20
--   write pb 20
--   write pa 5
-- @
--   The above code will print 20 and 25; writing 20 to pa gets filtered out,
--   as 20 does not fulfull (< 10) so no signal is fired. b isn't so filtered
--   however, so the 20 goes through just fine, and is added to the last good
--   value of a (which is 0 - its initial value). The final 5 does fulfill
--   (< 10), so the signal goes through and we get 25.
filterS :: (a -> Bool) -> Signal a -> Signal a
filterS = Filter
