{-# LANGUAGE GADTs, BangPatterns, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module FRP.Fursuit.Signal (Signal, Pipe, sink, accumS, filterS, pipe,
                           emptyPipe, write, new, union) where
import Data.IORef
import System.IO.Unsafe
import Control.Applicative
import qualified Data.IntMap as M
import Data.Maybe

type SinkID = Int
type Origin = Bool
type Sig a = IO (Maybe (a, Origin))
type SinkList = M.IntMap (IO ())

-- New is implemented as a thin wrapper around unsafePerformIO, to make sure
-- it's only evaluated once and never outside of sink.
data Signal a where
  App    :: Signal (a -> b) -> Signal a -> Signal b
  Pure   :: a -> Signal a
  Pipe   :: IORef (Maybe a) -> IORef SinkList -> IORef Origin -> Signal a
  Filter :: (a -> Bool) -> Signal a -> Signal a
  Accum  :: a -> Signal (a -> a) -> Signal a
  New    :: Signal a -> Signal a
  Union  :: Signal a -> Signal a -> Signal a

{-# NOINLINE sinkIDs #-}
sinkIDs :: IORef SinkID
sinkIDs = unsafePerformIO $ newIORef 0

-- | Generate a new sink ID, and update the global list of such IDs.
newSinkID :: IO SinkID
newSinkID = do
  sid <- readIORef sinkIDs
  writeIORef sinkIDs $! sid+1
  return sid

-- | Attach a signal to an actuator.
sink :: (a -> IO ()) -> Signal a -> IO ()
sink act sig = do
  sig' <- compile sig >>= return . mkSnk act
  sid <- newSinkID
  mapM_ (\sinks -> modifyIORef sinks (M.insert sid sig')) (sources sig)
  where
    mkSnk action signal = do
      result <- signal
      case result of
        Just (val, actuallyHappened) | actuallyHappened -> action val
        _                                               -> return ()

    -- Find all sources for this signal
    sources :: Signal a -> [IORef SinkList]
    sources (App f x)      = sources f ++ sources x
    sources (Pure _)       = []
    sources (Pipe _ src _) = [src]
    sources (Filter _ s)   = sources s
    sources (Accum _ s)    = sources s
    sources (New s)        = sources s
    sources (Union a b)    = sources a ++ sources b
    
    -- Compile the signal into an IO action we can trigger whenever one of its
    -- sources gets a signal.
    compile :: Signal a -> IO (Sig a)
    compile (App sf sx) = do
      f <- compile sf
      x <- compile sx
      return (f `appS` x)
    compile (Pure x) = do
      return (return $ Just (x, False))
    compile (Pipe value _ origin) = do
      return $ do
        mval <- readIORef value
        orig <- readIORef origin
        return $ mval >>= \val -> return (val, orig)
    compile (Filter predicate s) = do
      s' <- compile s
      ms <- s'
      lastGood <- case ms of
        Just (initial, _) | predicate initial -> newIORef (Just initial)
        _                                     -> newIORef Nothing
      return (fltS predicate lastGood s')
    compile (Accum initially s) = do
      s' <- compile s
      ref <- newIORef initially
      return (accS ref s')
    compile (New signal) = do
      compile signal
    compile (Union a b) = do
      a' <- compile a
      b' <- compile b
      -- Prefer to initialize with the value of the left signal.
      maval <- a'
      initial <- case maval of
        Just (x, _) -> return (Just x)
        _           -> do
          mbval <- b'
          case mbval of
            Just (x, _) -> return (Just x)
            _           -> return Nothing
      prev <- newIORef initial
      return $ uniS a' b' prev

    -- Union of two events.
    uniS sa sb prevref = do
      ma <- sa
      mb <- sb
      prev <- readIORef prevref
      case listToMaybe $ filter snd $ catMaybes [ma, mb] of
        Nothing -> do
          return $ fmap (, False) prev
        val -> do
          writeIORef prevref (fmap fst val)
          return val

    -- Basically ap or <*>, but takes the origin indicator into account.
    appS sf sx = do
      mf <- sf
      mx <- sx
      return $ do
        (f, origf) <- mf
        (x, origx) <- mx
        return (f x, origf || origx)

    -- filterS, with a memo reference for the last value that passed through.
    fltS predicate lastGood signal = do
      msig <- signal
      case msig of
        Just (val, orig) -> do
          if predicate val && orig
            then do
              writeIORef lastGood (Just val)
              return $ Just (val, True)
            else do
              mlast <- readIORef lastGood
              case mlast of
                Just lastVal -> return $ Just (lastVal, False)
                _            -> return Nothing
        _ -> do
          return Nothing

    -- accumS
    accS lastRef sf = do
      mf <- sf
      case mf of
        Just (f, orig) -> do
          if orig
            then do
              val <- readIORef lastRef
              let !x = f val
              writeIORef lastRef x
              return $ Just (x, True)
            else do
              lastVal <- readIORef lastRef
              return $ Just (lastVal, False)
        _ -> do
          return Nothing

data Pipe a = P {
    piperef :: IORef (Maybe a),
    cbref   :: IORef (M.IntMap (IO ())),
    origref :: IORef Origin
  }

instance Functor Signal where
  fmap f x = pure f <*> x

instance Applicative Signal where
  pure  = Pure
  (<*>) = App

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

-- | Create a pipe. Writing to a pipe is the only way to manually trigger a
--   signal.
emptyPipe :: IO (Pipe a, Signal a)
emptyPipe = do
  ref <- newIORef Nothing
  orig <- newIORef False
  sinks <- newIORef M.empty
  return (P ref sinks orig, Pipe ref sinks orig)

-- | Create a pipe with an initial value.
pipe :: a -> IO (Pipe a, Signal a)
pipe !initially = do
  ps@(P ref _ _, _) <- emptyPipe
  writeIORef ref (Just initially)
  return ps

-- | Write a value into a pipe. This will cause the pipe's associated signal
--   to fire.
write :: Pipe a -> a -> IO ()
write (P value listeners origin) !x = do
  writeIORef origin True
  writeIORef value (Just x)
  readIORef listeners >>= M.foldl' (>>) (return ())
  writeIORef origin False

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
