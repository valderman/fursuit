-- | Oversimplified FRP library, using no concurrency primitives.
--   As it's primarily intended to be used with haste-compiler, compiled to
--   Javascript, this library is NOT thread-safe, though it could be made so
--   by simply slamming a global lock around the write and newSinkID
--   operations.
module FRP.Fursuit (module Signal, module Sink) where
import FRP.Fursuit.Signal as Signal
import FRP.Fursuit.Sink as Sink
