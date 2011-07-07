module Builder(test2, test) where

import qualified Data.Sequence.Chunked.Builder as B
import Data.Sequence.Chunked
import Data.Monoid
import qualified Data.Vector.Unboxed as V

instance Unbox Double where
    defaultChunkSize _ = 128

test :: Int -> Seq Int
test n = B.toSeq (stupidReplicate n 0)

stupidReplicate :: Int -> Int -> B.Builder Int
stupidReplicate n a = go n where
  go 0 = mempty
  go n = B.singleton a `mappend` go (n-1)

test2 :: Int -> Seq Int
test2 n = B.toSeq (B.singleton n `mappend` B.singleton n `mappend` B.singleton n)

{-# SPECIALISE snocChunk :: Seq Int -> V.Vector Int -> Seq Int #-}
