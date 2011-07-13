{-# LANGUAGE TupleSections, BangPatterns, ScopedTypeVariables, FlexibleInstances #-}

module Main(uniformRVector, main, firstHalfS, firstHalfSC, {-, snocManyBS, snocManySC, snocManySCB, snocManyS-}) where

import Data.List(nub)
import Criterion.Main
import Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Sequence.Chunked as SC
import qualified Data.Sequence.Chunked.Builder as SCB
import qualified Data.Binary.Builder as BSB
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.Rope as R
import Data.Word
import Control.DeepSeq

import System.Random.MWC
import qualified Data.Vector.Fusion.Stream.Monadic as Stream
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as GV
import Control.Applicative
import Control.Monad.ST


------------------------------------------------------------------------------------------
force :: BS.ByteString -> a -> a
force b x = BS.foldlChunks (flip seq) x b

instance NFData BS.ByteString where
 rnf b = force b ()

instance SC.Unbox Word8 where
   defaultChunkSize _ = 128
------------------------------------------------------------------------------------------

-------------------------------------------snoc-------------------------------------------
snocManyBS :: Int -> BS.ByteString
snocManyBS n = BSB.toLazyByteString $ goB n where
 goB 0 = mempty
 goB n = BSB.singleton 0 `mappend` goB (n-1)

snocManySC :: Int -> SC.Seq Word8
snocManySC n = goSC n SC.empty where
 goSC 0 acc = acc
 goSC n acc = acc `seq` goSC (n-1) (0 SC.<| acc)

snocManySCB :: Int -> SC.Seq Word8
snocManySCB n = SCB.toSeq (goSCB n) where
   goSCB 0 = mempty
   goSCB n = SCB.singleton 0 `mappend` goSCB (n-1)

snocManyS :: Int -> S.Seq Word8
snocManyS n = go n mempty where
 go 0 acc = acc
 go n acc = acc `seq` go (n-1) (acc S.|> 0)

consManyL :: Int -> [Word8]
consManyL n = go n [] where
   go 0 acc = acc
   go n acc = acc `seq` go (n-1) (0:acc)

len = 10000

-------------------------------------------split------------------------------------------
--firstHalfBS 
uniformRVector :: (Int, Int) -> Int -> V.Vector Int
uniformRVector range n = runST $ do
   gen <- create
   let str = Stream.unfoldrNM n step ()
       step () = (Just . (,())) <$> uniformR range gen
   mv <- GV.munstream str
   v <- V.unsafeFreeze mv
   return v

class Splittable seq where
    gsnocMany :: Int -> seq
    gsplitAt :: Int -> seq -> (seq, seq)
instance Splittable (SC.Seq Word8) where
    gsnocMany = snocManySC
    gsplitAt = SC.splitAt
instance Splittable (S.Seq Word8) where
    gsnocMany = snocManyS
    gsplitAt = S.splitAt
instance Splittable R.Rope where
    gsnocMany n = R.fromString (replicate n '0')
    gsplitAt = R.splitAt

splitTimes = 100

benchSplit :: forall seq. Splittable seq => seq -> Int -> Pure
benchSplit _ size = 
   let randoms = uniformRVector (0, size-1) splitTimes
       theSeq = gsnocMany size :: seq
       step sequence () r = fst (gsplitAt r sequence) `seq` ()
       loop sequence = V.foldl' (step sequence) () randoms
   in whnf loop theSeq

firstHalfSC :: Int -> SC.Seq Word8 -> SC.Seq Word8
firstHalfSC len s = fst (SC.splitAt (len `div` 2) s)

firstHalfS :: Int -> S.Seq Word8 -> S.Seq Word8
firstHalfS len s = fst (S.splitAt (len `div` 2) s)

firstHalfR :: Int -> R.Rope -> R.Rope
firstHalfR len r = fst (R.splitAt (len `div` 2) r)

----------------------------------------index----------------------------------------------
indexSC :: Int -> SC.Seq Word8 -> Word8
indexSC len s = SC.index (len `div` 2) s

indexS :: Int -> S.Seq Word8 -> Word8
indexS len s = S.index s (len `div` 2)


logscale' :: Int -> Double -> Double -> [Double]
logscale' n a b = take (n+1) $ iterate (*r) a where
  r = (b/a)**(1 / fromIntegral n)

logscale :: Int -> Int -> Int -> [Int]
logscale n a b = map round $ logscale' n (fromIntegral a) (fromIntegral b)

main = defaultMain [
 bgroup "snocMany" [
    bench "chunked-builder" $ whnf snocManySCB len,
    bench "chunked-slow" $ whnf snocManySC len,
    bench "bytestring" $ nf snocManyBS len,
    bench "unchunked" $ whnf snocManyS len,
    bench "list" $ whnf consManyL len
    ],
 bgroup "split" [
   bgroup "chunked" $ map splitNChunked range,
   bgroup "unchunked" $ map splitNUnchunked range,
   bgroup "rope" $ map splitNRope range
 ],
 bgroup "index" [
   bgroup "chunked" $ map (\n -> bench (show n) $ whnf (indexSC n) (snocManySC n)) range,
   bgroup "unchunked" $ map (\n -> bench (show n) $ whnf (indexS n) (snocManyS n)) range
 ],
 bgroup "splitrandom" [
   bgroup "chunked" $ map (\n -> bench (show n) $ benchSplit (SC.singleton (0 :: Word8))  n) range,
   bgroup "unchunked" $ map (\n -> bench (show n) $ benchSplit (S.singleton (0 :: Word8)) n) range,
   bgroup "rope" $ map (\n -> bench (show n) $ benchSplit R.empty n) range
 ]
 ]
 where
    range = nub $ logscale 50 1 (10^7)
    splitNChunked n = bench (show n) $ whnf (firstHalfSC n) chunked
      where
          chunked = snocManySC n
    splitNUnchunked n = bench (show n) $ whnf (firstHalfS n) unchunked
      where
          unchunked = snocManyS n
    splitNRope n = bench (show n) $ whnf (firstHalfR n) rope
      where
          rope = R.fromString (replicate n '0')

