module Main(main, firstHalfSC, {-, snocManyBS, snocManySC, snocManySCB, snocManyS-}) where

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

len = 10000

-------------------------------------------split------------------------------------------
--firstHalfBS 
firstHalfSC :: Int -> SC.Seq Word8 -> SC.Seq Word8
firstHalfSC len s = fst (SC.splitAt (len `div` 2) s)

firstHalfS :: Int -> S.Seq Word8 -> S.Seq Word8
firstHalfS len s = fst (S.splitAt (len `div` 2) s)

firstHalfR :: Int -> R.Rope -> R.Rope
firstHalfR len r = fst (R.splitAt (len `div` 2) r)

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
     bench "unchunked" $ whnf snocManyS len
     ],
  bgroup "split" [
    bgroup "chunked" $ map splitNChunked range,
    bgroup "unchunked" $ map splitNUnchunked range,
    bgroup "rope" $ map splitNRope range
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

