{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, FlexibleContexts, TypeSynonymInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields -Odph #-}

module Data.Sequence.Chunked
  (
   Unbox(..),
   Seq,
   empty,
   singleton,
   (><),
   (<|),
   Data.Sequence.Chunked.splitAt,
--   create,
   consChunk,
   snocChunk,
   consLargeChunk,
   snocLargeChunk,
   index,
  )
   where

import Data.Monoid
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Unboxed(Vector)
import qualified Data.FingerTree.Unboxed as F
import Data.FingerTree.Unboxed(FingerTree, Measured, ViewL(..), ViewR(..))
import Data.Unboxed
import Data.Sequence.Chunked.Internal

class V.Unbox a => Unbox a where
    defaultChunkSize :: a -> Int
instance Unbox Int where
    defaultChunkSize _ = 128

{-# INLINE empty #-}
empty :: Unbox a => Seq a
empty = Seq F.empty

{-# INLINABLE singleton #-}
singleton :: Unbox a => a -> Seq a
singleton = Seq . F.singleton . Chunk . V.singleton

{-# INLINABLE (><) #-}
(><) :: forall a. Unbox a => Seq a -> Seq a -> Seq a
(Seq l) >< (Seq r) = Seq $ case (F.viewr l, F.viewl r) of
    (EmptyR, _) -> r
    (_, EmptyL) -> l
    (l1 :> Chunk l2, Chunk r1 :< r2) -> 
       if V.length l2 + V.length r1 <= defaultChunkSize (undefined :: a)
       then l1 F.|> (Chunk $ l2 V.++ r1) F.>< r2
       else l F.>< r

-- the singleton isn't fused when we call consChunk -- or is it?
{-# INLINABLE (<|) #-}
(<|) :: forall a. Unbox a => a -> Seq a -> Seq a
x <| (Seq xs) 
  = case F.viewl xs of
      EmptyL -> singleton x
      Chunk c :< cs -> Seq $
         if V.length c < defaultChunkSize (undefined :: a)
         then (Chunk $ V.cons x c) F.<| cs
         else (Chunk $ V.singleton x) F.<| xs

{-# INLINABLE consChunk #-} -- we want to fuse the concatenation when possible -- should we inline?
consChunk :: forall a. Unbox a => V.Vector a -> Seq a -> Seq a
consChunk chunk (Seq xs) | V.length chunk == 0 = Seq xs
                         | otherwise =
   case F.viewl xs of
       EmptyL -> Seq . F.singleton . Chunk $ chunk
       Chunk x :< xs' -> Seq $
          if V.length chunk + V.length x <= defaultChunkSize (undefined :: a)
          then (Chunk $ chunk V.++ x) F.<| xs'
          else Chunk chunk F.<| xs

{-# INLINABLE snocChunk #-} -- we want to fuse the concatenation when possible -- should we inline?
snocChunk :: forall a. Unbox a => Seq a -> V.Vector a -> Seq a
snocChunk (Seq xs) chunk | V.length chunk == 0 = Seq xs
                         | otherwise =
   case F.viewr xs of
       EmptyR -> Seq . F.singleton . Chunk $ chunk
       xs' :> Chunk x -> Seq $
          if V.length chunk + V.length x <= defaultChunkSize (undefined :: a)
          then xs' F.|> (Chunk $ x V.++ chunk)
          else xs F.|> Chunk chunk

{-# INLINABLE snocLargeChunk #-}
snocLargeChunk :: forall a. Unbox a => Seq a -> V.Vector a -> Seq a
snocLargeChunk (Seq xs) chunk = Seq (xs F.|> Chunk chunk)

{-# INLINABLE consLargeChunk #-}
consLargeChunk :: forall a. Unbox a => V.Vector a -> Seq a -> Seq a
consLargeChunk chunk (Seq xs) = Seq (Chunk chunk F.<| xs)

{-# INLINABLE splitAt #-}
splitAt :: Unbox a => Int -> Seq a -> (Seq a, Seq a)
splitAt n (Seq xs) = 
  case F.split (\(Size s) -> s >= n) xs of
      (l,x@(Chunk x'),r)
            | F.measure l == Size n -> (Seq l, Seq (x F.<| r))
            | F.measure l + Size (V.length x') == Size n -> (Seq (l F.|> x), Seq r)
            | otherwise -> case V.splitAt (n - unSize (F.measure l)) x' of
                     (xl, xr) -> (snocChunk (Seq l) xl, consChunk xr (Seq r))

{-# INLINABLE index #-}
index :: Unbox a => Int -> Seq a -> a
index n (Seq xs) =
  case F.split (\(Size s) -> s > n) xs of
      (l,Chunk x', _) -> V.unsafeIndex x' (n - unSize (F.measure l))
