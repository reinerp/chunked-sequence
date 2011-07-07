
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, FlexibleContexts, TypeSynonymInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Sequence.Chunked.Internal where

import Data.Monoid
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Unboxed(Vector)
import qualified Data.FingerTree.Unboxed as F
import Data.FingerTree.Unboxed(FingerTree, Measured, ViewL(..))
import Data.FingerTree.Unboxed
import Data.Unboxed

--newtype Size = Size { unSize :: Int }
data Size = Size { unSize :: {-# UNPACK #-} !Int }
  deriving(Eq)
newtype Chunk a = Chunk { unElem :: V.Vector a }
newtype Seq a = Seq { unSeq :: FingerTree Size (Chunk a) }

instance Monoid Size where
    {-# INLINE mappend #-}
    mappend (Size a) (Size b) = Size (a + b)
    {-# INLINE mempty #-}
    mempty = Size 0

instance V.Unbox a => Measured Size (Chunk a) where
    {-# INLINE measure #-}
    measure (Chunk v) = Size $ V.length v

------------------------------------------------------------------------------------------
$(F.defineFingerTree [t| Size |])
instance F.Unpacked1 (Node Size) where
    {-# INLINE mk1 #-}
    mk1 = mk
    {-# INLINE unMk1 #-}
    unMk1 = unMk
instance F.Unpacked1 (FingerTree Size) where
    {-# INLINE mk1 #-}
    mk1 = mk
    {-# INLINE unMk1 #-}
    unMk1 = unMk
instance F.Unbox Size
------------------------------------------------------------------------------------------
