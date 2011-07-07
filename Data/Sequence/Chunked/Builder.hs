{-# LANGUAGE ScopedTypeVariables #-}

module Data.Sequence.Chunked.Builder(
  Builder,
  toSeq,
  chunk,
  largeChunk,
  singleton,
 ) where

import Data.Monoid
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Unboxed(Vector)
import Data.Sequence.Chunked hiding(singleton)
import GHC.IO(unsafeDupablePerformIO)


------------------------------------------------------------------------------------------
--                                         Builders                                     --
------------------------------------------------------------------------------------------

-- we export Builder abstractly, so we can guarantee that 'Buffer' is used linearly.
newtype Builder a = Builder { runBuilder :: Buffer a -> IO (Buffer a) }

instance Monoid (Builder a) where
    {-# INLINE mempty #-}
    mempty = Builder (\b -> return b)
    {-# INLINE mappend #-}
    mappend (Builder f) (Builder g) = Builder (\b -> f b >>= g)

-- unfortunately, we can't unpack the vector, primarily because of http://hackage.haskell.org/trac/ghc/ticket/3990
data Buffer a = Buffer !(MV.IOVector a)
                       {-# UNPACK #-} !Int        -- write point
                       !(Seq a)                   -- completed chunks

{-# INLINE toSeq #-}
toSeq :: forall a. Unbox a => Builder a -> Seq a
toSeq (Builder f) = unsafeDupablePerformIO $ do
    mv <- MV.unsafeNew (defaultChunkSize (undefined :: a))
    Buffer mv' n s <- f $ Buffer mv 0 empty
    if n == 0 
     then return s
     else do 
         v <- V.unsafeFreeze (MV.unsafeSlice 0 n mv')
         return (snocChunk s v)

{-# INLINABLE chunk #-}
chunk :: forall a. Unbox a => V.Vector a -> Builder a
chunk c = 
  Builder $ \(Buffer mv n seq) -> 
     if n + V.length c <= defaultChunkSize (undefined :: a)
     then do
       V.unsafeCopy (MV.unsafeSlice n (V.length c) mv) c
       return (Buffer mv (n+V.length c) seq)
     else do
       -- at this stage, we will definitely neeed a new buffer vector, so allocate it now
       (mv', seq') <- if n == 0 
              then return (mv, seq)
              else do
                  v <- V.unsafeFreeze (MV.unsafeSlice 0 n mv)
                  mv' <- MV.unsafeNew (defaultChunkSize (undefined :: a))
                  return (mv', snocChunk seq v)
       -- now decide whether to copy 'c' into the buffer, or just send it directly to the stream.
       if V.length c <= defaultChunkSize (undefined :: a) `div` 2
        then do
         V.unsafeCopy (MV.unsafeSlice 0 (V.length c) mv') c
         return (Buffer mv' (V.length c) seq')
        else return (Buffer mv' 0 (snocLargeChunk seq' c))

{-# INLINABLE largeChunk #-}
largeChunk :: forall a. Unbox a => V.Vector a -> Builder a
largeChunk c =
  Builder $ \(Buffer mv n seq) -> 
    if n == 0
    then return $ Buffer mv 0 (snocLargeChunk seq c)
    else do
           v <- V.unsafeFreeze (MV.unsafeSlice 0 n mv)
           mv' <- MV.unsafeNew (defaultChunkSize (undefined :: a))
           return (Buffer mv' 0 (snocLargeChunk seq v))

{-
{-# INLINE singleton #-}
singleton :: Unbox a => a -> Builder a
singleton a = chunk (V.singleton a)
-}

{-# INLINE singleton #-}
singleton :: forall a. Unbox a => a -> Builder a
singleton a = ensureNFree 1 `mappend` (Builder $ \(Buffer mv n seq) -> do
       MV.unsafeWrite mv n a
       return (Buffer mv (n+1) seq)
   )

{-# INLINE ensureNFree #-}
ensureNFree :: forall a. Unbox a => Int -> Builder a
ensureNFree n = Builder $ \(Buffer mv i seq) -> 
   if i + n <= defaultChunkSize (undefined :: a) 
   then return $ Buffer mv i seq
   else do
       v <- V.unsafeFreeze (MV.unsafeSlice 0 i mv)
       mv' <- MV.new (defaultChunkSize (undefined :: a))
       return (Buffer mv' 0 (snocLargeChunk seq v))
     
