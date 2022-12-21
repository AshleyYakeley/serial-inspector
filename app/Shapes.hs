module Shapes where

import Prelude
import Data.TypeRig
import qualified "shapes" Data.Serializer as Shapes
import Data.PrimitiveSerial
import Data.ByteString.Builder
import Data.Traversable
import Control.Applicative

instance Shapes.HasSerializer () where
    serializer = rUnit

instance Shapes.HasSerializer Char where
    serializer = Shapes.MkSerializer mempty empty

instance {-# OVERLAPPABLE #-} Shapes.HasSerializer a => Shapes.HasSerializer [a] where
    serializer = let
        lengthSerializer :: Shapes.Serializer Int
        lengthSerializer = Shapes.sleb128Serializer
        s :: [a] -> Builder
        s aa = Shapes.serialize lengthSerializer (length aa) <> mconcat (fmap (Shapes.serialize Shapes.serializer) aa)
        d = do
            len <- Shapes.deserialize lengthSerializer
            for [1..len] $ \_ -> Shapes.deserialize Shapes.serializer
        in Shapes.MkSerializer s d
