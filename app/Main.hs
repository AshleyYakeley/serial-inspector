module Main (main) where

import Numeric
import Data.List (intercalate)
import Data.Foldable
import Data.Traversable
import Data.Char
import Data.Int
import Data.Word
import Data.ByteString (ByteString)
import Data.ByteString.Lazy as BS (fromStrict,toStrict)
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Foreign.Storable as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Marshal as Foreign
import System.IO.Unsafe
import Shapes ()

-- Candidates
import qualified "cereal" Data.Serialize as Cereal
import qualified "cereal-text" Data.Serialize.Text as Cereal ()
import qualified "serialise" Codec.Serialise as Serialise
import qualified "binary" Data.Binary as Binary
import qualified "winery" Codec.Winery as Winery
import qualified "store" Data.Store as Store
--import qualified "flat" Flat as Flat
import qualified "persist" Data.Persist as Persist
import qualified "shapes" Data.Serializer as Shapes

data Dict c = c => MkDict

class (
        Eq a,
        Show a,
        Cereal.Serialize a,
        Serialise.Serialise a,
        Binary.Binary a,
        Winery.Serialise a,
        Store.Store a,
--        Flat.Flat a,
        Persist.Persist a,
        Shapes.HasSerializer a
    ) => Puttable a where
    typeName :: String
    foreignInstance :: Maybe (Dict (Foreign.Storable a))
    default foreignInstance :: Foreign.Storable a => Maybe (Dict (Foreign.Storable a))
    foreignInstance = Just MkDict

instance Puttable () where
    typeName = "Unit"

instance Puttable Bool where
    typeName = "Bool"

instance Puttable Char where
    typeName = "Char"

instance Puttable Word8 where
    typeName = "Word8"

instance Puttable Int8 where
    typeName = "Int8"

instance Puttable Word16 where
    typeName = "Word16"

instance Puttable Int16 where
    typeName = "Int16"

instance Puttable Word32 where
    typeName = "Word32"

instance Puttable Int32 where
    typeName = "Int32"

instance Puttable Integer where
    typeName = "Integer"
    foreignInstance = Nothing

instance Puttable Float where
    typeName = "Float"

instance Puttable Double where
    typeName = "Double"

instance Puttable Rational where
    typeName = "Rational"
    foreignInstance = Nothing

instance {-# OVERLAPPABLE #-} Puttable a => Puttable [a] where
    typeName = "[" <> typeName @a <> "]"
    foreignInstance = Nothing

instance Puttable ByteString where
    typeName = "ByteString"
    foreignInstance = Nothing

instance Puttable String where
    typeName = "String"
    foreignInstance = Nothing

instance Puttable Text where
    typeName = "Text"
    foreignInstance = Nothing

data TestItem = forall a. Puttable a => MkTestItem a

data Candidate = MkCandidate {
    cName :: String,
    cCodec :: forall a. Puttable a => Maybe (a -> ByteString, ByteString -> Maybe a)
}

showWord8 :: Word8 -> String
showWord8 i = fmap toUpper $ showHex (div i 16) "" <> showHex (mod i 16) ""

showBS :: ByteString -> String
showBS bs = intercalate " " $ fmap showWord8 $ BS.unpack bs

testCandidateItem :: TestItem -> Candidate -> IO ()
testCandidateItem (MkTestItem value) candidate = putStrLn $ " " <> cName candidate <> ": " <> case cCodec candidate of
    Just (encode,decode) -> let
        bs = encode value
        in case decode bs of
            Just value' | value == value' -> showBS bs <> " (" <> (show $ BS.length bs) <> ")"
            _ -> "FAILED"
    Nothing -> "no"

foreignCodec :: forall a. Puttable a => Maybe (a -> ByteString, ByteString -> Maybe a)
foreignCodec = do
    MkDict <- foreignInstance @a
    let
        encode :: a -> ByteString
        encode value = unsafePerformIO $ Foreign.with value $ \ptr -> do
            bb <- for [0 .. pred (Foreign.sizeOf value)] $ \i -> Foreign.peek $ Foreign.plusPtr ptr i
            return $ BS.pack bb
        decode :: ByteString -> Maybe a
        decode bs = do
            let vsize = Foreign.sizeOf (undefined :: a)
            if BS.length bs == vsize then return () else Nothing
            return $ unsafePerformIO $ Foreign.alloca $ \ptr -> do
                for_ [0 .. pred vsize] $ \i -> Foreign.poke (Foreign.plusPtr ptr i) $ BS.unpack bs !! i
                Foreign.peek ptr
    return (encode,decode)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right v) = Just v
eitherToMaybe (Left _) = Nothing

winery_deserialiseOnly :: forall a. Winery.Serialise a => ByteString -> Either Winery.WineryException a
winery_deserialiseOnly bs = do
    decoder <- Winery.getDecoder $ Winery.schema $ Nothing @a
    return $ Winery.evalDecoder decoder bs

candidates :: [Candidate]
candidates =
    [ MkCandidate "foreign" foreignCodec
    , MkCandidate "cereal" $ Just (Cereal.encode, eitherToMaybe . Cereal.decode)
    , MkCandidate "store" $ Just (Store.encode, eitherToMaybe . Store.decode)
    , MkCandidate "binary" $ Just (BS.toStrict . Binary.encode, \bs -> do
        (remaining,_,a) <- eitherToMaybe $ Binary.decodeOrFail $ BS.fromStrict bs
        if remaining == mempty then return a else Nothing
        )
    , MkCandidate "serialise" $ Just (BS.toStrict . Serialise.serialise, eitherToMaybe . Serialise.deserialiseOrFail . BS.fromStrict)
    , MkCandidate "winery-schemed" $ Just (Winery.serialise, eitherToMaybe . Winery.deserialise)
    , MkCandidate "winery-only" $ Just (Winery.serialiseOnly, eitherToMaybe . winery_deserialiseOnly)
--    , MkCandidate "flat" $ Just (Flat.flat, eitherToMaybe . Flat.unflat)
    , MkCandidate "persist" $ Just (Persist.encode, eitherToMaybe . Persist.decode)
    , MkCandidate "shapes" $ Just (Shapes.serializerStrictEncode Shapes.serializer, Shapes.serializerStrictDecode Shapes.serializer)
    ]

items :: [TestItem]
items =
    [ MkTestItem @() ()
    , MkTestItem @Bool False
    , MkTestItem @Bool True
    , MkTestItem @Word8 0
    , MkTestItem @Word8 34
    , MkTestItem @Word16 0
    , MkTestItem @Word16 34
    , MkTestItem @Word16 50000
    , MkTestItem @Int32 0
    , MkTestItem @Int32 34
    , MkTestItem @Int32 (-7465726)
    , MkTestItem @Integer 0
    , MkTestItem @Integer 1
    , MkTestItem @Integer (-1)
    , MkTestItem @Integer 34
    , MkTestItem @Integer 2354723896
    , MkTestItem @Integer 0xFF123456789ABCDEF0
    , MkTestItem @Integer 0xFFFFFFFFFFFFFFFFFF
    , MkTestItem @Integer 0x1000000000000000000
    , MkTestItem @Integer (-0xFFFFFFFFFFFFFFFFFF)
    , MkTestItem @Integer (-0x1000000000000000000)
    , MkTestItem @Float 0
    , MkTestItem @Float 34
    , MkTestItem @Float (-372.572348967482590)
    , MkTestItem @Double 0
    , MkTestItem @Double 34
    , MkTestItem @Double (-372.572348967482590)
    , MkTestItem @Rational 34
    , MkTestItem @Rational $ 34/27
    , MkTestItem @[()] []
    , MkTestItem @[()] [()]
    , MkTestItem @[Word8] []
    , MkTestItem @[Word8] [0,1,255]
    , MkTestItem @[Double] []
    , MkTestItem @[Double] [0,1,255,-372.572348967482590]
    , MkTestItem @ByteString mempty
    , MkTestItem @ByteString $ BS.pack [0,1,255]
    , MkTestItem @ByteString $ BS.pack [15,17,240,12,0,11,1]
    , MkTestItem @Char 'a'
    , MkTestItem @Char '\x2588'
    , MkTestItem @String ""
    , MkTestItem @String "a"
    , MkTestItem @String "\x2588"
    , MkTestItem @String "hello"
    , MkTestItem @Text ""
    , MkTestItem @Text "a"
    , MkTestItem @Text "\x2588"
    , MkTestItem @Text "hello"
    ]

main :: IO ()
main = for_ items $ \item@(MkTestItem (value :: a)) -> do
    putStrLn $ typeName @a <> " " <> show value
    for_ candidates $ testCandidateItem item
