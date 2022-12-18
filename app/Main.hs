module Main (main) where

import Numeric
import Data.List (intercalate)
import Data.Foldable
import Data.Traversable
import Data.Int
import Data.Word
import Data.ByteString (ByteString)
import Data.ByteString.Lazy as BS (toStrict)
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Foreign.Storable as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Marshal.Alloc as Foreign
import System.IO.Unsafe
import qualified Data.Serialize as Cereal
import qualified Data.Serialize.Text as Cereal ()
import qualified Codec.Serialise as Serialise
import qualified Data.Binary as Binary
import qualified Codec.Winery as Winery
import qualified Data.Store as Store
import qualified Flat as Flat

data Dict c = c => MkDict

class (
        Show a,
        Cereal.Serialize a,
        Serialise.Serialise a,
        Binary.Binary a,
        Winery.Serialise a,
        Store.Store a,
        Flat.Flat a
    ) => Puttable a where
    typeName :: String
    foreignInstance :: Maybe (Dict (Foreign.Storable a))
    default foreignInstance :: Foreign.Storable a => Maybe (Dict (Foreign.Storable a))
    foreignInstance = Just MkDict

instance Puttable () where
    typeName = "Unit"

instance Puttable Bool where
    typeName = "Bool"

instance Puttable Word8 where
    typeName = "Word8"

instance Puttable Int8 where
    typeName = "Int8"

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
    cPut :: forall a. Puttable a => Maybe (a -> ByteString)
}

showWord8 :: Word8 -> String
showWord8 i = showHex (div i 16) "" <> showHex (mod i 16) ""

showBS :: ByteString -> String
showBS bs = intercalate " " $ fmap showWord8 $ BS.unpack bs

testCandidateItem :: TestItem -> Candidate -> IO ()
testCandidateItem (MkTestItem value) candidate = putStrLn $ " " <> cName candidate <> ": " <> case cPut candidate of
    Just f -> let
        bs = f value
        in showBS bs <> " (" <> (show $ BS.length bs) <> ")"
    Nothing -> "no"

foreignEncode :: forall a. Puttable a => Maybe (a -> ByteString)
foreignEncode = do
    MkDict <- foreignInstance @a
    return $ \value -> unsafePerformIO $ Foreign.alloca @a $ \ptr -> do
        Foreign.poke ptr value
        bb <- for [0 .. pred (Foreign.sizeOf value)] $ \i -> Foreign.peek $ Foreign.plusPtr ptr i
        return $ BS.pack bb

candidates :: [Candidate]
candidates =
    [ MkCandidate "foreign" foreignEncode
    , MkCandidate "cereal" $ Just $ Cereal.encode
    , MkCandidate "store" $ Just $ Store.encode
    , MkCandidate "binary" $ Just $ BS.toStrict . Binary.encode
    , MkCandidate "serialise" $ Just $ BS.toStrict . Serialise.serialise
    , MkCandidate "winery-schema" $ Just $ Winery.serialise
    , MkCandidate "winery-only" $ Just $ Winery.serialiseOnly
    , MkCandidate "flat" $ Just $ Flat.flat
    ]

items :: [TestItem]
items =
    [ MkTestItem @() ()
    , MkTestItem @Bool False
    , MkTestItem @Bool True
    , MkTestItem @Int32 0
    , MkTestItem @Int32 34
    , MkTestItem @Integer 0
    , MkTestItem @Integer 34
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
    , MkTestItem @String ""
    , MkTestItem @String "hello"
    , MkTestItem @Text ""
    , MkTestItem @Text "hello"
    ]

main :: IO ()
main = for_ items $ \item@(MkTestItem (value :: a)) -> do
    putStrLn $ typeName @a <> " " <> show value
    for_ candidates $ testCandidateItem item
