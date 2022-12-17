module Main (main) where

import Numeric
import Data.List (intercalate)
import Data.Foldable
import Data.Int
import Data.Word
import Data.ByteString as BS hiding (intercalate)
import qualified Data.Serialize as Cereal
import qualified Codec.Serialise as Serialise
import qualified Data.Binary as Binary
import qualified Codec.Winery as Winery

class (
        Show a,
        Cereal.Serialize a,
        Serialise.Serialise a,
        Binary.Binary a,
        Winery.Serialise a
    ) => Puttable a where
    typeName :: String

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

instance Puttable Float where
    typeName = "Float"

instance Puttable Double where
    typeName = "Double"

instance Puttable a => Puttable [a] where
    typeName = "[" <> typeName @a <> "]"

data TestItem = forall a. Puttable a => MkTestItem a

data Candidate = MkCandidate {
    cName :: String,
    cPut :: forall a. Puttable a => a -> ByteString
}

showWord8 :: Word8 -> String
showWord8 i = showHex (div i 16) "" <> showHex (mod i 16) ""

showBS :: ByteString -> String
showBS bs = intercalate " " $ fmap showWord8 $ BS.unpack bs

testCandidateItem :: TestItem -> Candidate -> IO ()
testCandidateItem (MkTestItem value) candidate = let
    bs = cPut candidate value
    in putStrLn $ " " <> cName candidate <> ": " <> showBS bs <> " (" <> (show $ BS.length bs) <> ")"

candidates :: [Candidate]
candidates =
    [ MkCandidate "cereal" $ Cereal.encode
    , MkCandidate "binary" $ BS.toStrict . Binary.encode
    , MkCandidate "serialise" $ BS.toStrict . Serialise.serialise
    , MkCandidate "winery" $ Winery.serialise
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
    , MkTestItem @Double 0
    , MkTestItem @Double 34
    , MkTestItem @[()] []
    , MkTestItem @[()] [()]
    , MkTestItem @[Word8] []
    , MkTestItem @[Word8] [0,1,255]
    ]

main :: IO ()
main = for_ items $ \item@(MkTestItem (value :: a)) -> do
    putStrLn $ typeName @a <> " " <> show value
    for_ candidates $ testCandidateItem item
