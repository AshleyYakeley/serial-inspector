module Main (main) where

import Data.Foldable
import Data.Int
import Data.Word
import Data.ByteString as BS
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

instance Puttable Int32 where
    typeName = "Int32"

instance Puttable Word32 where
    typeName = "Word32"

instance Puttable Integer where
    typeName = "Integer"

instance Puttable Float where
    typeName = "Float"

instance Puttable Double where
    typeName = "Double"

data TestItem = forall a. Puttable a => MkTestItem a

data Candidate = MkCandidate {
    cName :: String,
    cPut :: forall a. Puttable a => a -> ByteString
}

testCandidateItem :: TestItem -> Candidate -> IO ()
testCandidateItem (MkTestItem value) candidate = do
    putStrLn $ " " <> cName candidate <> ": " <> (show $ BS.length $ cPut candidate value)

candidates :: [Candidate]
candidates =
    [ MkCandidate "cereal" $ Cereal.encode
    , MkCandidate "serialise" $ BS.toStrict . Serialise.serialise
    , MkCandidate "binary" $ BS.toStrict . Binary.encode
    , MkCandidate "winery" $ Winery.serialise
    ]

items :: [TestItem]
items =
    [ MkTestItem @() ()
    , MkTestItem @Bool False
    , MkTestItem @Bool True
    , MkTestItem @Int32 0
    , MkTestItem @Integer 0
    , MkTestItem @Float 0
    , MkTestItem @Double 0
    , MkTestItem @Int32 34
    , MkTestItem @Integer 34
    , MkTestItem @Float 34
    , MkTestItem @Double 34
    ]

main :: IO ()
main = for_ items $ \item@(MkTestItem (value :: a)) -> do
    putStrLn $ typeName @a <> " " <> show value
    for_ candidates $ testCandidateItem item
