module Main (main) where

import Data.Foldable
import Data.Int
import Data.Word
import Data.ByteString as BS
import qualified Data.Serialize as Cereal

class (Show a,Cereal.Serialize a) => Puttable a where
    typeName :: String

instance Puttable Int32 where
    typeName = "Int32"

instance Puttable Word32 where
    typeName = "Word32"

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
    [ MkCandidate "cereal" $ \val -> Cereal.runPut $ Cereal.put val
    ]

items :: [TestItem]
items =
    [ MkTestItem @Int32 34
    ]

main :: IO ()
main = for_ items $ \item@(MkTestItem (value :: a)) -> do
    putStrLn $ typeName @a <> " (" <> show value <> ")"
    for_ candidates $ testCandidateItem item
