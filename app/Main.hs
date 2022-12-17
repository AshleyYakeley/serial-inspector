module Main (main) where

import Data.Int
--import Data.Word
import Data.ByteString as BS
import qualified Data.Serialize as Cereal

putCereal :: ByteString
putCereal = Cereal.runPut $ do
    Cereal.put (34 :: Int32)

main :: IO ()
main = do
    putStrLn $ "cereal: " <> show (BS.length putCereal)
