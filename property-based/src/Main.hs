{-# LANGUAGE DeriveGeneric #-}

import           Test.QuickCheck
import qualified V2

main :: IO ()
main = do
  quickCheck V2.prop_serialize_parse
  quickCheck V2.prop_serialize_returns_json
