{-# LANGUAGE DeriveGeneric #-}

import           Test.QuickCheck
import qualified V2

main :: IO ()
main = quickCheck V2.prop_serialize_parse
