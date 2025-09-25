module GHCiPrint (iprint) where

import Data.Typeable (Typeable, cast)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

-- Print Strings/Text/ByteString "raw" (no quotes, real newlines); everything else via Show
iprint :: forall a. (Typeable a, Show a) => a -> IO ()
iprint x =
  case (cast x :: Maybe String) of
    Just s -> putStrLn s
    Nothing ->
      case (cast x :: Maybe T.Text) of
        Just t -> TIO.putStrLn t
        Nothing ->
          case (cast x :: Maybe B.ByteString) of
            Just b -> BC.putStrLn b
            Nothing -> print x
