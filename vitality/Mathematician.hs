{-# LANGUAGE OverloadedStrings #-}

module Mathematician where

import Data.Text.Prettyprint.Doc
import Data.Aeson
import qualified Data.Text as T

data Mathematician = Mathematician
    { name :: T.Text
    , nationality :: T.Text
    , born :: Int
    , died :: Maybe Int
    } deriving (Show)

instance FromJSON Mathematician where
    parseJSON (Object v) = Mathematician
        <$> (v .: "name")
        <*> (v .: "nationality")
        <*> (v .: "born")
        <*> (v .:? "died")

unjust :: Maybe a -> a
unjust (Just a) = a

greet m = (show.name) m ++" was born in the year "++ (show.born) m

instance Pretty Mathematician where
  pretty m =
    vsep
      [ "Mathematician {"
      , indent 2 ("name = " <> pretty (name m))
      , indent 2 ("nationality = " <> pretty (nationality m))
      , indent 2 ("born = " <> pretty (born m))
      , indent 2 ("died = " <> pretty (died m))
      , "}"
      ]