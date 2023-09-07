{-# LANGUAGE OverloadedStrings #-}

module ParseLoc where

import Data.Text.Prettyprint.Doc
import Data.Aeson
import qualified Data.Text as T
import  Data.Text (Text,append,pack)

-- Define a data type to represent the structure of the JSON

data Location = Location
  { database:: Text
  , dbdesc:: Text
  , host:: Text
  , loc_name :: Text
  , loc_class :: Text
  , loc_directory :: Text
  , loc_remote_node :: Text
  , loc_remote_login :: Text
  , loc_remote_pwd :: Text
  , loc_remote_port :: Int
  , loc_db_name :: Text
  , loc_db_user :: Text
  , loc_description :: Text
  } deriving (Show)
   
instance FromJSON Location where
    parseJSON (Object v) = Location
        <$> (v .: "database")
        <*> (v .: "dbdesc")
        <*> (v .: "host")
        <*> (v .: "loc_name")
        <*> (v .: "loc_class")
        <*> (v .: "loc_directory")
        <*> (v .: "loc_remote_node")
        <*> (v .: "loc_remote_login")
        <*> (v .: "loc_remote_pwd")
        <*> (v .: "loc_remote_port")
        <*> (v .: "loc_db_name")
        <*> (v .: "loc_db_user")
        <*> (v .: "loc_description")
        
instance Pretty Location where
  pretty loc =
    vsep
      [ "Location {"
      , indent 2 ("database = " <> pretty (database loc))
      , indent 2 ("dbdesc = " <> pretty (dbdesc loc))
      , indent 2 ("host = " <> pretty (host loc))   
      , indent 2 ("loc_name = " <> pretty (loc_name loc))
      , indent 2 ("loc_class = " <> pretty (loc_class loc))
      , indent 2 ("loc_directory = " <> pretty (loc_directory loc))
      , indent 2 ("loc_remote_node = " <> pretty (loc_remote_node loc))
      , indent 2 ("loc_remote_login = " <> pretty (loc_remote_login loc))
      , indent 2 ("loc_remote_pwd = " <> pretty (loc_remote_pwd loc))
      , indent 2 ("loc_remote_port = " <> pretty (loc_remote_port loc))
      , indent 2 ("loc_db_name = " <> pretty (loc_db_name loc))
      , indent 2 ("loc_db_user = " <> pretty (loc_db_user loc))
      , indent 2 ("loc_description = " <> pretty (loc_description loc))
      , "}"
      ]


data Location' = Location'
  { database':: Text
  , dbdesc':: Text
  , host':: Text
  , loc_name' :: Text
  , loc_class' :: Text
  , loc_directory'::Text
  , loc_remote_node' :: Text  -- agent used 
  , loc_remote_login' :: Text -- user agent 
  , loc_remote_port' :: Text     -- agent port
  , loc_db_user' :: Text
  , loc_description' :: Text
  , db_node_name :: Text      -- infra location where data is stored
  , db_instance :: Text  -- logical locaton
  } deriving (Show)
  
instance Pretty Location' where
  pretty loc =
    vsep
      [ "Location {"
      , indent 2 ("database         = " <> pretty (database' loc))
      , indent 2 ("dbdesc           = " <> pretty (dbdesc' loc))
      , indent 2 ("host             = " <> pretty (host' loc))   
      , indent 2 ("loc_class        = " <> pretty (loc_class' loc))
      , indent 3 ("loc_name         = " <> pretty (loc_name' loc))
      , indent 3 ("loc_directory    = " <> pretty (loc_directory' loc))     
      , indent 3 ("loc_remote_node  = " <> pretty (loc_remote_node' loc))
      , indent 3 ("loc_remote_login = " <> pretty (loc_remote_login' loc))
      , indent 3 ("loc_remote_port  = " <> pretty (loc_remote_port' loc))
      , indent 3 ("loc_db_user      = " <> pretty (loc_db_user' loc))
      , indent 3 ("loc_description  = " <> pretty (loc_description' loc))
      , indent 3 ("db_node_name     = " <> pretty (db_node_name loc))
      , indent 3 ("db_instance      = " <> pretty (db_instance loc))
      , "}"
      ]  

loc2Location :: Location -> Text -> [Text] -> Location'
loc2Location x classSuffix [db_user,node_name,db_instance] = 
                              Location' {  database'= database x
                                         , dbdesc'= dbdesc x
                                         , host'=  host x
                                         , loc_name'  = loc_name x 
                                         , loc_class' =     loc_class x `append` classSuffix
                                         , loc_directory' = loc_directory x
                                         , loc_remote_node'  = loc_remote_node x
                                         , loc_remote_login' = loc_remote_login x
                                         , loc_remote_port'  = (pack .show . loc_remote_port) x
                                         , loc_db_user'      = db_user                  ---- is it agent user or db user?
                                         , loc_description'  = loc_description x
                                         , db_node_name      = node_name
                                         , db_instance       = db_instance
                                         }


unjust :: Maybe a -> a
unjust (Just a) = a