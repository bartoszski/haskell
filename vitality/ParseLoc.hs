{-# LANGUAGE OverloadedStrings #-}

module ParseLoc where

import Data.Text.Prettyprint.Doc
import Data.Aeson
import  Data.Text (Text,append,pack)
import Text.Read  (readMaybe) 
import Data.Maybe (fromMaybe )
import Data.List.Split (splitOn)
import Data.List (intercalate)
import qualified Data.Text as T (Text,append,pack,unpack,head,take,splitOn,intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as V
import GHC.Generics

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
  } deriving (Show,Generic)
   
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
  } deriving (Show,Generic)
  
instance Pretty Location' where
  pretty loc =
    vsep
      [ "Location {"
      , indent 2 ("database         = " <> pretty (database' loc))
      , indent 2 ("dbdesc           = " <> pretty (dbdesc' loc))
      , indent 2 ("host             = " <> pretty (host' loc))   
      , indent 2 ("loc_class        = " <> pretty (loc_class' loc))
      , indent 2 ("loc_name         = " <> pretty (loc_name' loc))
      , indent 2 ("loc_directory    = " <> pretty (loc_directory' loc))     
      , indent 2 ("loc_remote_node  = " <> pretty (loc_remote_node' loc))
      , indent 2 ("loc_remote_login = " <> pretty (loc_remote_login' loc))
      , indent 2 ("loc_remote_port  = " <> pretty (loc_remote_port' loc))
      , indent 2 ("loc_db_user      = " <> pretty (loc_db_user' loc))
      , indent 2 ("loc_description  = " <> pretty (loc_description' loc))
      , indent 2 ("db_node_name     = " <> pretty (db_node_name loc))
      , indent 2 ("db_instance      = " <> pretty (db_instance loc))
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


--- Redshift parser

parseRedshift :: Location  -> [T.Text]
parseRedshift l  = getUSer l : (map T.pack . concat . filterCols . fromMaybe []. parseJson .  getJsonStr)  l  
   where
       getJsonStr  = BSL.pack. T.unpack .last . T.splitOn "=" . loc_db_name  -- get loc_db_name and return string after first '='
       parseJson s = decode s :: Maybe [[String]]            -- parse to list
       filterCols  = map tail .  filter (\[k, _] -> k `elem` ["db_node", "db_name"])
       getUSer     = head . T.splitOn "/" . loc_db_user
--- Kafka parser

getKafkaString   = BSL.pack .T.unpack . last . T.splitOn "=" 

valueFromString :: BSL.ByteString -> Maybe Value
valueFromString  s = decode  s :: Maybe Value

unArray :: Value -> [Value]
unArray (Array x )  = V.toList x

valueToText :: Value ->  T.Text
valueToText val = case toJSON val of
     String txt ->  txt

listFromArray    = T.intercalate ";" . map valueToText . unArray

unString :: [Value] -> [T.Text]
unString  [String x , String y] = [x,  y]
unString  [String x , Array y]  = [x , listFromArray (Array y) ]

getval :: T.Text -> [[T.Text]] -> T.Text
getval key records =
    case filter (\[k, _] -> k == key) records of
        [] -> ""  -- Return an empty string when no match is found
        [[_, value]] -> value
        _ -> error "Multiple matches found"  -- Handle this case as needed

getvals :: [T.Text] -> [[T.Text]] -> [T.Text]
getvals keys lst  = map (`getval` lst) keys

parseKafkaString =    map (unString . unArray) . unArray . unjust .  valueFromString

--- PARSER 

parseLoc :: Location  -> Location'
parseLoc l 
    | loc_class l == "file" && ((T.head . loc_directory)   l  == '/' )     = file_locdir l
    | loc_class l == "file" && ((T.take 4 . loc_directory) l  == "s3s:" )  = file_s3s l
    | loc_class l == "file" && ((T.take 5 . loc_directory) l  == "sftp:")  = file_sftp l
    | loc_class l `elem` ["mysql","greenplum","postgresql"]                = mysql l 
    | loc_class l == "sqlserver"                                           = sqlserver l
    | loc_class l == "teradata"                                            = teradata l 
    | loc_class l == "salesforce"                                          = salesforce l 
    | loc_class l == "redshift"                                            = redsfhit l  
    | loc_class l == "kafka"                                               = kafka l  
    | otherwise   = loc2Location l ""  ["###","###","###"]                                           
     where
        file_locdir x = loc2Location x  "_locdir"  [loc_remote_login  x ,loc_remote_node x, loc_directory x]
        --
        getBacket  =  head . T.splitOn "/"  . last. T.splitOn "@" .loc_directory
        getPrefix  =  T.intercalate  "/" .tail . T.splitOn "/" . last. T.splitOn "@" .loc_directory
        file_s3s x =  loc2Location x "_s3s" ["###",getBacket x ,getPrefix x]
        --
        getsftpNode   = getBacket
        getsftpFolder =  getPrefix
        file_sftp x   = loc2Location x "_sftp" ["###",getsftpNode x ,getsftpFolder x]
        --
        getNodeMysql  =  head . T.splitOn "~" . loc_db_name
        getInstMysql  =  last . T.splitOn "~" . loc_db_name
        getUserMysql  =  head . T.splitOn "/" . loc_db_user
        mysql x =  loc2Location x ""   [ getUserMysql x , getNodeMysql x, getInstMysql x]
        --
        getNodeMsSql  =  head . T.splitOn "\\" . loc_db_name
        getInstMsSql  =  last . T.splitOn "\\" . loc_db_name
        getUserMsSql  =  getUserMysql
        sqlserver x   = loc2Location x ""   [getUserMsSql x, getNodeMsSql x,getInstMsSql x]
        --
        getUserTd     =  getUserMysql
        teradata x = loc2Location x ""   [getUserTd x, loc_db_name x,getUserTd x]
        --
        getNodeSf     = last .T.splitOn "//" . loc_directory
        getInstSf     = last .T.splitOn "@" . head . T.splitOn "/" . loc_db_user
        getUserSf     = head .T.splitOn "@" . head . T.splitOn "/" . loc_db_user
        salesforce x  = loc2Location x ""   [getUserSf x, getNodeSf x,getInstSf x]
        --
        redsfhit x    = loc2Location x ""  (parseRedshift x)
        --
        parseKafka =  getvals ["ssl_key","urls","dummy"] . parseKafkaString . getKafkaString .loc_db_name
        kafka x    =  loc2Location x ""  (parseKafka x)
       