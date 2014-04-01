{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}

module Utility where

import Prelude
import qualified Control.Exception as E
import qualified GHC.Exception as GE
import Data.List.Split (splitOneOf)
import Data.Text (Text, pack)
import Data.Word (Word16)
import Database.Persist.MongoDB (MongoConf(..), MongoAuth(..))
import Network
import Network.Socket (PortNumber(..))
import System.Console.CmdArgs
import System.Environment

parseAndApplyMongoDBUrl :: MongoConf -> String -> MongoConf
parseAndApplyMongoDBUrl conf url =
    let tokens   = splitOneOf ":@/" url
        user     = pack (tokens !! 3)
        password = pack (tokens !! 4)
        host     = pack (tokens !! 5)
        port     = PortNumber $ PortNum $ read (tokens !! 6) -- PortNumを使うのは正しくない気がしているが、他の方法が見付からず・・・
        database = pack (tokens !! 7)
    in
    conf { mgDatabase = database
         , mgHost     = host
         , mgPort     = port
         , mgAuth     = Just $ MongoAuth user password
         }

main = lookupMongoDBUrlFromArgs' >>= print

lookupEnv :: String -> IO (Maybe Text)
lookupEnv key = (getEnv key >>= return . Just . pack) `E.catch` (\(_::GE.SomeException) -> return Nothing)

{--
lookupMongoDBUrlFromArgs' :: IO (Maybe String)
lookupMongoDBUrlFromArgs' = do
    args <- getArgs
    --}

groupn :: [a] -> [(a,a)]
groupn [] = []
groupn xs =
  let (xs1, xs2) = splitAt 2 xs
  in  (xs1 !! 0, xs1 !! 1) : groupn xs2

lookupMongoDBUrlFromArgs' :: IO (Maybe String)
lookupMongoDBUrlFromArgs' = getArgs >>= return . groupn >>= return . lookup "--mongodb-url"

lookupMongoDBUrlFromArgs :: IO (Maybe String)
lookupMongoDBUrlFromArgs = do
    args <- cmdArgs $ MongoDBArgs Nothing 80
    return $ mongodb_url args

data MongoDBArgs = MongoDBArgs { mongodb_url :: Maybe String
                               , port :: Int
                               } deriving (Data, Typeable, Show)
