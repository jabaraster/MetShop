{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}

module Utility where

import Prelude
import Data.List.Split (splitOneOf)
import Data.Text (pack)
import Database.Persist.MongoDB (MongoConf(..), MongoAuth(..))
import Network (PortID(PortNumber))
-- import Network.Socket (PortNumber(..))
import System.Environment

import qualified Control.Exception as E
import qualified GHC.Exception as GE

parseAndApplyMongoDBUrl :: MongoConf -> String -> MongoConf
parseAndApplyMongoDBUrl conf url =
    let tokens   = splitOneOf ":@/" url
        user     = pack (tokens !! 3)
        password = pack (tokens !! 4)
        host     = pack (tokens !! 5)
        intPort  = read (tokens !! 6) :: Int
        port     = PortNumber $ fromIntegral intPort -- PortNumを使うのは正しくない気がしているが、他の方法が見付からず・・・
        database = pack (tokens !! 7)
    in
    conf { mgDatabase = database
         , mgHost     = host
         , mgPort     = port
         , mgAuth     = Just $ MongoAuth user password
         }

lookupMongoDBUrlFromEnv :: IO (Maybe String)
lookupMongoDBUrlFromEnv = lookupFromEnv "MONGODB_URL_ENV_NAME" >>= maybe (return Nothing) lookupFromEnv
  where
    lookupFromEnv :: String -> IO (Maybe String)
    lookupFromEnv key = (getEnv key >>= return . Just) `E.catch` (\(_::GE.SomeException) -> return Nothing)

{--
lookupMongoDBUrlFromArgs :: IO (Maybe String)
lookupMongoDBUrlFromArgs = getArgs >>= return . groupn >>= return . lookup "--mongodb-url"
  where
    groupn :: [a] -> [(a,a)]
    groupn [] = []
    groupn xs =
      let (xs1, xs2) = splitAt 2 xs
      in  (xs1 !! 0, xs1 !! 1) : groupn xs2
--}

