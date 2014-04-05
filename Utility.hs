{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable, NoMonomorphismRestriction #-}

module Utility where

import Prelude
import Data.List.Split (splitOneOf)
import Data.Text (pack)
import Database.Persist.MongoDB (MongoConf(..), MongoAuth(..))
import Network (PortID(PortNumber))
import System.Environment

import qualified Control.Exception as E (catch)
import qualified GHC.Exception as GE

import Yesod.Core.Handler

import Yesod.Core

parseAndApplyMongoDBUrl :: MongoConf -> String -> MongoConf
parseAndApplyMongoDBUrl conf url =
    let tokens   = splitOneOf ":@/" url -- 本当はNetwork.URI.parseURIでパースした方が安全なのだが、細かい文字列処理が増えて面倒なので、ここではsplitOneOfを使う.
        user     = pack (tokens !! 3)
        password = pack (tokens !! 4)
        host     = pack (tokens !! 5)
        port     = PortNumber $ fromIntegral (read (tokens !! 6) :: Int)
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

isLogin :: MonadHandler m => m Bool
isLogin = lookupSession "_ID" >>= maybe (return False) (\_ -> return True)

loginOperation :: MonadHandler m => m a -> m a -> m a
loginOperation f g = do
    b <- isLogin
    case b of
        False -> f
        True  -> g
