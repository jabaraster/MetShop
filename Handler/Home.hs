{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    session <- getSession
    mId     <- lookupSession "_ID"
    defaultLayout $ do
        setTitle "Welcome To MetShop!"
        $(widgetFile "home")

postHomeR :: Handler Html
postHomeR = undefined
