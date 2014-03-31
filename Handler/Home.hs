{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    session <- getSession
    defaultLayout $ do
        setTitle "Welcome To MetShop!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = undefined
