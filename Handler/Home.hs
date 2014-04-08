{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Welcome To MetShop!"
        $(widgetFile "home")

postHomeR :: Handler Html
postHomeR = notFound
