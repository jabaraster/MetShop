module Handler.User where

import Import

getUserR :: UserId -> Handler Html
getUserR userId = do
    user <- runDB $ get404 userId
    defaultLayout $ do
      setTitle "ユーザ詳細"
      userDetailWidget user

userDetailWidget :: User -> Widget
userDetailWidget user = $(widgetFile "userDetail")
