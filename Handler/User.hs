module Handler.User where

import Import

getUserR :: UserId -> Handler Html
getUserR userId = do
    user <- runDB $ get404 userId
    defaultLayout $ do
      setTitle ""
      $(widgetFile "user-detail")
