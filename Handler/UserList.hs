module Handler.UserList where

import Import

getUserListR :: Handler Html
getUserListR = do
    users <- runDB $ selectList [] [Asc UserIdent]
    defaultLayout $ do
      setTitle "ユーザ一覧"
      $(widgetFile "user-list")
