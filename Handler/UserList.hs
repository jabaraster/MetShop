module Handler.UserList where

import Import

getUserListR :: Handler Html
getUserListR = do
    users <- runDB $ selectList [] [Asc UserIdent]
    defaultLayout $ do
      setTitle "ユーザ一覧"
      userListWidget users

userListWidget :: [Entity User] -> Widget
userListWidget users = do
    login <- isLogin
    $(widgetFile "userList")
