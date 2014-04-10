module Handler.UserCreate where

import Import
import Yesod.Form.Bootstrap3

getUserCreateR :: Handler Html
getUserCreateR = do
    (widget, _) <- generateFormPost userForm
    defaultLayout $ do
      setTitle "ユーザ追加"
      userFormWidget widget

postUserCreateR :: Handler Html
postUserCreateR = do
    ((result, widget), _) <- runFormPost userForm
    case result of
      FormSuccess user -> do
                            _ <- runDB $ insert user
                            redirect UserListR
      _ -> defaultLayout $ do
             setTitle "ユーザ追加"
             userFormWidget widget

userForm :: Form User
userForm = renderBootstrap3 BootstrapBasicForm ( User
    <$> areq textField     "ユーザID"   Nothing
    <*> aopt passwordField "パスワード" Nothing
    )


userFormWidget :: Widget -> Widget
userFormWidget widget = $(widgetFile "userForm")
