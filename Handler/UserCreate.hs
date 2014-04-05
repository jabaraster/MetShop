module Handler.UserCreate where

import Import

getUserCreateR :: Handler Html
getUserCreateR = loginOperation (redirect HomeR) getUserCreateRCore
{--
    mId <- lookupSession "_ID"
    case mId of
      Nothing -> redirect HomeR
      _       -> do
                   (widget, enctype) <- generateFormPost userForm
                   defaultLayout $ do
                     setTitle "ユーザ追加"
                     $(widgetFile "user-form-")
--}

getUserCreateRCore :: Handler Html
getUserCreateRCore = do
     (widget, enctype) <- generateFormPost userForm
     defaultLayout $ do
       setTitle "ユーザ追加"
       $(widgetFile "user-form-")

postUserCreateR :: Handler Html
postUserCreateR = do
    ((result, widget), enctype) <- runFormPost userForm
    case result of
      FormSuccess user -> do
                            _ <- runDB $ insert user
                            redirect UserListR
      _ -> defaultLayout $ do
             setTitle "ユーザ追加"
             $(widgetFile "user-form-")

userForm :: Form User
userForm = renderBootstrap $ User
    <$> areq textField     "ユーザID"   Nothing
    <*> aopt passwordField "パスワード" Nothing
