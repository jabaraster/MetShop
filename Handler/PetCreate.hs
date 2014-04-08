module Handler.PetCreate where

import Import
import Yesod.Form.Bootstrap3

getPetCreateR :: Handler Html
getPetCreateR = do
    (widget, _) <- generateFormPost petForm
    defaultLayout $ do
        setTitle "ペット登録"
        petFormWidget widget

postPetCreateR :: Handler Html
postPetCreateR = do
    ((result, widget), _) <- runFormPost petForm
    case result of
      FormSuccess pet -> do
                           _ <- runDB $ insert pet
                           redirect PetListR
      _ -> defaultLayout $ do
             setTitle "ペット追加"
             petFormWidget widget

petForm :: Form Pet
petForm = renderBootstrap3 BootstrapInlineForm $ Pet
            <$> areq textField "ペットの名前" Nothing

petFormWidget :: Widget -> Widget
petFormWidget widget = $(widgetFile "petForm-")
