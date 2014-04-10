module Handler.PetCreate where

import Import
import Yesod.Form.Bootstrap3

getPetCreateR :: Handler Html
getPetCreateR = do
    (widget, _) <- generateFormPost petForm
    defaultLayout $ do
        petFormWidget widget

postPetCreateR :: Handler Html
postPetCreateR = do
  ((result, widget), _) <- runFormPost petForm
  case result of
    FormSuccess petInput -> do
                              maybeCategoryId <- getOrInsertCategory $ petInputCategory petInput
                              _ <- runDB $ insert $ Pet { petName = (petInputName petInput)
                                                        , petPetCategory = maybeCategoryId
                                                        }
                              redirect PetListR
    _ -> defaultLayout $ do
           petFormWidget widget
  where
    getOrInsertCategory Nothing = return Nothing
    getOrInsertCategory (Just petCategoryName) = do
        maybePetCategory <- runDB $ getBy $ UniquePetCategoryName petCategoryName
        case maybePetCategory of
            Nothing -> do
                         key <- runDB $ insert $ PetCategory petCategoryName
                         return $ Just key
            Just (Entity key _) -> return $ Just key

petForm :: Form PetInput
petForm = renderBootstrap3 BootstrapInlineForm $ PetInput
            <$> areq textField "ペットの名前" Nothing
            <*> aopt textField "ペット分類"   Nothing

petFormWidget :: Widget -> Widget
petFormWidget widget = do
    setTitle "ペット登録"
    $(widgetFile "petForm-")

data PetInput = PetInput {
                  petInputName :: Text
                  , petInputCategory :: Maybe Text
                }
