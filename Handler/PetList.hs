{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, NoMonomorphismRestriction #-}

module Handler.PetList where

import Import

getPetListR :: Handler Html
getPetListR = do
    pets0 <- runDB $ selectList [] [Asc PetName]
    categories <- mapM getPetCategory pets0
    pets <- return $ zip pets0 categories
    defaultLayout $ do
      petListWidget pets
  where
    getPetCategory (Entity _ pet) = do
        mCategoryId <- return $ petPetCategory pet
        case mCategoryId of
            Nothing -> return Nothing
            Just categoryId -> runDB $ get categoryId

petListWidget :: [(Entity Pet, Maybe PetCategory)] -> Widget
petListWidget pets = do
    setTitle "ペット一覧"
    $(widgetFile "petList")

