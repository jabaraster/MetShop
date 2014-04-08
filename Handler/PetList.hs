module Handler.PetList where

import Import

getPetListR :: Handler Html
getPetListR = do
    pets <- runDB $ selectList [] []
    defaultLayout $ do
      setTitle "ペット一覧"
      petListWidget pets

petListWidget :: [Entity Pet] -> Widget
petListWidget pets = $(widgetFile "petList")
