module Widgets where

import Foundation
import Settings
import Utility

menuWidget :: Widget
menuWidget = do
    login <- isLogin
    $(widgetFile "menu-")
