module Widgets where

import Foundation
import Settings

userWidget :: Widget -> Widget
userWidget widget = $(widgetFile "user-form-")
