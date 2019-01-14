module Data.Window (Window, window) where

import Effect (Effect)

foreign import data Window :: Type
foreign import window :: Effect Window
