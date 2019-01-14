module Data.Window.WebStorage (WebStorage, localStorage, sessionStorage) where

import Effect (Effect)
import Data.Window (Window)

foreign import data WebStorage :: Type
foreign import localStorage :: Window -> Effect WebStorage
foreign import sessionStorage :: Window -> Effect WebStorage
