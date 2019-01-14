module Effect.WebStorage (getItem, setItem, removeItem) where

import Prelude (Unit, (<<<), map)

import Data.Window.WebStorage (WebStorage)
import Effect (Effect)
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe)

foreign import _getItem :: String -> WebStorage -> Effect (Nullable String)
foreign import setItem :: String -> String -> WebStorage -> Effect Unit
foreign import removeItem :: String -> WebStorage -> Effect Unit

getItem :: String -> WebStorage -> Effect (Maybe String)
getItem key = map toMaybe <<< _getItem key
