module Effect.Alert (alert, confirm, prompt) where

import Prelude ((<<<), map)
import Effect (Effect)
import Data.Unit (Unit)
import Data.Window (Window)
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe)

foreign import alert :: String -> Window -> Effect Unit
foreign import confirm :: String -> Window -> Effect Boolean
foreign import _prompt :: String -> Window -> Effect (Nullable String)

prompt :: String -> Window -> Effect (Maybe String)
prompt msg = map toMaybe <<< _prompt msg
