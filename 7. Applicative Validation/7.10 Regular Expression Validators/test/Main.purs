module Test.Main where

import Prelude

import Effect
import Effect.Console
import Data.AddressBook (examplePerson)
import Data.AddressBook.Validation (validatePerson)

main :: Effect Unit
main = log $ show $ validatePerson examplePerson
