module Data.AddressBook.Validation where

import Prelude
import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), address, person, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, unV, invalid)
import Partial.Unsafe (unsafePartial)

type Errors = Array String

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
nonEmpty _     _  = pure unit

-- Exercise 2
notAllSpaces :: String -> String -> V Errors Unit
notAllSpaces field = matches field hasNonWhitespaceRegex
  where
    hasNonWhitespaceRegex :: Regex
    hasNonWhitespaceRegex =
      unsafePartial
        case regex "[^\\s]" noFlags of
          Right r -> r

arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid ["Field '" <> field <> "' must contain at least one value"]
arrayNonEmpty _     _  = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | length value /= len = invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _     _   _     = pure unit

phoneNumberRegex :: Regex
phoneNumberRegex =
  unsafePartial
    case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
      Right r -> r

matches :: String -> Regex -> String -> V Errors Unit
matches _     regex value | test regex value = pure unit
matches field _     _     = invalid ["Field '" <> field <> "' did not match the required format"]

-- Exercise 1
addressRegex :: Regex
addressRegex =
  unsafePartial
    -- U.S. state abbreviations all comprise of exactly 2
    -- uppercase English letters, no more, no less
    -- Source: https://www.50states.com/abbreviations.htm
    case regex "^[A-Z]{2}$" noFlags of
      Right r -> r

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  -- Exercise 2 modification
  address <$> (notAllSpaces "Street" o.street *> pure o.street)
          -- Exercise 2 modification
          <*> (notAllSpaces "City"   o.city   *> pure o.city)
          -- Exercise 1 modification
          <*> (lengthIs "State" 2 o.state *> matches "State" addressRegex o.state *> pure o.state)

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o."type"
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  -- Exercise 2 modification
  person <$> (notAllSpaces "First Name" o.firstName *> pure o.firstName)
         -- Exercise 2 modification
         <*> (notAllSpaces "Last Name"  o.lastName  *> pure o.lastName)
         <*> validateAddress o.homeAddress
         <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p
