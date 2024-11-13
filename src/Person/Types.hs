-- | Types are re-exported in Person module, but only the validating constructor
-- methods are exported to prevent invalid data from being respresented.
module Person.Types
  ( MaritalStatus (..),
    PhoneNumber,
    SocialSecurityNumber,
    PersonError (..),
    mkPhoneNumber,
    mkSocialSecurityNumber,
  )
where

import Data.Digits (digits)
import Data.Either.Validation (Validation (..))
import Data.List (intercalate)

-- could include others e.g. Widowed, Divorced, etc.
data MaritalStatus = Single | Married
  deriving (Eq, Show, Read)

data PhoneNumber = PhoneNumber
  { areaCode :: Word, -- 3 digit unsigned integer
    prefix :: Word, -- 3 digit unsigned integer
    number :: Word -- 4 digit unsigned integer
  }
  deriving (Eq)

instance Show PhoneNumber where
  show x =
    "("
      <> show
        (areaCode x)
      <> ") "
      <> show (prefix x)
      <> "-"
      <> show (number x)

newtype SocialSecurityNumber = SocialSecurityNumber Int
  deriving (Eq)

instance Show SocialSecurityNumber where
  show (SocialSecurityNumber n) =
    let allDigits = concatMap show (digits 10 n)
        part1 = take 3 allDigits
        part2 = take 2 (drop 3 allDigits)
        part3 = drop 5 allDigits
     in intercalate "-" [part1, part2, part3]

data PersonError
  = EmptyFirstName
  | EmptyLastName
  | InvalidAreaCode
  | InvalidExchange
  | InvalidPhoneNumber
  | InvalidSSN
  deriving (Eq, Show)

-- etc

-- | Construct a valid US PhoneNumber, or return Failure with a list of PersonErrors
-- this should decompose the number into area code, exchange, and number and
-- validate each part
-- Note this is stubbed out and not yet implemented
mkPhoneNumber :: Int -> Validation [PersonError] PhoneNumber
mkPhoneNumber _ = Success $ PhoneNumber 123 555 1212

-- | Construct a valid Social Security Number
-- A social security number must have 9 digits
-- Note this is stubbed out and not yet implemented
mkSocialSecurityNumber :: Int -> Validation [PersonError] SocialSecurityNumber
mkSocialSecurityNumber num = Success $ SocialSecurityNumber num