-- | Types are re-exported in (public) Person module, but only the validating
-- constructor methods are exported to prevent invalid data from being represented.
-- The internal constructors are exported here to allow internal code to access,
-- e.g. tests.
module Person.Types
  ( MaritalStatus (..),
    PhoneNumber (..),
    SocialSecurityNumber (..),
    PersonError (..),
    mkPhoneNumber,
    mkSocialSecurityNumber,
  )
where

import Data.Digits (digits, unDigits)
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
-- This is stubbed out and only checks to make sure the number has ten digits total.
-- A full implemention should validate the area code and exchange as well.
mkPhoneNumber :: Int -> Validation [PersonError] PhoneNumber
mkPhoneNumber num =
  let base = 10
      dgts = digits base num
      anyNegative = any (< 0) dgts
      wrongNumDigits = length dgts /= 10
      pref = fromIntegral . unDigits base $ take 3 dgts
      exch = fromIntegral . unDigits base $ take 3 (drop 3 dgts)
      rest = fromIntegral . unDigits base $ drop 6 dgts
   in if anyNegative || wrongNumDigits
        then Failure [InvalidPhoneNumber]
        else Success $ PhoneNumber pref exch rest

-- | Construct a valid Social Security Number
-- A social security number must have 9 digits
mkSocialSecurityNumber :: Int -> Validation [PersonError] SocialSecurityNumber
mkSocialSecurityNumber num =
  let dgts = digits 10 num
      anyNegative = any (< 0) dgts
      wrongNumDigits = length dgts /= 9
   in if anyNegative || wrongNumDigits
        then Failure [InvalidSSN]
        else Success (SocialSecurityNumber num)
