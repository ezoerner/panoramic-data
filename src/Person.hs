-- | This is the "public module" which is imported by the application.
-- Only the validating constructors are exported to make sure no invalid data
-- can be represented.
--
-- A Person is constructed by calling 'validatePerson'.
module Person
  ( module Person.EditPerson,
    MaritalStatus (..),
    PhoneNumber,
    SocialSecurityNumber,
    Person,
    validatePerson,
  )
where

import Data.Either.Validation (Validation (..))
import Data.Text (Text)
import Data.Text qualified as T
import Person.EditPerson (EditPerson (..))
import Person.Types
  ( MaritalStatus (..),
    PersonError (EmptyFirstName, EmptyLastName),
    PhoneNumber,
    SocialSecurityNumber,
    mkPhoneNumber,
    mkSocialSecurityNumber,
  )
import Person.ValidPerson (Person (..))

-- | Construct a valid Person or return a Failure with a list of errors
validatePerson :: EditPerson -> Validation [PersonError] Person
validatePerson (EditPerson fn ln ss ms ph) =
  Person
    <$> mkFirstName fn
    <*> mkLastName ln
    <*> mkSocialSecurityNumber ss
    <*> Success ms
    <*> mkPhoneNumber ph

mkFirstName :: Text -> Validation [PersonError] Text
mkFirstName txt
  | T.null txt = Failure [EmptyFirstName]
  | otherwise = Success txt

mkLastName :: Text -> Validation [PersonError] Text
mkLastName txt
  | T.null txt = Failure [EmptyLastName]
  | otherwise = Success txt