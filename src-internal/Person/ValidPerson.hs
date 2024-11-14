-- | Internal module defining Person and exporting Person constructor,
-- for testing purposes
module Person.ValidPerson
  ( Person (..),
  )
where

import Data.Text (Text)
import Person.Types

data Person = Person
  { firstName :: Text,
    lastName :: Text,
    ssn :: SocialSecurityNumber,
    martialStatus :: MaritalStatus,
    usPhoneNumber :: PhoneNumber
  }
  deriving (Eq, Show)
