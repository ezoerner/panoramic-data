-- | Represents an editable Person.
-- Data and constructor are re-exported in Person module to
-- make publicly available.
module Person.EditPerson (EditPerson (..)) where

import Data.Text (Text)
import Person.Types (MaritalStatus)

data EditPerson = EditPerson
  { firstName :: Text,
    lastName :: Text,
    ssn :: Int,
    maritalStatus :: MaritalStatus,
    usPhoneNumber :: Int
  }
