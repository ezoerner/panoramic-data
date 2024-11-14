{-# LANGUAGE OverloadedStrings #-}

import Person

{-
>>> validatePerson $ EditPerson "John" "Doe" 123456789 Single 1234567890
Success (Person {firstName = "John", lastName = "Doe", ssn = 123-45-6789, martialStatus = Single, usPhoneNumber = (123) 456-7890})

>>> validatePerson $ EditPerson "" "" 1234 Married 12345
Failure [EmptyFirstName,EmptyLastName,InvalidSSN,InvalidPhoneNumber]

>>> validatePerson $ EditPerson "John" "Doe" (-123456789) Married (-1234567890)
Failure [InvalidSSN,InvalidPhoneNumber]
-}
main :: IO ()
main = do
  print . validatePerson $ EditPerson "John" "Doe" 123456789 Single 1234567890
  print . validatePerson $ EditPerson "" "" 1234 Married 12345
  print . validatePerson $ EditPerson "John" "Doe" (-123456789) Married (-1234567890)
