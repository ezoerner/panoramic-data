{-# LANGUAGE OverloadedStrings #-}

import Data.Either.Validation (Validation (..))
import Person
import Person.Types
import Person.ValidPerson
import System.Exit qualified as Exit
import Test.HUnit

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

tests :: Test
tests =
  TestList
    [ TestLabel "happy path" happyPathTest,
      TestLabel "empty names" emptyNamesTest,
      TestLabel "nums too short" numsTooShortTest,
      TestLabel "negative nums" negativeNumsTest
    ]

{-
>>> validatePerson $ EditPerson "John" "Doe" 123456789 Single 1234567890
Success (Person {firstName = "John", lastName = "Doe", ssn = 123-45-6789, martialStatus = Single, usPhoneNumber = (123) 456-7890})
-}
happyPathTest :: Test
happyPathTest = TestCase $ do
  let expected = Success (Person "John" "Doe" (SocialSecurityNumber 123456789) Single (PhoneNumber 123 456 7890))
      actual = validatePerson $ EditPerson "John" "Doe" 123456789 Single 1234567890
  assertEqual "expect success" expected actual

{-
>>> validatePerson $ EditPerson "" "" 123456789 Single 1234567890
Failure [EmptyFirstName,EmptyLastName]
-}
emptyNamesTest :: Test
emptyNamesTest = TestCase $ do
  let expected = Failure [EmptyFirstName, EmptyLastName]
      actual = validatePerson $ EditPerson "" "" 123456789 Single 1234567890
  assertEqual "fail on empty names" expected actual

{-
>>> validatePerson $ EditPerson "John" "Doe" 1234 Married 12345
Failure [InvalidSSN,InvalidPhoneNumber]
-}
numsTooShortTest :: Test
numsTooShortTest = TestCase $ do
  let expected = Failure [InvalidSSN, InvalidPhoneNumber]
      actual = validatePerson $ EditPerson "John" "Doe" 1234 Married 12345
  assertEqual "fail on numbers too short" expected actual

{-
>>> validatePerson $ EditPerson "John" "Doe" (-123456789) Married (-1234567890)
Failure [InvalidSSN,InvalidPhoneNumber]
-}
negativeNumsTest :: Test
negativeNumsTest = TestCase $ do
  let expected = Failure [InvalidSSN, InvalidPhoneNumber]
      actual = validatePerson $ EditPerson "John" "Doe" (-123456789) Married (-1234567890)
  assertEqual "fail on negative numbers" expected actual
