{-# LANGUAGE OverloadedStrings #-}

import Person

main :: IO ()
main = do
  print . validatePerson $ EditPerson "John" "Doe" 123456789 Single 1234567890
  print . validatePerson $ EditPerson "" "" 1234 Married 12345
  print . validatePerson $ EditPerson "John" "Doe" (-123456789) Married (-1234567890)