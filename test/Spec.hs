{-# LANGUAGE OverloadedStrings #-}

import Person

main :: IO ()
main = do
  print . validatePerson $ EditPerson "John" "Doe" 123456789 Single 1234567890
  print . validatePerson $ EditPerson "" "" 123456789 Married 12345678