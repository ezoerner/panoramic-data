# panoramic-data

An exercise in data modeling in Haskell.

The Person module is the "public module" which is imported by the application.
Only the validating constructors are exported to make sure no invalid data
can be represented.

A Person is constructed by calling the `validatePerson` function.


## BUILD

Build with Haskell `stack`.