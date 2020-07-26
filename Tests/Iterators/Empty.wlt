BeginTestSection["EmptyIterator"]

Needs["LazyList`"]

VerificationTest[
  CreateIterator["Empty", 1]
  ,
  $Failed
  ,
  {CreateIterator::cargr}
  ,
  TestID->"Empty-iterator-construct-failed"
]

VerificationTest[
  CreateIterator["Empty"]@"Collect"
  ,
  {}
  ,
  TestID->"Empty-iterator-collect"
]

EndTestSection[]
