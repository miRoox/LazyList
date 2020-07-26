BeginTestSection["ListIterator"]

Needs["LazyList`"]

VerificationTest[
  CreateIterator["List"]
  ,
  $Failed
  ,
  {CreateIterator::cargb}
  ,
  TestID->"List-iterator-construct-failed-0"
]

VerificationTest[
  CreateIterator["List", {}, 1, 1, 0]
  ,
  $Failed
  ,
  {CreateIterator::cargb}
  ,
  TestID->"List-iterator-construct-failed-4"
]

EndTestSection[]
