BeginTestSection["OnceIterator"]

VerificationTest[
  CreateIterator["Once"]
  ,
  $Failed
  ,
  {CreateIterator::cargx}
  ,
  TestID->"Once-iterator-construct-failed-0"
]

VerificationTest[
  CreateIterator["Once", 2, 5]
  ,
  $Failed
  ,
  {CreateIterator::cargx}
  ,
  TestID->"Once-iterator-construct-failed-2"
]

VerificationTest[
  Block[{i=CreateIterator["Once", 1]},
    i@"Next"
  ]
  ,
  1
  ,
  TestID->"Once-iterator-next-once"
]

VerificationTest[
  Block[{i=CreateIterator["Once", 1]},
    i@"Next";
    i@"Next"
  ]
  ,
  Nothing
  ,
  TestID->"Once-iterator-next-twice"
]

VerificationTest[
  CreateIterator["Once", 1]@"Collect"
  ,
  {1}
  ,
  TestID->"Once-iterator-collect"
]

EndTestSection[]
