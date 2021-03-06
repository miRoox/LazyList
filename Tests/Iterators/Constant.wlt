BeginTestSection["ConstantIterator"]

Needs["LazyList`"]

VerificationTest[
  CreateIterator["Constant"]
  ,
  $Failed
  ,
  {CreateIterator::cargx}
  ,
  TestID->"Constant-iterator-construct-failed-0"
]

VerificationTest[
  CreateIterator["Constant", 2, 5]
  ,
  $Failed
  ,
  {CreateIterator::cargx}
  ,
  TestID->"Constant-iterator-construct-failed-2"
]

VerificationTest[
  Block[{i=CreateIterator["Constant", 1]},
    Table[i@"Next", 10]
  ]
  ,
  ConstantArray[1, 10]
  ,
  TestID->"Constant-iterator-consume"
]

VerificationTest[
  CreateIterator["Constant", 1]/*"Take"[10]/*"Collect"
  ,
  ConstantArray[1, 10]
  ,
  TestID->"Constant-iterator-take-consume"
]

EndTestSection[]
