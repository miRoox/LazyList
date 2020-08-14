BeginTestSection["SuccessorsIterator"]

Needs["LazyList`"]

VerificationTest[
  CreateIterator["Successors"]
  ,
  $Failed
  ,
  {CreateIterator::cargrx}
  ,
  TestID->"Successors-iterator-construct-failed-0"
]

VerificationTest[
  CreateIterator["Successors", 2]
  ,
  $Failed
  ,
  {CreateIterator::cargr}
  ,
  TestID->"Successors-iterator-construct-failed-1"
]

VerificationTest[
  CreateIterator["Successors", f, x, 2]
  ,
  $Failed
  ,
  {CreateIterator::cargrx}
  ,
  TestID->"Successors-iterator-construct-failed-3"
]

VerificationTest[
  Block[{i=CreateIterator["Successors", f, x]},
    Table[Read@i, 10]
  ]
  ,
  NestList[f, x, 9]
  ,
  TestID->"Successors-iterator-consume"
]

VerificationTest[
  Block[{i=CreateIterator["Successors", 4*#*(1 - #) &, 0.3]/*"Take"[10]},
    Normal@i
  ]
  ,
  NestList[4*#*(1 - #) &, 0.3, 9]
  ,
  TestID->"Successors-iterator-take"
]

VerificationTest[
  Block[{i=CreateIterator["Successors", Log, 100]/*"TakeWhile"[Positive]},
    Normal@i
  ]
  ,
  NestWhileList[Log, 100, Positive, 1, \[Infinity], -1]
  ,
  TestID->"Successors-iterator-takewhile"
]

EndTestSection[]
