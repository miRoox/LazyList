(* Mathematica Test File *)

BeginTestSection["LazyList"]

BeginTestSection["Iterators"]

BeginTestSection["Empty"]

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
  Block[{i=CreateIterator["Empty"]},
    Normal[i]
  ]
  ,
  {}
  ,
  TestID->"Empty-iterator-collect"
]

EndTestSection[]

BeginTestSection["Constant"]

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
    Table[Read@i, 10]
  ]
  ,
  ConstantArray[1, 10]
  ,
  TestID->"Constant-iterator-consume"
]

EndTestSection[]

EndTestSection[]

BeginTestSection["LazyRange"]

VerificationTest[
  LazyRange[]
  ,
  Unevaluated@LazyRange[1, DirectedInfinity[1], 1]
  ,
  TestID->"LazyRange-0-argument-constructor"
]

VerificationTest[
  LazyRange[10.6]
  ,
  Unevaluated@LazyRange[1, 10, 1]
  ,
  TestID->"LazyRange-1-argument-constructor"
]

EndTestSection[]

EndTestSection[]
