BeginTestSection["RangeIterator"]

VerificationTest[
  CreateIterator["Range", 1, 2, 3, 4]
  ,
  $Failed
  ,
  {CreateIterator::cargb}
  ,
  TestID->"Range-iterator-construct-failed-4"
]

VerificationTest[
  CreateIterator["Range", I, 4I]
  ,
  $Failed
  ,
  {LazyRange::range}
  ,
  TestID->"Range-iterator-construct-failed-range"
]

VerificationTest[
  CreateIterator["Range"]
  ,
  CreateIterator["Range", 1, Infinity, 1]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-construct-0"
]

VerificationTest[
  CreateIterator["Range", 100.5]
  ,
  CreateIterator["Range", 1, 100, 1]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-construct-1"
]

VerificationTest[
  CreateIterator["Range", -1/2, 17/5]
  ,
  CreateIterator["Range", -1/2, 5/2, 1]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-construct-2"
]

VerificationTest[
  CreateIterator["Range", -100, Infinity]
  ,
  CreateIterator["Range", -100, Infinity, 1]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-construct-2-inf"
]

VerificationTest[
  CreateIterator["Range", -Infinity, 1]
  ,
  CreateIterator["Range", -Infinity, 1, 1]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-construct-2-ninf"
]

VerificationTest[
  CreateIterator[Inactivate@Range[100.5]]
  ,
  CreateIterator["Range", 1, 100, 1]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-Range-construct-1"
]

VerificationTest[
  CreateIterator[Inactive[Range][-1/2, 17/5]]
  ,
  CreateIterator["Range", -1/2, 5/2, 1]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-Range-construct-2"
]

VerificationTest[
  CreateIterator[Inactive[Range][-100, Infinity]]
  ,
  CreateIterator["Range", -100, Infinity, 1]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-Range-construct-2-inf"
]

VerificationTest[
  CreateIterator[Inactive[Range][-Infinity, 1]]
  ,
  CreateIterator["Range", -Infinity, 1, 1]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-Range-construct-2-ninf"
]

VerificationTest[
  CreateIterator[Inactive[Range][-100, Infinity, 2]]
  ,
  CreateIterator["Range", -100, Infinity, 2]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-Range-construct-3-inf"
]

VerificationTest[
  CreateIterator[Inactive[Range][Infinity, 1, -1]]
  ,
  CreateIterator["Range", Infinity, 1, -1]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-Range-construct-3-rinf"
]

VerificationTest[
  CreateIterator[LazyRange[-100, Infinity, 2]]
  ,
  CreateIterator["Range", -100, Infinity, 2]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-LazyRange-construct"
]

VerificationTest[
  CreateIterator["Range"]@"Next"
  ,
  1
  ,
  TestID->"Range-iterator-next"
]

VerificationTest[
  Block[{i=CreateIterator["Range"]},
    i@"Next";
    i
  ]
  ,
  CreateIterator["Range", 2, Infinity]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-next-it"
]

VerificationTest[
  CreateIterator["Range", 100]@"Collect"
  ,
  Range[100]
  ,
  TestID->"Range-iterator-collect"
]

VerificationTest[
  Block[{i=CreateIterator["Range", 100]},
    i@"Collect";
    i
  ]
  ,
  CreateIterator["Range", 101, 100]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-collect-it"
]

VerificationTest[
  CreateIterator["Range", Infinity, 1, -1]@"NextBack"
  ,
  1
  ,
  TestID->"Range-iterator-nextback"
]

VerificationTest[
  Block[{i=CreateIterator["Range", Infinity, 1, -1]},
    i@"NextBack";
    i
  ]
  ,
  CreateIterator["Range", Infinity, 2, -1]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-nextback-it"
]

VerificationTest[
  (CreateIterator["Range", 100]@"Select"[Mod[#, 3]==1 && Mod[#, 5]==1 &])@"Collect"
  ,
  Range[100]//Select[Mod[#, 3]==1 && Mod[#, 5]==1 &]
  ,
  TestID->"Range-iterator-select"
]

EndTestSection[]
