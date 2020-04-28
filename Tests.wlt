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

BeginTestSection["Once"]

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
    Read@i
  ]
  ,
  1
  ,
  TestID->"Once-iterator-next-once"
]

VerificationTest[
  Block[{i=CreateIterator["Once", 1]},
    Read@i;
    Read@i
  ]
  ,
  Nothing
  ,
  TestID->"Once-iterator-next-twice"
]

VerificationTest[
  Block[{i=CreateIterator["Once", 1]},
    Normal@i
  ]
  ,
  {1}
  ,
  TestID->"Once-iterator-collect"
]

EndTestSection[]

BeginTestSection["Range"]

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
  CreateIterator["Range"]
  ,
  CreateIterator["Range", 1, Infinity, 1]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-construct-0"
]

VerificationTest[
  CreateIterator["Range", 100]
  ,
  CreateIterator["Range", 1, 100, 1]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-construct-1"
]

VerificationTest[
  CreateIterator["Range", -100, Infinity]
  ,
  CreateIterator["Range", -100, Infinity, 1]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-construct-2"
]

VerificationTest[
  CreateIterator["Range", -Infinity, 1]
  ,
  CreateIterator["Range", -Infinity, 1, 1]
  ,
  SameTest->Equal
  ,
  TestID->"Range-iterator-construct-2-inf"
]

VerificationTest[
  Block[{i=CreateIterator["Range"]},
    i@"Next"
  ]
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
  Block[{i=CreateIterator["Range", 100]},
    i@"Collect"
  ]
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

EndTestSection[]

BeginTestSection["Successors"]

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

EndTestSection[]

eginTestSection["Stream"]

VerificationTest[
  CreateIterator["Stream"]
  ,
  $Failed
  ,
  {CreateIterator::cargt}
  ,
  TestID->"Stream-iterator-construct-failed-0"
]

VerificationTest[
  CreateIterator["Stream", "Build.wls", Character, 0]
  ,
  $Failed
  ,
  {CreateIterator::cargt}
  ,
  TestID->"Stream-iterator-construct-failed-3"
]

VerificationTest[
  Block[{str, it},
    GeneralUtilities`SetupTeardown[
      str=StringToStream["123 abc"];
      it=CreateIterator["Stream", str],
      Normal@it,
      it@"Dispose"
    ]
  ]
  ,
  ToCharacterCode["123 abc"]
  ,
  TestID->"Stream-iterator-byte-consume"
]

VerificationTest[
  Block[{str, it},
    GeneralUtilities`SetupTeardown[
      str=StringToStream["123 abc"];
      it=CreateIterator["Stream", str, Character],
      Normal@it,
      it@"Dispose"
    ]
  ]
  ,
  Characters["123 abc"]
  ,
  TestID->"Stream-iterator-character-consume"
]

VerificationTest[
  Block[{str, it},
    GeneralUtilities`SetupTeardown[
      str=StringToStream["123\nabc"];
      it=CreateIterator["Stream", str, String],
      Normal@it,
      it@"Dispose"
    ]
  ]
  ,
  StringSplit["123\nabc", "\n"]
  ,
  TestID->"Stream-iterator-string-consume"
]

EndTestSection[]

EndTestSection[]

BeginTestSection["LazyRange"]

VerificationTest[
  LazyRange[]
  ,
  Unevaluated@LazyRange[1, DirectedInfinity[1], 1]
  ,
  TestID->"LazyRange-0-argument-construct"
]

VerificationTest[
  LazyRange[10.6]
  ,
  Unevaluated@LazyRange[1, 10, 1]
  ,
  TestID->"LazyRange-1-argument-construct-1"
]

VerificationTest[
  LazyRange[Pi]
  ,
  Unevaluated@LazyRange[1, 3, 1]
  ,
  TestID->"LazyRange-1-argument-construct-2"
]

VerificationTest[
  LazyRange[-2]
  ,
  Unevaluated@LazyRange[1, 0, 1]
  ,
  TestID->"LazyRange-1-argument-construct-3"
]

VerificationTest[
  LazyRange[I]
  ,
  $Failed
  ,
  {LazyRange::range}
  ,
  TestID->"LazyRange-1-argument-construct-failed-1"
]

VerificationTest[
  LazyRange[x]
  ,
  $Failed
  ,
  {LazyRange::range}
  ,
  TestID->"LazyRange-1-argument-construct-failed-2"
]

VerificationTest[
  LazyRange[-5, 10.6]
  ,
  Unevaluated@LazyRange[-5, 10, 1]
  ,
  TestID->"LazyRange-2-argument-construct-1"
]

VerificationTest[
  LazyRange[1.0, 10]
  ,
  Unevaluated@LazyRange[1.0, 10.0, 1]
  ,
  TestID->"LazyRange-2-argument-construct-2"
]

VerificationTest[
  LazyRange[I, 4+I]
  ,
  Unevaluated@LazyRange[I, 4+I, 1]
  ,
  TestID->"LazyRange-2-argument-construct-3"
]

VerificationTest[
  LazyRange[x, 4+x]
  ,
  Unevaluated@LazyRange[x, 4+x, 1]
  ,
  TestID->"LazyRange-2-argument-construct-4"
]

VerificationTest[
  LazyRange[3, 0]
  ,
  Unevaluated@LazyRange[3, 2, 1]
  ,
  TestID->"LazyRange-2-argument-construct-5"
]

VerificationTest[
  LazyRange[-Infinity, 0]
  ,
  Unevaluated@LazyRange[DirectedInfinity[-1], 0, 1]
  ,
  TestID->"LazyRange-2-argument-construct-6"
]

VerificationTest[
  LazyRange[Infinity, 0]
  ,
  Unevaluated@LazyRange[1, 0, 1]
  ,
  TestID->"LazyRange-2-argument-construct-7"
]

VerificationTest[
  LazyRange[0, I]
  ,
  $Failed
  ,
  {LazyRange::range}
  ,
  TestID->"LazyRange-2-argument-construct-failed-1"
]

VerificationTest[
  LazyRange[x, 2x]
  ,
  $Failed
  ,
  {LazyRange::range}
  ,
  TestID->"LazyRange-2-argument-construct-failed-2"
]

VerificationTest[
  LazyRange[-Infinity, Infinity]
  ,
  $Failed
  ,
  {Infinity::indet, LazyRange::range}
  ,
  TestID->"LazyRange-2-argument-construct-failed-3"
]

VerificationTest[
  LazyRange[-5, 10.6, 2]
  ,
  Unevaluated@LazyRange[-5, 9, 2]
  ,
  TestID->"LazyRange-3-argument-construct-1"
]

VerificationTest[
  LazyRange[1.0, 10, 2]
  ,
  Unevaluated@LazyRange[1.0, 9.0, 2]
  ,
  TestID->"LazyRange-3-argument-construct-2"
]

VerificationTest[
  LazyRange[1, 10, 2.0]
  ,
  Unevaluated@LazyRange[1.0, 9.0, 2.0]
  ,
  TestID->"LazyRange-3-argument-construct-3"
]

VerificationTest[
  LazyRange[-Pi/2, 3Pi, Pi]
  ,
  Unevaluated@LazyRange[Rational[-1,2]*Pi, Rational[5,2]*Pi, Pi]
  ,
  TestID->"LazyRange-3-argument-construct-4"
]

VerificationTest[
  LazyRange[I, 2.5+3.5I, 1+I]
  ,
  Unevaluated@LazyRange[I, Complex[2, 3], Complex[1, 1]]
  ,
  TestID->"LazyRange-3-argument-construct-5"
]

VerificationTest[
  LazyRange[I, 2.5+3.5I, 1.0+I]
  ,
  Unevaluated@LazyRange[Complex[0.0, 1.0], Complex[2.0, 3.0], Complex[1.0, 1.0]]
  ,
  TestID->"LazyRange-3-argument-construct-6"
]

VerificationTest[
  LazyRange[1+3x, 1+0.5x, -x]
  ,
  Unevaluated@LazyRange[1+3x, 1+x, -x]
  ,
  TestID->"LazyRange-3-argument-construct-7"
]

VerificationTest[
  LazyRange[0, 4, -2]
  ,
  Unevaluated@LazyRange[0, 2, -2]
  ,
  TestID->"LazyRange-3-argument-construct-8"
]

VerificationTest[
  LazyRange[0, 4, -2.0]
  ,
  Unevaluated@LazyRange[0.0, 2.0, -2.0]
  ,
  TestID->"LazyRange-3-argument-construct-9"
]

VerificationTest[
  LazyRange[-Infinity, 0, 2]
  ,
  Unevaluated@LazyRange[DirectedInfinity[-1], 0, 2]
  ,
  TestID->"LazyRange-3-argument-construct-10"
]

VerificationTest[
  LazyRange[Infinity, 0, 2]
  ,
  Unevaluated@LazyRange[2, 0, 2]
  ,
  TestID->"LazyRange-3-argument-construct-11"
]

VerificationTest[
  LazyRange[-Infinity, 0, 2.0]
  ,
  Unevaluated@LazyRange[DirectedInfinity[-1], 0.0, 2.0]
  ,
  TestID->"LazyRange-3-argument-construct-12"
]

VerificationTest[
  LazyRange[0, 1+I, I]
  ,
  $Failed
  ,
  {LazyRange::range}
  ,
  TestID->"LazyRange-3-argument-construct-failed-1"
]

VerificationTest[
  LazyRange[x, 2x+1, x]
  ,
  $Failed
  ,
  {LazyRange::range}
  ,
  TestID->"LazyRange-3-argument-construct-failed-2"
]

VerificationTest[
  LazyRange[0, 1, Infinity]
  ,
  $Failed
  ,
  {Infinity::indet, LazyRange::range}
  ,
  TestID->"LazyRange-3-argument-construct-failed-3"
]

EndTestSection[]

EndTestSection[]
