BeginTestSection["LazyRange"]

Needs["LazyList`"]

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
