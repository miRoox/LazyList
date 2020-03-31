(* Mathematica Package *)

BeginPackage["LazyList`"];

Unprotect[
  Iterator,
  CreateIterator
];

GeneralUtilities`SetUsage[Iterator,
  "Iterator[type$, data$] represents a iterator object."
];
GeneralUtilities`SetUsage[CreateIterator,
  "CreateIterator[expr$] creates a new iterator object from expr$.",
  "CreateIterator[type$, args$$] creates a new iterator object."
];

SetAttributes[Iterator, HoldRest];
SetAttributes[CreateIterator, HoldAll];

Begin["`Private`"];

iter_Iterator[method_String]:=iter@method[]

CreateIterator[type_, args___]:=Module[{$data},
  Block[{iter=Iterator[type, $data]},
    iter@"__init__"[];
    iter@"Setup"[args];
    iter
  ]
]

End[]; (* `Private` *)

Protect[
  Iterator,
  CreateIterator
];

EndPackage[] (* LazyList` *)

BeginPackage["LazyList`IteratorBase`", {"LazyList`"}];

Unprotect[
  DeclareIterator,
  $IteratorSelf,
  $IteratorData,
  $IteratorType
];

GeneralUtilities`SetUsage[DeclareIterator,
  "DeclareIterator[type$, data$, impl$] declares a new iterator type$."
];
GeneralUtilities`SetUsage[IteratorTraitInfo,
  "IteratorTraitInfo[] gives all iterator traits name.",
  "IteratorTraitInfo[trait$] show the infomation about trait$."
]

$IteratorSelf::usage="$IteratorSelf is a placeholder for the iterator itself.";
$IteratorType::usage="$IteratorType is a placeholder for the type of the iterator itself.";
$IteratorData::usage="$IteratorData is a placeholder to access the data of the iterator itself.";

IteratorTraitInfo::trait="Unknown trait named `1`.";

Begin["`Private`"];

$traits = <|
  "Base" -> <|
    "Deps" -> {},
    "Info" -> "Base trait for all iterators.",
    "Methods" -> <|
      "Next" -> Undefined,
      "SizeHint" -> Function[{}, Interval[{0,Infinity}]],
      "Collect" -> Function[{}, defaultCollect[$IteratorSelf]]
    |>
  |>,
  "Constructor" -> <|
    "Deps" -> {},
    "Info" -> "Iterator constructor.",
    "Methods" -> <|
      "__init__" -> Automatic,
      "Setup" -> (Null&) (* allow any number of parameters *)
    |>
  |>,
  "Copyable" -> <|
    "Deps" -> {},
    "Info" -> "Copyable iterators.",
    "Methods" -> <|
      "Copy" -> Function[{}, Module[{$data=$IteratorData}, Iterator[$IteratorType, $data]]]
    |>
  |>
|>;

defaultCollect[iter_Iterator]:=Block[
  {bag=Internal`Bag[],next},
  While[
    next=iter@"Next"[];
    next!=Nothing,
    Internal`StuffBag[bag,next]
  ];
  Internal`BagPart[bag,All]
]

IteratorTraitInfo[]:=Keys[$traits]
IteratorTraitInfo[trait_]:=GeneralUtilities`CatchFailureAndMessage[
  Lookup[$traits, trait,
    GeneralUtilities`ThrowFailure[IteratorTraitInfo::trait, trait]
  ][["Info"]]
]

DeclareIterator[type_String, data_Association, impl_Association]:=GeneralUtilities`CatchFailureAndMessage[
]

End[]; (* `Private` *)

Protect[
  DeclareIterator,
  $IteratorSelf,
  $IteratorData,
  $IteratorType
];

EndPackage[] (* LazyList`IteratorBase` *)