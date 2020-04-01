(* Mathematica Package *)

BeginPackage["LazyList`"];

Unprotect[
  Iterator,
  CreateIterator,
  IteratorTypeQ
];

GeneralUtilities`SetUsage[Iterator,
  "Iterator[type$, data$] represents a iterator object."
];
GeneralUtilities`SetUsage[CreateIterator,
  "CreateIterator[expr$] creates a new iterator object from expr$.",
  "CreateIterator[type$, args$$] creates a new iterator object."
];
GeneralUtilities`SetUsage[IteratorTypeQ,
  "IteratorTypeQ[iter$, type$] returns True if iter$ is an iterator of the type$, and return False otherwise.",
  "IteratorTypeQ[type$] represents an operator form of IteratorTypeQ that can be applied to an expression."
]

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

IteratorTypeQ[Iterator[type_,_], type_]:=True
IteratorTypeQ[Iterator[type_[___],_], type_]:=True
IteratorTypeQ[_, _]:=False
IteratorTypeQ[type_][e_]:=IteratorTypeQ[e,type]

End[]; (* `Private` *)

Protect[
  Iterator,
  CreateIterator,
  IteratorTypeQ
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

DeclareIterator::trait="Unknown trait named `1`.";
DeclareIterator::satis="Iterator type must satisfy the trait `1`.";
DeclareIterator::mdeps="The dependencies `2` for `1` is missing.";
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
  GeneralUtilities`Scope[
    traits = resolveDependencies[Keys@impl];
  ]
]

resolveDependencies[traits_List]:=GeneralUtilities`Scope[
  If[!MemberQ[traits, "Base"],
    GeneralUtilities`ThrowFailure[DeclareIterator::satis, "Base"]
  ];
  depschain={};
  Do[
    deps=Lookup[$traits, trait,
      GeneralUtilities`ThrowFailure[DeclareIterator::trait, trait]
    ][["Deps"]];
    If[deps=={},
      AppendTo[depschain,trait->None],
      If[!SubsetQ[traits, deps],
        GeneralUtilities`ThrowFailure[DeclareIterator::mdeps, trait, Complement[deps, traits]]
      ];
      depschain=Join[depschain,Thread[trait->deps]]
    ],
    {trait, traits}
  ];
  TopologicalSort[depschain]//Most//Reverse
]

End[]; (* `Private` *)

Protect[
  DeclareIterator,
  $IteratorSelf,
  $IteratorData,
  $IteratorType
];

EndPackage[] (* LazyList`IteratorBase` *)