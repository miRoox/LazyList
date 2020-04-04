(* Mathematica Package *)

BeginPackage["LazyList`"];

Unprotect[
  Iterator,
  CreateIterator,
  IteratorTypeMatchQ
];

GeneralUtilities`SetUsage[Iterator,
  "Iterator[type$, data$] represents a iterator object."
];
GeneralUtilities`SetUsage[CreateIterator,
  "CreateIterator[expr$] creates a new iterator object from expr$.",
  "CreateIterator[type$, args$$] creates a new iterator object."
];
GeneralUtilities`SetUsage[IteratorTypeMatchQ,
  "IteratorTypeQ[iter$, type$] returns True if iter$ is an iterator of the type$, and return False otherwise.",
  "IteratorTypeQ[type$] represents an operator form of IteratorTypeQ that can be applied to an expression."
];

SetAttributes[Iterator, HoldRest];
SetAttributes[CreateIterator, HoldAll];

CreateIterator::ntype="Unknown iterator type `1`.";

Begin["`Private`"];

iter_Iterator[method_String]:=iter@method[]

IteratorTypeMatchQ[_, _]:=False
IteratorTypeMatchQ[type_][e_]:=IteratorTypeMatchQ[e,type]

End[]; (* `Private` *)

Protect[
  Iterator,
  CreateIterator,
  IteratorTypeMatchQ
];

EndPackage[] (* LazyList` *)

BeginPackage["LazyList`IteratorBase`", {"LazyList`"}];

Unprotect[
  DeclareIterator,
  ImplementIterator,
  $IteratorSelf,
  $IteratorData,
  $IteratorType
];

GeneralUtilities`SetUsage[DeclareIterator,
  "DeclareIterator[type$, field$] declares a new iterator type$ with data field$."
];
GeneralUtilities`SetUsage[ImplementIterator,
  "ImplementIterator[type$, trait$, methods$] implememt trait$ for the iterator type$ with the methods$."
];
GeneralUtilities`SetUsage[IteratorTraitInfo,
  "IteratorTraitInfo[] gives all iterator traits name.",
  "IteratorTraitInfo[trait$] show the infomation about trait$."
];

$IteratorSelf::usage="$IteratorSelf is a placeholder for the iterator itself.";
$IteratorType::usage="$IteratorType is a placeholder for the type of the iterator itself.";
$IteratorData::usage="$IteratorData is a placeholder to access the data of the iterator itself.";

DeclareIterator::typestr="Type name `1` should be a string.";
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
      "Setup" -> (Null&), (* allow any number of parameters *)
      "Next" -> Undefined,
      "SizeHint" -> Function[{}, Interval[{0,Infinity}]],
      "Collect" -> Function[{}, defaultCollect[$IteratorSelf]]
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

traitQ[name_]:=KeyExistsQ[$traits, name]

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

$type=<||>;

traitImplQ[_, _]:=False

GeneralUtilities`BlockProtected[{IteratorTypeMatchQ},
  IteratorTypeMatchQ[Iterator[itype_,_], type_]:=typeMatchQ[itype, type]
]

typeMatchQ[type_, type_]:=True
typeMatchQ[type_, trait_?traitQ]:=traitImplQ[type, trait]
typeMatchQ[ptype_[__], ptype_]:=True
typeMatchQ[ptype_[param1__], ptype_[param2__]] /; Length[{param1}]===Length[{param2}] :=
    And@@MapThread[typeMatchQ, {{param1}, {param2}}]
typeMatchQ[_, _]:=False

GeneralUtilities`BlockProtected[{CreateIterator},
  CreateIterator[type_, args___]:=GeneralUtilities`CatchFailureAndMessage@Module[
    {$data = Lookup[$type, typeLabel[type],
      GeneralUtilities`ThrowFailure[CreateIterator::ntype, type]
    ][["Data"]]},
    Block[{iter = Iterator[type, $data]},
      iter@"Setup"[args];
      iter
    ]
  ]
]

DeclareIterator[type_, field_Association]:=GeneralUtilities`CatchFailureAndMessage[
  AssociateTo[$type, typeLabel[type] -> <|
    "Data" -> field
  |>];
]

ImplementIterator[type_, trait_, methods_]:=GeneralUtilities`CatchFailureAndMessage[
  traitImplQ[type, trait]=True;
]
ImplementIterator[type_, trait_]:=ImplementIterator[type, trait, <||>]

typeLabel[inner_[___]]:=typeLabel[inner]
typeLabel[type_String]:=type
typeLabel[e_]:=GeneralUtilities`ThrowFailure[DeclareIterator::typestr, e]

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
  ImplementIterator,
  $IteratorSelf,
  $IteratorData,
  $IteratorType
];

EndPackage[] (* LazyList`IteratorBase` *)