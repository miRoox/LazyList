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

ImplementIterator::trait="Unknown trait named `1`.";
ImplementIterator::mdeps="The dependencies `2` for `1` is missing.";
ImplementIterator::require="The method `1` is required.";
IteratorTraitInfo::trait="Unknown trait named `1`.";

Begin["`Private`"];

$traits = <|
  "Any" -> <|
    "Deps" -> {},
    "Info" -> "Base trait for all iterators.",
    "Methods" -> {
      "Setup"[___] :> Null,
      "Next"[] -> Undefined,
      "SizeHint"[] :> Interval[{0,Infinity}],
      "Collect"[] :> defaultCollect[$IteratorSelf]
    }
  |>,
  "Copyable" -> <|
    "Deps" -> {"Any"},
    "Info" -> "Copyable iterators.",
    "Methods" -> {
      "Copy"[] :> Module[{$data=$IteratorData}, Iterator[$IteratorType, $data]]
    }
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

$types=<||>;
$typeTemplates=<||>;

instantiatedTypeQ[t_]:=KeyExistsQ[$types, t]

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

instantiateType[type_String]:=If[!instantiatedTypeQ[type],
  GeneralUtilities`ThrowFailure[CreateIterator::ntype, type]
]
instantiateType[ptype_[params___]]:=(
  Scan[instantiateType,{params}]
)(*todo*)

GeneralUtilities`BlockProtected[{CreateIterator},
  CreateIterator[type_, args___]:=GeneralUtilities`CatchFailureAndMessage[
    instantiateType[type];
    Module[
      {$data = Lookup[$types, type,
        GeneralUtilities`ThrowFailure[CreateIterator::ntype, type]
      ][["Data"]]},
      Block[{iter = Iterator[type, $data]},
        iter@"Setup"[args];
        iter
      ]
    ]
  ]
]

$nonParamatricTypePatt=_String|_String[params__String/;AllTrue[{params},instantiatedTypeQ]]

DeclareIterator[type:$nonParamatricTypePatt, field_Association]:=GeneralUtilities`CatchFailureAndMessage[
  AssociateTo[$types, type -> <|
    "Data" -> field
  |>];
]
act:DeclareIterator[_String[__], _Association]:=GeneralUtilities`CatchFailureAndMessage[
  registerTypeTemplate[Inactivate[act]];
]

ImplementIterator[type:$nonParamatricTypePatt, None|PatternSequence[], methods_List]:=GeneralUtilities`CatchFailureAndMessage[
  doImplMethods[type, methods]
]
ImplementIterator[type:$nonParamatricTypePatt, trait_String, methods_List]:=GeneralUtilities`CatchFailureAndMessage[
  checkTraitName[trait];
  Internal`InheritedBlock[{traitImplQ},
    traitImplQ[type, trait]=True;
    checkTraitDeps[type, trait];
    doImplMethods[type, resolveTraitMethods[trait, methods]]
  ];
  traitImplQ[type, trait]=True;
]
act:ImplementIterator[_String[__], trait_String, _List]:=GeneralUtilities`CatchFailureAndMessage[
  checkTraitName[trait];
  registerTypeTemplate[Inactivate[act]];
]
act:ImplementIterator[_String[__], None|PatternSequence[], _List]:=GeneralUtilities`CatchFailureAndMessage[
  registerTypeTemplate[Inactivate[act]];
]
ImplementIterator[type_, trait_String]:=ImplementIterator[type, trait, {}]

checkTraitName[trait_]:=If[!traitQ[trait],
  GeneralUtilities`ThrowFailure[ImplementIterator::trait, trait]
]
checkTraitDeps[type_, trait_]:=Block[
  {mdeps=Select[$traits[[trait, "Deps"]], !traitImplQ[type, #]&]},
  If[mdeps=!={},
    GeneralUtilities`ThrowFailure[ImplementIterator::mdeps, trait, mdeps]
  ]
]

doImplMethods[type_, methods_]:=GeneralUtilities`BlockProtected[{Iterator},
  Activate[substImplMethods[type]/@methods, SetDelayed]
]
substImplMethods[type_][Rule[lhs_, Undefined]]:=GeneralUtilities`ThrowFailure[ImplementIterator::require, lhs]
substImplMethods[type_][(Rule|RuleDelayed)[lhs_, rhs_]]:=TemplateApply[
  Inactivate[
    ($IteratorSelf:Iterator[type, $IteratorData_])[lhs]:=TemplateEvaluate[expandMethodRHS[type, rhs]],
    SetDelayed
  ]
]
SetAttributes[expandMethodRHS, HoldRest]
expandMethodRHS[type_, rhs_]:=Hold[rhs]/.{$IteratorType->type}//ReleaseHold

registerTypeTemplate[act:f_Inactive[ptype_[__], args__]]:=If[KeyExistsQ[$typeTemplates, ptype],
  AppendTo[$typeTemplates[[ptype]], act],(*todo: sort?*)
  AssociateTo[$typeTemplates, ptype->{act}]
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