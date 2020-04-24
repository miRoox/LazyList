(* Mathematica Package *)

BeginPackage["LazyList`Developer`", {"LazyList`"}];

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
  "ImplementIterator[type$, trait$, methods$] implememt trait$ for the iterator type$ with the methods$.",
  "ImplementIterator[type$, trait$] implememt trait$ for the iterator type$.",
  "ImplementIterator[type$, methods$] implememt the iterator type$ with the methods$."
];
GeneralUtilities`SetUsage[IteratorSetupArgumentsCheck,
  "IteratorSetupArgumentsCheck[type$, argnum$, num$] throws an error if argnum$ and num$ are not equal.",
  "IteratorSetupArgumentsCheck[type$, argnum$, {min$, max$}] throws an error if argnum$ is not between min$ and max$."
];
GeneralUtilities`SetUsage[DynamicIteratorItem,
  "DynamicIteratorItem[expr$] is an iterator macro that displays expr$ if the iterator is not dropped, otherwise show Missing['Dropped']."
];

$IteratorSelf::usage="$IteratorSelf is a placeholder for the iterator itself.";
$IteratorType::usage="$IteratorType is a placeholder for the type of the iterator itself.";
$IteratorData::usage="$IteratorData is a placeholder to access the data of the iterator itself.";

SetAttributes[DeclareIterator, ReadProtected];
SetAttributes[ImplementIterator, ReadProtected];
SetAttributes[IteratorSetupArgumentsCheck, ReadProtected];
SetAttributes[DynamicIteratorItem, HoldFirst];

SyntaxInformation[DeclareIterator]={"ArgumentsPattern" -> {_, _}};
SyntaxInformation[ImplementIterator]={"ArgumentsPattern" -> {_, _, _.}};
SyntaxInformation[IteratorSetupArgumentsCheck]={"ArgumentsPattern" -> {_, _, _}};
SyntaxInformation[DynamicIteratorItem]={"ArgumentsPattern" -> {_}};

ImplementIterator::trait="Unknown trait named `1`.";
ImplementIterator::mdeps="The dependencies `2` for `1` is missing.";
ImplementIterator::require="The method `1` is required.";
ImplementIterator::nfor="The method `1` is not for trait `2`.";

Begin["`Iterator`Private`"];

$traits = <|
  "Any" -> <|
    "Super" -> {},
    "Methods" -> {
      "Setup"[args___] :> IteratorSetupArgumentsCheck[$IteratorType, Length@{args}, 0],
      "Dispose"[] :> Null,
      "Equal"[HoldPattern@Iterator[type_, data_]]:=type==$IteratorType&&data==$IteratorData,
      "SummaryItems"[] :> <|
        "Type: " -> $IteratorType,
        "Dropped: " -> Dynamic[!ValueQ@$IteratorData]
      |>
    }
  |>,
  "Copyable" -> <|
    "Super" -> {"Any"},
    "Methods" -> {
      "Copy"[] :> Module[{$data=$IteratorData}, System`Private`SetNoEntry@Iterator[$IteratorType, $data]]
    }
  |>,
  "Forward" -> <|
    "Super" -> {"Any"},
    "Methods" -> {
      "Next"[] -> Undefined,
      "SizeHint"[] :> Interval[{0,Infinity}],
      "Collect"[] :> defaultCollect[$IteratorSelf],
      "Take"[n_Integer?NonNegative] :> CreateIterator["Take"[$IteratorType], MoveIterator@$IteratorSelf, n]
    }
  |>,
  "Peekable" -> <|
    "Super" -> {"Any", "Forward"},
    "Methods" -> {
      "Peek"[] -> Undefined
    }
  |>,
  "DoubleEnded" -> <|
    "Super" -> {"Any", "Forward"},
    "Methods" -> {
      "NextBack"[] -> Undefined,
      "Reverse"[] :> CreateIterator["Reverse"[$IteratorType], MoveIterator@$IteratorSelf]
    }
  |>,
  "Bidirectional" -> <|
    "Super" -> {"Any", "Forward"},
    "Methods" -> {
      "Previous"[] -> Undefined
    }
  |>,
  "ExactSize" -> <|
    "Super" -> {"Any", "Forward"},
    "Methods" -> {
      "Length"[] :> Block[
        {len=$IteratorSelf@"SizeHint"[]},
        Assert@MatchQ[len,_Integer?NonNegative];
        len
      ],
      "Collect"[] :> Table[$IteratorSelf@"Next"[], $IteratorSelf@"SizeHint"[]]
    }
  |>
|>;

traitQ[name_]:=KeyExistsQ[$traits, name]

defaultCollect[iter_Iterator]:=Block[
  {bag=Internal`Bag[],next},
  While[
    next=iter@"Next"[];
    next=!=Nothing,
    Internal`StuffBag[bag,next]
  ];
  Internal`BagPart[bag,All]
]

$types=<||>;
$typeTemplates=<||>;

instantiatedTypeQ[t_]:=KeyExistsQ[$types, t]

traitImplQ[_, _]:=False

ResourceFunction["BlockProtected"][{IteratorTypeMatchQ},
  IteratorTypeMatchQ[iter:Iterator[itype_,_Symbol]/;System`Private`HoldNoEntryQ[iter], type_]:=typeMatchQ[itype, type]
]

typeMatchQ[type_, type_]:=True
typeMatchQ[type_, trait_?traitQ]:=traitImplQ[type, trait]
typeMatchQ[ptype_[__], ptype_]:=True
typeMatchQ[ptype_[param1__], ptype_[param2__]] /; Length[{param1}]===Length[{param2}] :=
    And@@MapThread[typeMatchQ, {{param1}, {param2}}]
typeMatchQ[_, _]:=False

typeMoreSpecificQ[_?instantiatedTypeQ, _]:=True
typeMoreSpecificQ[_[__], _?traitQ]:=True
typeMoreSpecificQ[t1_?traitQ, t2_?traitQ]:=MemberQ[$traits[[t1, "Super"]], t2]
typeMoreSpecificQ[_?traitQ, None]:=True
typeMoreSpecificQ[ptype_[param1__], ptype_[param2__]]:=And@@MapThread[typeMoreSpecificQ, {{param1}, {param2}}]
typeMoreSpecificQ[_, _]:=False

instantiateType[_?instantiatedTypeQ]:=Null
instantiateType[type_String]:=GeneralUtilities`ThrowFailure[CreateIterator::ntype, type]
instantiateType[type: _[params__]]:=Block[{cands},
  Scan[instantiateType, {params}];
  cands=KeySort[KeySelect[$typeTemplates, typeMatchQ[type, #]&], Not@*typeMoreSpecificQ];
  Do[
    If[KeyExistsQ[cand, "Data"],
      DeclareIterator[type, cand[["Data"]]]
    ],
    {cand, cands}
  ];
  Do[
    If[KeyExistsQ[cand, "Implements"],
      KeyValueMap[ImplementIterator[type, #1, #2]&,
        KeySort[cand[["Implements"]], Not@*typeMoreSpecificQ]
      ]
    ],
    {cand, cands}
  ];
]

ResourceFunction["BlockProtected"][{CreateIterator},
  CreateIterator[type_, args___]:=GeneralUtilities`CatchFailureAndMessage[
    instantiateType[type];
    Module[
      {$data = ResourceFunction["NestedLookup"][$types, {type, "Data"},
        GeneralUtilities`ThrowFailure[CreateIterator::ntype, type]
      ]},
      Block[{iter = System`Private`SetNoEntry@Iterator[type, $data]},
        iter@"Setup"[args];
        iter
      ]
    ]
  ]
]

$nonParamatricTypePatt=_String|_String[params__String/;AllTrue[{params},instantiatedTypeQ]];

DeclareIterator[type:$nonParamatricTypePatt, field_Association]:=GeneralUtilities`CatchFailureAndMessage[
  $types = ResourceFunction["NestedAssociate"][$types, {type, "Data"} -> field];
]
DeclareIterator[type:_String[__String], field_Association]:=GeneralUtilities`CatchFailureAndMessage[
  $typeTemplates = ResourceFunction["NestedAssociate"][$typeTemplates, {type, "Data"} -> field];
]

ImplementIterator[type:$nonParamatricTypePatt, None|PatternSequence[], methods_List]:=GeneralUtilities`CatchFailureAndMessage[
  doImplMethods[type, methods];
]
ImplementIterator[type:$nonParamatricTypePatt, trait_String, methods_List]:=GeneralUtilities`CatchFailureAndMessage[
  checkTraitName[trait];
  Internal`InheritedBlock[{traitImplQ},
    traitImplQ[type, trait]=True;
    checkTraitDeps[type, trait];
    Block[{omethods}, (* combine with old methods *)
      omethods=ResourceFunction["NestedLookup"][$types, {type, "Implements", trait}, {}];
      Do[
        omethods=GeneralUtilities`PatternAppend[omethods, method],
        {method, methods}
      ];
      doImplMethods[type, resolveTraitMethods[trait, omethods]];
      $types = ResourceFunction["NestedAssociate"][$types, {type, "Implements", trait} -> omethods];
    ];
  ];
  traitImplQ[type, trait]=True;
]
ImplementIterator[type:_String[__String], trait_String, methods_List]:=GeneralUtilities`CatchFailureAndMessage[
  checkTraitName[trait];
  $typeTemplates = ResourceFunction["NestedAssociate"][$typeTemplates, {type, "Implements", trait} -> methods];
]
ImplementIterator[type:_String[__String], None|PatternSequence[], methods_List]:=GeneralUtilities`CatchFailureAndMessage[
  $typeTemplates = ResourceFunction["NestedAssociate"][$typeTemplates, {type, "Implements", None} -> methods];
]
ImplementIterator[type_, trait_String]:=ImplementIterator[type, trait, {}]

checkTraitName[trait_]:=If[!traitQ[trait],
  GeneralUtilities`ThrowFailure[ImplementIterator::trait, trait]
]
checkTraitDeps[type_, trait_]:=Block[
  {mdeps=Select[$traits[[trait, "Super"]], !traitImplQ[type, #]&]},
  If[mdeps=!={},
    GeneralUtilities`ThrowFailure[ImplementIterator::mdeps, trait, mdeps]
  ]
]

doImplMethods[type_, methods_]:=ResourceFunction["BlockProtected"][{Iterator},
  Activate[substImplMethods[type]/@methods, SetDelayed]
]
substImplMethods[type_][Rule[lhs_, Undefined]]:=GeneralUtilities`ThrowFailure[ImplementIterator::require, lhs]
substImplMethods[type_][(Rule|RuleDelayed)[lhs_, rhs_]]:=TemplateApply[
  Inactivate[
    (self:Iterator[type, data_?AssociationQ])[lhs]:=TemplateEvaluate[expandMethodRHS[rhs, type, self, data]],
    SetDelayed
  ]
]
SetAttributes[expandMethodRHS, {HoldAll, SequenceHold}]
expandMethodRHS[rhs_, type_, self_, data_]:=quoteRHS[rhs]//.{
  $IteratorType->type,
  $IteratorSelf->self,
  $IteratorData->data,
  DynamicIteratorItem[expr_] :> PaneSelector[
    {
      True -> expr,
      False -> Missing["Dropped"]
    },
    Dynamic[AssociationQ@data]
  ]
}
SetAttributes[quoteRHS, {HoldAll, SequenceHold}]
quoteRHS/:SetDelayed[lhs_, quoteRHS[rhs_]]:=SetDelayed[lhs,rhs]

overrideMethods[trait_][methods_List, override:(Rule|RuleDelayed)[lhs_,_]]:=GeneralUtilities`Match[
  Position[methods, method_/;GeneralUtilities`EquivalentPatternQ[method, override], 1],
  {{i_}} :> ReplacePart[methods, i->override],
  {} :> GeneralUtilities`ThrowFailure[ImplementIterator::nfor, lhs, trait]
]
resolveTraitMethods[trait_, methods_List]:=Fold[overrideMethods[trait], $traits[[trait, "Methods"]], methods]

IteratorSetupArgumentsCheck[type_, argnum_Integer?NonNegative, num_Integer?NonNegative]:=If[num!=argnum,
  Which[
    argnum==1, GeneralUtilities`ThrowFailure[CreateIterator::cargr, type, num],
    num==1, GeneralUtilities`ThrowFailure[CreateIterator::cargx, type, argnum],
    True, GeneralUtilities`ThrowFailure[CreateIterator::cargrx, type, argnum, num]
  ]
]
IteratorSetupArgumentsCheck[type_, argnum_Integer?NonNegative, {min_Integer, max_Integer}/; 0<=min<=max]:=If[!min<=argnum<=max,
  Which[
    max===Infinity,
    If[argnum==1,
      GeneralUtilities`ThrowFailure[CreateIterator::cargmu, type, min],
      GeneralUtilities`ThrowFailure[CreateIterator::cargm, type, argnum, min]
    ],
    max-min==1,
    If[argnum==1,
      GeneralUtilities`ThrowFailure[CreateIterator::cargtu, type, min, max],
      GeneralUtilities`ThrowFailure[CreateIterator::cargt, type, argnum, min, max]
    ],
    True,
    If[argnum==1,
      GeneralUtilities`ThrowFailure[CreateIterator::cargbu, type, min, max],
      GeneralUtilities`ThrowFailure[CreateIterator::cargb, type, argnum, min, max]
    ]
  ]
]

End[]; (* `Iterator`Private` *)

Protect[
  DeclareIterator,
  ImplementIterator,
  $IteratorSelf,
  $IteratorData,
  $IteratorType
];

EndPackage[] (* LazyList`Developer` *)