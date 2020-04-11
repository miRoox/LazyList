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
GeneralUtilities`SetUsage[IteratorTypeMatchQ,
  "IteratorTypeQ[iter$, type$] returns True if iter$ is an iterator of the type$, and return False otherwise.",
  "IteratorTypeQ[type$] represents an operator form of IteratorTypeQ that can be applied to an expression."
];
GeneralUtilities`SetUsage[IteratorTypeOf,
  "IteratorTypeOf[iterator$] returns the iterator$ type name."
];

SetAttributes[Iterator, HoldRest];
SetAttributes[CreateIterator, HoldAll];

Iterator::nmethod="`1` is not a known method with `2` parameters for the iterator of type `3`.";
CreateIterator::ntype="Unknown iterator type `1`.";
CreateIterator::cargb="Construct `1` iterator with `2` arguments; between `3` and `4` arguments are expected.";
CreateIterator::cargbu="Construct `1` iterator with 1 argument; between `2` and `3` arguments are expected.";
CreateIterator::cargm="Construct `1` iterator with `2` arguments; `3` or more arguments are expected.";
CreateIterator::cargmu="Construct `1` iterator with 1 argument; `2` or more arguments are expected.";
CreateIterator::cargt="Construct `1` iterator with `2` arguments; `3` or `4` arguments are expected.";
CreateIterator::cargtu="Construct `1` iterator with 1 argument; `2` or `3` arguments are expected.";
CreateIterator::cargr="Construct `1` iterator with 1 argument; `2` arguments are expected.";
CreateIterator::cargx="Construct `1` iterator with `2` arguments; 1 argument is expected.";
CreateIterator::cargrx="Construct `1` iterator with `2` arguments; `3` arguments are expected.";

Begin["`Private`"];

(*see IteratorDeveloper*)
iter_Iterator[method_String]:=iter@method[]
Iterator[type_,_][method_[params___]]:=GeneralUtilities`CatchFailureAndMessage[
  GeneralUtilities`ThrowFailure[Iterator::nmethod, method, Length@{params}, type]
]
Iterator/:Normal[iter_Iterator]:=iter@"Collect"[]
Iterator/:ReadList[iter_Iterator]:=iter@"Collect"[]
Iterator/:Read[iter_Iterator]:=iter@"Next"[]
Iterator/:MakeBoxes[iter_Iterator?System`Private`NoEntryQ, fmt_] /; BoxForm`UseIcons := Module[
  {items=iter@"SummaryItems"[],alwaysGrids,sometimesGrids={}},
  If[AssociationQ[items],
    {alwaysGrids,sometimesGrids}=Map[
      KeyValueMap[BoxForm`SummaryItem@*List],
      TakeDrop[items,UpTo[2]]
    ],
    alwaysGrids={BoxForm`SummaryItem@{"Type: ", IteratorTypeOf@iter}}
  ];
  BoxForm`ArrangeSummaryBox[Iterator,iter,iteratorIcon,alwaysGrids,sometimesGrids,fmt]
]

iteratorIcon=Graphics[
  {
    {
      Thickness @ 0.04,
      Circle[{-0.6, 0}, 0.2],
      Circle[{0.6, 0}, 0.2]
    },
    {
      Arrowheads @ 0.1,
      Arrow @ BezierCurve @ {{-0.4, 0.15}, {0, 0.75}, {0.4, 0.15}},
      Arrow @ BezierCurve @ {{-1.6, 0.15}, {-1.2, 0.75}, {-0.8, 0.15}},
      Arrow @ BezierCurve @ {{0.8, 0.15}, {1.2, 0.75}, {1.6, 0.15}}
    },
    Text[Style["Next", FontSize -> 6], {0, 0.7}]
  },
  ImageSize -> 29.4,
  PlotRange -> {{-1, 1}, {-1, 1}}
];

(*see IteratorDeveloper*)
IteratorTypeMatchQ[_, _]:=False
IteratorTypeMatchQ[type_][e_]:=IteratorTypeMatchQ[e,type]

IteratorTypeOf[Iterator[type_,_]]:=type

End[]; (* `Private` *)

Protect[
  Iterator,
  CreateIterator
];

EndPackage[] (* LazyList` *)

Get["LazyList`IteratorDeveloper`"]
Get["LazyList`Iterators`"]
