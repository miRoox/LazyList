(* Mathematica Package *)

BeginPackage["LazyList`"];

Unprotect[
  Iterator,
  CreateIterator,
  LazyRange
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
GeneralUtilities`SetUsage[LazyRange,
  "LazyRange[] constructs a range from 1 to \[Infinity] with a step size of 1.",
  "LazyRange[stop$] constructs a range from 1 to stop$ with a step size of 1.",
  "LazyRange[start$, stop$] constructs a range from start$ to stop$ with a step size of 1.",
  "LazyRange[start$, stop$, step$] constructs a range from start$ to stop$ with the step$."
];

SetAttributes[Iterator, HoldRest];
SetAttributes[CreateIterator, HoldAll];

SyntaxInformation[Iterator]={"ArgumentsPattern" -> {_, _}};
SyntaxInformation[CreateIterator]={"ArgumentsPattern" -> {_, ___}};
SyntaxInformation[IteratorTypeMatchQ]={"ArgumentsPattern" -> {_, _.}};
SyntaxInformation[IteratorTypeOf]={"ArgumentsPattern" -> {_}};

Iterator::nmethod="`1` is not a known method with `2` parameters for the iterator of type `3`.";
Iterator::nelem="`1` cannot appear in the element of the iterator";
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
LazyRange::range="Range specification in `1` does not have appropriate bounds.";

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


(* LazyRange *)

LazyRange[]:=System`Private`ConstructNoEntry[LazyRange, 1, Infinity, 1]
LazyRange[stop_]/;rangeCollinearQ[1, stop, 1]:=
    System`Private`ConstructNoEntry[LazyRange ,1, Simplify@Floor[stop], 1]
r:LazyRange[_]:=(
  Message[LazyRange::range, HoldForm@r];
  $Failed
)
LazyRange[start_, stop_]/;rangeCollinearQ[start, stop, 1]:=
    System`Private`ConstructNoEntry[LazyRange, start, Simplify[Floor@minusOneClip[stop-start]+start], 1]
r:LazyRange[_,_]:=(
  Message[LazyRange::range, HoldForm@r];
  $Failed
)
r:LazyRange[start_, stop_, step_]/;!rangeCollinearQ[start, stop, step]:=(
  Message[LazyRange::range, HoldForm@r];
  $Failed
)
LazyRange[start_?ExactNumberQ, stop_, step_?InexactNumberQ]:=LazyRange[N@start,stop,step]
r:LazyRange[start_, stop_, step_]/;System`Private`HoldEntryQ[r]:=
    System`Private`ConstructNoEntry[LazyRange, start, Simplify[minusOneClip@Quotient[stop-start,step]*step+start], step]
r:LazyRange[start_, stop_, step_?PossibleZeroQ]/;System`Private`HoldEntryQ[r]:=
    System`Private`ConstructNoEntry[LazyRange, start, start, 0]

LazyRange/:Normal[HoldPattern@LazyRange[start_, stop_, step_]]:=Range[start, stop, step]

rangeCollinearQ[start_, stop_, step_?PossibleZeroQ]:=TrueQ[start==stop]
rangeCollinearQ[start_, stop_, step_]:=realOrInfinityQ[(stop - start)/step]
realOrInfinityQ[DirectedInfinity[1|-1]]:=True
realOrInfinityQ[x_]:=TrueQ@Simplify@Element[x, Reals]
minusOneClip[x_]:=Clip[x, {-1, Infinity}]

End[]; (* `Private` *)

Protect[
  Iterator,
  CreateIterator,
  LazyRange
];

EndPackage[] (* LazyList` *)

Get["LazyList`IteratorDeveloper`"]
Get["LazyList`Iterators`"]
