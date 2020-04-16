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

SetAttributes[Iterator, {HoldRest, ReadProtected}];
SetAttributes[CreateIterator, ReadProtected];
SetAttributes[IteratorTypeMatchQ, ReadProtected];
SetAttributes[IteratorTypeOf, ReadProtected];
SetAttributes[LazyRange, ReadProtected];

SyntaxInformation[Iterator]={"ArgumentsPattern" -> {_, _}};
SyntaxInformation[CreateIterator]={"ArgumentsPattern" -> {_, ___}};
SyntaxInformation[IteratorTypeMatchQ]={"ArgumentsPattern" -> {_, _.}};
SyntaxInformation[IteratorTypeOf]={"ArgumentsPattern" -> {_}};
SyntaxInformation[LazyRange]={"ArgumentsPattern" -> {_., _., _.}};

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

SetAttributes[mutIterator, HoldAllComplete];
doUnset=False;
mutIterator[Unset[iter_]]/;!TrueQ@doUnset:=(
  iter@"Dispose"[];
  Block[{doUnset=True}, Unset[iter]]
)
mutIterator[___]:=Language`MutationFallthrough
Language`SetMutationHandler[Iterator, mutIterator]

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
HoldPattern@LazyRange[stop_]/;rangeCollinearQ[1, stop, 1]:=
    System`Private`ConstructNoEntry[LazyRange ,1, FunctionExpand@Floor@Ramp[stop], 1]
r:HoldPattern@LazyRange[_]:=(
  Message[LazyRange::range, HoldForm@r];
  $Failed
)
HoldPattern@LazyRange[start:DirectedInfinity[_], stop_]/;rangeCollinearQ[start, stop, 1]:=
    System`Private`ConstructNoEntry[LazyRange, Simplify[stop-FunctionExpand@Floor@minusOneClip[stop-start]], stop, 1]
HoldPattern@LazyRange[start_, stop_]/;rangeCollinearQ[start, stop, 1]:=
    System`Private`ConstructNoEntry[LazyRange, start, Simplify[start+FunctionExpand@Floor@minusOneClip[stop-start]], 1]
r:HoldPattern@LazyRange[_,_]:=(
  Message[LazyRange::range, HoldForm@r];
  $Failed
)
r:HoldPattern@LazyRange[start_, stop_, step_]/;!rangeCollinearQ[start, stop, step]:=(
  Message[LazyRange::range, HoldForm@r];
  $Failed
)
HoldPattern@LazyRange[start_?ExactNumberQ, stop_, step_?InexactNumberQ]:=LazyRange[N@start, N@stop, step]
HoldPattern@LazyRange[start_, stop_?ExactNumberQ, step_?InexactNumberQ]:=LazyRange[N@start, N@stop, step]
r:HoldPattern@LazyRange[start:DirectedInfinity[_], stop_, step_]/;System`Private`HoldEntryQ[r]:=
    System`Private`ConstructNoEntry[LazyRange, Simplify[stop-FunctionExpand@minusOneClip@Quotient[stop-start,step]*step], stop, step]
r:HoldPattern@LazyRange[start_, stop_, step_]/;System`Private`HoldEntryQ[r]:=
    System`Private`ConstructNoEntry[LazyRange, start, Simplify[start+FunctionExpand@minusOneClip@Quotient[stop-start,step]*step], step]
r:HoldPattern@LazyRange[start_, stop_, step_?PossibleZeroQ]/;System`Private`HoldEntryQ[r]:=
    System`Private`ConstructNoEntry[LazyRange, start, start, 0]

LazyRange/:Normal[HoldPattern@LazyRange[start_, stop_, step_]]:=Range[start, stop, step]
LazyRange/:Length[HoldPattern@LazyRange[start_, stop_, step_]]:=If[step===0, 0, Quotient[stop-start,step]] + 1
LazyRange/:Total[r:HoldPattern@LazyRange[start_, stop_, _]]:=(start+stop)*Length[r]/2
LazyRange/:Part[r_LazyRange, i_Integer]:=With[{val=rangeIndex[r, i]},
  If[val===$Failed,
    Message[Part::partw, i, HoldForm@r];$Failed,
    val
  ]
]
LazyRange/:Part[r_LazyRange, spec_List/;VectorQ[spec, IntegerQ]]:=r[[#]]&/@spec
LazyRange/:Part[r_LazyRange, i_;;j_;;(k_:1)]:=GeneralUtilities`CatchFailureAndMessage@rangeTake[r, {
  If[i===All, 1, i],
  If[j===All, -1, j],
  If[k===All, 1, k]
}]
LazyRange/:Take[r_LazyRange, None]:={}
LazyRange/:Take[r_LazyRange, All]:=r
LazyRange/:Take[r_LazyRange, n_Integer]:=GeneralUtilities`CatchFailureAndMessage@rangeTake[r, {1 ,n}]
LazyRange/:Take[r_LazyRange, {n_Integer}]:=GeneralUtilities`CatchFailureAndMessage@rangeTake[r, {n ,n}]
LazyRange/:Take[r_LazyRange, {m_Integer, n_Integer}]:=GeneralUtilities`CatchFailureAndMessage@rangeTake[r, {m ,n}]
LazyRange/:Take[r_LazyRange, {m_Integer, n_Integer, s_Integer}]:=GeneralUtilities`CatchFailureAndMessage@rangeTake[r, {m ,n, s}]
LazyRange/:Reverse[HoldPattern@LazyRange[start_, stop_, step_]]:=LazyRange[stop, start, -step]

LazyRange/:MakeBoxes[r:HoldPattern@LazyRange[start_, stop_, step_]/;System`Private`HoldNoEntryQ[r], fmt_]:=Module[
  {alwaysGrids,sometimesGrids},
  alwaysGrids={
    BoxForm`SummaryItem@{"Start: ", start},
    BoxForm`SummaryItem@{"Stop: ", stop}
  };
  sometimesGrids={
    BoxForm`SummaryItem@{"Step: ", step}
  };
  BoxForm`ArrangeSummaryBox[LazyRange,r,None,alwaysGrids,sometimesGrids,fmt]
]

rangeCollinearQ[start_, stop_, step_?PossibleZeroQ]:=TrueQ[start==stop]
rangeCollinearQ[start_, stop_, step_]:=realOrInfinityQ[(stop - start)/step]
realOrInfinityQ[DirectedInfinity[1|-1]]:=True
realOrInfinityQ[x_]:=TrueQ@FullSimplify@Element[Chop[x], Reals]
minusOneClip[x_]:=Clip[x, {-1, Infinity}]

rangeIndex[r:HoldPattern@LazyRange[start_, _, step_], i_Integer?Positive]:=If[
  i<=Length[r],
  start+(i-1)*step,
  $Failed
]
rangeIndex[r:HoldPattern@LazyRange[_, stop_, step_], i_Integer?Negative]:=If[
  -i<=Length[r],
  stop+(i+1)*step,
  $Failed
]
rangeTake[r:HoldPattern@LazyRange[_, _, step_], {i_, j_, k_:1}]:=LazyRange[
  Sequence@@Map[
    If[#=!=$Failed, #,
      GeneralUtilities`ThrowFailure["take", i, j, HoldForm@r]
    ]&@*(rangeIndex[r, #]&),
    {i, j}
  ],
  step*k
]

End[]; (* `Private` *)

Protect[
  Iterator,
  CreateIterator,
  LazyRange
];

EndPackage[] (* LazyList` *)

Get["LazyList`IteratorDeveloper`"]
Get["LazyList`Iterators`"]
