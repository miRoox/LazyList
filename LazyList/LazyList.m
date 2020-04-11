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

(*see IteratorDeveloper*)
IteratorTypeMatchQ[_, _]:=False
IteratorTypeMatchQ[type_][e_]:=IteratorTypeMatchQ[e,type]

End[]; (* `Private` *)

Protect[
  Iterator,
  CreateIterator,
  IteratorTypeMatchQ
];

EndPackage[] (* LazyList` *)

Get["LazyList`IteratorDeveloper`"]
Get["LazyList`Iterators`"]
