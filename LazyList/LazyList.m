(* Mathematica Package *)

BeginPackage["LazyList`", {"GeneralUtilities`"}];

Unprotect[LazyList]

GeneralUtilities`SetUsage[LazyList,
    "LazyList[expr$] create a lazy list object."
];

LazyList::cons="Invalid constructor `1`.";

SetAttributes[LazyList,HoldFirst];

Begin["`Private`"];

$registry={};

SetAttributes[constructor,HoldAll];
constructor[e_]:=GeneralUtilities`ThrowFailure[LazyList::cons,HoldForm@e]

LazyList[e_]:=GeneralUtilities`CatchFailure@constructor[e]
LazyList[]:=LazyList[{}]

(** Default methods **)

LazyList/:Normal@LazyList[type_[___]]:=GeneralUtilities`Panic["Unimplemented", "Method `1` is not implemented for type `2`!", Normal, type]
LazyList/:Length[l:LazyList[_]]:=Length@Normal[l]
LazyList/:LengthWhile[l:LazyList[_],crit_]:=Length@Normal[TakeWhile[l,crit]]
LazyList/:Part[l:LazyList[_],i_Integer]:=Part[Normal[l],i]
LazyList/:First[l:LazyList[_],def_]:=First[Normal[l],def]
LazyList/:Last[l:LazyList[_],def_]:=Last[Normal[l],def]
LazyList/:Extract[l:LazyList[_],i_]:=Extract[Normal[l],i]
LazyList/:AllTrue[l:LazyList[_],test_]:=Fold[And,test/@l]
LazyList/:AnyTrue[l:LazyList[_],test_]:=Fold[Or,test/@l]
LazyList/:NoneTrue[l:LazyList[_],test_]:=Fold[And,Not@*test/@l]

loadImpl[]/;$loading=!=True:=Block[{$loading=True},Get["LazyListImpl`"]]

End[]; (* `Private` *)

Protect[LazyList]

EndPackage[]

loadImpl[];
