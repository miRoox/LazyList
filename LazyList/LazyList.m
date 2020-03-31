(* Mathematica Package *)

BeginPackage["LazyList`", {"GeneralUtilities`"}];

Unprotect[LazyList,ExtendLazyList]

GeneralUtilities`SetUsage[LazyList,
  "LazyList[expr$] create a lazy list object."
];
GeneralUtilities`SetUsage[ExtendLazyList,
  "ExtendLazyList[type$, cons$, methods$] extend lazy list type."
];

LazyList::cons="Invalid constructor `1`.";
ExtendLazyList::type="Type specification `1` should be \"name\" or alias=\"name\" form.";
ExtendLazyList::typeals="In type specification `1`, `2` should be a symbol.";
ExtendLazyList::typenam="In type specification `1`, `2` should be a string.";
ExtendLazyList::consrhs="In constructor specification `1`, the right-hand side `2` is not the target type.";
ExtendLazyList::cons="Constructor specification `1` should be lhs:=rhs or a list of assignments."

SetAttributes[LazyList,HoldFirst];
SetAttributes[ExtendLazyList,HoldAll];

Begin["`Private`"];

$registry={};

SetAttributes[constructor,HoldAll];
constructor[e_]:=GeneralUtilities`ThrowFailure[LazyList::cons,HoldForm@e]

LazyList[e_]:=GeneralUtilities`CatchFailure@constructor[e]
LazyList[]:=LazyList[{}]

ExtendLazyList[typespec_,cons_,methods_]:=GeneralUtilities`CatchFailure@Block[
  {type,rule,constmt,methodstmt},
  {type,rule}=parseTypeSpec[typespec];
  constmt=expandConstructors[cons,rule,type];
  methodstmt=expandMethods[methods,rule,type];
  ReleaseHold[constmt];
  ReleaseHold[methodstmt];
]

SetAttributes[parseTypeSpec,HoldAll];
parseTypeSpec[type_String]:={type,type[data___]:>LazyList[type[data]]}
parseTypeSpec[Set[alias_Symbol,type_String]]:={type,HoldPattern@alias[data___]:>LazyList[type[data]]}
parseTypeSpec[spec:Set[_Symbol,type_]]:=GeneralUtilities`ThrowFailure[ExtendLazyList::typenam,HoldForm[spec],type]
parseTypeSpec[spec:Set[alias_,_]]:=GeneralUtilities`ThrowFailure[ExtendLazyList::typeals,HoldForm[spec],HoldForm[alias]]
parseTypeSpec[spec_]:=GeneralUtilities`ThrowFailure[ExtendLazyList::type,spec]

SetAttributes[expandConstructors,HoldFirst];
expandConstructors[cons:SetDelayed[lhs_,rhs_],rule_,type_]:=With[
  {e=Hold@SetDelayed[constructor[lhs],rhs]/.rule},
  If[MatchQ[e, Hold@SetDelayed[_, LazyList[type[___]]]],
    e,
    GeneralUtilities`ThrowFailure[ExtendLazyList::consrhs,HoldForm[cons],HoldForm[rhs]]
  ]
]
expandConstructors[cons_List,rule_,type_]:=expandConstructors[#,rule,type]&/@cons
expandConstructors[cons_,_,_]:=GeneralUtilities`ThrowFailure[ExtendLazyList::cons,HoldForm[cons]]

SetAttributes[expandMethods,HoldFirst];
expandMethods[method_SetDelayed,rule_,type_]:=expandMethods[{method},rule,type]

SetAttributes[expandMethod,HoldFirst];

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

Protect[LazyList,ExtendLazyList]

EndPackage[]

loadImpl[];
