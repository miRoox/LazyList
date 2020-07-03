Begin["`List`Private`"]

DeclareIterator["List", <|
  "Data"->{},
  "Index"->1,
  "Sentinel"->1
|>]

ImplementIterator["List", "Any", {
  "Setup"[args___] :> setup[$IteratorData, args],
  "SummaryItems"[] :> <|
    "Type: " -> $IteratorType,
    "Data: " -> DynamicIteratorItem@Shallow[$IteratorData[["Data"]], {3,5}],
    "Index: " -> DynamicIteratorItem@Dynamic[$IteratorData[["Index"]]],
    "Sentinel: " -> DynamicIteratorItem@Dynamic[$IteratorData[["Sentinel"]]]
  |>
}]

ImplementIterator["List", "Forward", {
  "Next"[] :> If[$IteratorData[["Index"]] < $IteratorData[["Sentinel"]],
    $IteratorData[["Data"]][[$IteratorData[["Index"]]++]],
    Nothing
  ],
  "SizeHint"[] :> $IteratorData[["Sentinel"]]-$IteratorData[["Index"]],
  "Collect"[] :> Block[
    {list=$IteratorData[["Data"]][[$IteratorData[["Index"]];;$IteratorData[["Sentinel"]]-1]]},
    $IteratorData[["Index"]]=$IteratorData[["Sentinel"]];
    list
  ]
}]

ImplementIterator["List", "Peekable", {
  "Peek"[] :> If[$IteratorData[["Index"]] < $IteratorData[["Sentinel"]],
    $IteratorData[["Data"]][[$IteratorData[["Index"]]]],
    Nothing
  ]
}]

ImplementIterator["List", "DoubleEnded", {
  "NextBack"[] :> If[$IteratorData[["Index"]] < $IteratorData[["Sentinel"]],
    $IteratorData[["Data"]][[--$IteratorData[["Sentinel"]]]],
    Nothing
  ]
}]

ImplementIterator["List", "Bidirectional", {
  "Previous"[] :> If[0 < $IteratorData[["Index"]] < $IteratorData[["Sentinel"]],
    $IteratorData[["Data"]][[--$IteratorData[["Index"]]]],
    Nothing
  ]
}]

ImplementIterator["List", "Copyable"]
ImplementIterator["List", "ExactSize"]

SetAttributes[setup, HoldFirst];
setup[data_, list_]:=setup[data, list, 1, Length@list]
setup[data_, list_, All]:=setup[data, list, 1, Length@list]
setup[data_, list_, from_]:=setup[data, list, from, Length@list]
setup[data_, list_List, from_Integer, to_Integer]:=(
  data[["Data"]]=list;
  data[["Index"]]=normalize[list, from];
  data[["Sentinel"]]=normalize[list, to]+1;
)
setup[_, args__]:=IteratorSetupArgumentsCheck["List", Length@{args}, {1, 3}]

normalize[list_, spec_Integer?Positive]:=spec
normalize[list_, spec_Integer?Negative]:=Length[list]+spec+1
normalize[list_, specs_List]:=normalize[list,#]&/@specs

End[]
