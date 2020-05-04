Begin["`TakeWhile`Private`"]

DeclareIterator["TakeWhile"["Any"], <|
  "Iter"->Nothing,
  "Predicate"->(True&),
  "Finished"->False
|>]

ImplementIterator["TakeWhile"["Any"], "Any", {
  "Setup"[args___] :> setup[$IteratorData, args],
  "SummaryItems"[] :> <|
    "Type: " -> $IteratorType,
    "Predicate: " -> $IteratorData[["Predicate"]],
    "From: " -> DynamicIteratorItem@$IteratorData[["Iter"]]
  |>,
  "Dispose"[] :> $IteratorData[["Iter"]]@"Dispose"[]
}]

ImplementIterator["TakeWhile"["Forward"], "Forward", {
  "Next"[] :> If[$IteratorData[["Finished"]],
    Nothing,
    Block[{item=$IteratorData[["Iter"]]@"Next"[]},
      Which[
        item===Nothing, Nothing,
        TrueQ@$IteratorData[["Predicate"]][item], item,
        True, $IteratorData[["Finished"]]=True; Nothing
      ]
    ]
  ],
  "SizeHint"[] :> If[$IteratorData[["Finished"]],
    0,
    Interval@{0, Max[$IteratorData[["Iter"]]@"SizeHint"[]]}
  ]
}]

ImplementIterator["TakeWhile"["Peekable"], "Peekable", {
  "Peek"[] :> If[$IteratorData[["Finished"]],
    Nothing,
    $IteratorData[["Iter"]]@"Peek"[]
  ]
}]

ImplementIterator["TakeWhile"["Copyable"], "Copyable"]

SetAttributes[setup, HoldFirst];
setup[data_, iter_, pred_]:=(
  data@"Iter"=iter;
  data@"Predicate"=pred;
)

End[]
