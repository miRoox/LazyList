Begin["`Range`Private`"]

DeclareIterator["Range", <|
  "Start"->1,
  "Stop"->Infinity,
  "Step"->1
|>]

ImplementIterator["Range", "Any", {
  "Setup"[args___] :> setup[$IteratorData, args],
  "SummaryItems"[] :> <|
    "Type: " -> $IteratorType,
    "From: " -> DynamicIteratorItem@Dynamic[$IteratorData[["Start"]]],
    "To: " -> DynamicIteratorItem@Dynamic[$IteratorData[["Stop"]]],
    "Step: " -> $IteratorData[["Step"]]
  |>
}]

ImplementIterator["Range", "Forward", {
  "Next"[] :> Block[{item=$IteratorData[["Start"]]},
    If[item <= $IteratorData[["Stop"]],
      $IteratorData[["Start"]] += $IteratorData[["Step"]];
      item,
      Nothing
    ]
  ],
  "SizeHint"[] :> Quotient[$IteratorData[["Stop"]]-$IteratorData[["Start"]], $IteratorData[["Step"]]]+1,
  "Collect"[] :> Range[$IteratorData[["Start"]], $IteratorData[["Stop"]], $IteratorData[["Step"]]]
}]

ImplementIterator["Range", "Peekable", {
  "Peek"[] :> If[$IteratorData[["Start"]] <= $IteratorData[["Stop"]],
    $IteratorData[["Start"]],
    Nothing
  ]
}]

ImplementIterator["Range", "DoubleEnded", {
  "NextBack"[] :> Block[{item=$IteratorData[["Stop"]]},
    If[$IteratorData[["Start"]] <= item,
      $IteratorData[["Stop"]] -= $IteratorData[["Step"]];
      item,
      Nothing
    ]
  ]
}]

ImplementIterator["Range", "Copyable"]

SetAttributes[setup, HoldFirst];
setup[data_, start_|PatternSequence[], stop_|PatternSequence[], step_|PatternSequence[]]:=ResourceFunction["WithMessageHandler"][
  isetup[data, LazyRange[start, stop, step]],
  Throw
]
setup[_, args__]:=IteratorSetupArgumentsCheck["Range", Length@{args}, {0, 3}]
SetAttributes[isetup, HoldFirst];
isetup[data_, HoldPattern@LazyRange[start_, stop_, step_]]:=(
  data[["Start"]]=start;
  data[["Stop"]]=stop;
  data[["Step"]]=step;
)

End[]
