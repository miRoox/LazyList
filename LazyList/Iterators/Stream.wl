Begin["`Stream`Private`"]

DeclareIterator["Stream", <|"Stream"->Nothing, "Type"->Byte|>]

ImplementIterator["Stream", "Any", {
  "Setup"[args___] :> setup[$IteratorData, args],
  "Dispose"[] :> Close[$IteratorData[["Stream"]]],
  "SummaryItems"[] :> <|
    "Type: " -> $IteratorType,
    "Item Type: " -> $IteratorData[["Type"]],
    "Stream: " -> $IteratorData[["Stream"]]
  |>
}]

ImplementIterator["Stream", "Forward", {
  "Next"[] :> Block[{item=Read[$IteratorData[["Stream"]], $IteratorData[["Type"]]]},
    If[MatchQ[item, EndOfFile|EndOfBuffer],
      Nothing,
      item
    ]
  ]
}]

SetAttributes[setup, HoldFirst];
Default[setup, 3]=Byte;
setup[data_, istream_InputStream, type_.]:=Block[{},
  data[["Stream"]]=istream;
  data[["Type"]]=type;
]
setup[data_, file_, type_.]:=ResourceFunction["WithMessageHandler"][
  setup[data, OpenRead[file], type],
  GeneralUtilities`ThrowRawFailure
]
setup[_, args___]:=IteratorSetupArgumentsCheck["Stream", Length@{args}, {1, 2}]

End[]
