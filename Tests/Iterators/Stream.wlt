BeginTestSection["Stream"]

VerificationTest[
  CreateIterator["Stream"]
  ,
  $Failed
  ,
  {CreateIterator::cargt}
  ,
  TestID->"Stream-iterator-construct-failed-0"
]

VerificationTest[
  CreateIterator["Stream", "Build.wls", Character, 0]
  ,
  $Failed
  ,
  {CreateIterator::cargt}
  ,
  TestID->"Stream-iterator-construct-failed-3"
]

VerificationTest[
  Block[{str, it},
    GeneralUtilities`SetupTeardown[
      str=StringToStream["123 abc"];
      it=CreateIterator["Stream", str],
      Normal@it,
      it@"Dispose"
    ]
  ]
  ,
  ToCharacterCode["123 abc"]
  ,
  TestID->"Stream-iterator-byte-consume"
]

VerificationTest[
  Block[{str, it},
    GeneralUtilities`SetupTeardown[
      str=StringToStream["123 abc"];
      it=CreateIterator["Stream", str, Character],
      Normal@it,
      it@"Dispose"
    ]
  ]
  ,
  Characters["123 abc"]
  ,
  TestID->"Stream-iterator-character-consume"
]

VerificationTest[
  Block[{str, it},
    GeneralUtilities`SetupTeardown[
      str=StringToStream["123\nabc"];
      it=CreateIterator["Stream", str, String],
      Normal@it,
      it@"Dispose"
    ]
  ]
  ,
  StringSplit["123\nabc", "\n"]
  ,
  TestID->"Stream-iterator-string-consume"
]

EndTestSection[]
