#!/usr/bin/env wolframscript
(* ::Package:: *)

Print["Wolfram Language Version: ", $Version];

If[FailureQ@Environment["WOLFRAM_ID"],
  CloudConnect[],
  CloudConnect[Environment["WOLFRAM_ID"], Environment["WOLFRAM_PASS"]]
];
If[!TrueQ@$CloudConnected,
  Print["Cloud connection failed!"];
  Exit[1]
]

$baseDir=DirectoryName[$InputFileName];
$pacletDir=FileNameJoin@{$baseDir, "LazyList"};

If[FailureQ@Environment["PACLET_PATH"],
  AppendTo[$Path, $baseDir];
  If[$VersionNumber>=12.1,
    System`PacletDirectoryLoad[$pacletDir],
    PacletManager`PacletDirectoryAdd[$pacletDir]
  ],
  PacletInstall[Environment["PACLET_PATH"]]
]

Needs["MUnit`"]

$testFile=FileNameJoin@{$baseDir, "Tests", "Tests.wlt"};

If[MUnit`TestRun[$testFile, "Loggers" -> {MUnit`VerbosePrintLogger[]}],
  Exit[],
  Exit[1]
]
