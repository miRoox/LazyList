#!/usr/bin/env wolframscript
(* ::Package:: *)

Print["Wolfram Language Version: ", $Version];
Print["Running tests...","\n"];

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

Needs["LazyList`"]

Module[{report, time, results, failIdx},
  report=TestReport[
    FileNameJoin[{$baseDir, "Tests.wlt"}]
  ];
  time=report["TimeElapsed"];
  results=report["TestResults"];
  failIdx=report["TestsFailedIndices"];
  Print["  ", Length[results], " tests run in ", TextString@time, "."];
  If[TrueQ@report["AllTestsSucceeded"]
    ,
    Print["  All tests succeeded!"];
    Exit[]
    ,
    Print["  ", Length[failIdx], " tests failed!"];
    Do[
      Print@TextString@Row[Values@results[i][{"TestIndex", "TestID", "Outcome", "ActualMessages", "ActualOutput"}], " | "],
      {i, failIdx}
    ];
    Exit[1]
  ]
]
