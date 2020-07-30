#!/usr/bin/env wolframscript
(* ::Package:: *)

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
Get["https://raw.githubusercontent.com/miRoox/CILogger-MUnit/master/CILogger/CILogger.wl"]

$testDir=FileNameJoin@{$baseDir, "Tests"};

If[
  AllTrue[
    FileNames["*.mt"|"*.wlt", $testDir],
    MUnit`TestRun[#, "Loggers" :> {CILogger`CILogger[]}]&
  ],
  Exit[],
  Exit[1]
]
