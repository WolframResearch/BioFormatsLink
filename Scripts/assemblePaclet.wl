(* ::Package:: *)

(* ::Section:: *)
(*Assemble the paclet, and build a new .paclet file*)


date = DateString[{"Year", "Month", "Day"}];
time = DateString[{"Hour24", "Minute", "Second"}];


$scriptsDirectory = Which[
	Environment["WORKSPACE"] =!= $Failed,
		FileNameJoin[{Environment["WORKSPACE"],"scripts"}],
	$InputFileName =!= "",
		DirectoryName[$InputFileName],
	True,
		NotebookDirectory[]
];

$buildDirectory = ToFileName[{ParentDirectory[$scriptsDirectory], "build"}];

$versionNumber = If[Environment["SET_VERSION_NUMBER"] =!= $Failed,
	Environment["SET_VERSION_NUMBER"],
	"0.0."<>date<>"."<>time
];
	
$source = ToFileName[{ParentDirectory[$scriptsDirectory], "BioFormatsLink"}];
$java = ToFileName[{ParentDirectory[$scriptsDirectory], "Java"}];
$assembled = ToFileName[{$buildDirectory, date <> "-" <> time, "BioFormatsLink"}];

CreateDirectory[$assembled, CreateIntermediateDirectories -> True];

$sourceFolderSet = {"Kernel"};

$builtDocs = FileNameJoin[{
	$buildDirectory,
	"BioFormatsLink",
	"Documentation"
}
];

CopyDirectory[$builtDocs, FileNameJoin[{$assembled, "Documentation"}]]
CopyDirectory[ToFileName[{$source, #}], ToFileName[{$assembled, #}]]& /@ $sourceFolderSet;
CopyDirectory[$java,  ToFileName[{$assembled, "Java"}]];

FileTemplateApply[
	FileTemplate[ToFileName[{$source}, "PacletInfoTemplate.m"]],
	<| "version" -> $versionNumber |>,
	ToFileName[{$assembled}, "PacletInfo.m"]
];

(* get rid of any .DS* files or other hidden files *)
DeleteFile /@ FileNames[".*", $assembled, Infinity];


PackPaclet[$assembled]


(* ::Section:: *)
(*notes*)


(*
Re version numbering:

The code above which builds the .paclet file starts from a PacletInfoTemplate.m
file, and creates a new PacletInfo.m file, using the current date and time as
part of a newly synthesized version number.

One consequence of this is that the static PacletInfo.m file will not be used,
except by developers who install the original source of BioFormatsLink, which should be
limited to only people who are developing BioFormatsLink.

For this reason, the version number in the static PacletInfo.m file should
always be greater than the version number synthesized here. That way, BioFormatsLink
developers can install the original source, and have it preferred over versions
of this paclet installed from the internal paclet server.
*)
