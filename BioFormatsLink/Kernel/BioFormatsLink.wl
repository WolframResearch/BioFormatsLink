(* Wolfram Language Package *)

BeginPackage["BioFormatsLink`", {"JLink`"}];

Begin["`Private`"];

ReadOMEXMLMetadata::nffil = ReadOriginalMetadata::nffil = ReadImage::nffil = "File `1` not found.";
ReadImage::serieserr = "Expecting a positive integer value smaller or equal to `1` instead of `2`.";
ReadSeriesCount::fmterr = ReadOMEXMLMetadata::fmterr = ReadOriginalMetadata::fmterr = ReadImage::fmterr = "Cannot import data as BioFormats format.";

$BioFormatsLinkPath = ParentDirectory[DirectoryName[$InputFileName]];
$BioFormatsJar = First@FileNames["bioformats_package.jar", FileNameJoin[{$BioFormatsLinkPath, "Java"}]];

$CloudMaxJVMMemoryGB = 5;

(*
	Returns 3/4 of available memory in the format accepted by JVM: -XmxNg.
	In the could environment the maximal memory size is controlled by $CloudMaxJVMMemoryGB.
*)

GetMaxJVMMemory[] :=
    Block[{maxMem = Max[1, Round[3/4 * N[MemoryAvailable[] / 2^30]]]},
	    If[$CloudEvaluation,
		    maxMem = Round[Min[maxMem, $CloudMaxJVMMemoryGB]];
	    ];
        "-Xmx" <> ToString[maxMem] <> "g"
	];

$privateJVM := JLink`GetJVM[JLink`InstallJava[JLink`ForceLaunch -> True, Default -> False, JLink`JVMArguments-> GetMaxJVMMemory[]]];

(* Initialization *)
If[$InitializedQ =!= True,
	JLink`AddToClassPath[$privateJVM, $BioFormatsJar];
	JLink`LoadJavaClass[$privateJVM, "loci.formats.ImageReader"];
	JLink`LoadJavaClass[$privateJVM, "loci.formats.gui.BufferedImageReader"];
	JLink`LoadJavaClass[$privateJVM, "loci.common.services.ServiceFactory"];
	JLink`LoadJavaClass[$privateJVM, "com.wolfram.jlink.JLinkClassLoader"];
	$InitializedQ = True;
];

(*Convert pixel types and color space.*)
BioFormatsToMathematicaTypeAndColorSpace[type_Integer] :=
	Switch[type,
		1|2|3|4|5|6|7, {"Byte", "RGB"},
		8|9, {"Bit16", "RGB"},
		10|12, {"Byte", "Grayscale"},
		11, {"Bit16", "Grayscale"},
		_, {"Real", Automatic}
	];

(* Read the number of series.*)
ReadSeriesCount[file_?StringQ] :=
	JLink`JavaBlock@Module[ {ir = JLink`JavaNew[$privateJVM, "loci.formats.ImageReader"], res},
		If[!Quiet[FileExistsQ[file] === True],
			ir@close[];
			Message[ReadSeriesCount::nffil, file];
			Return[$Failed];
		];
		If[Quiet[Check[ir@setId[file], $Failed]] === $Failed,
			ir@close[];
			Message[ReadSeriesCount::fmterr];
			Return[$Failed];
		];
		Quiet[res = ir@getSeriesCount[]];
		ir@close[];
		If[!Internal`PositiveMachineIntegerQ[res],
			Message[ReadSeriesCount::fmterr];
			Return[$Failed];
			,
			Return[res];
		];
	];

(* Read image.*)
ReadImage[file_?StringQ, series_:1] :=
	JLink`JavaBlock@Module[ {ir = JLink`JavaNew[$privateJVM, "loci.formats.ImageReader"],
		reader, image, raster, pixels, channels, bands, timeSeries, slices, index, height, width, type, colorspace = Automatic, seriesCount, res},
		If[!Quiet[FileExistsQ[file] === True],
			ir@close[];
			Message[ReadImage::nffil, file];
			Return[$Failed];
		];
		If[Quiet[Check[ir@setId[file], $Failed]] === $Failed,
			ir@close[];
			Message[ReadImage::fmterr];
			Return[$Failed];
		];
		Quiet[seriesCount = ir@getSeriesCount[]];
		If[!(Internal`PositiveMachineIntegerQ[series] && series <= seriesCount),
			ir@close[];
			Message[ReadImage::serieserr, seriesCount, series];
			Return[$Failed];
		];
		(*Quiet[*)
			ir@setSeries[series - 1];
			reader = BufferedImageReader`makeBufferedImageReader[ir];
			channels = reader@getEffectiveSizeC[];
			timeSeries = reader@getSizeT[];
			slices = reader@getSizeZ[];
		(*];*)
		If[!VectorQ[{channels, timeSeries, slices}, Internal`PositiveMachineIntegerQ],
			ir@close[];
			Message[ReadImage::fmterr];
			Return[$Failed];
		];
		res = Quiet[
			Table[
				Table[
					ColorCombine[
						Table[
							index = reader@getIndex[z, c, t];
							image = reader@openImage[index];
							raster = image@getRaster[];
							height = image@getHeight[];
							width = image@getWidth[];
							{type, colorspace} = BioFormatsToMathematicaTypeAndColorSpace[image@getType[]];
							bands = raster@getNumBands[];
							pixels = Map[ArrayReshape[raster@getSamples[0, 0, width, height, #, JLink`JavaNew[$privateJVM, "[I", width * height]],{height, width}]&, Range[0,bands-1]];
							Image[pixels, type, ColorSpace->colorspace, Interleaving -> False],
							{c, 0, channels - 1}
						],
						colorspace
					],
					{z, 0, slices - 1}
				],
				{t, 0, timeSeries - 1}
			]
		];
		ir@close[];
		If[!(ImageQ[res] || ArrayQ[res, _, ImageQ]),
			Message[ReadImage::fmterr];
			Return[$Failed];
			,
			Return[res];
		]
	];

(*Read original metadata*)
ReadOriginalMetadata[file_?StringQ] :=
	JLink`JavaBlock@Module[ {ir = JLink`JavaNew[$privateJVM, "loci.formats.ImageReader"], metastring, res},
		If[!Quiet[FileExistsQ[file] === True],
			ir@close[];
			Message[ReadOriginalMetadata::nffil, file];
			Return[$Failed];
		];
		If[Quiet[Check[ir@setId[file], $Failed]] === $Failed,
			ir@close[];
			Message[ReadOriginalMetadata::fmterr];
			Return[$Failed];
		];
		Quiet[metastring = ir@getCoreMetadataList[]@toString[]];
		If[!(StringQ[metastring] && StringLength[metastring] > 0),
			ir@close[];
			Message[ReadOriginalMetadata::fmterr];
			Return[$Failed];
		];
		ir@close[];
		res = Quiet[Map[
			First@
				StringReplace[#, StartOfString ~~ token__ ~~ " = " ~~ value__ ~~ EndOfString :>
					(StringTrim[token] ->
						If[ StringMatchQ[value, NumberString],
							ToExpression[value]
							,
							value
						]
					)
				] &,
			Map[Rest, StringSplit[StringSplit[StringTake[metastring, {2, -2}], ", "], "\n"]],
			{2}
		]];
		If[!(ArrayQ[res, _, Head[#] === Rule&] && Length[res] > 0),
			Message[ReadOriginalMetadata::fmterr];
			Return[$Failed];
			,
			Return[res];
		];
	];

(*Read OME-XML metadata*)
ReadOMEXMLMetadata[file_?StringQ] :=
	JLink`JavaBlock@Module[ {ir = JLink`JavaNew[$privateJVM, "loci.formats.ImageReader"], factory, service, serviceClass, meta, metastring},
		If[!Quiet[FileExistsQ[file] === True],
			ir@close[];
			Message[ReadOMEXMLMetadata::nffil, file];
			Return[$Failed];
		];
		(*Quiet[*)
			factory = JLink`JavaNew[$privateJVM, "loci.common.services.ServiceFactory"];
			serviceClass = JLinkClassLoader`classFromName["loci.formats.services.OMEXMLService"];
			service = factory@getInstance[serviceClass];
			meta = service@createOMEXMLMetadata[];
			ir@setMetadataStore[meta];
			ir@setOriginalMetadataPopulated[True];
			ir@setMetadataFiltered[True];
		(*];*);
		If[Quiet[Check[ir@setId[file], $Failed]] === $Failed,
			ir@close[];
			Message[ReadOMEXMLMetadata::fmterr];
			Return[$Failed];
		];
		Quiet[metastring = service@getOMEXML[meta]];
		ir@close[];
		If[!(StringQ[metastring] && StringLength[metastring] > 0),
			Message[ReadOMEXMLMetadata::fmterr];
			Return[$Failed];
		];
		Return[ImportString[metastring, "XML"]];
	];

$BioFormatsAvailableElements = {"ImageList", "OMEXMLMetaInformation", "OriginalMetaInformation", "SeriesCount"};

GetBioFormatsElements[___] := "Elements" -> $BioFormatsAvailableElements;

GetBioFormatsSeriesCount[format_][file_] := Block[{res},
	res = Quiet[ReadSeriesCount[file]];
	If[!Internal`PositiveMachineIntegerQ[res],
		Message[Import::fmterr, format];
		Return[$Failed];
	];
	Return["SeriesCount" -> res];
];

GetBioFormatsImageList[format_][series_][file_] := Block[{seriesCount, res},
	If[MatchQ[series, All | Automatic],
		seriesCount = Quiet[ReadSeriesCount[file]];
		If[!Internal`PositiveMachineIntegerQ[seriesCount],
			Message[Import::fmterr, format];
			Return[$Failed];
		];
		res = Quiet[Map[ReadImage[file, #]&, Range[seriesCount]]];
		If[res === $Failed,
			Message[Import::fmterr, format];
			Return[$Failed];
		];
		Return["ImageList" -> res];
		,
		res = Quiet[ReadImage[file, series]];
		If[res === $Failed,
			Message[Import::fmterr, format];
			Return[$Failed];
		];
		Return["ImageList" -> series -> res];
	];
];

GetBioFormatsOriginalMetaInformation[format_][file_] := Block[{res},
	res = Quiet[ReadOriginalMetadata[file]];
	If[res === $Failed,
		Message[Import::fmterr, format];
		Return[$Failed];
	];
	Return["OriginalMetaInformation" -> res];
];

GetBioFormatsOMEXMLMetaInformation[format_][file_] := Block[{res},
	res = Quiet[ReadOMEXMLMetadata[file]];
	If[res === $Failed,
		Message[Import::fmterr, format];
		Return[$Failed];
	];
	Return["OMEXMLMetaInformation" -> res];
];

If[$VersionNumber < 13.1,
	ImportExport`RegisterImport[#,
		{
			"ImageList" | {"ImageList", Automatic|All|"All"} :> GetBioFormatsImageList[#][All],
			{"ImageList", s:(_Integer)} :> GetBioFormatsImageList[#][s],
			"OriginalMetaInformation" :> GetBioFormatsOriginalMetaInformation[#],
			"OMEXMLMetaInformation" :> GetBioFormatsOMEXMLMetaInformation[#],
			"SeriesCount" :> GetBioFormatsSeriesCount[#],
			"Elements" :> GetBioFormatsElements,
			GetBioFormatsElements
		},
		"BinaryFormat" -> True,
		"AvailableElements" -> $BioFormatsAvailableElements,
		"DefaultElement" -> "ImageList"
	]& /@ {"BioFormats", "BioImageFormat"};
];

End[];
EndPackage[];

