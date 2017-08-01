(* Wolfram Language Package *)

BeginPackage["BioFormatsLink`", {"JLink`"}];

Begin["`Private`"];

ReadOMEXMLMetadata::nffil = ReadOriginalMetadata::nffil = ReadImage::nffil = "File `1` not found.";

$BioFormatsLinkPath = ParentDirectory[DirectoryName[$InputFileName]];
$BioFormatsJar = First@FileNames["bioformats_package.jar", FileNameJoin[{$BioFormatsLinkPath, "Java"}]];

InstallJava[];
JLink`AddToClassPath[$BioFormatsJar];
JLink`LoadJavaClass["java.awt.image.BufferedImage"];
JLink`LoadJavaClass["loci.formats.gui.BufferedImageReader"];

(*Convert pixel types and color space.*)
BioFormatToMathematicaTypeAndColorSpace[type_Integer] :=
	Switch[type,
		0, {"Real", Automatic},
		10, {"Byte", "Grayscale"},
		11, {"Bit16", "Grayscale"},
		12, {"Byte", "Grayscale"},
		_, {"Byte", "RGB"}
	];

(* Read image.*)
ReadImage[file_?StringQ] :=
	JavaBlock@Module[ {ir = JLink`JavaNew[JLink`LoadJavaClass["loci.formats.ImageReader"]],
		reader, image, raster, series, channels, timeSeries, slices, index, height, width, type, stack, colorspace = Automatic},
		If[!Quiet[FileExistsQ[file] === True],
			Message[ReadImage::nffil, file];
			Return[$Failed];
		];
		ir@setId[file];
		reader = BufferedImageReader`makeBufferedImageReader[ir];
		series = reader@getSeriesCount[];
		channels = reader@getEffectiveSizeC[];
		timeSeries = reader@getSizeT[];
		slices = reader@getSizeZ[];
		Table[
			stack = Table[
				ColorCombine[
					Table[
						index = reader@getIndex[z, c, t];
						image = reader@openImage[index];
						raster = image@getRaster[];
						height = image@getHeight[];
						width = image@getWidth[];
						{type, colorspace} = BioFormatToMathematicaTypeAndColorSpace[image@getType[]];
						Image[
							ArrayReshape[
								raster@getPixels[0, 0, width, height,
									JLink`JavaNew["[I", width * height]], {height, width}],
							type],
						{c, 0, channels - 1}
					],
					colorspace
				],
				{z, 0, slices - 1}
			];
			If[slices > 1,
				Map[Image, stack, {-1}]
				,
				Image @@ stack
			],
			{t, 0, timeSeries - 1}
		]
	];

(*Read original metadata*)
ReadOriginalMetadata[file_?StringQ] :=
	JavaBlock@Module[ {ir = JLink`JavaNew[JLink`LoadJavaClass["loci.formats.ImageReader"]], metastring},
		If[!Quiet[FileExistsQ[file] === True],
			Message[ReadOriginalMetadata::nffil, file];
			Return[$Failed];
		];
		ir@setId[file];
		metastring = ir@getCoreMetadataList[]@toString[];
		Map[
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
		]
	];

(*Read OME-XML metadata*)
ReadOMEXMLMetadata[file_?StringQ] :=
	JavaBlock@Module[ {ir = JLink`JavaNew[JLink`LoadJavaClass["loci.formats.ImageReader"]], factory, service, serviceClass, meta, metastring},
		If[!Quiet[FileExistsQ[file] === True],
			Message[ReadOMEXMLMetadata::nffil, file];
			Return[$Failed];
		];
		factory = JLink`JavaNew[JLink`LoadJavaClass["loci.common.services.ServiceFactory"]];
		LoadJavaClass["loci.formats.services.OMEXMLService"];
		LoadJavaClass["com.wolfram.jlink.JLinkClassLoader"];
		serviceClass = JLinkClassLoader`classFromName["loci.formats.services.OMEXMLService"];
		service = factory@getInstance[serviceClass];
		meta = service@createOMEXMLMetadata[];
		ir@setMetadataStore[meta];
		ir@setOriginalMetadataPopulated[False];
		ir@setId[file];
		metastring = service@getOMEXML[meta];
		Return[{ImportString[metastring, "XML"]}];
	];

$BioFormatsAvailableElements = {"ImageList", "OMEXMLMetaInformation", "OriginalMetaInformation"};

GetBioFormatsElements[___] := "Elements" -> $BioFormatsAvailableElements;

GetBioFormatsImageList[file_] := "ImageList" -> ReadImage[file];

GetBioFormatsOriginalMetaInformation[file_] := "OriginalMetaInformation" -> ReadOriginalMetadata[file];

GetBioFormatsOMEXMLMetaInformation[file_] := "OMEXMLMetaInformation" -> ReadOMEXMLMetadata[file];

ImportExport`RegisterImport["BioFormats",
	{
		"ImageList" :> GetBioFormatsImageList,
		"OriginalMetaInformation" :> GetBioFormatsOriginalMetaInformation,
		"OMEXMLMetaInformation" :> GetBioFormatsOMEXMLMetaInformation,
		"Elements" :> GetBioFormatsElements,
		GetBioFormatsElements
	},
	"BinaryFormat" -> True,
	"AvailableElements" -> $BioFormatsAvailableElements,
	"DefaultElement" -> "ImageList"
];

End[];
EndPackage[];

