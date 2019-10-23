(* ::Package:: *)

(* ::Section:: *)
(*Definition*)


(* ::Subsubsection:: *)
(*Wrap Function*)


Options[QRImageEmbed] = {
	Method -> "V2",
	"Level" -> "H"
};
QRImageEmbed[text_, img_, o : OptionsPattern[]] := Block[
	{lv = OptionValue["Level"]},
	Switch[
		OptionValue[Method],
		"V2", createQR[text, img, lv],
		"V1", createQR$V1[text, img, lv],
		_, Print["No such method"];Return[]
	]
];


(* ::Subsubsection::Closed:: *)
(*Main Functions*)


createQR$V1[text_, img_, lv_ : "H"] := Block[
	{qr = BarcodeImage[text, {"QR", lv}, 1], data},
	data = ImageData@ColorConvert[Rasterize[img, ImageSize -> (3ImageDimensions@qr)], "Grayscale"];
	Image@replicate@refineQR$V1@dithering[data, ImageData@qr]
];
createQR[text_, img_Image, lv_ : "H"] := Block[
	{qr = BarcodeImage[text, {"QR", lv}, 1], data},
	data = ImageData@ImageResize[ColorConvert[img, "Grayscale"], 3ImageDimensions@qr];
	Image@replicate@refineQR@dithering[data, ImageData@qr]
];
createQR[text_, img_Graphics, lv_ : "H"] := Block[
	{qr = BarcodeImage[text, {"QR", lv}, 1], data},
	data = ImageData@ColorConvert[Rasterize[img, ImageSize -> (3ImageDimensions@qr)], "Grayscale"];
	Image@replicate@refineQR@dithering[data, ImageData@qr]
];


(* ::Subsubsection::Closed:: *)
(*Utility Functions*)


replicate = (Flatten[ConstantArray[#, {3, 3}], {{3, 1}, {4, 2}}] &);

refineQR$V1[data_] := Block[
	{qrd = data, d = Length[data]},
	(*Corner*)
	(qrd[[#1 ;; 24 #1 ;; #1, #2 ;; 24 #2 ;; #2]] = replicate[qrd[[2 #1 ;; 23 #1 ;; 3 #1, 2 #2 ;; 23 #2 ;; 3 #2]]]) & @@@ {{1, 1}, {1, -1}, {-1, 1}};
	(*Edge*)
	qrd[[22 ;; d - 21, 19 ;; 21]] = Transpose[qrd[[19 ;; 21, 22 ;; d - 21]] = replicate[{Mod[Range[(d + 1) / 3 - 14], 2]}]];
	Return@qrd
];

refineQR[data_] := Block[
	{
		qrd = data, d = Length[data], p,
		temp = Fold[ArrayPad[#1, 1, #2] &, {{{0}}, 1, 0}]
	},
	p = Position[Round@ListCorrelate[temp, data[[2 ;; ;; 3, 2 ;; ;; 3]], {{1, 1}, {-1, -1}}, 0, Abs@*Subtract], 0, 2];
	(*Corner*)
	(qrd[[#1 ;; 24 #1 ;; #1, #2 ;; 24 #2 ;; #2]] = replicate[qrd[[2 #1 ;; 23 #1 ;; 3 #1, 2 #2 ;; 23 #2 ;; 3 #2]]]) & @@@ {{1, 1}, {1, -1}, {-1, 1}};
	(*Edge*)
	qrd[[22 ;; d - 21, 19 ;; 21]] = Transpose[qrd[[19 ;; 21, 22 ;; d - 21]] = replicate[{Mod[Range[(d + 1) / 3 - 14], 2]}]];
	(*Special*)
	(qrd[[3 #1 - 2 ;; 3 #1 + 12, 3 #2 - 2 ;; 3 #2 + 12]] = replicate@temp) & @@@ p;
	Return@qrd
];

dithering[image_, data_] := Block[
	{
		img = image, f = UnitStep[# - .5] &,
		dimX, dimY, tmp1, tmp2
	},
	{dimX, dimY} = Dimensions@image;
	Quiet@Do[
		(*Rounding*)
		tmp1 = If[
			Mod[{i, j}, 3] == {2, 2}, data[[(i + 1) / 3, (j + 1) / 3]],
			f[img[[i, j]]]
		];
		tmp2 = Clip[img[[i, j]] - tmp1, {-.5, .5}];
		(*Diffuse Error*)
		img[[i, j]] = tmp1;
		img[[i, j + 1]] += 0.4375 tmp2;
		If[j != 1, img[[i + 1, j - 1]] += 0.1875 tmp2];
		img[[i + 1, j]] += 0.3125 tmp2;
		img[[i + 1, j + 1]] += 0.0625 tmp2,
		{i, dimX}, {j, dimY}
	];
	img
];
