(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["AntonAntonov`PaLMMode`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


PaLMMode::usage = "Restyle notebooks to use the PaLM external execution theme.";

PaLMInputExecute::usage = "Umbrella execution function for PaLMLink functions. \
Used PaLMInputExecuteToText and PaLMInputExecuteToImage.";

PaLMInputExecuteToText::usage = "Execution function for the cell style \"PaLMInputExecuteToText\".";

PaLMInputExecuteToChat::usage = "Execution function for the cell style \"PaLMInputExecuteToChat\".";

PaLMModeNotebookStyle::usage = "The PaLMMode notebook style.";

DeleteCells::usage = "Delete cells of a specified style.";

CellPrintWL::usage = "CellPrintWL[s_String]";

CellPrintAndRunWL::usage = "CellPrintAndRunWL[s_String]";

CellPrintJulia::usage = "CellPrintJulia[s_String]";

CellPrintAndRunJulia::usage = "CellPrintAndRunJulia[s_String]";

CellPrintR::usage = "CellPrintR[s_String]";

CellPrintAndRunR::usage = "CellPrintAndRunR[s_String]";

CellPrintPython::usage = "CellPrintPython[s_String]";

CellPrintAndRunPython::usage = "CellPrintAndRunPython[s_String]";

PacletInstall["AntonAntonov/PaLMLink", AllowVersionUpdate -> False];

Begin["`Private`"];

Needs["AntonAntonov`PaLMLink`"];

(***********************************************************)
(* Input execution                                         *)
(***********************************************************)

nbPaLMStyle =
    Notebook[{
      Cell[StyleData[StyleDefinitions -> "Default.nb"]],

      Cell[StyleData["Input"],
        StyleKeyMapping -> {
          "|" -> "PaLMInputExecuteToText",
          "!" -> "PaLMInputExecuteToText",
          "=" -> "WolframAlphaShort",
          ">" -> "ExternalLanguage",
          "Tab" -> "PaLMInputExecuteToText"}],

      Cell[StyleData["PaLMInputExecuteToText"],
        CellFrame -> True,
        CellMargins -> {{66, 10}, {5, 10}},
        StyleKeyMapping -> {"Tab" -> "PaLMInputExecuteToChat"},
        Evaluatable -> True,
        CellEvaluationFunction -> (AntonAntonov`PaLMMode`PaLMInputExecuteToText[ToString[#1], Options[AntonAntonov`PaLMMode`PaLMInputExecuteToText]] &),
        CellFrameColor -> GrayLevel[0.92],
        CellFrameLabels -> {{Cell[BoxData[rbPaLM]], None}, {None, None}},
        AutoQuoteCharacters -> {}, FormatType -> InputForm,
        MenuCommandKey :> "8", FontFamily -> "Courier",
        FontWeight -> Bold, Magnification -> 1.15` Inherited,
        FontColor -> GrayLevel[0.4], Background -> RGBColor[0.97, 1, 0.95]
      ],

      Cell[StyleData["PaLMInputExecuteToText", "SlideShow"], FontSize -> 20],

      Cell[StyleData["PaLMInputExecuteToChat"],
        CellFrame -> True,
        CellMargins -> {{66, 10}, {5, 10}},
        StyleKeyMapping -> {"Tab" -> "PaLMInputExecuteToText"},
        Evaluatable -> True,
        CellEvaluationFunction -> (AntonAntonov`PaLMMode`PaLMInputExecuteToChat[ToString[#1], Options[AntonAntonov`PaLMMode`PaLMInputExecuteToChat]] &),
        CellFrameColor -> GrayLevel[0.92],
        CellFrameLabels -> {{Cell[BoxData[rbPaLM]], None}, {None, None}},
        AutoQuoteCharacters -> {}, FormatType -> InputForm,
        MenuCommandKey :> "8", FontFamily -> "Courier",
        FontWeight -> Bold, Magnification -> 1.15` Inherited,
        FontColor -> GrayLevel[0.4], Background -> RGBColor[1, 1, 0.95]
      ],

      Cell[StyleData["PaLMInputExecuteToChat", "SlideShow"], FontSize -> 20],

      Cell[StyleData["Code"],
        MenuSortingValue -> 10000,
        MenuCommandKey :> None
      ]
    },
      WindowSize -> {857, 887},
      WindowMargins -> {{373, Automatic}, {Automatic, 219}},
      FrontEndVersion -> "13.2.1 for Mac OS X ARM (64-bit) (January 27, 2023)",
      StyleDefinitions -> "PrivateStylesheetFormatting.nb"
    ];


(***********************************************************)
(* Notebook style                                          *)
(***********************************************************)

Clear[PaLMModeNotebookStyle];
PaLMModeNotebookStyle[] := nbPaLMStyle;

(***********************************************************)
(* Input execution                                         *)
(***********************************************************)

Clear[FullFunctionName];
FullFunctionName[func_] :=
    Which[
      MemberQ[{PaLMGenerateText, "PaLMGenerateText", AntonAntonov`PaLMLink`PaLMGenerateText}, func],
      AntonAntonov`PaLMLink`PaLMGenerateText,

      MemberQ[{PaLMGenerateMessage, "PaLMGenerateMessage", AntonAntonov`PaLMLink`PaLMGenerateMessage}, func],
      AntonAntonov`PaLMLink`PaLMGenerateMessage,


      True,
      AntonAntonov`PaLMLink`PaLMGenerateText
    ];

Clear[PaLMInputExecute];
Options[PaLMInputExecute] = {
  Function -> PaLMGenerateText,
  Epilog -> Identity,
  "APIKey" :> $PaLMAPIKey,
  "User" :> $PaLMUser,
  "Model" -> Automatic,
  "Temperature" -> Automatic,
  "TopProbability" -> Automatic,
  "TopTokensCount" -> Automatic,
  "MaxOutputTokens" -> Automatic,
  "StopSequences" -> Automatic,
  "SafetySettings" -> Automatic
};

PaLMInputExecute[boxData : (_String | _PaLMChatMessageObject), opts : OptionsPattern[]] :=
    Block[{epilogFunc = OptionValue[PaLMInputExecute, Epilog],
      func = FullFunctionName @ OptionValue[PaLMInputExecute, Function]},
      epilogFunc @ func[boxData, FilterRules[{opts}, Options[func]]]
    ];

(***********************************************************)
(* Delegation of execution                                 *)
(***********************************************************)

Clear[PaLMInputExecuteToText];
Options[PaLMInputExecuteToText] = Options[AntonAntonov`PaLMLink`PaLMGenerateText];
Options[PaLMInputExecuteToText] = Append[Options[PaLMInputExecuteToText], Epilog -> Identity];
PaLMInputExecuteToText[boxData_String, opts : OptionsPattern[]] :=
    PaLMInputExecute[boxData, Function -> AntonAntonov`PaLMLink`PaLMGenerateText, opts];

Clear[PaLMInputExecuteToChat];
Options[PaLMInputExecuteToChat] = Options[AntonAntonov`PaLMLink`PaLMGenerateMessage];
Options[PaLMInputExecuteToChat] = Append[Options[PaLMInputExecuteToChat], Epilog -> Identity];
PaLMInputExecuteToChat[boxData_String, opts : OptionsPattern[]] :=
    PaLMInputExecute[boxData, Function -> AntonAntonov`PaLMLink`PaLMGenerateMessage, opts];

(***********************************************************)
(* PaLMMode function                                     *)
(***********************************************************)

Clear[PaLMMode] ;
Options[PaLMMode] := {"TokenLimit" -> Automatic, ImageSize -> Small};

PaLMMode[True] := PaLMMode[];
PaLMMode[True, opts : OptionsPattern[]] := PaLMMode[opts];

PaLMMode[] := PaLMMode[EvaluationNotebook[]];
PaLMMode[opts : OptionsPattern[]] := PaLMMode[EvaluationNotebook[], opts];

PaLMMode[nb_NotebookObject, True, opts : OptionsPattern[]] := PaLMMode[nb, opts];

PaLMMode[nb_NotebookObject, opts : OptionsPattern[]] :=
    Block[{tokenLimit, imgSize},
      tokenLimit = OptionValue[PaLMMode, "TokenLimit"];
      If[IntegerQ[tokenLimit] && tokenLimit > 0 || TrueQ[tokenLimit === Automatic],
        SetOptions[PaLMInputExecuteToText, "MaxOutputTokens" -> tokenLimit]
      ];

      (*
      imgSize = OptionValue[PaLMMode, ImageSize];
      SetOptions[PaLMInputExecuteToImage, ImageSize -> imgSize];
      *)
      SetOptions[nb, StyleDefinitions -> BinaryDeserialize[BinarySerialize[nbPaLMStyle]]]
    ];

PaLMMode[False] := SetOptions[EvaluationNotebook[], StyleDefinitions -> "Default.nb"];

PaLMMode[nb_NotebookObject, False] := SetOptions[nb, StyleDefinitions -> "Default.nb"];

(*===========================================================*)
(* CellPrint                                                 *)
(*===========================================================*)

Clear[CellPrintWL];
CellPrintWL[s_String] := NotebookWrite[EvaluationNotebook[], Cell[s, "Input"], All];

Clear[CellPrintAndRunWL];
CellPrintAndRunWL[s_String] := (
  NotebookWrite[EvaluationNotebook[], Cell[s, "Input"], All];
  SelectionEvaluateCreateCell[EvaluationNotebook[]]
);

Clear[CellPrintJulia];
CellPrintJulia[s_String] :=
    NotebookWrite[EvaluationNotebook[], Cell[s, "ExternalLanguage", CellEvaluationLanguage -> "Julia"]];

Clear[CellPrintAndRunJulia];
CellPrintAndRunJulia[s_String] := (
  NotebookWrite[EvaluationNotebook[], Cell[s, "ExternalLanguage", CellEvaluationLanguage -> "Julia"], All];
  SelectionEvaluateCreateCell[EvaluationNotebook[]]
);

Clear[CellPrintR];
CellPrintR[s_String] :=
    NotebookWrite[EvaluationNotebook[], Cell["{\n" <> s <> "\n}", "ExternalLanguage", CellEvaluationLanguage -> "R"]];

Clear[CellPrintAndRunR];
CellPrintAndRunR[s_String] := (
  NotebookWrite[EvaluationNotebook[], Cell["{\n" <> s <> "\n}", "ExternalLanguage", CellEvaluationLanguage -> "R"], All];
  SelectionEvaluateCreateCell[EvaluationNotebook[]]);

Clear[CellPrintPython];
CellPrintPython[s_String] :=
    NotebookWrite[EvaluationNotebook[], Cell[s, "ExternalLanguage", CellEvaluationLanguage -> "Python"]];

Clear[CellPrintAndRunPython];
CellPrintAndRunPython[s_String] := (
  NotebookWrite[EvaluationNotebook[], Cell[s, "ExternalLanguage", CellEvaluationLanguage -> "Python"], All];
  SelectionEvaluateCreateCell[EvaluationNotebook[]]
);

Clear[CellPrintRaku];
CellPrintRaku[s_String] :=
    NotebookWrite[EvaluationNotebook[], Cell[s, "RakuInputExecute"]];

Clear[CellPrintAndRunRaku];
CellPrintAndRunRaku[s_String] := (
  NotebookWrite[EvaluationNotebook[], Cell[s, "RakuInputExecute"], All];
  SelectionEvaluateCreateCell[EvaluationNotebook[]]
);

aTargetLanguageToCellPrintFunc =
    <| "R" -> CellPrintR, "Python" -> CellPrintPython, "Julia" -> CellPrintJulia, "WL" -> CellPrintWL|>;

aTargetLanguageToCellPrintAndRunFunc =
    <| "R" -> CellPrintAndRunR, "Python" -> CellPrintAndRunPython, "Julia" -> CellPrintAndRunJulia, "WL" -> CellPrintAndRunWL|>;


(***********************************************************)
(* Icon                                                    *)
(***********************************************************)

rbPaLM =
    GraphicsBox[
      TagBox[RasterBox[CompressedData["
1:eJzt20FqlFEQBOAhK5deIbdw69JtJAdIcAxuRpgIwWt5Am82ZkIW6mT+/1V3
9av3hiqIIDTdX/0YFebP9f33m69Xm83m8d3zLzd3Tx/3+7ufn98//+Z29/jt
Ybf98mn3Y/uw3X+4P479fv06OI7jOI7jOI7jOI7jOI7jOBeQXycZb2P7pexF
/sbAqehB/sbEMfgefWH2GnKRvI52sPEidxvzYstN5i6+f+0qb5OkgIgP3T1/
GNrS/S/Q1dNCPuO4lJ8/L+ZnAXJ+jjAAP4MYgh9nDMIP/zswCj8oGYcfsozE
D/wXZiw+XmAwPgoajg+RwOHxCozIrynQkV9RoCufX6Azn12gO59bQMBnFpDw
eQVEfFYBGZ9TQMhnFNBpKQXO7a2l/3UsyV/sVY2vzdR4VgWlPt9ArT9mbv0x
k/NjBdTmfzM5Hy6g5p5mcj5SQC09l8n5jQXUyIXM7m8poCYuZ3L+agE1bz2T
83nvt6gyOZ/2fo4uF+pXs9pzkX41Con92kz+B+giv3+naXBOD2ZAkkgvaMDm
Kz9Hmq5BDX/E15dGLFDH79KglF9foJhfXaCcP9Drw+MV6MHXvz7PeSdCx8+/
K6H0LwwKfnIzwl96p0RXADgc6VZeADgbKlfsR67G2tUWQG4G66n9q7PIsyAX
gA6GCyr9DcPBx9Gb3/JOYecC2LFMR42/bTrxSHryG99J7VgAPJSs2dvfPJ58
LEX+9nFsbyc/MI4t5vnRI4Sq1ALoCUbXPn5sHlxe7wfnwe3lftK8yo/Os/qS
/Og8XrjUj84HGlf60flI5UI/Oh/qXOdH52Oly/zofLB1lR+dj9Ym8f/fDI6H
e1Pop4vB8XhxAvyNveB4onma/dZacBwF1fgP2M6EH31UjcFWZvzgo2oMtjLl
B58VQoKGw/5DAf9lKzCa8r8siBBZSfvFsV8b+7WxXxv7tbFfG/u1sV8b+7Wx
Xxv7tbFfG/vFmZw/vb/009wumZw/vb/us8RumZx/KPokyHEcx3Ecef4AhlnA
PA==
    "], {{0, 192.}, {192., 0}}, {0, 255},
        ColorFunction->GrayLevel],
        BoxForm`ImageTag["Byte", ColorSpace -> "Grayscale", Interleaving -> None],
        Selectable->False],
      DefaultBaseStyle->"ImageGraphics",
      ImageSizeRaw->{22., 22.},
      PlotRange->{{0, 192.}, {0, 192.}}];

rbPaLMEdges =
    GraphicsBox[
      TagBox[RasterBox[CompressedData["
1:eJztmk1u21AMhI2suuwVeotuu+w2RQ+QoE7QjQs4BYJeyyfozdIU+bFl6z1x
SD6SAmaABFI0b+ajIudH8qfbX9d3V5vN5uHD86frm8cv+/3Nn28fn3e+7x5+
3u+2P77ufm/vt/vPt/9tf18/niiKoiiKoiiKoiiKoiiKoiiK8tHhcFhBZKNm
ooqJ0iZ7o38i3qUudA801OGFznHmQqzSNcyrU1zql5QyQBK+pFjQ7BKSN0Am
vr1dsH7sr2EbQDq+DaEAvgWiBL4eowi+FqQMvg6lEL7qx2glfAVNLXyYpxo+
SFQPH/srsiA+AlUSH/gnqia+/LwWxZef2KL44jNbFV86QFl86X2csvhdtmX+
VPA3CQaojK/nz8U+apl/1pLKPJXmG5BLfKG18/cv8hr4SwQAP5Trz94qazk6
yySxg+BnqmT8kuAY+uUJZr4oTI6hv2ha5JdHx9CfNy3w68/MMPqzpi4/Gh5D
P23q8CvSg/AbA0z3VelB+PMDTHaV6UH4swOc7qnTY+hnEU92LPFB+JeQx21b
fBD+Beb7pjU+CP/8vuHbpj0+mv/krHvgS/mt+FPW1w2X9CD8yRXkmh6EfzqA
b3oQ/vG+iXd6ML97eBD+bNGo3BH4yncQaHLH8KveQaDJHcXv/tKdjY3iHxRL
URRlU8H3b0DmAg8zFoGK8QvPZ/C9cn/8mgMA+BUHgPDLDdCnmKOFlybiH+Z8
6No0/pYNXp6D375S0PXF8KsMIC3HjoUNIK4GD0YNIC5Gj8bwy3vhwxEDAK34
8YABgE6FYTg/UqlxjB4AKVRZxvJDfTrP0AGgNqVpID9WpnWNGwCrUtvC+aVu
j9QwfDF/2ABgjcUZyA/YvZI9+RG7W3QEPsQfMQDaYHUH8IN+13hYaP5oPyo0
HuYJ5kf9+Aof7kY66lcs8aBuhaN+zRo7czMb9asWWYnb0ahft8rG20lG/cpl
FtpeMOrXrtOzdnNRv3qhlrSfC9oV18MY/PNHiUIKzfU8kB+06/jRKnkq5lbz
g1XiUMxt4Ae7RsjGny/y54r8uSJ/rsifK/Lnivy5In+uyJ8r8ueK/Lkif67I
nyvyJ2vl+KvnH/o0N0Qrxx/3LDFMK8d/epkgm4GiKIqiKIqqpH/YwW3w
    "], {{0, 192.}, {192., 0}}, {0, 255},
        ColorFunction->GrayLevel],
        BoxForm`ImageTag["Byte", ColorSpace -> "Grayscale", Interleaving -> None],
        Selectable->False],
      DefaultBaseStyle->"ImageGraphics",
      ImageSizeRaw->{22., 22.},
      PlotRange->{{0, 192.}, {0, 192.}}];

End[];
EndPackage[];
