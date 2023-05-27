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

PaLMModeCellIcon::usage = "Gives PaLM cells icon.";

PacletInstall["AntonAntonov/PaLMLink", AllowVersionUpdate -> False];

PacletInstall["AntonAntonov/NotebookModifiers", AllowVersionUpdate -> False];

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

Clear[PaLMModeCellIcon];
Options[PaLMModeCellIcon] = {"Edges" -> False, "Colors" -> False};
PaLMModeCellIcon[] := rbPaLM;
PaLMModeCellIcon[opts:OptionsPattern[]] :=
    Which[
      TrueQ @ OptionValue[PaLMModeCellIcon, "Edges"], rbPaLMEdges,
      TrueQ @ OptionValue[PaLMModeCellIcon, "Colors"], rbPaLMColor,
      True, rbPaLM
    ];

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
        ColorFunction -> GrayLevel],
        BoxForm`ImageTag["Byte", ColorSpace -> "Grayscale", Interleaving -> None],
        Selectable -> False],
      DefaultBaseStyle -> "ImageGraphics",
      ImageSizeRaw -> {22., 22.},
      PlotRange -> {{0, 192.}, {0, 192.}}];

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
        ColorFunction -> GrayLevel],
        BoxForm`ImageTag["Byte", ColorSpace -> "Grayscale", Interleaving -> None],
        Selectable -> False],
      DefaultBaseStyle -> "ImageGraphics",
      ImageSizeRaw -> {22., 22.},
      PlotRange -> {{0, 192.}, {0, 192.}}];

rbPaLMColor =
    GraphicsBox[
      TagBox[RasterBox[CompressedData["
1:eJztnU9sHFcdx1fAgaNxBOtyqQFx42CJS29xnSLEBVtFHLAdrVPVVHBI3Hir
gm2ydmvHJS3aqOVqr+kBqUnlMQiktQCtUW8maC0OCKkHB1Wq1Esdya7/Nh3e
b7wO7mZ3Z2b3vfm937zvR/rVbROvZ8ef75v3b2a/8dy1Z3/6hUwmM/ll9Y9n
r7z89PXrV375oy71Hz++OvmzF66OP/+Dq/nxF8avP/XcF9X/3KjVlzIAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAEB2/kO09/lXPxbM6Kny9j/uYpOIXq33+bzYvPqpitZf7mEAmE7g987Vr
x9M9paOpnsrRdM/28XTWj1hV+p7jmZ4CvYbL+Qj8fuPeNf/WZkFVxX/9H1VV
frTa3K59Tyl4DZUP7veTVoJ2fDpbJHdjeB65jqayO+r1PcoDXT+4368pqP0O
XH1901Pu7kR3PVZRhorIQ+dQG01umnA+7BqRliz83/k4bbumooypawv3OZBI
0EdJ3vsG14aeysl0tp/7fMRF+ddf658k63zjHFS4z4ckjmayY9zeNxw3TPfk
uM9NGMq3HEtbH1Zv3BvjPjdSUH3xNQt8b3w9UGNtG68HQXtPY1Juz5vW5hr3
OZKC8n+D2/PwHGQ9G8YHQf+exrPsfofWBve5ksLxVHaF2+/IpcboXOcpmLfk
9zpa3dpc4TpP0qD+BbvXca4FaozsF57sSur8qDa/y5qxbdQqVp1dZ2kHS8fA
LTKQ3UliXKDGkUMG5+7NFMa+bSEtA0EO1DGbOh+1eXx+n+F+YkjMAO3L0H0e
fNpnwO0y3GfB9QzAfeBqBuA+OMO1DMB9UI8rGYD7oBkiMzD1xETU9+ff2pxg
dxnuW43EDES51ya4J4XbZbgvAmkZoDWyVuvEtXVdrG2ByMjLQE/TvfDi9jTA
fSuQloGTmSeG6t+DuD4/3LcKSRmo7weJ6/fAfSuRlAHa43123LQ3mN1puJ8K
JGWA5oP84L4tC7yG+6lBSgaC+wakjHnhviikZODh4jK/23A/lUjIwEnhGX6/
4X5qkZCBz379J37P4X5qsT0DJ3PD/K7D/VRjdQZmvsXvO9xPPTZn4OHNN/m9
h/upx9YMnMwOwn2QCLZmAO6DpLAxA2xrAXDfSWzLwKevjMN90JTvze/1Xbq5
lxuY3ytcmt/1Bhb2KgPzu9vqq9+kKlRPL+yWgu9R30uvcf41bcpA4mthde57
uaO+1cv7udXR/YL6WlJfK1Te6IHfqNSfbQd/Z2Tfq31Pjl4jUSlSzDML+xfJ
25rHzRxvt1Qu9or0M2zKQJLu//Hy/kXlbbGV4+1WLTsF+hncHkni0uIng0F7
vbC7Y8D5JrW78/z85nt/vzHK7v9nr71j3P1/5++/p9rsHd3ON82C+ll0PfnD
5cNBbr9s5PuLB72n7XySzjeun8z/x39r7i3/g5nvsvj/cGHRqPv/vPphIs6H
9JkKXu6gl9s7bsj707ae1/lmtTj3u8Rz8OmrL6bW/ceyoK4JLubAdu85c2Bq
L5Bt7ruYg8Gi32VLPydu/XD+Q395dsH/eObbZv03MAdks/uPMkBjhKBf5Cf2
OSJJcmlxtz9krlJE0fjg3o1BMf5LcP9zORjd3/ZGDvq5fdUFtfk0z8jtre6i
MbKRDGjcCyrN/bocFKVfC4J5nYXdKrerpmr81U0j4wLX3T+XgarUccHAwidD
Evv5cYvGBbr7Q3D/XAZonUJYf0iNcce4vUy6/lz4uRX+p8n9uhrj9joKtf0K
7D5KzgDcb3YtOLzG7XcrJM3pm6qOx8U3noL7LWv/NrfnjRi4uTvB7Z4t1cl1
oJ35T3fcf1Rj3L6fx8X+vqkMxPXfQfetysDpPA+/bzbWRhv7SePcA+Ow+6fF
PC9E95K4MMfZbtHcaNz1gaj735x3f7Q2N8p0v02wlyfFa1u6itbI4uwZivIc
FLh/LgPBGlny68SY64leb879NrL/Yc9ChPsNMnB5v+PPGY8D+vzxK+oaMdxv
tw4f+0wpE9T6PejzxyzaNxrWD2o19wP3W1ewbzSBftCl+b3b3C5JrbB+ULOx
L9yPWmbXxk7ne/g9klyt5oMa3fsO92OWwf2iA2aeQ+JUTbzy18b+z3wT7mso
eu6KCfeD+7cs8CcN1WgsXH/f779e/IDdJbFlYF1MjXlXuL1JSzW6Bpyf9//v
1Pv8DsmuFe3+z+9ucHuTpvrcNeBc3wfua6iRgw3t/qP911qLc28/tucH7mur
Fd3+o/+vv87WA2jNF+5rLEP74jD3r7feKfwiGPfCfZ1ldg2gdl/7Frc7aagr
r235H91YtsCZFNTI/lZSeyCkkS15vReWV0vdy55vU739+wH/b2vf8UfW83ZV
+aVSrvJyL/fvDXTGkyWvq3t5tcDteTP3j+5mgrIyA6pGy/lCrlIQ/QwqV7lQ
8oa6l1a3uT0Pc9/2DKjaHi2/hH6FIFRfp8jteBz3BWTAH16fLHL/XkFrvlry
+lSbX+V2vB33JWRgpDxZxbjATmru73A73on7EjIwXM7v5Nbz+Iw7i+gueWPc
futyX0IGqEbX82Pcv3eQTveRARCFNLuPDIBWBP19Cxw36b6UDGA8kCxpGuum
IQMYEydHsKZr6bqWKfclZEDVNtaKzWPz/D6NR0y5f1bU37bA9cbXgfW8kXtv
wSnKrwl2x1u4T8do2n/6GTZnYKScn2CVJKXQHk5r+/w194kk/CdszUAwFsAa
sXaU+xV2z0PcJ5Lyn7A2A+gHacXafk+d+0SS/hO2ZgDrAnqozffY1+9p4D6R
tP+EjRk47QdhPqhTrGz7m7hPcPhP2JgBXAM6x7q5/hbuE1z+ExZmYFurDI5h
3f6eEPcJTv8J2zKAa0D7WNX2R3Cf4PafsCwDuAa0QXD/LrfzMd0nbPCfsCoD
f5nsb8cBl7mw5N1m9z6m+4Qt/hO2ZEAdh5Wf2W4zVvR9YrpP2OQ/YUkG0AeK
gRV7+9twn7DNf8KGDGB/dHS+suTNSnSfsNF/gj8Dk7PtHrtrsO5x7sB9wlb/
CdYMlCernRy7S0h1n7DZf4IzA50euwuw9f01uE/Y7j/BlQGMAcJhmffX5D4h
wX+CIwN4jmg4iY99NbpPSPGfSD4DGAOH0b3krUl1n5DkP5FsBibXdB9/2lBe
bkh1n5DmP5FYBsqTGyaOP00k4r8h9wmJ/hOJZAD+h2Lcf4PuE1L9J4xnAP6H
YvReR8PuE5L9JwxnAPuAQpDsPiHdf8JkBpI4fslIdp9Ig/+EqQwkdfxSUb7e
l+o+kRb/Ce0ZKOfvJ3n8EtE6/k3YfSJN/hNaM4Dxbyja/Gdwn0ib/4S+DGD9
Kwwt+x+Y3CfS6D+hJwPY/xBGx/4zuk+k1X+i8wzA/zA62v/J7D6RZv+JTjKA
/Z/hBM/7FOo+kXb/iXYzgOeBRiP2HKgl7hMu+E/EzcDwen6L+5ilcGHJW5Ho
PuGK/0ScDOAZQNGJ/NxPy9wnXPKfiJoBPAc0OsFnHQl0n3DNfyJKBvCZSPFo
eR+Ype4TLvpPtMrAcDm/wn180qjNA23Vuf/AZvcJV/0nggyUJx/Uj3sx79M+
5HuwJlbyJqhfxH08YbjsP0H9HPoMVFrrQp/fPVz3H7gN/AcuA/+By8B/4DLw
H7gM/AcuA/+By8B/4DLwH7gM/AcuA/+By8B/4DLwH7gM/AcuA/+By8B/4DLw
H7gM/AcuA/+By8B/4DLwH7gM/Acuc3gns2XK/cO7mQ3u9wdAK47uZNaMtf/q
tbnfHwCtOHo3M2bMf/Xa3O8PgFb4XqbLlP/02tzvD4AwlKu3Dfg/y/2+AIgC
tdNqrPpA47j3Adp+IImTu5l+bW2/l+njfj8AxEXLWBhjXiAYarvb6QsF34N2
H6SA2pzQbJQc1P7OLPr7IG0EOVD9mcM7mRVay1Vf7wd1+u8rh+9mhuA9AAAA
AAAAAAAAAAAAAAAAAAAAEJ//AfMvwps=
    "], {{0, 192.}, {192., 0}}, {0, 255},
        ColorFunction->RGBColor],
        BoxForm`ImageTag["Byte", ColorSpace -> "RGB", Interleaving -> True],
        Selectable->False],
      DefaultBaseStyle->"ImageGraphics",
      ImageSizeRaw->{22., 22.},
      PlotRange->{{0, 192.}, {0, 192.}}];

End[];
EndPackage[];
