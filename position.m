(* ::Package:: *)

GetXY[input_]:= Block[{Xi,Eta,X3,Y3,RA,DEC,output},

(* It returns the cartesian coordinates in arcsec 
of an object around a galaxy. *)

RA=Table[input[[i,1]],{i,Length[input]}];
DEC=Table[input[[i,2]],{i,Length[input]}];

Xi=Table[Sin[RA[[i]] Degree-RA0 Degree]*Cos[DEC[[i]] Degree],{i,Length[RA]}]*180/Pi*3600;
Eta=Table[((Sin[DEC[[i]]Degree]*Cos[DEC0 Degree])-Cos[RA[[i]] Degree-RA0 Degree]*Cos[DEC[[i]] Degree]*Sin[DEC0 Degree]),{i,Length[DEC]}]*180/Pi*3600;

X3=Table[-Xi[[i]]*Sin[PAphot Degree]-Eta[[i]]*Cos[PAphot Degree],{i,Length[Eta]}];
Y3=Table[-Xi[[i]]*Cos[PAphot Degree]+Eta[[i]]*Sin[PAphot Degree],{i,Length[Eta]}];

output=Table[{X3[[i]],Y3[[i]]},{i,Length[X3]}];

Return[output]
]


(* - - - - - - - - - - - - - - - - - - - - - -  *)

GetRc[input_]:= Block[{Xi,Eta,X3,Y3,RA,DEC,Rc},

(* It returns the circularized galactocentric radius in arcsec 
of an object around a galaxy.*)

RA=Table[input[[i,1]],{i,Length[input]}];
DEC=Table[input[[i,2]],{i,Length[input]}];

Xi=Table[Sin[RA[[i]] Degree-RA0 Degree]*Cos[DEC[[i]] Degree],{i,Length[RA]}]*180/Pi*3600;
Eta=Table[((Sin[DEC[[i]]Degree]*Cos[DEC0 Degree])-Cos[RA[[i]] Degree-RA0 Degree]*Cos[DEC[[i]] Degree]*Sin[DEC0 Degree]),{i,Length[DEC]}]*180/Pi*3600;

X3=Table[-Xi[[i]]*Sin[PAphot Degree]-Eta[[i]]*Cos[PAphot Degree],{i,Length[Eta]}];
Y3=Table[-Xi[[i]]*Cos[PAphot Degree]+Eta[[i]]*Sin[PAphot Degree],{i,Length[Eta]}];

output=Table[Sqrt[Y3[[i]]^2/axial+(axial X3[[i]]^2)],{i,Length[X3]}];

Return[output]

]


GetPA[input_]:= Block[{Xi,Eta,X3,Y3,RA,DEC,Rc},

(* It returns the position angle in degrees 
of an object around a galaxy.*)


RA=Table[input[[i,1]],{i,Length[input]}];
DEC=Table[input[[i,2]],{i,Length[input]}];

Xi=Table[Sin[RA[[i]] Degree-RA0 Degree]*Cos[DEC[[i]] Degree],{i,Length[RA]}]*180/Pi*3600;
Eta=Table[((Sin[DEC[[i]]Degree]*Cos[DEC0 Degree])-Cos[RA[[i]] Degree-RA0 Degree]*Cos[DEC[[i]] Degree]*Sin[DEC0 Degree]),{i,Length[DEC]}]*180/Pi*3600;

X3=Table[-Xi[[i]]*Sin[PAphot Degree]-Eta[[i]]*Cos[PAphot Degree],{i,Length[Eta]}];
Y3=Table[-Xi[[i]]*Cos[PAphot Degree]+Eta[[i]]*Sin[PAphot Degree],{i,Length[Eta]}];

output=Table[2*ArcTan[Eta[[i]]/(Sqrt[Xi[[i]]^2+Eta[[i]]^2]+Xi[[i]])]*180/Pi,{i,Length[Xi]}];

Return[output]

]
