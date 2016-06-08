// This file is released under the 3-clause BSD license. See COPYING-BSD.
//=================================
toolbox_dir=getenv("toolbox_dir");
c = filesep();

// creation du modele
[erreur, id] = createMASCARET();
assert_checkequal(id,1);

// importation du modele
path_xml = "file://"+toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test27"+c+"data"+c+"xml"+c+"mascaret0.xcas";
TabNomFichier = [
                 strsubst(path_xml,'\','/'), ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test27"+c+"data"+c+"xml"+c+"mascaret0.geo", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test27"+c+"data"+c+"xml"+c+"mascaret0.casier", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test27"+c+"data"+c+"xml"+c+"mascaret0_0.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test27"+c+"data"+c+"xml"+c+"mascaret0_1.loi", ..
                 toolbox_dir+c+"mascaret0.lis", ..
                 toolbox_dir+c+"mascaret0.cas_lis", ..
                 toolbox_dir+c+"mascaret0.liai_lis", ..
                 toolbox_dir+c+"mascaret0_ecr.opt", ..
                 toolbox_dir+c+"mascaret0_ecr.cas_opt", ..
                 toolbox_dir+c+"mascaret0_ecr.liai_opt"];
 
TypeNomFichier = ["xcas","geo","casier","loi","loi","listing","listing_casier","listing_liaison","res","res_casier","res_liaison"];
impression = 0;
erreur = importModelMASCARET(id,TabNomFichier,TypeNomFichier,impression);
assert_checkequal(erreur,0);

// initialisation
erreur = initStateNameMASCARET(id,toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test27"+c+"data"+c+"xml"+c+"mascaret0.lig",impression);
assert_checkequal(erreur,0);

// acces aux pas de temps de simulation
[erreur,pasTps] = getDoubleMASCARET(id,"Model.DT",0,0,0);
assert_checkequal(erreur,0);
[erreur,T0] = getDoubleMASCARET(id,"Model.InitTime",0,0,0);
assert_checkequal(erreur,0);
[erreur,TF] = getDoubleMASCARET(id,"Model.MaxCompTime",0,0,0);
assert_checkequal(erreur,0);

// recuperation nombre de sections
[erreur,nbSec,taille2,taille3] = getSizeVarMASCARET(id,"Model.X", 0);
assert_checkequal(erreur,0);

// resultats temporels
ZCG2 = zeros(4201,1); // evolution temporelle de la cote du casier G2
ZCG4 = zeros(4201,1); // evolution temporelle de la cote du casier G4
ZCG5 = zeros(4201,1); // evolution temporelle de la cote du casier G5
ZCG7 = zeros(4201,1); // evolution temporelle de la cote du casier G7
ZS15 = zeros(4201,1); // evolution temporelle de la cote a la section 15 (pk = 300)
ZS39 = zeros(4201,1); // evolution temporelle de la cote a la section 39 (pk = 900)
ZS63 = zeros(4201,1); // evolution temporelle de la cote a la section 63 (pk = 1500)
ZS87 = zeros(4201,1); // evolution temporelle de la cote a la section 87 (pk = 2100)
TPS  = zeros(4201,1); // les temps de calcul

// premieres valeurs
[erreur,ZCG2(1)] = getDoubleMASCARET(id,"State.StoArea.Level",2,0,0);
assert_checkequal(erreur,0);
[erreur,ZCG4(1)] = getDoubleMASCARET(id,"State.StoArea.Level",4,0,0);
assert_checkequal(erreur,0);
[erreur,ZCG5(1)] = getDoubleMASCARET(id,"State.StoArea.Level",5,0,0);
assert_checkequal(erreur,0);
[erreur,ZCG7(1)] = getDoubleMASCARET(id,"State.StoArea.Level",7,0,0);
assert_checkequal(erreur,0);
[erreur,ZS15(1)] = getDoubleMASCARET(id,"State.Z",15,0,0);
assert_checkequal(erreur,0);
[erreur,ZS39(1)] = getDoubleMASCARET(id,"State.Z",39,0,0);
assert_checkequal(erreur,0);
[erreur,ZS63(1)] = getDoubleMASCARET(id,"State.Z",63,0,0);
assert_checkequal(erreur,0);
[erreur,ZS87(1)] = getDoubleMASCARET(id,"State.Z",87,0,0);
assert_checkequal(erreur,0);

TPS(1) = 0.;
tpsCalcul = pasTps;
i = 2;

// calcul
while (tpsCalcul <= TF)
  erreur = computeMASCARET(id,T0,tpsCalcul,pasTps,impression);
  assert_checkequal(erreur,0);
  T0 = tpsCalcul;
  tpsCalcul = tpsCalcul + pasTps;
  [erreur,ZCG2(i)] = getDoubleMASCARET(id,"State.StoArea.Level",2,0,0);
  assert_checkequal(erreur,0);
  [erreur,ZCG4(i)] = getDoubleMASCARET(id,"State.StoArea.Level",4,0,0);
  assert_checkequal(erreur,0);
  [erreur,ZCG5(i)] = getDoubleMASCARET(id,"State.StoArea.Level",5,0,0);
  assert_checkequal(erreur,0);
  [erreur,ZCG7(i)] = getDoubleMASCARET(id,"State.StoArea.Level",7,0,0);
  assert_checkequal(erreur,0);
  [erreur,ZS15(i)] = getDoubleMASCARET(id,"State.Z",15,0,0);
  assert_checkequal(erreur,0);
  [erreur,ZS39(i)] = getDoubleMASCARET(id,"State.Z",39,0,0);
  assert_checkequal(erreur,0);
  [erreur,ZS63(i)] = getDoubleMASCARET(id,"State.Z",63,0,0);
  assert_checkequal(erreur,0);
  [erreur,ZS87(i)] = getDoubleMASCARET(id,"State.Z",87,0,0);
  assert_checkequal(erreur,0);
  TPS(i) = T0;
  i = i + 1;
end

// lescture des resultats CARIMA
ResRef1 = read(toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test27"+c+"ref"+c+"res1.txt",203,5); // l'evolution de la cote sur le bief
ResRef2 = read(toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test27"+c+"ref"+c+"res2.txt",203,5); // l'evolution de la cote dans les casiers

// interpolation des resultats pour comparaison
ZCG2_interp  = zeros(203,1);
ZCG4_interp  = zeros(203,1);
ZCG5_interp  = zeros(203,1);
ZCG7_interp  = zeros(203,1);
ZS15_interp  = zeros(203,1);
ZS39_interp  = zeros(203,1);
ZS63_interp  = zeros(203,1);
ZS87_interp  = zeros(203,1);
for i = 1:203
    ZCG2_interp(i) = interpln([TPS';ZCG2'],ResRef2(i,1));
    ZCG4_interp(i) = interpln([TPS';ZCG4'],ResRef2(i,1));
    ZCG5_interp(i) = interpln([TPS';ZCG5'],ResRef2(i,1));
    ZCG7_interp(i) = interpln([TPS';ZCG7'],ResRef2(i,1));
    ZS15_interp(i) = interpln([TPS';ZS15'],ResRef1(i,1));
    ZS39_interp(i) = interpln([TPS';ZS39'],ResRef1(i,1));
    ZS63_interp(i) = interpln([TPS';ZS63'],ResRef1(i,1));
    ZS87_interp(i) = interpln([TPS';ZS87'],ResRef1(i,1));
end

// comparaison des resultats sur les cotes du bief
code_retour = assert_checkalmostequal(ZS15_interp,ResRef1(:,2),1.D-3);
assert_checktrue(code_retour);
code_retour = assert_checkalmostequal(ZS39_interp,ResRef1(:,3),2.D-3);
assert_checktrue(code_retour);
code_retour = assert_checkalmostequal(ZS63_interp,ResRef1(:,4),3.D-3);
assert_checktrue(code_retour);
code_retour = assert_checkalmostequal(ZS87_interp,ResRef1(:,5),2.D-3);
assert_checktrue(code_retour);

// comparaison des resultats sur les cotes des casiers
code_retour = assert_checkalmostequal(ZCG2_interp,ResRef2(:,2),3.D-4);
assert_checktrue(code_retour);
code_retour = assert_checkalmostequal(ZCG4_interp,ResRef2(:,3),9.D-3);
assert_checktrue(code_retour);
code_retour = assert_checkalmostequal(ZCG5_interp,ResRef2(:,4),9.D-3);
assert_checktrue(code_retour);
code_retour = assert_checkalmostequal(ZCG7_interp,ResRef2(:,5),2.D-2);
assert_checktrue(code_retour);

// destruction du modele
erreur=deleteMASCARET(id);
assert_checkequal(erreur,0);

