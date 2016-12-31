// This file is released under the 3-clause BSD license. See COPYING-BSD.
//=================================
toolbox_dir=getenv("toolbox_dir");
c = filesep();

// creation du modele
[erreur, id] = MASCARET_create();
assert_checkequal(id,1);

// importation du modele
path_xml = "file://"+toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test21"+c+"data"+c+"xml"+c+"mascaret0.xcas";
TabNomFichier = [
                 strsubst(path_xml,'\','/'), ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test21"+c+"data"+c+"xml"+c+"mascaret0.geo", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test21"+c+"data"+c+"xml"+c+"mascaret0_0.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test21"+c+"data"+c+"xml"+c+"mascaret0_1.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test21"+c+"data"+c+"xml"+c+"mascaret0_2.loi", ..
                 toolbox_dir+c+"mascaret0.lis", ..
                 toolbox_dir+c+"mascaret0_ecr.opt"];
 
TypeNomFichier = ["xcas","geo","loi","loi","loi","listing","res"];
impression = 0;
erreur = MASCARET_importModel(id,TabNomFichier,TypeNomFichier,impression);
assert_checkequal(erreur,0);

// initialisation
erreur = MASCARET_initStateName(id,toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test21"+c+"data"+c+"xml"+c+"mascaret0.lig",impression);
assert_checkequal(erreur,0);

// acces aux pas de temps de simulation
[erreur,pasTps] = MASCARET_getDouble(id,"Model.DT",0,0,0);
assert_checkequal(erreur,0);
[erreur,T0] = MASCARET_getDouble(id,"Model.InitTime",0,0,0);
assert_checkequal(erreur,0);
[erreur,TF] = MASCARET_getDouble(id,"Model.MaxCompTime",0,0,0);
assert_checkequal(erreur,0);
TF = 9865.09;

ZB3  = zeros(9866,1); // evolution temporelle de la cote en x = 11 km (bief 3)
QB3  = zeros(9866,1); // evolution temporelle du debit en x = 11 km (bief 3)
ZB2  = zeros(9866,1); // evolution temporelle de la cote en x = 8.3 km (bief 2)
QB2  = zeros(9866,1); // evolution temporelle du debit en x = 8.3 km (bief 2)
TPS  = zeros(9866,1); // les temps de calcul

[erreur,ZB3(1)] = MASCARET_getDouble(id,"State.Z",110,0,0);
assert_checkequal(erreur,0);
[erreur,QB3(1)] = MASCARET_getDouble(id,"State.Q",110,0,0);
assert_checkequal(erreur,0);
[erreur,ZB2(1)] = MASCARET_getDouble(id,"State.Z",89,0,0);
assert_checkequal(erreur,0);
[erreur,QB2(1)] = MASCARET_getDouble(id,"State.Q",89,0,0);
assert_checkequal(erreur,0);
TPS(1) = 0.;

tpsCalcul = pasTps;
i = 2;
// calcul
while (tpsCalcul <= TF)
  erreur = MASCARET_compute(id,T0,tpsCalcul,pasTps,impression);
  assert_checkequal(erreur,0);
  T0 = tpsCalcul;
  tpsCalcul = tpsCalcul + pasTps;
  [erreur,ZB3(i)] = MASCARET_getDouble(id,"State.Z",110,0,0);
  assert_checkequal(erreur,0);
  [erreur,QB3(i)] = MASCARET_getDouble(id,"State.Q",110,0,0);
  assert_checkequal(erreur,0);
  [erreur,ZB2(i)] = MASCARET_getDouble(id,"State.Z",89,0,0);
  assert_checkequal(erreur,0);
  [erreur,QB2(i)] = MASCARET_getDouble(id,"State.Q",89,0,0);
  assert_checkequal(erreur,0);
  TPS(i) = T0;
  i = i + 1;
end

// recuperation des resultats de reference
ResRef = read(toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test21"+c+"ref"+c+"res.txt",11,3);
ResRef2 = read(toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test21"+c+"ref"+c+"res2.txt",7000,5);

// interpolation des resultats pour comparaison avec Telemac 2D
QB2_interpT2D = zeros(11,1);
QB3_interpT2D = zeros(11,1);
for i = 1:11
    QB2_interpT2D(i) = interpln([TPS';QB2'],ResRef(i,1));
    QB3_interpT2D(i) = interpln([TPS';QB3'],ResRef(i,1));
end

// interpolation des resultats pour comparaison avec Telemac 2D
QB2_interp = zeros(7000,1);
QB3_interp = zeros(7000,1);
ZB2_interp = zeros(7000,1);
ZB3_interp = zeros(7000,1);
for i = 1:7000
    QB2_interp(i) = interpln([TPS';QB2'],ResRef2(i,1));
    QB3_interp(i) = interpln([TPS';QB3'],ResRef2(i,1));
    ZB2_interp(i) = interpln([TPS';ZB2'],ResRef2(i,1));
    ZB3_interp(i) = interpln([TPS';ZB3'],ResRef2(i,1));
end

// comparaison des cotes et debits / solutions de references anterieures
code_retour = assert_checkalmostequal(ZB3_interp,ResRef2(:,2),1.D-3);
assert_checktrue(code_retour);
code_retour = assert_checkalmostequal(QB3_interp,ResRef2(:,3),%eps,71.);
assert_checktrue(code_retour);
code_retour = assert_checkalmostequal(ZB2_interp,ResRef2(:,4),3.D-3);
assert_checktrue(code_retour);
code_retour = assert_checkalmostequal(QB2_interp,ResRef2(:,5),%eps,53.);
assert_checktrue(code_retour);

// comparaison des debits / Telemac 2D
code_retour = assert_checkalmostequal(QB3_interpT2D,ResRef(:,2),%eps,390.);
assert_checktrue(code_retour);
code_retour = assert_checkalmostequal(QB2_interpT2D,ResRef(:,3),%eps,970.);
assert_checktrue(code_retour);

// destruction du modele
erreur=MASCARET_delete(id);
assert_checkequal(erreur,0);

