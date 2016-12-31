// This file is released under the 3-clause BSD license. See COPYING-BSD.
//=================================
toolbox_dir=getenv("toolbox_dir");
c = filesep();

// creation du modele
[erreur, id] = MASCARET_create();
assert_checkequal(id,1);

// importation du modele
path_xml = "file://"+toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test7"+c+"data"+c+"REZO"+c+"xml"+c+"mascaret0.xcas";
TabNomFichier = [
                 strsubst(path_xml,'\','/'), ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test7"+c+"data"+c+"REZO"+c+"xml"+c+"mascaret0.geo", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test7"+c+"data"+c+"REZO"+c+"xml"+c+"mascaret0_0.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test7"+c+"data"+c+"REZO"+c+"xml"+c+"mascaret0_1.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test7"+c+"data"+c+"REZO"+c+"xml"+c+"mascaret0_2.loi", ..
                 toolbox_dir+c+"mascaret0.lis", ..
                 toolbox_dir+c+"mascaret0_ecr.opt"];
 
TypeNomFichier = ["xcas","geo","loi","loi","loi","listing","res"];
impression = 0;
erreur = MASCARET_importModel(id,TabNomFichier,TypeNomFichier,impression);
assert_checkequal(erreur,0);

// initialisation
erreur = MASCARET_initStateName(id,toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test7"+c+"data"+c+"REZO"+c+"cas"+c+"mascaret0.lig",impression);
assert_checkequal(erreur,0);

// acces aux pas de temps de simulation
[erreur,pasTps] = MASCARET_getDouble(id,"Model.DT",0,0,0);
assert_checkequal(erreur,0);
[erreur,T0] = MASCARET_getDouble(id,"Model.InitTime",0,0,0);
assert_checkequal(erreur,0);
[erreur,TF] = MASCARET_getDouble(id,"Model.MaxCompTime",0,0,0);
assert_checkequal(erreur,0);
TF = 10000.;

ZTP = zeros(1001);
[erreur,ZTP(1)] = MASCARET_getDouble(id,"State.Z",41,0,0); // x = 4000 m en section no. 41
assert_checkequal(erreur,0);

tpsCalcul = pasTps;
i = 2;
// calcul
while (tpsCalcul <= TF)
  erreur = MASCARET_compute(id,T0,tpsCalcul,pasTps,impression);
  assert_checkequal(erreur,0);
  T0 = tpsCalcul;
  tpsCalcul = tpsCalcul + pasTps;
  [erreur,ZTP(i)] = MASCARET_getDouble(id,"State.Z",41,0,0); // x = 4000 m en section no. 41
  assert_checkequal(erreur,0);
  i = i + 1;
end

// recuperation des resultats
[erreur,nbSec,taille2,taille3] = MASCARET_getSizeVar(id,"Model.X", 0);
assert_checkequal(erreur,0);

// recuperation des resultats
Z = zeros(nbSec,1);
for i = 1:nbSec
    [erreur,Z(i)] = MASCARET_getDouble(id,"State.Z",i,0,0);
    assert_checkequal(erreur,0);
end

ResRef = read(toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test7"+c+"ref"+c+"res1.txt",nbSec,5);
ResRefTP = read(toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test7"+c+"ref"+c+"res2.txt",1001,2);

// test de la solution sur la cote
code_retour = assert_checkalmostequal(Z,ResRef(:,4),%eps,1.D-3);
assert_checktrue(code_retour);

// test de la solution sur la cote par rapport a la v5.0
code_retour = assert_checkalmostequal(Z,ResRef(:,5),%eps,1.D-3);
assert_checktrue(code_retour);

// test de la solution sur l'evolution de la cote en x = 4000 m
code_retour = assert_checkalmostequal(ZTP,ResRefTP(:,2),%eps,1.D-6);
assert_checktrue(code_retour);

// destruction du modele
erreur=MASCARET_delete(id);
assert_checkequal(erreur,0);

