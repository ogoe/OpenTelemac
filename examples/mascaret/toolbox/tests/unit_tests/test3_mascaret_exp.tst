// This file is released under the 3-clause BSD license. See COPYING-BSD.
//=================================
toolbox_dir=getenv("toolbox_dir");
c = filesep();

// creation du modele
[erreur, id] = MASCARET_create();
assert_checkequal(id,1);

// importation du modele
path_xml = "file://"+toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test3"+c+"data"+c+"MASCARET_EXPLICITE"+c+"xml"+c+"mascaret0.xcas";
TabNomFichier = [
                 strsubst(path_xml,'\','/'), ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test3"+c+"data"+c+"MASCARET_EXPLICITE"+c+"xml"+c+"mascaret0.geo", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test3"+c+"data"+c+"MASCARET_EXPLICITE"+c+"xml"+c+"mascaret0_0.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test3"+c+"data"+c+"MASCARET_EXPLICITE"+c+"xml"+c+"mascaret0_1.loi", ..
                 toolbox_dir+c+"mascaret0.lis", ..
                 toolbox_dir+c+"mascaret0_ecr.opt"];
 
TypeNomFichier = ["xcas","geo","loi","loi","listing","res"];
impression = 0;
erreur = MASCARET_importModel(id,TabNomFichier,TypeNomFichier,impression);
assert_checkequal(erreur,0);

// initialisation
erreur = MASCARET_initStateName(id,toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test3"+c+"data"+c+"MASCARET_EXPLICITE"+c+"xml"+c+"mascaret0.lig",impression);
assert_checkequal(erreur,0);

// acces aux pas de temps de simulation
[erreur,pasTps] = MASCARET_getDouble(id,"Model.DT",0,0,0);
assert_checkequal(erreur,0);
[erreur,T0] = MASCARET_getDouble(id,"Model.InitTime",0,0,0);
assert_checkequal(erreur,0);
[erreur,TF] = MASCARET_getDouble(id,"Model.MaxCompTime",0,0,0);
assert_checkequal(erreur,0);
TF = 3339.6;

// calcul
erreur = MASCARET_compute(id,T0,TF,pasTps,impression);
assert_checkequal(erreur,0);

// recuperation des resultats
[erreur,nbSec,taille2,taille3] = MASCARET_getSizeVar(id,"Model.X", 0);
Z = zeros(nbSec,1);
Zr = zeros(nbSec,1);
H = zeros(nbSec,1);
Q1 = zeros(nbSec,1);
Q2 = zeros(nbSec,1);
for i = 1:nbSec
    [erreur,Z(i)] = MASCARET_getDouble(id,"State.Z",i,0,0);
    assert_checkequal(erreur,0);
    [erreur,Q1(i)] = MASCARET_getDouble(id,"State.Q1",i,0,0);
    assert_checkequal(erreur,0);
    [erreur,Q2(i)] = MASCARET_getDouble(id,"State.Q2",i,0,0);
    assert_checkequal(erreur,0);
    [erreur,Zr(i)] = MASCARET_getDouble(id,"Model.Zbot",i,0,0);
    assert_checkequal(erreur,0);
end
H = Z - Zr;
ResRef = read(toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test3"+c+"ref"+c+"res.txt",nbSec,11);

// test de la solution sur le debit mineur
code_retour = assert_checkalmostequal(Q1,ResRef(:,5),%eps,1.D-3);
assert_checktrue(code_retour);

// test de la solution sur le debit majeur
code_retour = assert_checkalmostequal(Q2,ResRef(:,6),%eps,1.D-3);
assert_checktrue(code_retour);

// test de la solution sur la hauteur
code_retour = assert_checkalmostequal(H,ResRef(:,7),%eps,1.D-3);
assert_checktrue(code_retour);

// destruction du modele
erreur=MASCARET_delete(id);
assert_checkequal(erreur,0);

