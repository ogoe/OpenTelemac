// This file is released under the 3-clause BSD license. See COPYING-BSD.
//=================================
toolbox_dir=getenv("toolbox_dir");
c = filesep();

// creation du modele
[erreur, id] = MASCARET_create();
assert_checkequal(id,1);

// importation du modele
path_xml = "file://"+toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test19"+c+"data"+c+"SARAP"+c+"xml"+c+"mascaret0.xcas";
TabNomFichier = [
                 strsubst(path_xml,'\','/'), ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test19"+c+"data"+c+"SARAP"+c+"xml"+c+"mascaret0.geo", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test19"+c+"data"+c+"SARAP"+c+"xml"+c+"mascaret0_0.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test19"+c+"data"+c+"SARAP"+c+"xml"+c+"mascaret0_1.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test19"+c+"data"+c+"SARAP"+c+"xml"+c+"mascaret0_2.loi", ..
                 toolbox_dir+c+"mascaret0.lis", ..
                 toolbox_dir+c+"mascaret0_ecr.opt"];
 
TypeNomFichier = ["xcas","geo","loi","loi","loi","listing","res"];
impression = 0;
erreur = MASCARET_importModel(id,TabNomFichier,TypeNomFichier,impression);
assert_checkequal(erreur,0);

// initialisation
[erreur,nbSec,taille2,taille3] = MASCARET_getSizeVar(id,"Model.X", 0);
Qinit = zeros(nbSec,1);
Zinit = 2*ones(nbSec,1);
erreur = MASCARET_initState(id,Qinit,Zinit);
assert_checkequal(erreur,0);

// acces aux pas de temps de simulation
[erreur,pasTps] = MASCARET_getDouble(id,"Model.DT",0,0,0);
assert_checkequal(erreur,0);
[erreur,T0] = MASCARET_getDouble(id,"Model.InitTime",0,0,0);
assert_checkequal(erreur,0);
[erreur,TF] = MASCARET_getDouble(id,"Model.MaxCompTime",0,0,0);
assert_checkequal(erreur,0);
TF = 1.0;

// calcul
erreur = MASCARET_compute(id,T0,TF,pasTps,impression);
assert_checkequal(erreur,0);

// recuperation des resultats
Z = zeros(nbSec,1);
Q = zeros(nbSec,1);
F = zeros(nbSec,1);
H = zeros(nbSec,1);
ZF = zeros(nbSec,1);
for i = 1:nbSec
    [erreur,Z(i)] = MASCARET_getDouble(id,"State.Z",i,0,0);
    assert_checkequal(erreur,0);
    [erreur,Q(i)] = MASCARET_getDouble(id,"State.Q",i,0,0);
    assert_checkequal(erreur,0);
    [erreur,F(i)] = MASCARET_getDouble(id,"State.Froude",i,0,0);
    assert_checkequal(erreur,0);
    [erreur,ZF(i)] = MASCARET_getDouble(id,"Model.Zbot",i,0,0);
    assert_checkequal(erreur,0);
    H(i) = Z(i) - ZF(i);
end

ResRef = read(toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test19"+c+"ref"+c+"res.txt",nbSec,9);

// test de la solution sur le debit
code_retour = assert_checkalmostequal(Q(1:569),12.36*ones(1:569)',%eps,1.D-10);
assert_checktrue(code_retour);
code_retour = assert_checkalmostequal(Q(570:nbSec),13.21*ones(1:316)',%eps,1.D-10);
assert_checktrue(code_retour);

// test de la solution sur la cote / solution anterieure de reference
code_retour = assert_checkalmostequal(Z,ResRef(:,2),%eps,1.D-3);
assert_checktrue(code_retour);

// test de la solution sur la hauteur / solution anterieure de reference
code_retour = assert_checkalmostequal(H,ResRef(:,4),%eps,1.D-3);
assert_checktrue(code_retour);

// test de la solution sur le nombre de Froude / solution anterieure de reference
code_retour = assert_checkalmostequal(F,ResRef(:,3),%eps,1.D-3);
assert_checktrue(code_retour);

// destruction du modele
erreur=MASCARET_delete(id);
assert_checkequal(erreur,0);

