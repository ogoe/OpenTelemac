// This file is released under the 3-clause BSD license. See COPYING-BSD.
//=================================
toolbox_dir=getenv("toolbox_dir");
c = filesep();

// creation du modele
[erreur, id] = MASCARET_create();
assert_checkequal(id,1);

// importation du modele
path_xml = "file://"+toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test16"+c+"data"+c+"SARAP_CHOC"+c+"xml"+c+"mascaret0.xcas";
TabNomFichier = [
                 strsubst(path_xml,'\','/'), ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test16"+c+"data"+c+"SARAP_CHOC"+c+"xml"+c+"mascaret0.geo", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test16"+c+"data"+c+"SARAP_CHOC"+c+"xml"+c+"mascaret0_0.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test16"+c+"data"+c+"SARAP_CHOC"+c+"xml"+c+"mascaret0_1.loi", ..
                 toolbox_dir+c+"mascaret0.lis", ..
                 toolbox_dir+c+"mascaret0_ecr.opt"];
 
TypeNomFichier = ["xcas","geo","loi","loi","listing","res"];
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

// calcul
erreur = MASCARET_compute(id,T0,TF,pasTps,impression);
assert_checkequal(erreur,0);

// recuperation des resultats
Z = zeros(nbSec,1);
X = zeros(nbSec,1);
for i = 1:nbSec
    [erreur,Z(i)] = MASCARET_getDouble(id,"State.Z",i,0,0);
    assert_checkequal(erreur,0);
    [erreur,X(i)] = MASCARET_getDouble(id,"Model.X",i,0,0);
    assert_checkequal(erreur,0);
end

[erreur,Q] = MASCARET_getDouble(id,"State.Q",20,0,0); // section no. 20 au hasard
assert_checkequal(erreur,0);

ResRef1 = read(toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test16"+c+"ref"+c+"res1.txt",nbSec,7);
ResRef2 = read(toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test16"+c+"ref"+c+"res2.txt",102,4);

// test de la solution sur le debit
code_retour = assert_checkequal(Q,0.18);
assert_checktrue(code_retour);

// test de la solution sur la cote / solution anterieure de reference
code_retour = assert_checkalmostequal(Z,ResRef1(:,2),%eps,1.D-3);
assert_checktrue(code_retour);

// test de la solution sur la cote / solution analytique
Zinterp = zeros(102,1);
for i = 1:102
    Zinterp(i) = interpln([X';Z'],ResRef2(i,1));
end

code_retour = assert_checkalmostequal(Zinterp,ResRef2(:,4),%eps,5.D-2);
assert_checktrue(code_retour);

// destruction du modele
erreur=MASCARET_delete(id);
assert_checkequal(erreur,0);

