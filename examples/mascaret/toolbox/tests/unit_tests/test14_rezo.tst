// This file is released under the 3-clause BSD license. See COPYING-BSD.
//=================================
toolbox_dir=getenv("toolbox_dir");
c = filesep();

// creation du modele
[erreur, id] = MASCARET_create();
assert_checkequal(id,1);

// importation du modele
path_xml = "file://"+toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test14"+c+"data"+c+"xml"+c+"mascaret0.xcas";
TabNomFichier = [
                 strsubst(path_xml,'\','/'), ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test14"+c+"data"+c+"xml"+c+"mascaret0.geo", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test14"+c+"data"+c+"xml"+c+"mascaret0_0.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test14"+c+"data"+c+"xml"+c+"mascaret0_1.loi", ..
                 toolbox_dir+c+"mascaret0.lis", ..
                 toolbox_dir+c+"mascaret0_ecr.opt"];
 
TypeNomFichier = ["xcas","geo","loi","loi","listing","res"];
impression = 0;
erreur = MASCARET_importModel(id,TabNomFichier,TypeNomFichier,impression);
assert_checkequal(erreur,0);

// initialisation
erreur = MASCARET_initStateName(id,toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test14"+c+"data"+c+"xml"+c+"mascaret0.lig",impression);
assert_checkequal(erreur,0);

// acces aux pas de temps de simulation
[erreur,pasTps] = MASCARET_getDouble(id,"Model.DT",0,0,0);
assert_checkequal(erreur,0);
[erreur,T0] = MASCARET_getDouble(id,"Model.InitTime",0,0,0);
assert_checkequal(erreur,0);
[erreur,TF] = MASCARET_getDouble(id,"Model.MaxCompTime",0,0,0);
assert_checkequal(erreur,0);
TF = 43200.;

Q5000  = zeros(145,1); // evolution temporelle du debit en x = 5000 m
Q10000 = zeros(145,1); // evolution temporelle du debit en x = 10000 m
Q15000 = zeros(145,1); // evolution temporelle du debit en x = 15000 m

[erreur,Q5000(1)] = MASCARET_getDouble(id,"State.Q",11,0,0); // section amont
assert_checkequal(erreur,0);
[erreur,Q10000(1)] = MASCARET_getDouble(id,"State.Q",21,0,0); // section intermediaie
assert_checkequal(erreur,0);
[erreur,Q15000(1)] = MASCARET_getDouble(id,"State.Q",31,0,0); // section aval
assert_checkequal(erreur,0);

tpsCalcul = pasTps;
i = 2;
// calcul
while (tpsCalcul <= TF)
  erreur = MASCARET_compute(id,T0,tpsCalcul,pasTps,impression);
  assert_checkequal(erreur,0);
  T0 = tpsCalcul;
  tpsCalcul = tpsCalcul + pasTps;
  [erreur,Q5000(i)] = MASCARET_getDouble(id,"State.Q",11,0,0);
  assert_checkequal(erreur,0);
  [erreur,Q10000(i)] = MASCARET_getDouble(id,"State.Q",21,0,0);
  assert_checkequal(erreur,0);
  [erreur,Q15000(i)] = MASCARET_getDouble(id,"State.Q",31,0,0);
  assert_checkequal(erreur,0);
  i = i + 1;
end

// recuperation des resultats
[erreur,nbSec,taille2,taille3] = MASCARET_getSizeVar(id,"Model.X", 0);
assert_checkequal(erreur,0);

Z = zeros(nbSec,1);
for i = 1:nbSec
    [erreur,Z(i)] = MASCARET_getDouble(id,"State.Z",i,0,0);
    assert_checkequal(erreur,0);
end

ResRef = read(toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test14"+c+"ref"+c+"res1.txt",145,5);
ResRef2 = read(toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test14"+c+"ref"+c+"res2.txt",801,3);

// test de la solution sur la debit en x = 5000 m
code_retour = assert_checkalmostequal(Q5000,ResRef(:,2),%eps,1.D-3);
assert_checktrue(code_retour);

// test de la solution sur le debit en x = 10000 m
code_retour = assert_checkalmostequal(Q10000,ResRef(:,3),%eps,1.D-3);
assert_checktrue(code_retour);

// test de la solution sur le debit en x = 15000 m
code_retour = assert_checkalmostequal(Q15000,ResRef(:,4),%eps,1.D-3);
assert_checktrue(code_retour);

// test de la solution sur le debit en x = 15000 m par rapport a la version 5.0
code_retour = assert_checkalmostequal(Q15000,ResRef(:,5),%eps,1.D-3);
assert_checktrue(code_retour);

// test de la solution sur la cote au dernier pas de temps
code_retour = assert_checkalmostequal(Z,ResRef2(:,2),%eps,1.D-3);
assert_checktrue(code_retour);
code_retour = assert_checkalmostequal(Z,ResRef2(:,3),%eps,1.D-3); // par rapport a la v5.0
assert_checktrue(code_retour);

// destruction du modele
erreur=MASCARET_delete(id);
assert_checkequal(erreur,0);

