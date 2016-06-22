// This file is released under the 3-clause BSD license. See COPYING-BSD.
//=================================
toolbox_dir=getenv("toolbox_dir");
c = filesep();

// creation du modele
[erreur, id] = MASCARET_create();
assert_checkequal(id,1);

// importation du modele
path_xml = "file://"+toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test13"+c+"data"+c+"REZO"+c+"xml"+c+"mascaret0.xcas";
TabNomFichier = [
                 strsubst(path_xml,'\','/'), ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test13"+c+"data"+c+"REZO"+c+"xml"+c+"mascaret0.geo", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test13"+c+"data"+c+"REZO"+c+"xml"+c+"mascaret0_0.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test13"+c+"data"+c+"REZO"+c+"xml"+c+"mascaret0_1.loi", ..
                 toolbox_dir+c+"mascaret0.lis", ..
                 toolbox_dir+c+"mascaret0_ecr.opt"];
 
TypeNomFichier = ["xcas","geo","loi","loi","listing","res"];
impression = 0;
erreur = MASCARET_importModel(id,TabNomFichier,TypeNomFichier,impression);
assert_checkequal(erreur,0);

// initialisation
erreur = MASCARET_initStateName(id,toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test13"+c+"data"+c+"REZO"+c+"xml"+c+"mascaret0.lig",impression);
assert_checkequal(erreur,0);

// acces aux pas de temps de simulation
[erreur,pasTps] = MASCARET_getDouble(id,"Model.DT",0,0,0);
assert_checkequal(erreur,0);
[erreur,T0] = MASCARET_getDouble(id,"Model.InitTime",0,0,0);
assert_checkequal(erreur,0);
[erreur,TF] = MASCARET_getDouble(id,"Model.MaxCompTime",0,0,0);
assert_checkequal(erreur,0);
TF = 86400.;

Q0    = zeros(289,1); // evolution temporelle du debit en x = 0 m
Q2000 = zeros(289,1); // evolution temporelle du debit en x = 2000 m
Q4000 = zeros(289,1); // evolution temporelle du debit en x = 4000 m

[erreur,Q0(1)] = MASCARET_getDouble(id,"State.Q",1,0,0); // section amont
assert_checkequal(erreur,0);
[erreur,Q2000(1)] = MASCARET_getDouble(id,"State.Q",21,0,0); // section intermediaie
assert_checkequal(erreur,0);
[erreur,Q4000(1)] = MASCARET_getDouble(id,"State.Q",41,0,0); // section aval
assert_checkequal(erreur,0);

tpsCalcul = pasTps;
i = 2;
// calcul
while (tpsCalcul <= TF)
  erreur = MASCARET_compute(id,T0,tpsCalcul,pasTps,impression);
  assert_checkequal(erreur,0);
  T0 = tpsCalcul;
  tpsCalcul = tpsCalcul + pasTps;
  [erreur,Q0(i)] = MASCARET_getDouble(id,"State.Q",1,0,0);
  assert_checkequal(erreur,0);
  [erreur,Q2000(i)] = MASCARET_getDouble(id,"State.Q",21,0,0);
  assert_checkequal(erreur,0);
  [erreur,Q4000(i)] = MASCARET_getDouble(id,"State.Q",41,0,0);
  assert_checkequal(erreur,0);
  i = i + 1;
end

ResRef = read(toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test13"+c+"ref"+c+"res1.txt",289,4);

// test de la solution sur la debit en x = 0 m
code_retour = assert_checkalmostequal(Q0,ResRef(:,2),%eps,1.D-3);
assert_checktrue(code_retour);

// test de la solution sur le debit en x = 2000 m
code_retour = assert_checkalmostequal(Q2000,ResRef(:,3),%eps,1.D-3);
assert_checktrue(code_retour);

// test de la solution sur le debit en x = 4000 m
code_retour = assert_checkalmostequal(Q4000,ResRef(:,4),%eps,1.D-3);
assert_checktrue(code_retour);

// destruction du modele
erreur=MASCARET_delete(id);
assert_checkequal(erreur,0);
