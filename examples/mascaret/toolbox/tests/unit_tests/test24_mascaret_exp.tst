// This file is released under the 3-clause BSD license. See COPYING-BSD.
//=================================
toolbox_dir=getenv("toolbox_dir");
c = filesep();

// creation du modele
[erreur, id] = MASCARET_create();
assert_checkequal(id,1);

// importation du modele
path_xml = "file://"+toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test24"+c+"data"+c+"MASCARET_EXPLICITE"+c+"xml"+c+"mascaret0.xcas";
TabNomFichier = [
                 strsubst(path_xml,'\','/'), ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test24"+c+"data"+c+"MASCARET_EXPLICITE"+c+"xml"+c+"mascaret0.geo", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test24"+c+"data"+c+"MASCARET_EXPLICITE"+c+"xml"+c+"mascaret0_0.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test24"+c+"data"+c+"MASCARET_EXPLICITE"+c+"xml"+c+"mascaret0_1.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test24"+c+"data"+c+"MASCARET_EXPLICITE"+c+"xml"+c+"mascaret0_2.loi", ..
                 toolbox_dir+c+"mascaret0.lis", ..
                 toolbox_dir+c+"mascaret0_ecr.opt"];
 
TypeNomFichier = ["xcas","geo","loi","loi","loi","listing","res"];
impression = 0;
erreur = MASCARET_importModel(id,TabNomFichier,TypeNomFichier,impression);
assert_checkequal(erreur,0);

// initialisation
erreur = MASCARET_initStateName(id,toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test24"+c+"data"+c+"MASCARET_EXPLICITE"+c+"xml"+c+"mascaret0.lig",impression);
assert_checkequal(erreur,0);

// acces aux pas de temps de simulation
[erreur,pasTps] = MASCARET_getDouble(id,"Model.DT",0,0,0);
assert_checkequal(erreur,0);
[erreur,T0] = MASCARET_getDouble(id,"Model.InitTime",0,0,0);
assert_checkequal(erreur,0);
[erreur,TF] = MASCARET_getDouble(id,"Model.MaxCompTime",0,0,0);
assert_checkequal(erreur,0);
TF = 5782.0;
pasTps = 0.5;

// calcul
tpsCalcul = pasTps;
Z290m = 0.;
Z1600m = 0.;
Z2930m = 0.;
Z3830m = 0.;
Z5410m = 0.;
Z6720m = 0.;
Z8210m = 0.;
Z9005m = 0.;
Z10100m = 0.;
while (tpsCalcul <= TF)
  erreur = MASCARET_compute(id,T0,tpsCalcul,pasTps,impression);
  assert_checkequal(erreur,0);
  T0 = tpsCalcul;
  tpsCalcul = tpsCalcul + pasTps;
  [erreur,Zc] = MASCARET_getDouble(id,"State.Z",200,0,0); // section 200
  assert_checkequal(erreur,0);
  if(Zc>Z290m) then
      Z290m = Zc;
  end
  [erreur,Zc] = MASCARET_getDouble(id,"State.Z",250,0,0); // section 250
  assert_checkequal(erreur,0);
  if(Zc>Z1600m) then
      Z1600m = Zc;
  end
  [erreur,Zc] = MASCARET_getDouble(id,"State.Z",301,0,0); // section 301
  assert_checkequal(erreur,0);
  if(Zc>Z2930m) then
      Z2930m = Zc;
  end
  [erreur,Zc] = MASCARET_getDouble(id,"State.Z",336,0,0); // section 336
  assert_checkequal(erreur,0);
  if(Zc>Z3830m) then
      Z3830m = Zc;
  end
  [erreur,Zc] = MASCARET_getDouble(id,"State.Z",396,0,0); // section 396
  assert_checkequal(erreur,0);
  if(Zc>Z5410m) then
      Z5410m = Zc;
  end
  [erreur,Zc] = MASCARET_getDouble(id,"State.Z",447,0,0); // section 447
  assert_checkequal(erreur,0);
  if(Zc>Z6720m) then
      Z6720m = Zc;
  end
  [erreur,Zc] = MASCARET_getDouble(id,"State.Z",505,0,0); // section 505
  assert_checkequal(erreur,0);
  if(Zc>Z8210m) then
      Z8210m = Zc;
  end
  [erreur,Zc] = MASCARET_getDouble(id,"State.Z",536,0,0); // section 536
  assert_checkequal(erreur,0);
  if(Zc>Z9005m) then
      Z9005m = Zc;
  end
  [erreur,Zc] = MASCARET_getDouble(id,"State.Z",579,0,0); // section 579
  assert_checkequal(erreur,0);
  if(Zc>Z10100m) then
      Z10100m = Zc;
  end
end

// test de la solution la cote d'eau maximale a l'abscisse 290 m
code_retour = assert_checkalmostequal(Z290m,85.3,1.D-2);
assert_checktrue(code_retour);

// test de la solution la cote d'eau maximale a l'abscisse 1600 m
code_retour = assert_checkalmostequal(Z1600m,56.84,1.D-2);
assert_checktrue(code_retour);

// test de la solution la cote d'eau maximale a l'abscisse 2930 m
code_retour = assert_checkalmostequal(Z2930m,49.3,1.D-2);
assert_checktrue(code_retour);

// test de la solution la cote d'eau maximale a l'abscisse 3830 m
code_retour = assert_checkalmostequal(Z3830m,41.74,1.D-2);
assert_checktrue(code_retour);

// test de la solution la cote d'eau maximale a l'abscisse 5410 m
code_retour = assert_checkalmostequal(Z5410m,37.28,1.D-2);
assert_checktrue(code_retour);

// test de la solution la cote d'eau maximale a l'abscisse 6720 m
code_retour = assert_checkalmostequal(Z6720m,27.14,1.D-2);
assert_checktrue(code_retour);

// test de la solution la cote d'eau maximale a l'abscisse 8210 m
code_retour = assert_checkalmostequal(Z8210m,21.43,1.D-2);
assert_checktrue(code_retour);

// test de la solution la cote d'eau maximale a l'abscisse 9005 m
code_retour = assert_checkalmostequal(Z9005m,19.16,1.D-2);
assert_checktrue(code_retour);

// test de la solution la cote d'eau maximale a l'abscisse 10100 m
code_retour = assert_checkalmostequal(Z10100m,10.69,1.D-2);
assert_checktrue(code_retour);

// destruction du modele
erreur=MASCARET_delete(id);
assert_checkequal(erreur,0);

