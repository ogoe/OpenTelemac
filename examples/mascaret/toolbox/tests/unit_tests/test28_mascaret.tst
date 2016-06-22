// This file is released under the 3-clause BSD license. See COPYING-BSD.
//=================================
toolbox_dir=getenv("toolbox_dir");
c = filesep();

// creation du modele
[erreur, id] = MASCARET_create();
assert_checkequal(id,1);

// importation du modele
path_xml = "file://"+toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test28"+c+"data"+c+"xml"+c+"mascaret0.xcas";
TabNomFichier = [
                 strsubst(path_xml,'\','/'), ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test28"+c+"data"+c+"xml"+c+"mascaret0.geo", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test28"+c+"data"+c+"xml"+c+"mascaret0_0.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test28"+c+"data"+c+"xml"+c+"mascaret0_1.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test28"+c+"data"+c+"xml"+c+"mascaret0_2.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test28"+c+"data"+c+"xml"+c+"mascaret0_3.loi", ..
                 toolbox_dir+c+"mascaret0.lis", ..
                 toolbox_dir+c+"mascaret0_ecr.opt"];
 
TypeNomFichier = ["xcas","geo","loi","loi","loi","loi","listing","res"];
impression = 0;
erreur = MASCARET_importModel(id,TabNomFichier,TypeNomFichier,impression);
assert_checkequal(erreur,0);

// initialisation
erreur = MASCARET_initStateName(id,toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test28"+c+"data"+c+"xml"+c+"mascaret0.lig",impression);
assert_checkequal(erreur,0);

// acces aux pas de temps de simulation
[erreur,pasTps] = MASCARET_getDouble(id,"Model.DT",0,0,0);
assert_checkequal(erreur,0);
[erreur,T0] = MASCARET_getDouble(id,"Model.InitTime",0,0,0);
assert_checkequal(erreur,0);
[erreur,TF] = MASCARET_getDouble(id,"Model.MaxCompTime",0,0,0);
assert_checkequal(erreur,0);
TF = 6.0;
pasTps = 0.02;

// calcul
tpsCalcul = pasTps;
Z28m = zeros(600,1);
tpc  = zeros(600,1);
i = 1;
while (tpsCalcul <= TF)
  erreur = MASCARET_compute(id,T0,tpsCalcul,pasTps,impression);
  assert_checkequal(erreur,0);
  T0 = tpsCalcul;
  tpc(i) = T0;
  tpsCalcul = tpsCalcul + pasTps;
  [erreur,Zc] = MASCARET_getDouble(id,"State.Z",560,0,0); // section 560
  assert_checkequal(erreur,0);
  Z28m(i) = Zc;
  i = i + 1;
end

ResRef = read(toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test28"+c+"ref"+c+"res2.txt",284,2);

// interpolation necessaire sur le maillage d'origine
Zinterp = zeros(284,1);
for i = 1:284
    Zinterp(i) = interpln([tpc';Z28m'],ResRef(i,1));
end

// test de la solution par rapport a la solution de reference Mascaret v7.1.7
code_retour = assert_checkalmostequal(Zinterp,ResRef(:,2),7.D-2);
assert_checktrue(code_retour);

// destruction du modele
erreur=MASCARET_delete(id);
assert_checkequal(erreur,0);

