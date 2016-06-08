// This file is released under the 3-clause BSD license. See COPYING-BSD.
//=================================
toolbox_dir=getenv("toolbox_dir");
c = filesep();

// creation du modele
[erreur, id] = createMASCARET();
assert_checkequal(id,1);

// importation du modele
path_xml = "file://"+toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test23"+c+"data"+c+"xml"+c+"mascaret0.xcas";
TabNomFichier = [
                 strsubst(path_xml,'\','/'), ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test23"+c+"data"+c+"xml"+c+"mascaret0.geo", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test23"+c+"data"+c+"xml"+c+"mascaret0_0.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test23"+c+"data"+c+"xml"+c+"mascaret0_1.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test23"+c+"data"+c+"xml"+c+"mascaret0_2.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test23"+c+"data"+c+"xml"+c+"mascaret0_3.loi", ..
                 toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test23"+c+"data"+c+"xml"+c+"mascaret0_4.loi", ..
                 toolbox_dir+c+"mascaret0.lis", ..
                 toolbox_dir+c+"mascaret0_ecr.opt"];
 
TypeNomFichier = ["xcas","geo","loi","loi","loi","loi","loi","listing","res"];
impression = 0;
erreur = importModelMASCARET(id,TabNomFichier,TypeNomFichier,impression);
assert_checkequal(erreur,0);

// initialisation
erreur = initStateNameMASCARET(id,toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test23"+c+"data"+c+"xml"+c+"mascaret0.lig",impression);
assert_checkequal(erreur,0);

// acces aux pas de temps de simulation
[erreur,pasTps] = getDoubleMASCARET(id,"Model.DT",0,0,0);
assert_checkequal(erreur,0);
[erreur,T0] = getDoubleMASCARET(id,"Model.InitTime",0,0,0);
assert_checkequal(erreur,0);
[erreur,TF] = getDoubleMASCARET(id,"Model.MaxCompTime",0,0,0);
assert_checkequal(erreur,0);
TF = 7723.;

Q237  = zeros(7723,1); // evolution temporelle du debit en x = 9911 m (bief 1)
Q238  = zeros(7723,1); // evolution temporelle du debit en x = 10186 m (bief 2)
Q2326  = zeros(7723,1); // evolution temporelle du debit en x = 1000225 m (bief 5)
TPS  = zeros(7723,1); // les temps de calcul

[erreur,Q237(1)] = getDoubleMASCARET(id,"State.Q",237,0,0);
assert_checkequal(erreur,0);
[erreur,Q238(1)] = getDoubleMASCARET(id,"State.Q",238,0,0);
assert_checkequal(erreur,0);
[erreur,Q2326(1)] = getDoubleMASCARET(id,"State.Q",2326,0,0);
TPS(1) = 0.;

tpsCalcul = pasTps;
i = 2;
// calcul
while (tpsCalcul <= TF)
  erreur = computeMASCARET(id,T0,tpsCalcul,pasTps,impression);
  assert_checkequal(erreur,0);
  T0 = tpsCalcul;
  tpsCalcul = tpsCalcul + pasTps;
  [erreur,Q237(i)] = getDoubleMASCARET(id,"State.Q",237,0,0);
  assert_checkequal(erreur,0);
  [erreur,Q238(i)] = getDoubleMASCARET(id,"State.Q",238,0,0);
  assert_checkequal(erreur,0);
  [erreur,Q2326(i)] = getDoubleMASCARET(id,"State.Q",2326,0,0);

  TPS(i) = T0;
  i = i + 1;
end

// recuperation des resultats de reference
ResRef = read(toolbox_dir+c+".."+c+"test"+c+"Test_Plan"+c+"Test23"+c+"ref"+c+"res.txt",301,4);

// interpolation des resultats pour comparaison
Q237_interp  = zeros(301,1);
Q238_interp  = zeros(301,1);
Q2326_interp  = zeros(301,1);
for i = 1:301
    Q237_interp(i) = interpln([TPS';Q237'],ResRef(i,1));
    Q238_interp(i) = interpln([TPS';Q238'],ResRef(i,1));
    Q2326_interp(i) = interpln([TPS';Q2326'],ResRef(i,1));
end

// comparaison des cotes et debits / solutions de references anterieures
code_retour = assert_checkalmostequal(Q237_interp,ResRef(:,2),1.D0);
assert_checktrue(code_retour);
code_retour = assert_checkalmostequal(Q238_interp,ResRef(:,3),1.1D0);
assert_checktrue(code_retour);
code_retour = assert_checkalmostequal(Q2326_interp,ResRef(:,4),1.1D0);
assert_checktrue(code_retour);

// destruction du modele
erreur=deleteMASCARET(id);
assert_checkequal(erreur,0);

