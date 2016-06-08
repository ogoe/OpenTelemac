// Demo file for MASCARET toolbox in SCILAB
//
//      Case : Dam break wave (the Ritter case)
//
//      Computation kernel : Unsteady supercritical
//
//      Author(s) : Fabrice ZAOUI
//
//      Copyright EDF 2014
//
clc();
[a,b,c]=getVersionMASCARET();
disp("***************************************");
disp("MASCARET demo : Dam break wave (Ritter)");
disp("***************************************");
disp("Library version : "+string(a)+'.'+string(b)+'.'+string(c));
disp("---------------------------------------------------------------");
disp("PARAMETERS :");
disp("     Channel length        = 5 km");
disp("     Channel width         = 1 m");
disp("     Inflow discharge      = 0 m^3/s");
disp("     Outflow discharge     = 0 m^3/s");
disp("     Strickler coefficient : No dissipation");
disp("     Bed slope             = 0");
disp("     Initial water depth   = 6 m for x < 2 km and 0 m elsewhere");
disp("---------------------------------------------------------------");

toolbox_dir=getenv("toolbox_dir");
c = filesep();

// creation of the MASCARET model
[erreur, id] = createMASCARET();
assert_checktrue(id>0);

// read data from files
path_xml = "file://"+toolbox_dir+c+"demos"+c+"Ritter"+c+"mascaret0.xcas";
TabNomFichier = [strsubst(path_xml,'\','/'), ..
                 toolbox_dir+c+"demos"+c+"Ritter"+c+"mascaret0.geo", ..
                 toolbox_dir+c+"demos"+c+"Ritter"+c+"mascaret0_0.loi", ..
                 toolbox_dir+c+"demos"+c+"Ritter"+c+"mascaret0_1.loi", ..
                 toolbox_dir+c+"mascaret0.lis", ..
                 toolbox_dir+c+"mascaret0_ecr.opt"];
 
TypeNomFichier = ["xcas","geo","loi","loi","listing","res"];
impression = 0;
erreur = importModelMASCARET(id,TabNomFichier,TypeNomFichier,impression);
assert_checkequal(erreur,0);

// initialisation
erreur = initStateNameMASCARET(id,toolbox_dir+c+"demos"+c+"Ritter"+c+"mascaret0.lig",impression);
assert_checkequal(erreur,0);

// get the time parameters
[erreur,pasTps] = getDoubleMASCARET(id,"Model.DT",0,0,0);
assert_checkequal(erreur,0);
[erreur,T0] = getDoubleMASCARET(id,"Model.InitTime",0,0,0);
assert_checkequal(erreur,0);
[erreur,TF] = getDoubleMASCARET(id,"Model.MaxCompTime",0,0,0);
assert_checkequal(erreur,0);

// get the number of cross sections
[erreur,nbSec,taille2,taille3] = getSizeVarMASCARET(id,"Model.X", 0);
assert_checkequal(erreur,0);

// initialise the results
Z = zeros(nbSec,1);
Q = zeros(nbSec,1);
X = zeros(nbSec,1);
for i = 1:nbSec
    [erreur,Z(i)] = getDoubleMASCARET(id,"State.Z",i,0,0);
    assert_checkequal(erreur,0);
    [erreur,Q(i)] = getDoubleMASCARET(id,"State.Q",i,0,0);
    assert_checkequal(erreur,0);
    [erreur,X(i)] = getDoubleMASCARET(id,"Model.X",i,0,0);
    assert_checkequal(erreur,0);
end

// plot the initial state
f=figure();
f.background=35;
subplot(2,1,1);
X = X / 1000;
plot2d(X,Z);
a1 = gca();
e1 = gce();
a1.font_size = 3;
a1.title.font_size = 3;
a1.x_label.font_size = 3;
a1.y_label.font_size = 3;
a1.data_bounds(1,2) = 0;
a1.data_bounds(2,2) = 6.5;
a1.title.text = "Spatial results";
a1.x_label.text = "Channel length (km)";
a1.y_label.text = "Level (m)";
e1.children.foreground = color("blue");
e1.children.thickness = 1.5;
h2 = legend(['Water level'],1);

subplot(2,1,2);
plot2d(X,Q);
b1 = gca();
f1 = gce();
b1.font_size = 3;
b1.title.font_size = 3;
b1.x_label.font_size = 3;
b1.y_label.font_size = 3;
b1.data_bounds(1,2) = 0;
b1.data_bounds(2,2) = 14;
//b1.title.text = "Spatial results";
b1.x_label.text = "Channel length (km)";
b1.y_label.text = "Discharge (m^3/s)";
f1.children.foreground = color("red");
f1.children.thickness = 1.5;
g2 = legend(['Water discharge'],1);

// plotting update frequency
freqplot = 200;

// computation
tpsCalcul = pasTps;
j = 1;
k = 1;
while (tpsCalcul <= TF)
  erreur = computeMASCARET(id,T0,tpsCalcul,pasTps,impression);
  assert_checkequal(erreur,0);
  // get the spatial results on the water level
  if(modulo(j,freqplot)==0) then
     for i = 1:nbSec
       [erreur,Z(i)] = getDoubleMASCARET(id,"State.Z",i,0,0);
       assert_checkequal(erreur,0);
       [erreur,Q(i)] = getDoubleMASCARET(id,"State.Q",i,0,0);
       assert_checkequal(erreur,0);
     end
     e1.children.data = [X Z];
     f1.children.data = [X Q];
     a1.title.text = "Spatial results (at time "+string(T0)+" s)";
     j = 1;
  else
     j = j + 1;
  end
  T0 = tpsCalcul;
  tpsCalcul = tpsCalcul + pasTps;
end

// model deletion
erreur=deleteMASCARET(id);
assert_checkequal(erreur,0);

disp("--> Computation done with the unsteady supercritical kernel (FV Roe Scheme)");
disp("");
