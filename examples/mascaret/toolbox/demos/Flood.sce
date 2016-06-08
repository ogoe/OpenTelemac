// Demo file for MASCARET toolbox in SCILAB
//
//      Case : schematic flood wave in a flume
//
//      Computation kernel : Unsteady subcritical
//
//      Author(s) : Fabrice ZAOUI
//
//      Copyright EDF 2014
//
clc();
[a,b,c]=getVersionMASCARET();
disp("*************************************************");
disp("MASCARET demo : Schematic flood wave in a channel");
disp("*************************************************");
disp("Library version : "+string(a)+'.'+string(b)+'.'+string(c));
disp("-----------------------------------------------------------------------");
disp("PARAMETERS :");
disp("     Channel length        = 10 km");
disp("     Channel width         = 100 m");
disp("     Inflow discharge      = triangular wave with a peak at 1000 m^3/s");
disp("     Outflow discharge     = 0 m^3/s");
disp("     Strickler coefficient = 30 m^(1/3)/s");
disp("     Bed slope             = 0");
disp("     Initial water depth   = 0.5 m");
disp("-----------------------------------------------------------------------");

toolbox_dir=getenv("toolbox_dir");
c = filesep();

// creation of the MASCARET model
[erreur, id] = createMASCARET();
assert_checktrue(id>0);

// read data from files
path_xml = "file://"+toolbox_dir+c+"demos"+c+"Flood"+c+"mascaret0.xcas";
TabNomFichier = [strsubst(path_xml,'\','/'), ..
                 toolbox_dir+c+"demos"+c+"Flood"+c+"mascaret0.geo", ..
                 toolbox_dir+c+"demos"+c+"Flood"+c+"mascaret0_0.loi", ..
                 toolbox_dir+c+"demos"+c+"Flood"+c+"mascaret0_1.loi", ..
                 toolbox_dir+c+"mascaret0.lis", ..
                 toolbox_dir+c+"mascaret0_ecr.opt"];
 
TypeNomFichier = ["xcas","geo","loi","loi","listing","res"];
impression = 0;
erreur = importModelMASCARET(id,TabNomFichier,TypeNomFichier,impression);
assert_checkequal(erreur,0);

// initialisation
erreur = initStateNameMASCARET(id,toolbox_dir+c+"demos"+c+"Flood"+c+"mascaret0.lig",impression);
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
Zr = zeros(nbSec,1);
X = zeros(nbSec,1);
Q = zeros((TF-T0)/pasTps,1);
T = zeros((TF-T0)/pasTps,1);
for i = 1:nbSec
    [erreur,Z(i)] = getDoubleMASCARET(id,"State.Z",i,0,0);
    assert_checkequal(erreur,0);
    [erreur,Zr(i)] = getDoubleMASCARET(id,"Model.Zbot",i,0,0);
    assert_checkequal(erreur,0);
    [erreur,X(i)] = getDoubleMASCARET(id,"Model.X",i,0,0);
    assert_checkequal(erreur,0);
end

// plot the initial state
f=figure();
f.background=35;
subplot(1,2,1);
X = X / 1000;
plot2d(X,Zr);
d1 = gce();
plot2d(X,Z);
a1 = gca();
e1 = gce();
a1.font_size = 3;
a1.title.font_size = 3;
a1.x_label.font_size = 3;
a1.y_label.font_size = 3;
a1.data_bounds(1,2) = -1;
a1.data_bounds(2,2) = 6;
a1.title.text = "Spatial results";
a1.x_label.text = "Channel length (km)";
a1.y_label.text = "Level (m)";
e1.children.foreground = color("cyan");
e1.children.thickness = 5.0;
d1.children.thickness = 5.0;
h2 = legend(['Bottom';'Water'],3);

// plotting update frequency
freqplot = 20;

// computation
tpsCalcul = pasTps;
j = 1;
k = 1;
while (tpsCalcul <= TF)
  erreur = computeMASCARET(id,T0,tpsCalcul,pasTps,impression);
  assert_checkequal(erreur,0);
  // get the results on the water level
  if(modulo(j,freqplot)==0) then
     for i = 1:nbSec
       [erreur,Z(i)] = getDoubleMASCARET(id,"State.Z",i,0,0);
       assert_checkequal(erreur,0);
     end
     e1.children.data = [X Z];
     a1.title.text = "Spatial results (at time "+string(T0)+" s)";
     j = 1;
  else
     j = j + 1;
  end
  [erreur,Q(k)] = getDoubleMASCARET(id,"State.Q",101,0,0);
  assert_checkequal(erreur,0);
  T(k) = T0;
  k = k + 1;
  T0 = tpsCalcul;
  tpsCalcul = tpsCalcul + pasTps;
end

xv = zeros(4,1);yv = zeros(4,1);
xv(1) = X(1);xv(2) = X(1);xv(3) = X(nbSec);xv(4) = X(nbSec);
yv(1) = Z(1);yv(2) = Zr(1);yv(3) = Zr(nbSec);yv(4) = Z(nbSec);
xfpoly(xv,yv,-4); 

subplot(1,2,2);
comet(T,Q,"colors",[5]);
a2 = gca();
a2.font_size = 3;
a2.title.font_size = 3;
a2.x_label.font_size = 3;
a2.y_label.font_size = 3;
a2.title.text = "Temporal results at x = 5km";
a2.x_label.text = "Time (s)";
a2.y_label.text = "Discharge (m^3/s)";

// model deletion
erreur=deleteMASCARET(id);
assert_checkequal(erreur,0);

disp("--> Computation done with the unsteady subcritical kernel (FD Preissmann Scheme)");
disp("");
