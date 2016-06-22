// Demo file for MASCARET toolbox in SCILAB
//
//      Case : uniform equilibrium flow conditions
//
//      Computation kernel : Steady
//
//      Author(s) : Fabrice ZAOUI
//
//      Copyright EDF 2014
//
clc();
[a,b,c]=MASCARET_getVersion();
disp("*********************************************************");
disp("MASCARET demo : Normal flow conditions in an open channel");
disp("*********************************************************");
disp("Library version : "+string(a)+'.'+string(b)+'.'+string(c));
disp("--------------------------------------------");
disp("PARAMETERS :");
disp("     Channel length        = 5 km");
disp("     Channel width         = 100 m");
disp("     Inflow discharge      = 1000 m^3/s");
disp("     Strickler coefficient = 30.59 m^(1/3)/s");
disp("     Bed slope             = 0.0005");
disp("     Normal water depth    = 5 m");
disp("--------------------------------------------");

toolbox_dir=getenv("toolbox_dir");
c = filesep();

// creation of the MASCARET model
[erreur, id] = MASCARET_create();
assert_checktrue(id>0);

// read data from files
path_xml = "file://"+toolbox_dir+c+"demos"+c+"Uniform"+c+"mascaret0.xcas";
TabNomFichier = [strsubst(path_xml,'\','/'), ..
                 toolbox_dir+c+"demos"+c+"Uniform"+c+"mascaret0.geo", ..
                 toolbox_dir+c+"demos"+c+"Uniform"+c+"mascaret0_0.loi", ..
                 toolbox_dir+c+"demos"+c+"Uniform"+c+"mascaret0_1.loi", ..
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

// get the time parameters
[erreur,pasTps] = MASCARET_getDouble(id,"Model.DT",0,0,0);
assert_checkequal(erreur,0);
[erreur,T0] = MASCARET_getDouble(id,"Model.InitTime",0,0,0);
assert_checkequal(erreur,0);
[erreur,TF] = MASCARET_getDouble(id,"Model.MaxCompTime",0,0,0);
assert_checkequal(erreur,0);

// computation
erreur = MASCARET_compute(id,T0,TF,pasTps,impression);
assert_checkequal(erreur,0);

// get the results
Z = zeros(nbSec,1);
Zr = zeros(nbSec,1);
X = zeros(nbSec,1);
for i = 1:nbSec
    [erreur,Z(i)] = MASCARET_getDouble(id,"State.Z",i,0,0);
    assert_checkequal(erreur,0);
    [erreur,Zr(i)] = MASCARET_getDouble(id,"Model.Zbot",i,0,0);
    assert_checkequal(erreur,0);
    [erreur,X(i)] = MASCARET_getDouble(id,"Model.X",i,0,0);
    assert_checkequal(erreur,0);
end

// plot the results
f=figure();
f.background=35;
X = X / 1000;
plot2d(X,Zr);
d1 = gce();
plot2d(X,Z);
a1 = gca();
e1 = gce();
xv = zeros(4,1);yv = zeros(4,1);
xv(1) = X(1);xv(2) = X(1);xv(3) = X(nbSec);xv(4) = X(nbSec);
yv(1) = Z(1);yv(2) = Zr(1);yv(3) = Zr(nbSec);yv(4) = Z(nbSec);
xfpoly(xv,yv,-4); 
a1.font_size = 3;
a1.x_label.font_size = 3;
a1.y_label.font_size = 3;
a1.data_bounds(1,2) = 7;
a1.data_bounds(2,2) = 16;
a1.x_label.text = "Channel length (km)";
a1.y_label.text = "Level (m)";
e1.children.foreground = color("cyan");
e1.children.thickness = 5.0;
d1.children.thickness = 5.0;
h2 = legend(['Bottom';'Water']);

// model deletion
erreur=MASCARET_delete(id);
assert_checkequal(erreur,0);

disp("--> Computation done with the steady kernel in one iteration");
disp("");
