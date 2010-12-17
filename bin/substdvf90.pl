#!perl
#-----------------------------------------------------
#
#  Realisation automatique d'un jeu de sources pour
#     WindowsNT / DVF 6
#
#  Substitution des arguments du type
#        %R ou %I
#    en
#        %R(1)   ou  I(1)
#
#  Les fichiers de base Unix a transformer doivent etre
#  situes dans un sous-repertoire ".\unix\" du
#  repertoire courant.
#  Les fichiers modifies sont crees dans le repertoire
#  courant
#
#
#--------------------------------Mars-1999-DeltaCAD---
#
use File::Copy;
use File::Path;
use Benchmark;

############################################
#    /      PROGRAMME PRINCIPAL
############################################


if($ENV{"OS"} && ( $ENV{"OS"} eq "Windows_NT" ) )
    { $ps="\\";}     #win
else
    { $ps="/"; }     #unix

#--- Balayer les fichier FORTRAN du repertoire

opendir(CURDIR,"unix");
foreach ( grep {/\.f/} readdir(CURDIR)) 
  {
    printf "Working on file  $_ ...\n";
    
    $ficnam=$_;
    open (FIC, "unix$ps"."$_");             
    @lines = <FIC>;
    close (FIC);
    
    open(F, ">$ficnam");
#
#----- Chercher les %I %i et %R %r
#

foreach ( @lines)
{
   $lg = $_;
   $modif=0;
#%R et %r
   if ( $_ =~ /%R[ ,\)]/ )   
          {
             $lg =~ s/(%R)([ ,\)])/$1\(1\)$2/g;
             $modif=1;
          }
   if ( $_ =~ /%r[ ,\)]/ )   
          {
             $lg =~ s/(%r)([ ,\)])/$1\(1\)$2/g;
             $modif=1;
          }
#%I et %i
   if ( $_ =~ /%I[ ,\)]/ )   
          {
             $lg =~ s/(%I)([ ,\)])/$1\(1\)$2/g;
             $modif=1;
          }
   if ( $_ =~ /%i[ ,\)]/ )   
          {
             $lg =~ s/(%i)([ ,\)])/$1\(1\)$2/g;
             $modif=1;
          }

#Print
   if ( $modif == 1 )   
          {
             print "<<< $_"; 
             print ">>> $lg\n";
          }
#
#----- Reecrire le fichier modifie
#
  	print F "$lg";
} 

#--- Fichier suivant
  close(F);
  }
closedir (CURDIR);



##########################################################################
#    Fin
##########################################################################

exit;
 
 
 
 
