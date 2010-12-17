#!perl
#-----------------------------------------------------
#
#  Coupe les lignes source FORTRAN a partir de la
#  colonne 73
#  et elimine les caractères "blancs" de fin de ligne.
#
#
#  Ceci est effectué pour tous les fichiers Fortran
#  (extension ".f") du répertoire courant.
#  Les fichier originaux (avant modification) sont
#  copiés dans un sous-répertoire "orig".
#
#--------------------------------Aout-1999-DeltaCAD---
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

#--- Créer le répertoire de sauvegarde "orig"

mkdir("orig", 0777);


#--- Balayer les fichier FORTRAN du repertoire

opendir(CURDIR,".");
foreach ( grep {/\.f/} readdir(CURDIR)) 
  {
    $ficnam=$_; 
    print "\n>>>>>>>>>>>>>>>>>>>>>Working on file  $ficnam ...";
    
#--- Lecture du fichier original
    
    open (FIC, "<$ficnam");             
    @lines = <FIC>;
    close (FIC);
    
#s    open(F, ">$ficnam");
#
#----- Traiter chaque ligne
#

$modif=0;
foreach ( @lines)
{
   chomp   $_;
   $lgorig = $_;
   $lgorig =~ s/\s+$//  ;            #eliminer les "blancs" non significatifs
   
   $l1 = $lgorig;
   
   if (length($l1) > 72 )            #ne garder que les 72 premières colonnes
     {
     	if ( $modif == 0)
     	  {
     	     $modif=1;
             print "\n   - cutted lines are listed above :\n";
          }

     	$l1= substr($l1,0,72);
     	$l1=~ s/\s+$//  ;            #eliminer les "blancs" inutiles
     	$_=$l1;

#Print
        print "$lgorig\n";           #ligne initiale étant modifiée
     }
}   

#
#----- Reecrire le fichier si modifié et backup original
#

if ($modif == 1) 
   {
      copy("$ficnam", "orig$ps"."$ficnam");
      print "   - backup of original file $ficnam done into 'orig' dir\n";
      
      
      open(F, ">$ficnam");
      foreach ( @lines)
	{
           print F "$_\n";
        }
      close F;

    }
else
    {
    	print "nothing to do\n";
    }


#--- Fichier suivant

}
closedir (CURDIR);



##########################################################################
#    Fin
##########################################################################

exit;
 
 
 
 
