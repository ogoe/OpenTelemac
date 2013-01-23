#!perl

#Lance une execution pour chaque code du systeme TELEMAC

    $ps="/";
    if(`gethosttype` eq "win")
    {
	$ps="\\";
    }

    my @dirs=(
	      "artemis".$ps."arte_v3p1".$ps."test.fr".$ps."bj78_run15!artemis -s bj78.cas",
 	      "cowadis".$ps."cowa_v1p0".$ps."test.fr".$ps."roscoff!cowadis -s cas",
 	      "telemac2d".$ps."tel2d_v4p0".$ps."test.fr".$ps."init_1!telemac2d -s cas.ref",
 	      "telemac3d".$ps."tel3d_v2p2".$ps."test.fr".$ps."canal!telemac3d -s cas",
 	      "postel3d".$ps."postel3d_v1p2".$ps."test.fr".$ps."canal!postel3d -s cas.postth",
	      "sisyphe".$ps."sisyphe_v1p1".$ps."test.fr".$ps."penteinf!sisyphe -s penteinf.cas",
	      "stbtel".$ps."stb_v4p0".$ps."test.fr".$ps."amphi!stbtel -s amphi.cas",
	      "subief".$ps."sub_v4p0".$ps."test.fr".$ps."perpen!subief -s cas4p0"
#pas_conforme 	      "tomawac".$ps."v1p3".$ps."d".$ps."test.fr".$ps."bj78_run15!tomawac tomawac.cas"
	      );


{
    printf "\n\n\n";
    printf "Ready for a total TEST of System TELEMAC.\n\n";
    
#On regarde si le mode user confirm est actif.
    $askUser=0;
    if($ARGV[0])
    {
	if($ARGV[0] eq "-y")
	{
	    printf "User confirm mode.\n\n";
	    $askUser=1;
	}
	else
	{
	    printf "Invalid switch.\n\n";
	    usage();
	}
    }
    else
    {
	printf "Automatic mode.\n\n";
    }


# RUN for each code

    foreach $path (@dirs)
    {
      ($cdir,$cmd)   = split (/!/, $path);
      ($codnam, $tmp)= split (/ /, $cmd);
      printf "\n------------------- $codnam -----------------\n\n";

# Confirmation
      $make=0;
	if($askUser != 0)
	{
	    printf "\nDo you confirm testing '$codnam' [y(yes)/s(skip)]  ";
	    if(<STDIN>=~/y/)
	    {
		$make=1;
	    }
      }
      else
      {
	  $make=1;
      }
    
    if ($make ==1) 
    {
# Exec

    $project=`getproject`;
    chdir($project);

    chdir("$cdir");
    system("$cmd");
    }
# Next code
    }

#Fin
      printf "\n\n-------------------- EndOfTests -------------\n\n";
}

