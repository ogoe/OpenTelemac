C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE ADVECTION-DIFFUSION STEP.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, INTERFACE_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AFBORF, AFBORL, AFBORS, AGGLOD, BFBORF, BFBORL, BFBORS, CALFLU, CLIMAX, CLIMIN, DIRFLU, DT, FBORF, FBORL, FBORS, FC, FD, FLODEL, FLOPAR, FLUEXTF, FLUXF, FMAX, FMIN, FN, FSCE, H, IELM2H, IELM2V, IELM3, IKLE3, INCHYD, INFOR, IPBOT, IT1, IT2, LIFBOF, LIFBOL, LIFBOS, LV, MASKBR, MASKEL, MASKPT, MATR2H, MATR2V, MDIFF, MESH2D, MESH3D, MMURD, MSK, MSUPG, MTRA1, MTRA2, MURD_TF, NBOR3, NELEM2, NELEM3, NEWDIF, NFRLIQ, NPLAN, NPOIN2, NPOIN3, NPTFR3, NSCE, NUMLIQ, OPTBAN, OPTDIF, PLUIE, RAIN, S0F, S1F, SCHCF, SCHDF, SEM3D, SIGMAF, SIGMAG, SLVDIF, SOURCES, SVIDE, T2_01, T2_02, T2_03, T3_01, T3_02, T3_03, T3_04, TETADI, TRAV3, TRBAF, VISCF, VOLU, VOLUN, VOLUT, W1, WCC, YAS0F, YAS1F, YASEM3D, YAWCC, ZPROP, ZT
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::ADV_CAR ADV_CAR@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_LPO ADV_LPO@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_LPO_TF ADV_LPO_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC ADV_NSC@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC_TF ADV_NSC_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_PSI ADV_PSI@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_SUP ADV_SUP@endlink, 
!> @link DECLARATIONS_TELEMAC::KADH KADH@endlink, 
!> @link DECLARATIONS_TELEMAC::KENT KENT@endlink, 
!> @link DECLARATIONS_TELEMAC::KENTU KENTU@endlink, 
!> @link DECLARATIONS_TELEMAC::KSORT KSORT@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, I, IIS, IP, IPLAN, IPTFR, IPTFR2, IS, K, NPTFR, PARA, PARAPLUIE, SAVEZ, STOFD, TETASUPG, VELOCITY, VOLUME, YADIRFLU, YARAIN, YASCE
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CVDF3D, PARAPLUIE, SAVEZ, VOLUME
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DIFF3D(), MURD3D(), MURD3D_POS(), OS(), PARCOM(), P_DSUM(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 6.0                                       </center>
!> </td><td> 18/12/2009
!> </td><td> J.M. HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/1999
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AFBORF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AFBORL,F,S
!></td><td>--></td><td>FROTTEMENT AUX LIMITES IMPLICITE
!>    </td></tr>
!>          <tr><td>AFBORS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AGGLOD
!></td><td>--></td><td>MASS-LUMPING DANS LA DIFFUSION
!>    </td></tr>
!>          <tr><td>AMESH2
!></td><td>--></td><td>BLOC DES TABLEAUX DE REELS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>AMESH3
!></td><td>--></td><td>BLOC DES TABLEAUX DE REELS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>BFBORF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BFBORL,F,S
!></td><td>--></td><td>FROTTEMENT AUX LIMITES EXPLICITE
!>    </td></tr>
!>          <tr><td>BFBORS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CALFLU
!></td><td>--></td><td>INDIQUE SI ON CALCULE LE FLUX POUR LE BILAN
!>    </td></tr>
!>          <tr><td>CLIMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CLIMIN,MAX
!></td><td>--></td><td>AUTORISE OU NON LE CLIPPING
!>    </td></tr>
!>          <tr><td>DIFF
!></td><td><-></td><td>MATRICE DE DIFFUSION SYMETRIQUE
!>    </td></tr>
!>          <tr><td>DIRFLU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DMURD,XMURD
!></td><td>--></td><td>MATRICE MURD NON SYMETRIQUE
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>ELT
!></td><td>--></td><td>NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
!>                  CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>EPSDF
!></td><td>--></td><td>PRECISION POUR LA DIFFUSION DE F
!>    </td></tr>
!>          <tr><td>ETA
!></td><td>--></td><td>NUMEROS DES ETAGES AU PIED DES COURBES
!>                  CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>FBORF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FBORL,F,S
!></td><td>--></td><td>CONDITIONS AUX LIMITES DIRICHLET
!>    </td></tr>
!>          <tr><td>FBORS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FC
!></td><td><--</td><td>VARIABLE APRES CONVECTION
!>    </td></tr>
!>          <tr><td>FD
!></td><td><--</td><td>VARIABLE APRES DIFFUSION
!>    </td></tr>
!>          <tr><td>FLODEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLOPAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUEXT
!></td><td>--></td><td>FLUX EXTERIEUR PAR NOEUD
!>    </td></tr>
!>          <tr><td>FLUEXTF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUX
!></td><td><-></td><td>FLUX GLOBAL A INCREMENTER
!>    </td></tr>
!>          <tr><td>FLUXF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FMIN,FMAX
!></td><td>--></td><td>VALEURS DE CLIPPING
!>    </td></tr>
!>          <tr><td>FN
!></td><td>--></td><td>VARIABLE AU TEMPS N
!>    </td></tr>
!>          <tr><td>FSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>H
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM2H
!></td><td>--></td><td>TYPE DE DISCRETISATION 2DH
!>    </td></tr>
!>          <tr><td>IELM2V
!></td><td>--></td><td>TYPE DE DISCRETISATION 2DV
!>    </td></tr>
!>          <tr><td>IELM3
!></td><td>--></td><td>TYPE DE DISCRETISATION 3D
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>IDEM EN 2D
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td>--></td><td>CORRESPONDANCE NUMEROTATION LOCALE ET GLOBALE
!>    </td></tr>
!>          <tr><td>IMESH2
!></td><td>--></td><td>BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>IMESH3
!></td><td>--></td><td>BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>INCHYD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INFOR
!></td><td>--></td><td>INFORMATIONS SUR LES SOLVEURS
!>    </td></tr>
!>          <tr><td>IPBOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IT1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IT2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KNEU,KDIR,KDDL
!></td><td>--></td><td>TYPES DES CONDITIONS LIMITES TECHNIQUES
!>    </td></tr>
!>          <tr><td>LIFBOF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIFBOL,F,S
!></td><td>--></td><td>TYPE DE CONDITIONS LIMITES PHYSIQUES
!>    </td></tr>
!>          <tr><td>LIFBOS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMDIF
!></td><td>--></td><td>TYPE DE CONDITIONS LIMITES TECHNIQUES
!>    </td></tr>
!>          <tr><td>LV
!></td><td>--></td><td>LONGUEUR DU VECTEUR POUR LA VECTORISATION
!>    </td></tr>
!>          <tr><td>MASKBR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>MASQUAGE DES ELEMENTS
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>--></td><td>MASQUAGE DES POINTS
!>    </td></tr>
!>          <tr><td>MATR2H
!></td><td><-></td><td>MATRICE DE TRAVAIL 2DH
!>    </td></tr>
!>          <tr><td>MATR2V
!></td><td><-></td><td>MATRICE DE TRAVAIL 2DV
!>    </td></tr>
!>          <tr><td>MDIFF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH3D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MMURD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES
!>    </td></tr>
!>          <tr><td>MSUPG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MTRA1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MTRA2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MURD_TF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR3
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS FRONTIERES 3D
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS 2D
!>    </td></tr>
!>          <tr><td>NELEM3
!></td><td>--></td><td>NOMBRE D'ELEMENTS 3D
!>    </td></tr>
!>          <tr><td>NEWDIF
!></td><td>--></td><td>RECALCULE OU NON LA MATRICE DE DIFFUSION
!>    </td></tr>
!>          <tr><td>NFRLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NITDF
!></td><td>--></td><td>NOMBRE D'ITERATIONS POUR LA DIFFUSION DE F
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS 3D
!>    </td></tr>
!>          <tr><td>NPTFR3
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE BORDS LATERAUX
!>    </td></tr>
!>          <tr><td>NSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NUMLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTBAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTDIF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PLUIE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PREDF
!></td><td>--></td><td>PRECONDITIONNEMENT POUR LA DIFFUSION DE F
!>    </td></tr>
!>          <tr><td>RAIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>S0F ,YAS0F
!></td><td>--></td><td>TERME SOURCE EXPLICITE (DIM=F/T) (IF YAS0F)
!>    </td></tr>
!>          <tr><td>S1F ,YAS1F
!></td><td>--></td><td>TERME SOURCE IMPLICITE (DIM=1/T) (IF YAS1F)
!>    </td></tr>
!>          <tr><td>SCHCF
!></td><td>--></td><td>SCHEMA DE CONVECTION DE F
!>    </td></tr>
!>          <tr><td>SCHDF
!></td><td>--></td><td>SCHEMA DE DIFFUSION DE F
!>    </td></tr>
!>          <tr><td>SEM3D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHP
!></td><td>--></td><td>COORDONNEES BARYCENTRIQUES 2D AU PIED DES
!>                  COURBES CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>SHZ
!></td><td>--></td><td>COORDONNEES BARYCENTRIQUES SUIVANT Z AU PIED
!>                  DES COURBES CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>SIGMAF
!></td><td>--></td><td>COEFFICIENT DE REDUCTION DE LA VISCOSITE
!>    </td></tr>
!>          <tr><td>SIGMAG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SLVDIF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SOLDF
!></td><td>--></td><td>SOLVEUR POUR LA DIFFUSION DE F
!>    </td></tr>
!>          <tr><td>SOURCES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SUPG
!></td><td>--></td><td>MATRICE SUPG NON SYMETRIQUE
!>    </td></tr>
!>          <tr><td>SVIDE
!></td><td>--></td><td>STRUCTURE VIDE
!>    </td></tr>
!>          <tr><td>T2_01
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2_02
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2_03
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3_01
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3_02
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3_03
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3_04
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TBB
!></td><td>--></td><td>BLOC DE BLOCS DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TETADI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRA1,DTRA1,XTRA1
!></td><td><-></td><td>MATRICE DE TRAVAIL 3D
!>    </td></tr>
!>          <tr><td>TRA2
!></td><td><-></td><td>MATRICE DE TRAVAIL 3D
!>    </td></tr>
!>          <tr><td>TRAV3
!></td><td><-></td><td>STRUCTURE DE TABLEAUX DE TRAVAIL 3D
!>    </td></tr>
!>          <tr><td>TRBAF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UCONV,
!></td><td>--></td><td>COMPOSANTES DU CHAMP CONVECTEUR
!>    </td></tr>
!>          <tr><td>VISCF
!></td><td>--></td><td>COEFFICIENTS DE VISCOSITE
!>                  VISCF(*,1 OU 2) VISCOSITE HORIZONTALE
!>                  VISCF(*,3)      VISCOSITE VERTICALE
!>    </td></tr>
!>          <tr><td>VOLU
!></td><td>--></td><td>VOLUME DE CONTROLE A L'INSTANT N+1
!>    </td></tr>
!>          <tr><td>VOLUN
!></td><td>--></td><td>VOLUME DE CONTROLE A L'INSTANT N
!>    </td></tr>
!>          <tr><td>VOLUT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W1
!></td><td><-></td><td>TABLEAU DE TRAVAIL (CALCUL DES MATRICES...)
!>    </td></tr>
!>          <tr><td>WCC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YASEM3D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YAWCC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZERO
!></td><td>--></td><td>PLUS PETITE VALEUR NON NULLE AUTORISEE
!>    </td></tr>
!>          <tr><td>ZPROP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZT
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CVDF3D
     & (FD,FC,FN,VISCF,SIGMAF,S0F,YAS0F,S1F,YAS1F,
     &  FBORL,FBORF,FBORS,AFBORL,AFBORF,AFBORS,
     &  BFBORL,BFBORF,BFBORS,LIFBOL,LIFBOF,LIFBOS,
     &  FLUXF,FLUEXTF,FMIN,CLIMIN,FMAX,CLIMAX,
     &  SCHCF,SCHDF,SLVDIF,TRBAF,INFOR,NEWDIF,CALFLU,
     &  T2_01,T2_02,T2_03,
     &  T3_01,T3_02,T3_03,T3_04,MESH3D,IKLE3,MASKEL,MTRA1,
     &  W1,NPTFR3,MMURD,MURD_TF,VOLU,VOLUN,
     &  NBOR3,NPOIN3,NPOIN2,DT,MSK,NELEM2,NELEM3,
     &  NPLAN,LV,IELM3,MSUPG,IELM2H,IELM2V,MDIFF,MTRA2,
     &  INCHYD,MASKBR,MASKPT,SEM3D,YASEM3D,SVIDE,IT1,IT2,
     &  TRAV3,MESH2D,MATR2H,MATR2V,H,OPTBAN,OPTDIF,TETADI,
     &  YAWCC,WCC,AGGLOD,NSCE,SOURCES,FSCE,NUMLIQ,DIRFLU,NFRLIQ,
     &  VOLUT,ZT,ZPROP,RAIN,PLUIE,FLODEL,FLOPAR,SIGMAG,IPBOT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AFBORF         |---| 
C| AFBORL,F,S     |-->| FROTTEMENT AUX LIMITES IMPLICITE
C| AFBORS         |---| 
C| AGGLOD         |-->| MASS-LUMPING DANS LA DIFFUSION
C| AMESH2         |-->| BLOC DES TABLEAUX DE REELS DU MAILLAGE 2D
C| AMESH3         |-->| BLOC DES TABLEAUX DE REELS DU MAILLAGE 3D
C| BFBORF         |---| 
C| BFBORL,F,S     |-->| FROTTEMENT AUX LIMITES EXPLICITE
C| BFBORS         |---| 
C| CALFLU         |-->| INDIQUE SI ON CALCULE LE FLUX POUR LE BILAN
C| CLIMAX         |---| 
C| CLIMIN,MAX     |-->| AUTORISE OU NON LE CLIPPING
C| DIFF           |<->| MATRICE DE DIFFUSION SYMETRIQUE
C| DIRFLU         |---| 
C| DMURD,XMURD    |-->| MATRICE MURD NON SYMETRIQUE
C| DT             |-->| PAS DE TEMPS
C| ELT            |-->| NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
C|                |   | CARACTERISTIQUES.
C| EPSDF          |-->| PRECISION POUR LA DIFFUSION DE F
C| ETA            |-->| NUMEROS DES ETAGES AU PIED DES COURBES
C|                |   | CARACTERISTIQUES.
C| FBORF          |---| 
C| FBORL,F,S      |-->| CONDITIONS AUX LIMITES DIRICHLET
C| FBORS          |---| 
C| FC             |<--| VARIABLE APRES CONVECTION
C| FD             |<--| VARIABLE APRES DIFFUSION
C| FLODEL         |---| 
C| FLOPAR         |---| 
C| FLUEXT         |-->| FLUX EXTERIEUR PAR NOEUD
C| FLUEXTF        |---| 
C| FLUX           |<->| FLUX GLOBAL A INCREMENTER
C| FLUXF          |---| 
C| FMIN,FMAX      |-->| VALEURS DE CLIPPING
C| FN             |-->| VARIABLE AU TEMPS N
C| FSCE           |---| 
C| H             |---| 
C| IELM2H         |-->| TYPE DE DISCRETISATION 2DH
C| IELM2V         |-->| TYPE DE DISCRETISATION 2DV
C| IELM3          |-->| TYPE DE DISCRETISATION 3D
C| IKLE2          |-->| IDEM EN 2D
C| IKLE3          |-->| CORRESPONDANCE NUMEROTATION LOCALE ET GLOBALE
C| IMESH2         |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE 2D
C| IMESH3         |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE 3D
C| INCHYD         |---| 
C| INFOR          |-->| INFORMATIONS SUR LES SOLVEURS
C| IPBOT          |---| 
C| IT1            |---| 
C| IT2            |---| 
C| KNEU,KDIR,KDDL |-->| TYPES DES CONDITIONS LIMITES TECHNIQUES
C| LIFBOF         |---| 
C| LIFBOL,F,S     |-->| TYPE DE CONDITIONS LIMITES PHYSIQUES
C| LIFBOS         |---| 
C| LIMDIF         |-->| TYPE DE CONDITIONS LIMITES TECHNIQUES
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| MASKBR         |---| 
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MASKPT         |-->| MASQUAGE DES POINTS
C| MATR2H         |<->| MATRICE DE TRAVAIL 2DH
C| MATR2V         |<->| MATRICE DE TRAVAIL 2DV
C| MDIFF          |---| 
C| MESH2D         |---| 
C| MESH3D         |---| 
C| MMURD          |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
C| MSUPG          |---| 
C| MTRA1          |---| 
C| MTRA2          |---| 
C| MURD_TF        |---| 
C| NBOR3          |-->| NUMEROS GLOBAUX DES POINTS FRONTIERES 3D
C| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
C| NELEM3         |-->| NOMBRE D'ELEMENTS 3D
C| NEWDIF         |-->| RECALCULE OU NON LA MATRICE DE DIFFUSION
C| NFRLIQ         |---| 
C| NITDF          |-->| NOMBRE D'ITERATIONS POUR LA DIFFUSION DE F
C| NPLAN          |-->| NOMBRE DE PLANS DU MAILLAGE 3D
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPOIN3         |-->| NOMBRE DE POINTS 3D
C| NPTFR3         |-->| NOMBRE DE POINTS FRONTIERE BORDS LATERAUX
C| NSCE           |---| 
C| NUMLIQ         |---| 
C| OPTBAN         |---| 
C| OPTDIF         |---| 
C| PLUIE          |---| 
C| PREDF          |-->| PRECONDITIONNEMENT POUR LA DIFFUSION DE F
C| RAIN           |---| 
C| S0F ,YAS0F     |-->| TERME SOURCE EXPLICITE (DIM=F/T) (IF YAS0F)
C| S1F ,YAS1F     |-->| TERME SOURCE IMPLICITE (DIM=1/T) (IF YAS1F)
C| SCHCF          |-->| SCHEMA DE CONVECTION DE F
C| SCHDF          |-->| SCHEMA DE DIFFUSION DE F
C| SEM3D          |---| 
C| SHP            |-->| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
C|                |   | COURBES CARACTERISTIQUES.
C| SHZ            |-->| COORDONNEES BARYCENTRIQUES SUIVANT Z AU PIED
C|                |   | DES COURBES CARACTERISTIQUES.
C| SIGMAF         |-->| COEFFICIENT DE REDUCTION DE LA VISCOSITE
C| SIGMAG         |---| 
C| SLVDIF         |---| 
C| SOLDF          |-->| SOLVEUR POUR LA DIFFUSION DE F
C| SOURCES        |---| 
C| SUPG           |-->| MATRICE SUPG NON SYMETRIQUE
C| SVIDE          |-->| STRUCTURE VIDE
C| T2_01          |---| 
C| T2_02          |---| 
C| T2_03          |---| 
C| T3_01          |---| 
C| T3_02          |---| 
C| T3_03          |---| 
C| T3_04          |---| 
C| TBB            |-->| BLOC DE BLOCS DE TRAVAIL
C| TETADI         |---| 
C| TRA1,DTRA1,XTRA|<->| MATRICE DE TRAVAIL 3D
C| TRA2           |<->| MATRICE DE TRAVAIL 3D
C| TRAV3          |<->| STRUCTURE DE TABLEAUX DE TRAVAIL 3D
C| TRBAF          |---| 
C| UCONV,         |-->| COMPOSANTES DU CHAMP CONVECTEUR
C| VISCF          |-->| COEFFICIENTS DE VISCOSITE
C|                |   | VISCF(*,1 OU 2) VISCOSITE HORIZONTALE
C|                |   | VISCF(*,3)      VISCOSITE VERTICALE
C| VOLU           |-->| VOLUME DE CONTROLE A L'INSTANT N+1
C| VOLUN          |-->| VOLUME DE CONTROLE A L'INSTANT N
C| VOLUT          |---| 
C| W1             |<->| TABLEAU DE TRAVAIL (CALCUL DES MATRICES...)
C| WCC            |---| 
C| YASEM3D        |---| 
C| YAWCC          |---| 
C| ZERO           |-->| PLUS PETITE VALEUR NON NULLE AUTORISEE
C| ZPROP          |---| 
C| ZT             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_TELEMAC3D, EX_CVDF3D => CVDF3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FD, FC, FN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: S0F, S1F, VISCF
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: LIFBOL, LIFBOF, LIFBOS
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FBORL, FBORF, FBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: AFBORL, AFBORF, AFBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: BFBORL, BFBORF, BFBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: FLUEXTF,PLUIE
      DOUBLE PRECISION, INTENT(IN)    :: SIGMAF,FMIN,FMAX,DT
      DOUBLE PRECISION, INTENT(IN)    :: AGGLOD
      DOUBLE PRECISION, INTENT(INOUT) :: FLUXF,TETADI
      INTEGER, INTENT(IN)             :: SCHCF,SCHDF,TRBAF,NPTFR3,NFRLIQ
      INTEGER, INTENT(IN)             :: NUMLIQ(*),DIRFLU(*)
      LOGICAL, INTENT(IN)             :: CLIMIN,CLIMAX,RAIN,YAS0F,YAS1F
      LOGICAL, INTENT(IN)             :: INFOR,NEWDIF,CALFLU,MSK,SIGMAG
      TYPE(SLVCFG)                    :: SLVDIF
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKEL,IKLE3,FLODEL,FLOPAR
      TYPE(BIEF_OBJ), INTENT(IN)      :: NBOR3,WCC,SOURCES,ZPROP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T3_01,T3_02,T3_03,T3_04,W1
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T2_01,T2_02,T2_03,ZT
      TYPE(BIEF_OBJ), TARGET, INTENT(INOUT) :: VOLUT
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3D
      INTEGER, INTENT(IN)             :: NPOIN3,NPOIN2
      INTEGER, INTENT(IN)             :: IPBOT(NPOIN2)
      INTEGER, INTENT(IN)             :: NPLAN,NELEM2,NELEM3,LV
      INTEGER, INTENT(IN)             :: OPTBAN,OPTDIF
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: MMURD,MURD_TF,MTRA1
      TYPE(BIEF_OBJ), INTENT(IN)      :: VOLUN
      TYPE(BIEF_OBJ), TARGET, INTENT(IN) :: VOLU
      LOGICAL, INTENT(IN)             :: INCHYD,YASEM3D,YAWCC
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKPT,MASKBR,H,SVIDE
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2D
      INTEGER, INTENT(IN)             :: IELM3,IELM2H,IELM2V,NSCE
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SEM3D,IT1,IT2,TRAV3,MTRA2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: MSUPG,MDIFF,MATR2V,MATR2H
      DOUBLE PRECISION, INTENT(IN)    :: FSCE(NSCE)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IP,K,NPTFR,IPLAN,IPTFR,IS,IPTFR2,I,IIS,PARA
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
C     DOUBLE PRECISION, POINTER :: FFMIN(:),FFMAX(:)
      DOUBLE PRECISION STOFD,TETASUPG
      TYPE(BIEF_OBJ), POINTER :: VOLUME
      DOUBLE PRECISION, POINTER :: PARAPLUIE(:)
!
C     FUNCTIONS
!
      DOUBLE PRECISION P_DSUM,C
      EXTERNAL         P_DSUM
!
      LOGICAL YADIRFLU,YASCE,VELOCITY,YARAIN
!
!***********************************************************************
!
C     FFMIN  => T3_01%R(4*NFRLIQ+1:5*NFRLIQ)
C     FFMAX  => T3_01%R(5*NFRLIQ+1:6*NFRLIQ)
!
      VELOCITY=.FALSE.
      IF(FN%NAME(1:2).EQ.'UN'.OR.
     &   FN%NAME(1:2).EQ.'VN'.OR.
     &   FN%NAME(1:2).EQ.'WN') VELOCITY=.TRUE.
!
C     SAVES LIFBOL
!
      DO IP=1,NPTFR3
        IT2%I(IP)=LIFBOL%I(IP)
      ENDDO
!
      NPTFR=NPTFR3/NPLAN
!
      IF(CALFLU) FLUXF = 0.D0
!
C     WITH DISTRIBUTIVE SCHEMES : COMPUTES PRESCRIBED VALUES THAT
C     WILL ENSURE THE CORRECT FLUX (REAL PRESCRIBED VALUES DISCARDED)
C     THESE CORRECTED PRESCRIBED VALUES ARE SET BEFORE ADVECTION
!
C     YADIRFLU=.TRUE. : THERE IS AT LEAST ONE BOUNDARY WITH
C                       TREATMENT OF FLUXES AT BOUNDARIES = 2
      YADIRFLU=.FALSE.
C     DIRFLU DISCARDED FOR VELOCITIES
      IF(NFRLIQ.GT.0.AND..NOT.VELOCITY) THEN
        DO K=1,NFRLIQ
          IF(DIRFLU(K).EQ.2) YADIRFLU=.TRUE.
        ENDDO
      ENDIF
!
!=======================================================================
!
C     FOR TRACERS (=NOT VELOCITY) : DIRICHLET VALUES ARE NOT RESPECTED IF EXIT
C     THERE IS NO NEED TO TEST KENTU OR KADH FOR TRACERS
!
      IF(NFRLIQ.GT.0.AND..NOT.VELOCITY) THEN
        IF(NCSIZE.LE.1) THEN
          DO IPTFR=1,NPTFR3
            IF(LIFBOL%I(IPTFR).EQ.KENT) THEN
C              EXITS ARE TREATED AS FREE BOUNDARIES
               IP=NBOR3%I(IPTFR)
               IF(FLUEXTF%R(IP).GE.0.D0) LIFBOL%I(IPTFR)=KSORT
            ENDIF
          ENDDO
        ELSE
          CALL OS('X=Y     ',X=T3_02,Y=FLUEXTF)
          CALL PARCOM(T3_02,2,MESH3D)
          IF(NPTFR3.GT.0) THEN
          DO IPTFR=1,NPTFR3
            IF(LIFBOL%I(IPTFR).EQ.KENT) THEN
C              EXITS ARE TREATED AS FREE BOUNDARIES
               IP=NBOR3%I(IPTFR)
               IF(T3_02%R(IP).GE.0.D0) LIFBOL%I(IPTFR)=KSORT
            ENDIF
          ENDDO
          ENDIF
        ENDIF
      ENDIF
!
!=======================================================================
!
C     DISTRIBUTIVE, FINITE VOLUME SCHEMES AND SUPG:
C     A PRIORI CORRECTION OF FN FOR REAL ENTRANCES
C     I.E. LIFBOL STILL KENT DESPITE ABOVE CHANGE
!
      IF((SCHCF.EQ.ADV_SUP   .OR.SCHCF.EQ.ADV_NSC    .OR.
     &    SCHCF.EQ.ADV_PSI   .OR.SCHCF.EQ.ADV_LPO    .OR.
     &    SCHCF.EQ.ADV_LPO_TF.OR.SCHCF.EQ.ADV_NSC_TF)
     &                                              .AND.YADIRFLU) THEN
!
C       BUILDS TETA COEFFICIENT (STORED INTO T3_01) SEE BOOK PAGE 172
!
        IF(NCSIZE.LE.1) THEN
          DO I=1,NPOIN3
            T3_01%R(I)=-FLUEXTF%R(I)*DT/
     &                 (MAX(VOLUN%R(I),1.D-10)-FLUEXTF%R(I)*DT)
          ENDDO
        ELSE
C         IN PARALLEL MODE, MUST HAVE VALUES ASSEMBLED ON ALL SUB-DOMAINS HERE
          CALL OS('X=Y     ',X=T3_02,Y=FLUEXTF)
          CALL OS('X=Y     ',X=T3_03,Y=VOLUN)
          CALL PARCOM(T3_02,2,MESH3D)
          CALL PARCOM(T3_03,2,MESH3D)
          DO I=1,NPOIN3
            T3_01%R(I)=-T3_02%R(I)*DT/
     &                 (MAX(T3_03%R(I),1.D-10)-T3_02%R(I)*DT)
          ENDDO
        ENDIF
!
        IF(NPTFR.GT.0) THEN
!
        DO IPLAN=1,NPLAN
        DO IPTFR=1,NPTFR
          IF(NUMLIQ(IPTFR).GE.1) THEN
          IF(DIRFLU(NUMLIQ(IPTFR)).EQ.2) THEN
            IP=IPTFR+(IPLAN-1)*NPTFR
            IF(LIFBOL%I(IP).EQ.KENT) THEN
              I=NBOR3%I(IP)
              FN%R(I)=FN%R(I)+T3_01%R(I)*(FBORL%R(IP)-FN%R(I))
C             CORRECTION OF FLUX
C             IN THE PROOF OF MASS-CONSERVATION, FLUEXT IS MULTIPLIED
C             BY FN INSTEAD OF FBOR, TO INTERPRET THE ADDED MASS AS
C             A FLUX THIS CORRECTION IS NECESSARY
C             HERE IT IS THE FN MODIFIED ABOVE
              IF(CALFLU) THEN
                FLUXF=FLUXF+(FBORL%R(IP)-FN%R(I))*FLUEXTF%R(I)*DT
              ENDIF
C             AVOIDS A DIRICHLET TREATMENT HEREAFTER AND BY DIFF3D -
C             WILL BE RESTORED AFTER DIFF3D
              LIFBOL%I(IP)=KSORT
            ENDIF
          ENDIF
          ENDIF
        ENDDO
        ENDDO
!
        ENDIF
!
      ENDIF
!
!=======================================================================
!
C     PUTS DIRICHLET VALUES IN FN
C     MAY HAVE NO EFFECT IF TREATMENT OF FLUXES AT THE BOUNDARIES=2
C     BECAUSE LIFBOL CHANGED ABOVE
!
      IF(NPTFR.GT.0) THEN
      DO IPLAN=1,NPLAN
      DO IPTFR2=1,NPTFR
        IPTFR=IPTFR2+(IPLAN-1)*NPTFR
        IF(LIFBOL%I(IPTFR).EQ.KENT .OR.
     &     LIFBOL%I(IPTFR).EQ.KENTU.OR.
     &     LIFBOL%I(IPTFR).EQ.KADH) THEN
           I=NBOR3%I(IPTFR)
           FN%R(I) = FBORL%R(IPTFR)
        ENDIF
      ENDDO
      ENDDO
      ENDIF
!
C     HERE BOTTOM AND FREE SURFACE SHOULD BE TREATED AS WELL
!
!=======================================================================
!
C     3D ADVECTION (OTHER THAN SUPG)
!
!=======================================================================
!
C     WITH DISTRIBUTIVE SCHEMES, RIGHT-HAND SIDE MUST BE
C     IN INTEGRATED FORM (BEWARE, ORIGINAL S0F THUS MODIFIED)
!
      IF(SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI.OR.SCHCF.EQ.ADV_LPO.OR.
     &   SCHCF.EQ.ADV_NSC_TF.OR.SCHCF.EQ.ADV_LPO_TF) THEN
!
        IF(S0F%TYPR.NE.'0') THEN
!
          CALL VECTOR(S0F,'=','MASVEC          ',IELM3,1.D0,
     &                S0F,S0F,S0F,S0F,S0F,S0F,MESH3D,MSK,MASKEL)
          IF(NCSIZE.GT.1) CALL PARCOM(S0F,2,MESH3D)
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
C     PREPARES AN ASSEMBLED VERSION OF PLUIE IN PARALLEL
!
      IF(RAIN.AND.NCSIZE.GT.1) THEN
        PARAPLUIE=>T2_01%R
        CALL OS('X=Y     ',X=T2_01,Y=PLUIE)
        CALL PARCOM(T2_01,2,MESH2D)
      ELSE
        PARAPLUIE=>PLUIE%R
      ENDIF
!
!-----------------------------------------------------------------------
!
C     ADVECTION BY CHARACTERISTICS
!
      IF(SCHCF.EQ.ADV_CAR) THEN
!
C       THIS IS NOW DONE IN CHARAC CALLED BY PRECON
!
C       CALL CARA3D(FC%R,FN%R,SHP%R,SHZ%R,ELT%I,ETA%I,IKLE2%I,
C    &              NELEM2,NPOIN2,NPOIN3,DT,INFOR)
C       IF(NCSIZE.GT.1) CALL PARCOM(FC,2,MESH3D)
!
!-----------------------------------------------------------------------
!
C     ADVECTION BY MURD DISTRIBUTIVE SCHEME, OPTION N
!
      ELSEIF(SCHCF.EQ.ADV_NSC) THEN
!
        CALL MURD3D(FC%R,FN%R,VOLU%R,VOLUN%R,T3_01%R,T3_01,
     &              MMURD%D%R,MMURD%X%R,MMURD%D%R,MMURD%X%R,
     &              T3_02%R,T3_03%R,T3_04%R,T3_02,T3_03,T3_04,
     &              MESH3D%W%R,IKLE3%I,MESH3D,
     &              NELEM3,NPOIN3,DT,SCHCF,LV,MSK,MASKEL%R,INFOR,
     &              CALFLU,FLUXF,FLUEXTF%R,S0F,NSCE,SOURCES,FSCE,
     &              RAIN,PARAPLUIE,NPOIN2,
     &              TRAV3%ADR(5)%P,TRAV3%ADR(6)%P,MASKPT%R,OPTBAN,
     &              FLODEL%R,FLOPAR%R,MESH3D%GLOSEG%I,
     &              MESH3D%GLOSEG%DIM1,MESH2D%NSEG,NPLAN)
!
C       S0F CANCELLED TO AVOID A DUPLICATE TREATMENT
C       IF DIFF3D IS CALLED AFTER
C       CALL OS('X=C     ',X=S0F,C=0.D0)
        S0F%TYPR='0'
!
!-----------------------------------------------------------------------
!
C     ADVECTION BY MURD DISTRIBUTIVE SCHEME, OPTION PSI
!
      ELSEIF(SCHCF.EQ.ADV_PSI) THEN
!
         CALL MURD3D(FC%R,FN%R,VOLU%R,VOLUN%R,T3_01%R,T3_01,
     &               MMURD%D%R,MMURD%X%R,MESH3D%M%D%R,MESH3D%M%X%R,
     &               T3_02%R,T3_03%R,T3_04%R,T3_02,T3_03,T3_04,
     &               W1%R,IKLE3%I,MESH3D,
     &               NELEM3,NPOIN3,DT,SCHCF,LV,MSK,MASKEL%R,INFOR,
     &               CALFLU,FLUXF,FLUEXTF%R,S0F,NSCE,SOURCES,FSCE,
     &               RAIN,PARAPLUIE,NPOIN2,
     &               TRAV3%ADR(5)%P,TRAV3%ADR(6)%P,MASKPT%R,OPTBAN,
     &               FLODEL%R,FLOPAR%R,MESH3D%GLOSEG%I,
     &               MESH3D%GLOSEG%DIM1,MESH2D%NSEG,NPLAN)
!
C        S0F CANCELLED TO AVOID A DUPLICATE TREATMENT
C        IF DIFF3D IS CALLED AFTER
!
C        CALL OS('X=C     ',X=S0F,C=0.D0)
         S0F%TYPR='0'
!
!-----------------------------------------------------------------------
!
C     ADVECTION BY UPWIND EXPLICIT FINITE VOLUME SCHEME
!
      ELSEIF(SCHCF.EQ.ADV_LPO) THEN
!
         CALL MURD3D(FC%R,FN%R,VOLU%R,VOLUN%R,T3_01%R,T3_01,
     &               MMURD%D%R,MMURD%X%R,MESH3D%M%D%R,MESH3D%M%X%R,
     &               T3_02%R,T3_03%R,T3_04%R,T3_02,T3_03,T3_04,
     &               W1%R,IKLE3%I,MESH3D,
     &               NELEM3,NPOIN3,DT,SCHCF,LV,MSK,MASKEL%R,INFOR,
     &               CALFLU,FLUXF,FLUEXTF%R,S0F,NSCE,SOURCES,FSCE,
     &               RAIN,PARAPLUIE,NPOIN2,
     &               TRAV3%ADR(5)%P,TRAV3%ADR(6)%P,MASKPT%R,OPTBAN,
     &               FLODEL%R,FLOPAR%R,MESH3D%GLOSEG%I,
     &               MESH3D%GLOSEG%DIM1,MESH2D%NSEG,NPLAN)
!
C        S0F CANCELLED TO AVOID A DUPLICATE TREATMENT
C        IF DIFF3D IS CALLED AFTER
!
C        CALL OS('X=C     ',X=S0F,C=0.D0)
         S0F%TYPR='0'
!
!-----------------------------------------------------------------------
!
C     ADVECTION BY UPWIND EXPLICIT FINITE VOLUME SCHEME
!
      ELSEIF(SCHCF.EQ.ADV_LPO_TF) THEN
!
         CALL MURD3D_POS(FC%R,FN%R,VOLU%R,VOLU,VOLUN%R,VOLUN,
     &                   T3_01%R,T3_01,MESH3D%M%X%R,
     &                   T3_02%R,T3_03%R,T3_04%R,T3_02,T3_03,T3_04,
     &                   MESH2D,MESH3D,
     &                   NELEM3,NPOIN3,DT,SCHCF,MSK,MASKEL%R,INFOR,
     &                   CALFLU,FLUXF,FLUEXTF%R,S0F,NSCE,SOURCES,FSCE,
     &                   RAIN,PARAPLUIE,NPOIN2,MASKPT%R,OPTBAN,
     &                   FLODEL%R,FLOPAR%R,MESH3D%GLOSEG%I,
     &                   MESH3D%GLOSEG%DIM1,MESH2D%NSEG,NPLAN)
!
C        S0F CANCELLED TO AVOID A DUPLICATE TREATMENT
C        IF DIFF3D IS CALLED AFTER
!
C        CALL OS('X=C     ',X=S0F,C=0.D0)
         S0F%TYPR='0'
!
!-----------------------------------------------------------------------
!
C     ADVECTION BY UPWIND EXPLICIT FINITE VOLUME SCHEME
!
      ELSEIF(SCHCF.EQ.ADV_NSC_TF) THEN
!
         PARA=0
         IF(NCSIZE.GT.1) PARA=MESH3D%NSEG
         CALL MURD3D_POS(FC%R,FN%R,VOLU%R,VOLU,VOLUN%R,VOLUN,
     &                   T3_01%R,T3_01,MESH3D%M%X%R,
     &                   T3_02%R,T3_03%R,T3_04%R,T3_02,T3_03,T3_04,
     &                   MESH2D,MESH3D,
     &                   NELEM3,NPOIN3,DT,SCHCF,MSK,MASKEL%R,INFOR,
     &                   CALFLU,FLUXF,FLUEXTF%R,S0F,NSCE,SOURCES,FSCE,
     &                   RAIN,PARAPLUIE,NPOIN2,MASKPT%R,OPTBAN,
     &                   MURD_TF%X%R(1     :MESH3D%NSEG     ),
     &                   MURD_TF%X%R(1+PARA:MESH3D%NSEG+PARA),
     &                   MESH3D%GLOSEG%I,
     &                   MESH3D%GLOSEG%DIM1,MESH2D%NSEG,NPLAN)
!
C        S0F CANCELLED TO AVOID A DUPLICATE TREATMENT
C        IF DIFF3D IS CALLED AFTER
!
C        CALL OS('X=C     ',X=S0F,C=0.D0)
         S0F%TYPR='0'
!
!-----------------------------------------------------------------------
!
C     OTHER CASES (SUPG OR NO ADVECTION)
!
      ELSE
!
        CALL OS ( 'X=Y     ' , X=FC , Y=FN )
!
      ENDIF
!
!-----------------------------------------------------------------------
!
C     RE-ENFORCES DIRICHLET POINTS (MAY CAUSE MASS ERRORS)
C     IN FACT NOT DONE IF LIFBOL HAS BEEN CHANGED ABOVE INTO KSORT
C     HENCE NO EFFECT WHEN YADIRFLU=.TRUE.
!
      IF(NPTFR3.GT.0) THEN
      DO IP=1,NPTFR3
        IF(LIFBOL%I(IP).EQ.KENT .OR.
     &     LIFBOL%I(IP).EQ.KENTU.OR.
     &     LIFBOL%I(IP).EQ.KADH) THEN
           I=NBOR3%I(IP)
           FC%R(I) = FBORL%R(IP)
        ENDIF
      ENDDO
      ENDIF
!
      K = NPOIN3 - NPOIN2
      DO IP = 1,NPOIN2
        IF(LIFBOF%I(IP).EQ.KENT.OR.LIFBOF%I(IP).EQ.KADH) THEN
          FC%R(IP)   = FBORF%R(IP)
        ENDIF
        IF(LIFBOS%I(IP).EQ.KENT.OR.LIFBOS%I(IP).EQ.KADH) THEN
          FC%R(IP+K) = FBORS%R(IP)
        ENDIF
      ENDDO
!
!=======================================================================
!
C  SUPG ADVECTION AND/OR DIFFUSION
C (IN THIS CASE IT IS NECESSARY TO SOLVE A LINEAR SYSTEM)
!
!=======================================================================
!
      IF(SCHCF.EQ.ADV_SUP.OR.SCHDF.NE.0) THEN
!
        IF(SCHCF.EQ.ADV_SUP) THEN
C         IT SEEMS THAT WITH SUBITERATIONS ONLY TETASUPG=1-TETAH WORKS
C         FOR MASS-CONSERVATION. SEE ALSO DIFF3D WITH ANOTHER COMMENT
          TETASUPG=0.55D0
        ELSE
          TETASUPG=1.D0
        ENDIF
!
        IF(SCHCF.EQ.ADV_SUP.AND..NOT.VELOCITY) THEN
          CALL OS('X=CY    ',X=VOLUT,Y=VOLUN    ,C=     TETASUPG)
          CALL OS('X=X+CY  ',X=VOLUT,Y=VOLU     ,C=1.D0-TETASUPG)
          CALL OS('X=CY    ',X=ZT   ,Y=ZPROP    ,C=     TETASUPG)
          CALL OS('X=X+CY  ',X=ZT   ,Y=MESH3D%Z ,C=1.D0-TETASUPG)
C         ZT IS TEMPORARILY PUT IN MESH3D%Z
          SAVEZ=>MESH3D%Z%R
          MESH3D%Z%R=>ZT%R
          VOLUME=>VOLUT
        ELSE
          VOLUME=>VOLU
        ENDIF
!
        IF(SCHCF.EQ.ADV_CAR.OR.SCHCF.EQ.ADV_SUP) THEN
C         SOURCES HAVE TO BE TREATED
          YASCE=.TRUE.
          YARAIN=RAIN
        ELSE
C         SOURCES HAVE ALREADY BEEN TREATED BY DISTRIBUTIVE SCHEMES
          YASCE=.FALSE.
C         RAIN HAS ALREADY BEEN TREATED BY DISTRIBUTIVE SCHEMES
          YARAIN=.FALSE.
        ENDIF
!
        CALL DIFF3D(FD,FC,FN,VISCF,SIGMAF,
     &              S0F,YAS0F,S1F,YAS1F,
     &              FBORL,FBORF,FBORS,AFBORL,AFBORF,AFBORS,
     &              BFBORL,BFBORF,BFBORS,LIFBOF,LIFBOL,LIFBOS,
     &              FMIN,CLIMIN,FMAX,CLIMAX,
     &              SCHCF,SCHDF,SLVDIF,TRBAF,INFOR,NEWDIF,
     &              DT,T2_01,T2_02,T2_03,T3_01,T3_02,T3_03,T3_04,
     &              NPOIN2,NPOIN3,INCHYD,SEM3D,YASEM3D,IT1,
     &              NPTFR3,NBOR3,MASKPT,TRAV3,MESH2D,
     &              MESH3D,MTRA1,MTRA2,IELM3,MSUPG,IELM2H,IELM2V,
     &              MDIFF,MATR2V,MATR2H,MASKBR,SVIDE,MSK,MASKEL,H,
     &              NPLAN,OPTBAN,OPTDIF,TETADI,YAWCC,WCC,AGGLOD,
     &              VOLUME,YASCE,NSCE,FSCE,SOURCES,TETASUPG,
     &              VELOCITY,YARAIN,PLUIE%R,SIGMAG,IPBOT)
!
        IF(SCHCF.EQ.ADV_SUP.AND..NOT.VELOCITY) THEN
C         MESH3D%Z RESTORED
          MESH3D%Z%R=>SAVEZ
        ENDIF
!
      ELSE
        CALL OS ( 'X=Y     ', X=FD, Y=FC )
      ENDIF
!
!-----------------------------------------------------------------------
!
C     ADVECTIVE FLUXES AND SOURCES
!
      IF(CALFLU) THEN
!
        IF(SCHCF.EQ.ADV_CAR) THEN
          DO IP = 1,NPOIN3
            FLUXF = FLUXF + FN%R(IP)*FLUEXTF%R(IP)*DT
          ENDDO
        ELSEIF(SCHCF.EQ.ADV_SUP) THEN
          DO IP = 1,NPOIN3
            FLUXF = FLUXF + FLUEXTF%R(IP)*DT*
     &                      (TETASUPG*FD%R(IP)+(1.D0-TETASUPG)*FN%R(IP))
          ENDDO
        ENDIF
!
C       CHARACTERISTICS OR SUPG : FLUX DUE TO SOURCES
C       (FOR DISTRIBUTIVE SCHEMES IT IS DONE IN MURD3D)
!
        IF(NSCE.GT.0.AND.(SCHCF.EQ.ADV_CAR.OR.SCHCF.EQ.ADV_SUP)) THEN
          DO IS=1,NSCE
            IIS=IS
C           HERE IN PARALLEL SOURCES WITHOUT PARCOM
            IF(NCSIZE.GT.1) IIS=IIS+NSCE
            DO IP=1,NPOIN3
              IF(SOURCES%ADR(IS)%P%R(IP).GT.0.D0) THEN
                FLUXF=FLUXF-FSCE(IS)*SOURCES%ADR(IIS)%P%R(IP)*DT
              ELSE
C                           FN FOR CHARACTERISTICS ?
                FLUXF=FLUXF-FD%R(IP)*SOURCES%ADR(IIS)%P%R(IP)*DT
              ENDIF
            ENDDO
          ENDDO
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
C     A POSTERIORI CORRECTION OF SUPG RESULTS
!
      IF(SCHCF.EQ.ADV_SUP.AND.YADIRFLU) THEN
!
C       CORRECTED VALUE AND CORRESPONDING FLUX CORRECTION
!
        IF(NCSIZE.LE.1) THEN
          DO I=1,NPOIN3
            T3_01%R(I)=-FLUEXTF%R(I)*TETASUPG*DT/MAX(VOLU%R(I),1.D-10)
          ENDDO
        ELSE
C         IN PARALLEL MODE, MUST HAVE VALUES ASSEMBLED ON ALL SUB-DOMAINS HERE
          CALL OS('X=Y     ',X=T3_02,Y=FLUEXTF)
          CALL OS('X=Y     ',X=T3_03,Y=VOLU)
          CALL PARCOM(T3_02,2,MESH3D)
          CALL PARCOM(T3_03,2,MESH3D)
          DO I=1,NPOIN3
            T3_01%R(I)=-T3_02%R(I)*TETASUPG*DT/MAX(T3_03%R(I),1.D-10)
          ENDDO
        ENDIF
!
        DO IPLAN=1,NPLAN
        DO IPTFR=1,NPTFR
          IF(DIRFLU(NUMLIQ(IPTFR)).EQ.2) THEN
            IP=IPTFR+(IPLAN-1)*NPTFR
            IF(IT2%I(IP).EQ.KENT .OR.
     &         IT2%I(IP).EQ.KENTU.OR.
     &         IT2%I(IP).EQ.KADH     ) THEN
C              ONLY ENTRANCES
               I=NBOR3%I(IP)
               IF(FLUEXTF%R(I).LT.0.D0) THEN
                 STOFD=FD%R(I)
                 FD%R(I)=STOFD+T3_01%R(I)*(FN%R(I)-STOFD)
C                FD%R(I)=STOFD-
C    &           FLUEXTF%R(I)*TETASUPG*(FN%R(I)-STOFD)*DT/VOLU%R(I)
C                CORRECTION OF FLUX
                 IF(CALFLU) THEN
C                  A POSTERIORI ADDED MASS DUE TO CORRECTION
                   FLUXF=FLUXF-VOLU%R(I)*(FD%R(I)-STOFD)
                 ENDIF
               ENDIF
            ENDIF
          ENDIF
        ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(CALFLU) THEN
C       NOW RETURNS TO REAL FLUXES, NOT FLUXES*DT
        FLUXF = FLUXF / DT
C       PARALLEL MODE
        IF(NCSIZE.GT.1) FLUXF = P_DSUM(FLUXF)
      ENDIF
!
!-----------------------------------------------------------------------
!
C     RESTORES LIFBOR
!
!-----------------------------------------------------------------------
!
C     RESTORES LIFBOL
!
      DO IP=1,NPTFR3
        LIFBOL%I(IP)=IT2%I(IP)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C