C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE DIFFUSION AND SUPG ADVECTION STEPS
!>               (IF REQUIRED).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AFBORF, AFBORL, AFBORS, AGGLOD, BFBORF, BFBORL, BFBORS, CLIMAX, CLIMIN, DT, FBORF, FBORL, FBORS, FC, FD, FMAX, FMIN, FN, FSCE, H, IELM2H, IELM2V, IELM3, INCHYD, INFO, IPBOT, IT1, LIFBOF, LIFBOL, LIFBOS, MASKBR, MASKEL, MASKPT, MATR2H, MATR2V, MDIFF, MESH2D, MESH3D, MSK, MSUPG, MTRA1, MTRA2, NBOR3, NEWDIF, NPLAN, NPOIN2, NPOIN3, NPTFR3, NSCE, OPTBAN, OPTDIF, PLUIE, RAIN, S0F, S1F, SCHCF, SCHDF, SEM3D, SIGMAF, SIGMAG, SLVDIF, SOURCES, SVIDE, T2_01, T2_02, T2_03, T3_01, T3_02, T3_03, T3_04, TETADI, TETASUPG, TRAV3, TRBAF, VELOCITY, VISCF, VOLU, WCC, YAS0F, YAS1F, YASCE, YASEM3D, YAWCC
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::ADV_LPO ADV_LPO@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_LPO_TF ADV_LPO_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC ADV_NSC@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC_TF ADV_NSC_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_PSI ADV_PSI@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_SUP ADV_SUP@endlink, 
!> @link DECLARATIONS_TELEMAC::KADH KADH@endlink, 
!> @link DECLARATIONS_TELEMAC::KDDL KDDL@endlink, 
!> @link DECLARATIONS_TELEMAC::KDIR KDIR@endlink, 
!> @link DECLARATIONS_TELEMAC::KENT KENT@endlink, 
!> @link DECLARATIONS_TELEMAC::KENTU KENTU@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AGGLO, C, FORMUL, I, I2D, IIS, IPLAN, IPOIN2, IPOIN3, IPTFR3, IS, ITERD, NITERD, NP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CLIP(), CPSTVC(), DIRI01(), LUMP(), MATRIX(), MATVEC(), MITTIT(), OM(), OS(), OSDB(), OV(), PARCOM(), SOLVE(), STRESS(), UPWIND(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CVDF3D()

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
!> </td><td> 18/03/2010
!> </td><td> J.M. HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 14/12/2009
!> </td><td> JMH
!> </td><td> DIRICHLET POINTS ON THE BOTTOM
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
!></td><td>--></td><td>MASS-LUMPING DE LA DIFFUSION
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
!>          <tr><td>CLIMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CLIMIN,MAX
!></td><td>--></td><td>AUTORISE OU NON LE CLIPPING
!>    </td></tr>
!>          <tr><td>DIFF
!></td><td><-></td><td>MATRICE DE DIFFUSION SYMETRIQUE
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>EPSDF
!></td><td>--></td><td>PRECISION POUR LA DIFFUSION DE F
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
!></td><td>--></td><td>VARIABLE APRES CONVECTION
!>    </td></tr>
!>          <tr><td>FD
!></td><td><--</td><td>VARIABLE APRES DIFFUSION
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
!>          <tr><td>IMESH2
!></td><td>--></td><td>BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>IMESH3
!></td><td>--></td><td>BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>INCHYD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INFO
!></td><td>--></td><td>INFORMATIONS SUR LES SOLVEURS
!>    </td></tr>
!>          <tr><td>IPBOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IT1
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
!></td><td>--></td><td>TYPES DES CONDITIONS AUX LIMITES
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
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES
!>    </td></tr>
!>          <tr><td>MSUPG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MTRA1
!></td><td><-></td><td>MATRICE DE TRAVAIL
!>    </td></tr>
!>          <tr><td>MTRA2
!></td><td><-></td><td>MATRICE DE TRAVAIL
!>    </td></tr>
!>          <tr><td>NBOR3
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS FRONTIERES 3D
!>    </td></tr>
!>          <tr><td>NEWDIF
!></td><td>--></td><td>RECALCULE OU NON LA MATRICE DE DIFFUSION
!>    </td></tr>
!>          <tr><td>NITDF
!></td><td>--></td><td>NOMBRE D'ITERATIONS POUR LA DIFFUSION DE F
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
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
!>          <tr><td>OPTBAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTDIF
!></td><td>--></td><td>OPTION DE DIFFUSION DE F
!>    </td></tr>
!>          <tr><td>PLUIE
!></td><td>--></td><td>RAIN IN M/S MULTIPLIED BY VOLU2D
!>    </td></tr>
!>          <tr><td>PREDF
!></td><td>--></td><td>PRECONDITIONNEMENT POUR LA DIFFUSION DE F
!>    </td></tr>
!>          <tr><td>RAIN
!></td><td>--></td><td>IF YES, THERE IS RAIN OR EVAPORATION
!>    </td></tr>
!>          <tr><td>S0F
!></td><td>--></td><td>TERME SOURCE EXPLICITE (DIM=F/T)
!>    </td></tr>
!>          <tr><td>S1F
!></td><td>--></td><td>TERME SOURCE IMPLICITE (DIM=1/T)
!>    </td></tr>
!>          <tr><td>SCHCF
!></td><td>--></td><td>SCHEMA DE CONVECTION DE F
!>    </td></tr>
!>          <tr><td>SCHDF
!></td><td>--></td><td>SCHEMA DE DIFFUSION DE F
!>    </td></tr>
!>          <tr><td>SEM3D
!></td><td><-></td><td>SECOND MEMBRE
!>    </td></tr>
!>          <tr><td>SIGMAF
!></td><td>--></td><td>COEFFICIENT DE REDUCTION DE LA VISCOSITE
!>                  UTILISE SEULEMENT POUR K ET EPSILON
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
!>          <tr><td>T3_01,02,03
!></td><td><-></td><td>TABLEAUX DE TRAVAIL DE DIMENSION NPOIN3
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
!></td><td>--></td><td>COEF D'IMPLICITATION DE LA DIAGONALE DE LA
!>                  DIFFUSION SI  OPTDIF = 2
!>                  COEF D'IMPLICITATION DE LA DIFFUSION
!>                  SI  OPTDIF =1
!>    </td></tr>
!>          <tr><td>TETASUPG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV3
!></td><td><-></td><td>STRUCTURE DE TABLEAUX DE TRAVAIL 3D
!>    </td></tr>
!>          <tr><td>TRBAF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VELOCITY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VISCF
!></td><td>--></td><td>COEFFICIENTS DE VISCOSITE
!>                  VISCF(*,1 OU 2) VISCOSITE HORIZONTALE
!>                  VISCF(*,3)      VISCOSITE VERTICALE
!>    </td></tr>
!>          <tr><td>VOLU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W1
!></td><td><-></td><td>TABLEAU DE TRAVAIL (CALCUL DES MATRICES...)
!>    </td></tr>
!>          <tr><td>WCC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YAS0F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YAS1F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YASCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YASEM3D
!></td><td>--></td><td>IF TRUE, RIGHT HAND SIDE HAS BEEN PARTLY
!>                  COMPUTED BEFORE CALLING DIFF3D
!>    </td></tr>
!>          <tr><td>YAWCC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZERO
!></td><td>--></td><td>PLUS PETITE VALEUR NON NULLE AUTORISEE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DIFF3D
     &(FD,FC,FN,VISCF,SIGMAF,S0F,YAS0F,S1F,YAS1F,
     & FBORL,FBORF,FBORS,AFBORL,AFBORF,AFBORS,
     & BFBORL,BFBORF,BFBORS,LIFBOF,LIFBOL,LIFBOS,
     & FMIN,CLIMIN,FMAX,CLIMAX,SCHCF,SCHDF,SLVDIF,TRBAF,INFO,
     & NEWDIF,DT,T2_01,T2_02,T2_03,T3_01,T3_02,T3_03,T3_04,
     & NPOIN2,NPOIN3,INCHYD,SEM3D,YASEM3D,IT1,NPTFR3,NBOR3,MASKPT,
     & TRAV3,MESH2D,MESH3D,MTRA1,MTRA2,IELM3,MSUPG,IELM2H,IELM2V,
     & MDIFF,MATR2V,MATR2H,MASKBR,SVIDE,MSK,MASKEL,H,
     & NPLAN,OPTBAN,OPTDIF,TETADI,YAWCC,WCC,AGGLOD,VOLU,
     & YASCE,NSCE,FSCE,SOURCES,TETASUPG,VELOCITY,RAIN,PLUIE,SIGMAG,
     & IPBOT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AFBORF         |---| 
C| AFBORL,F,S     |-->| FROTTEMENT AUX LIMITES IMPLICITE
C| AFBORS         |---| 
C| AGGLOD         |-->| MASS-LUMPING DE LA DIFFUSION
C| AMESH2         |-->| BLOC DES TABLEAUX DE REELS DU MAILLAGE 2D
C| AMESH3         |-->| BLOC DES TABLEAUX DE REELS DU MAILLAGE 3D
C| BFBORF         |---| 
C| BFBORL,F,S     |-->| FROTTEMENT AUX LIMITES EXPLICITE
C| BFBORS         |---| 
C| CLIMAX         |---| 
C| CLIMIN,MAX     |-->| AUTORISE OU NON LE CLIPPING
C| DIFF           |<->| MATRICE DE DIFFUSION SYMETRIQUE
C| DT             |-->| PAS DE TEMPS
C| EPSDF          |-->| PRECISION POUR LA DIFFUSION DE F
C| FBORF          |---| 
C| FBORL,F,S      |-->| CONDITIONS AUX LIMITES DIRICHLET
C| FBORS          |---| 
C| FC             |-->| VARIABLE APRES CONVECTION
C| FD             |<--| VARIABLE APRES DIFFUSION
C| FMIN,FMAX      |-->| VALEURS DE CLIPPING
C| FN             |-->| VARIABLE AU TEMPS N
C| FSCE           |---| 
C| H             |---| 
C| IELM2H         |-->| TYPE DE DISCRETISATION 2DH
C| IELM2V         |-->| TYPE DE DISCRETISATION 2DV
C| IELM3          |-->| TYPE DE DISCRETISATION 3D
C| IMESH2         |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE 2D
C| IMESH3         |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE 3D
C| INCHYD         |---| 
C| INFO           |-->| INFORMATIONS SUR LES SOLVEURS
C| IPBOT          |---| 
C| IT1            |---| 
C| KNEU,KDIR,KDDL |-->| TYPES DES CONDITIONS LIMITES TECHNIQUES
C| LIFBOF         |---| 
C| LIFBOL,F,S     |-->| TYPE DE CONDITIONS LIMITES PHYSIQUES
C| LIFBOS         |---| 
C| LIMDIF         |-->| TYPES DES CONDITIONS AUX LIMITES
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| MASKBR         |---| 
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MASKPT         |-->| MASQUAGE DES POINTS
C| MATR2H         |<->| MATRICE DE TRAVAIL 2DH
C| MATR2V         |<->| MATRICE DE TRAVAIL 2DV
C| MDIFF          |---| 
C| MESH2D         |---| 
C| MESH3D         |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
C| MSUPG          |---| 
C| MTRA1          |<->| MATRICE DE TRAVAIL
C| MTRA2          |<->| MATRICE DE TRAVAIL
C| NBOR3          |-->| NUMEROS GLOBAUX DES POINTS FRONTIERES 3D
C| NEWDIF         |-->| RECALCULE OU NON LA MATRICE DE DIFFUSION
C| NITDF          |-->| NOMBRE D'ITERATIONS POUR LA DIFFUSION DE F
C| NPLAN          |---| 
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPOIN3         |-->| NOMBRE DE POINTS 3D
C| NPTFR3         |-->| NOMBRE DE POINTS FRONTIERE BORDS LATERAUX
C| NSCE           |---| 
C| OPTBAN         |---| 
C| OPTDIF         |-->| OPTION DE DIFFUSION DE F
C| PLUIE          |-->| RAIN IN M/S MULTIPLIED BY VOLU2D
C| PREDF          |-->| PRECONDITIONNEMENT POUR LA DIFFUSION DE F
C| RAIN           |-->| IF YES, THERE IS RAIN OR EVAPORATION
C| S0F            |-->| TERME SOURCE EXPLICITE (DIM=F/T)
C| S1F            |-->| TERME SOURCE IMPLICITE (DIM=1/T)
C| SCHCF          |-->| SCHEMA DE CONVECTION DE F
C| SCHDF          |-->| SCHEMA DE DIFFUSION DE F
C| SEM3D          |<->| SECOND MEMBRE
C| SIGMAF         |-->| COEFFICIENT DE REDUCTION DE LA VISCOSITE
C|                |   | UTILISE SEULEMENT POUR K ET EPSILON
C| SIGMAG         |---| 
C| SLVDIF         |---| 
C| SOLDF          |-->| SOLVEUR POUR LA DIFFUSION DE F
C| SOURCES        |---| 
C| SUPG           |-->| MATRICE SUPG NON SYMETRIQUE
C| SVIDE          |-->| STRUCTURE VIDE
C| T2_01          |---| 
C| T2_02          |---| 
C| T2_03          |---| 
C| T3_01,02,03    |<->| TABLEAUX DE TRAVAIL DE DIMENSION NPOIN3
C| T3_02          |---| 
C| T3_03          |---| 
C| T3_04          |---| 
C| TBB            |-->| BLOC DE BLOCS DE TRAVAIL
C| TETADI         |-->| COEF D'IMPLICITATION DE LA DIAGONALE DE LA
C|                |   | DIFFUSION SI  OPTDIF = 2
C|                |   | COEF D'IMPLICITATION DE LA DIFFUSION
C|                |   | SI  OPTDIF =1
C| TETASUPG       |---| 
C| TRAV3          |<->| STRUCTURE DE TABLEAUX DE TRAVAIL 3D
C| TRBAF          |---| 
C| VELOCITY       |---| 
C| VISCF          |-->| COEFFICIENTS DE VISCOSITE
C|                |   | VISCF(*,1 OU 2) VISCOSITE HORIZONTALE
C|                |   | VISCF(*,3)      VISCOSITE VERTICALE
C| VOLU           |---| 
C| W1             |<->| TABLEAU DE TRAVAIL (CALCUL DES MATRICES...)
C| WCC            |---| 
C| YAS0F          |---| 
C| YAS1F          |---| 
C| YASCE          |---| 
C| YASEM3D        |-->| IF TRUE, RIGHT HAND SIDE HAS BEEN PARTLY
C|                |   | COMPUTED BEFORE CALLING DIFF3D
C| YAWCC          |---| 
C| ZERO           |-->| PLUS PETITE VALEUR NON NULLE AUTORISEE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN)             :: SCHCF,SCHDF,TRBAF,OPTDIF,NSCE
      INTEGER, INTENT(IN)             :: NPOIN2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FD,FC,FN,S0F,S1F,VISCF
      TYPE(BIEF_OBJ), INTENT(IN)      :: LIFBOL,LIFBOF,LIFBOS
      TYPE(BIEF_OBJ), INTENT(IN)      :: FBORL,FBORF,FBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: AFBORL,AFBORF,AFBORS
      TYPE(BIEF_OBJ), INTENT(IN)      :: BFBORL,BFBORF,BFBORS,H,WCC
      DOUBLE PRECISION, INTENT(IN)    :: SIGMAF,FMIN,FMAX,DT,AGGLOD
      DOUBLE PRECISION, INTENT(IN)    :: FSCE(NSCE),TETASUPG
      DOUBLE PRECISION, INTENT(INOUT) :: TETADI
      LOGICAL, INTENT(IN)             :: CLIMIN,CLIMAX,YASCE,YAS0F
      LOGICAL, INTENT(IN)             :: INFO,NEWDIF,YASEM3D,YAS1F
      LOGICAL, INTENT(IN)             :: SIGMAG
      TYPE(SLVCFG)                    :: SLVDIF
!
      LOGICAL, INTENT(IN)             :: INCHYD,MSK,YAWCC,VELOCITY,RAIN
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKPT,MASKBR,MASKEL,VOLU
      TYPE(BIEF_OBJ), INTENT(IN)      :: NBOR3,SVIDE,SOURCES
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T3_01,T3_02,T3_03,T3_04
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T2_01,T2_02,T2_03
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3D,MESH2D
      INTEGER, INTENT(IN)             :: NPOIN3,IELM3,IELM2H,IELM2V
      INTEGER, INTENT(IN)             :: NPTFR3,NPLAN,OPTBAN
      INTEGER, INTENT(IN)             :: IPBOT(NPOIN2)
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SEM3D,IT1,TRAV3,MTRA1,MTRA2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: MSUPG,MDIFF
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: MATR2V,MATR2H
      DOUBLE PRECISION, INTENT(IN)    :: PLUIE(NPOIN2)
!
!-----------------------------------------------------------------------
!
      INTEGER IPOIN3,IPOIN2,IPTFR3,ITERD,NITERD,IS,IIS,I,IPLAN,I2D,NP
      DOUBLE PRECISION C,AGGLO
      CHARACTER(LEN=16) FORMUL
!
!-----------------------------------------------------------------------
!
C   SEMI-IMPLICITATION COEFFICIENT FOR SUPG
!
C     TETASUPG = 1.D0  (IS REQUIRED FOR MASS CONSERVATION)
C     IF NOT 1.D0, FLUX AT THE END OF CVDF3D SHOULD BE MODIFIED
C     TETASUPG = 0.55D0
!
      NITERD = SLVDIF%NITMAX
      IF(OPTDIF.EQ.2) TETADI = 0.D0
!
!=======================================================================
C   MASS MATRIX
!=======================================================================
!
C   COMPUTES MTRA2 : MASS MATRIX DIVIDED BY DT
!
      FORMUL='MATMAS          '
      CALL MATRIX(MTRA2, 'M=N     ', FORMUL, IELM3, IELM3, 1.D0/DT,
     &         SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE, MESH3D, MSK, MASKEL)
!
C MASS-LUMPING OF THE MASS MATRIX (FOR THE MOMENT ONLY IF
C EXPLICIT DIFFUSION OR 2 X IMPLICIT DIAGONAL)
C OF IF COMPATIBILITY WITH PSI SCHEME NEEDED (USE OF VOLU WHICH
C IS DIFFERENT FROM INTEGRAL OF TEST FUNCTIONS IF MASS-LUMPING
C IS DONE IN TELEMAC-2D)
!
      AGGLO=AGGLOD
      IF(TETADI.LT.0.001D0.OR.SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI
     &                    .OR.SCHCF.EQ.ADV_LPO.OR.SCHCF.EQ.ADV_LPO_TF
     &                    .OR.SCHCF.EQ.ADV_NSC_TF) AGGLO=1.D0
      IF(AGGLO.GT.0.001D0) THEN
C       VOLU/DT REPLACES MTRA2 LUMPED
        CALL OS('X=CY    ',X=T3_01,Y=VOLU,C=AGGLO/DT)
C       IF(MSK) THERE IS A RISK THAT VOLU=0 SOMEWHERE
C               THIS MAY CAUSE PROBLEMS IN LINEAR SYSTEMS
        IF(MSK) CALL OS('X=+(Y,C)',X=T3_01,Y=T3_01,C=1.D-3)
C       CALL LUMP(T3_01,MTRA2,MESH3D,AGGLO)
        CALL OM( 'M=CN    ',MTRA2,MTRA2 , SVIDE , 1.D0-AGGLO , MESH3D )
        CALL OM( 'M=M+D   ',MTRA2,MTRA2 , T3_01 , 0.D0       , MESH3D )
      ENDIF
!
!=======================================================================
C   SECOND MEMBER OF ADVECTION SCHEME + EXPLICIT SOURCE TERM
!=======================================================================
!
C   COMPUTES SEM3D = DT * S0F + FC
!
      IF(S0F%TYPR.NE.'0'.AND.YAS0F) THEN
        CALL OS( 'X=Y+CZ  ' , T3_04 , FC , S0F , DT )
      ELSE
        CALL OS( 'X=Y     ' , X=T3_04 , Y=FC )
      ENDIF
!
      IF(YASEM3D) THEN
C       HAS STARTED COMPUTING SEM3D IN TRISOU OR SOURCE
        CALL MATVEC ('X=X+CAY  ',SEM3D, MTRA2,T3_04,DT, MESH3D)
C                                                   DT BECAUSE MTRA2
C                                                   HAS A FACTOR 1/DT
      ELSE
        CALL MATVEC ('X=AY    ',SEM3D, MTRA2,T3_04, C, MESH3D)
      ENDIF
!
!=======================================================================
!
C     SOURCES INSIDE THE DOMAIN
!
      IF(YASCE.AND.NSCE.GT.0) THEN
      DO IS=1,NSCE
C       IF INTAKE FSCE=F, SO NO EXTRA TERM
        IIS=IS
C       IN PARALLEL MODE SOURCES WITHOUT PARCOM
        IF(NCSIZE.GT.1) IIS=IIS+NSCE
        DO I=1,NPOIN3
C         EXPLICIT SOURCE TERM
          SEM3D%R(I) = SEM3D%R(I)
     &               + MAX(SOURCES%ADR(IIS)%P%R(I),0.D0)*
     &            (FSCE(IS)-(1.D0-TETASUPG)*FN%R(I))
C         IMPLICIT SOURCE TERM : SEE BELOW
        ENDDO
      ENDDO
      ENDIF
!
!=======================================================================
!
C     RAIN (ALL TRACERS) - EXPLICIT PART
!
      IF(RAIN) THEN
        DO I=1,NPOIN2
          IPOIN3=NPOIN3-NPOIN2+I
          SEM3D%R(IPOIN3)=SEM3D%R(IPOIN3)-
     &                    PLUIE(I)*(1.D0-TETASUPG)*FN%R(IPOIN3)
        ENDDO
      ENDIF
!
!=======================================================================
!
C   EXPLICIT ADVECTION TERM:
!
      IF(SCHCF.EQ.ADV_SUP) THEN
        CALL MATVEC('X=X+CAY ',SEM3D,MSUPG,FN,TETASUPG-1.D0,MESH3D)
      ENDIF
!
!=======================================================================
C   IMPLICIT SOURCE TERM (MASS-LUMPED AND ADDED TO THE DIAGONAL)
!=======================================================================
!
C JMH PROPOSITION (LUMPED IMPLICIT SOURCE TERM)
C                  BEWARE THE + SIGN: THE TERM S1F HAS A + SIGN WHEN
C                  TO THE LEFT OF THE = SIGN
C                 (CAREFUL: OPPOSITE IN CVDFTR)
C                  VOLU USED FOR MASS-CONSERVATION
C                 (COMPATIBILITY WITH CONTINUITY EQUATION)
      IF(S1F%TYPR.NE.'0'.AND.YAS1F) THEN
        CALL OS( 'X=YZ    ' , X=T3_01 , Y=S1F , Z=VOLU )
        CALL OM( 'M=M+D   ' , MTRA2 , MTRA2 , T3_01 , C , MESH3D )
      ENDIF
!
!=======================================================================
!
C     SOURCES INSIDE THE DOMAIN
!
      IF(YASCE.AND.NSCE.GT.0) THEN
      DO IS=1,NSCE
C       IF INTAKE FSCE=T, SO NO EXTRA TERM
        IIS=IS
C       IN PARALLEL MODE SOURCES WITHOUT PARCOM
        IF(NCSIZE.GT.1) IIS=IIS+NSCE
        DO I=1,NPOIN3
C         IMPLICIT SOURCE TERM
          MTRA2%D%R(I)=MTRA2%D%R(I)+
     &                 MAX(SOURCES%ADR(IIS)%P%R(I),0.D0)*TETASUPG
        ENDDO
      ENDDO
      ENDIF
!
C     RAIN (ALL TRACERS) - IMPLICIT PART
!
      IF(RAIN) THEN
        DO I=1,NPOIN2
          IPOIN3=NPOIN3-NPOIN2+I
          MTRA2%D%R(IPOIN3)=MTRA2%D%R(IPOIN3)+PLUIE(I)*TETASUPG
        ENDDO
      ENDIF
!
!=======================================================================
C   DIFFUSION MATRIX + BOUNDARY TERMS
!=======================================================================
!
      IF(SCHDF.NE.0) THEN
!
         IF(INFO) CALL MITTIT(16,0.D0,0)
!
         IF(NEWDIF) THEN
!
C           RELEASE 5.7
C           FORMUL='MATDIF          '
C           RELEASE 5.8 : MONOTONICITY ENSURED
            FORMUL='MATDIF       MON'
            IF (INCHYD) FORMUL(7:7)='2'
!
             CALL MATRIX
     &      (MDIFF, 'M=N     ', FORMUL, IELM3, IELM3, 1.D0,
     &       VISCF%ADR(1)%P, VISCF%ADR(2)%P, VISCF%ADR(3)%P,
     &       SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
!
         ENDIF
!
!=======================================================================
!
         IF(OPTDIF.EQ.1) THEN
!
C        SEMI-IMPLICITATION OF THE DIFFUSION
!
         IF(TETADI.LT.0.999D0) THEN
C          IF(TETADI.GT.0.001) THEN
           CALL MATVEC ('X=X+CAY  ',SEM3D,MDIFF,FN,(TETADI-1.D0)/SIGMAF,
     &                              MESH3D)
C          ELSE
C          CALL VECTOR
C    *      (SEM3D, '+', 'VECDIF          ',IELM3,(TETADI-1.D0)/SIGMAF,
C    *       VISCF%ADR(1)%P,VISCF%ADR(2)%P,VISCF%ADR(3)%P,
C    *       FN, SVIDE, SVIDE, MESH3D,MSK, MASKEL)
C          ENDIF
         ENDIF
C        IF TETADI=0, NO NEED TO ADD MDIFF
         IF(TETADI.GT.0.001D0) THEN
           CALL OM('M=M+CN  ',MTRA2,MDIFF,SVIDE,TETADI/SIGMAF,MESH3D)
         ENDIF
!
         ENDIF
!
C TAKES THE IMPLICIT BOUNDARY TERMS INTO ACCOUNT (FRICTION FOR EXAMPLE)
! ---------------------------------------------
!
C   LATERAL FACES : (MASKBR SET ACCORDING TO MASKEL IN MASK3D)
C  (MASS-LUMPED FORM)
!
         IF(AFBORL%TYPR.NE.'0') THEN
           CALL VECTOR(T3_02,'=','MASBAS          ',IELM2V,-1.D0,SVIDE,
     &                 SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,MSK,MASKEL)
           CALL OS('X=XY    ',T3_02,AFBORL,AFBORL,0.D0)
           CALL OSDB( 'X=X+Y   ' , MTRA2%D , T3_02 , T3_02, C , MESH3D )
         ENDIF
!
C  (NON MASS-LUMPED FORM)
C        FORMUL='FMATMA          '
C        CALL MATRIX
C    *   (MATR2V, 'M=N     ', FORMUL, IELM2V, IELM2V, -1.D0, AFBORL,
C    *    SVIDE, SVIDE, SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKBR)
C        CALL OM('M=M+N   ',MTRA2,MATR2V,SVIDE,C,MESH3D)
!
C   BOTTOM (MASS-LUMPED FORM AS IN 2D):
!
         IF(AFBORF%TYPR.NE.'0') THEN
           CALL VECTOR(T2_03, '=','MASBAS          ',IELM2H,-1.D0,SVIDE,
     &                 SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH2D,MSK,MASKEL)
           CALL OV('X=XY    ',T2_03%R,AFBORF%R,AFBORF%R,0.D0,NPOIN2)
C          DRY ZONES OR CRUSHED ELEMENTS
C          SEE EQUIVALENT TREATMENT IN WAVE_EQUATION
           IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
             DO IPOIN2 = 1,NPOIN2
               DO NP=0,IPBOT(IPOIN2)
                 I=NP*NPOIN2+IPOIN2
                 MTRA2%D%R(I)=MTRA2%D%R(I)+T2_03%R(IPOIN2)
               ENDDO
             ENDDO
           ELSE
             DO IPOIN2 = 1,NPOIN2
               MTRA2%D%R(IPOIN2)=MTRA2%D%R(IPOIN2)+T2_03%R(IPOIN2)
             ENDDO
           ENDIF
         ENDIF
!
C   SURFACE (MASS-LUMPED FORM):
!
         IF(AFBORS%TYPR.NE.'0') THEN
           CALL VECTOR(T2_03, '=','MASBAS          ',IELM2H,-1.D0,SVIDE,
     &                 SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH2D,MSK,MASKEL)
           CALL OV('X=XY    ',T2_03%R,AFBORS%R,AFBORS%R,0.D0,NPOIN2)
           CALL OV('X=X+Y   ',MTRA2%D%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                        T2_03%R,T2_03%R,0.D0,NPOIN2)
         ENDIF
C           (NON MASS-LUMPED FORM):
C        FORMUL='FMATMA          '
C        CALL MATRIX
C    *   (MATR2H, 'M=N     ',FORMUL, IELM2H, IELM2H, -1.D0, AFBORS,
C    *    SVIDE, SVIDE, SVIDE, SVIDE, SVIDE, MESH2D, MSK, MASKEL)
C        CALL OM('M=M+NS  ', MTRA2, MATR2H, SVIDE, C, MESH3D)
!
C   TAKES THE EXPLICIT BOUNDARY TERMS INTO ACCOUNT
!   ---------------------------------------------
!
         CALL STRESS(SEM3D,'X=X+Y   ',T2_01,T3_02,
     &               BFBORL,BFBORF,BFBORS,NPOIN2,NPOIN3,MESH2D,
     &               MESH3D,IELM3,IELM2H,IELM2V,SVIDE,MSK,MASKBR,MASKEL)
!
!=======================================================================
C   SEDIMENT-SPECIFIC
C                                D
C   THE MATRIX - PSI1(J) * WCC * --( PSI2(I) ) IS ADDED
C                                DZ
C                                                     D          N+1
C   IT IS AN INTEGRATION BY PART OF TERM :  PSI2(I) * --( WCC * C    )
C                                                     DZ
!
C   CORRESPONDING BOUNDARY TERMS ARE THE DEPOSITION
C   THIS TERM IS INCLUDED IN ATABOF !!!
!
C   NOTE: IT IS DONE IF AND ONLY IF SED. DIFFUSION IS REQUIRED !
!=======================================================================
!
C
C       RELEASE 5.5
C
C       IF(YAWCC) THEN
C       FOR BOUNDARY TERMS, SEE SUBROUTINE FLUSED
C       CALL MATRIX
C    &      (MTRA1, 'M=N     ', 'MATFGR         Z', IELM3, IELM3, -1.D0,
C    &       WCC, SVIDE, SVIDE, SVIDE, SVIDE, SVIDE, MESH3D,MSK, MASKEL)
C
C       CALL OM('M=X(M)  ',MTRA2,MTRA2,SVIDE,C,MESH3D)
C       CALL OM('M=M+N   ',MTRA2,MTRA1,SVIDE,C,MESH3D)
C       ENDIF
C
C       RELEASE 5.6 (UPWINDING VERTICAL ADVECTION)
C
        IF(YAWCC) THEN
C       FOR BOUNDARY TERMS, SEE SUBROUTINE FLUSED
        CALL MATRIX
     &      (MTRA1, 'M=N     ', 'MATFGR         Z', IELM3, IELM3, -1.D0,
     &       WCC, SVIDE, SVIDE, SVIDE, SVIDE, SVIDE, MESH3D,MSK, MASKEL)
C       UPWINDING VERTICAL ADVECTION (HERE UPWIND COEFFICIENT 1.D0)
C       MTRA2 MUST STILL BE SYMMETRIC HERE
        CALL UPWIND(MTRA2,WCC,1.D0,MESH2D,MESH3D,NPLAN)
C       MTRA2 TRANSFORMED INTO NON SYMMETRIC MATRIX
        CALL OM('M=X(M)  ',MTRA2,MTRA2,SVIDE,C,MESH3D)
        CALL OM('M=M+N   ',MTRA2,MTRA1,SVIDE,C,MESH3D)
        ENDIF
!
      ENDIF
!
!=======================================================================
!
C   ADDS SUPG MATRIX IF SCHCF=AADV_SUP
!
!=======================================================================
!
!
      IF(SCHCF.EQ.ADV_SUP.AND.OPTDIF.EQ.1) THEN
!
         IF (INFO) CALL MITTIT(14,0.D0,0)
         IF(MTRA2%TYPEXT.EQ.'S') THEN
           CALL OM('M=X(M)  ',MTRA2,MTRA2,SVIDE,C,MESH3D)
         ENDIF
         CALL OM ('M=M+CN  ',MTRA2, MSUPG, SVIDE, TETASUPG, MESH3D )
!
      ENDIF
!
!=======================================================================
!
C   BOUNDARY CONDITIONS + PRECONDITIONING + SOLVER
!
!=======================================================================
!
C   BOUNDARY CONDITIONS FOR BOUNDARY POINTS (POINTS OF TYPE DIRICHLET)
!
      CALL CPSTVC(MTRA2%D,T3_03)
!
      DO IPOIN3 = 1,NPOIN3
        IT1%I(IPOIN3) = KDDL
        T3_03%R(IPOIN3) = 0.D0
      ENDDO
!
C   LATERAL BOUNDARY CONDITIONS
!
      DO IPTFR3 = 1,NPTFR3
        IF(LIFBOL%I(IPTFR3).EQ.KENT.OR.
     &    LIFBOL%I(IPTFR3).EQ.KENTU.OR.LIFBOL%I(IPTFR3).EQ.KADH) THEN
          IT1%I(NBOR3%I(IPTFR3)) = KDIR
          T3_03%R(NBOR3%I(IPTFR3)) = FBORL%R(IPTFR3)
        ENDIF
      ENDDO
!
C   BOTTOM AND SURFACE BOUNDARY CONDITIONS
!
      DO IPOIN2 = 1,NPOIN2
        IF(LIFBOF%I(IPOIN2).EQ.KENT.OR.LIFBOF%I(IPOIN2).EQ.KADH) THEN
          IT1%I(IPOIN2) = KDIR
          T3_03%R(IPOIN2) = FBORF%R(IPOIN2)
        ENDIF
        IF(LIFBOS%I(IPOIN2).EQ.KENT.OR.LIFBOS%I(IPOIN2).EQ.KADH) THEN
          IT1%I(NPOIN3-NPOIN2+IPOIN2) = KDIR
          T3_03%R(NPOIN3-NPOIN2+IPOIN2) = FBORS%R(IPOIN2)
        ENDIF
      ENDDO
!
C   CRUSHED POINTS AND TIDAL FLATS: FIRST TREATED AS DIRICHLET
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        IF(NCSIZE.GT.1) THEN
C         ONLY DIFFERENCE : VALUE OF MTRA2 EQUAL TO FAC INSTEAD OF 1
          CALL OS('X=Y     ',X=T3_02,Y=VOLU)
C         T3_02 WILL BE THE ASSEMBLED VOLUME
          CALL PARCOM(T3_02,2,MESH3D)
          DO I2D=1,NPOIN2
            IF(IPBOT(I2D).GT.0) THEN
              IF(LIFBOF%I(I2D).EQ.KENT.OR.LIFBOF%I(I2D).EQ.KADH) THEN
C               DIRICHLET POINT ON THE BOTTOM: ALL POINTS UP TO THE
C               FIRST FREE ARE TREATED AS DIRICHLET WITH FBORF VALUE
                DO I=0,IPBOT(I2D)
                  IPOIN3 = I2D+I*NPOIN2
                  MTRA2%D%R(IPOIN3)=MESH3D%FAC%R(IPOIN3)
                  IT1%I(IPOIN3)   = KDIR
                  T3_03%R(IPOIN3) = FBORF%R(I2D)
                ENDDO
              ELSE
C               POINTS WITH NO VOLUME
C               NECESSARY TO HAVE A WELL DEFINED LINEAR SYSTEM
C               NOT A DIRICHLET POINT ON THE BOTTOM
C               CRUSHED POINTS PROVISIONALLY SET TO FN
                DO I=0,IPBOT(I2D)-1
                  IPOIN3 = I2D+I*NPOIN2
                  IF(T3_02%R(IPOIN3).LT.1.D-10) THEN
                    MTRA2%D%R(IPOIN3)=MESH3D%FAC%R(IPOIN3)
                    IT1%I(IPOIN3)   = KDIR
                    T3_03%R(IPOIN3) = FN%R(IPOIN3)
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
          ENDDO
        ELSE
          DO I2D=1,NPOIN2
            IF(IPBOT(I2D).GT.0) THEN
              IF(LIFBOF%I(I2D).EQ.KENT.OR.LIFBOF%I(I2D).EQ.KADH) THEN
C               DIRICHLET POINT ON THE BOTTOM: ALL POINTS UP TO THE
C               FIRST FREE ARE TREATED AS DIRICHLET WITH FBORF VALUE
                DO I=0,IPBOT(I2D)
                  IPOIN3 = I2D+I*NPOIN2
                  MTRA2%D%R(IPOIN3)=1.D0
                  IT1%I(IPOIN3) = KDIR
                  T3_03%R(IPOIN3) = FBORF%R(I2D)
                ENDDO
              ELSE
C               POINTS WITH NO VOLUME
C               NECESSARY TO HAVE A WELL DEFINED LINEAR SYSTEM
C               NOT A DIRICHLET POINT ON THE BOTTOM
C               CRUSHED POINTS PROVISIONALLY SET TO FN
                DO I=0,IPBOT(I2D)-1
                  IPOIN3 = I2D+I*NPOIN2
                  IF(VOLU%R(IPOIN3).LT.1.D-10) THEN
                    MTRA2%D%R(IPOIN3)=1.D0
                    IT1%I(IPOIN3) = KDIR
                    T3_03%R(IPOIN3) = FN%R(IPOIN3)
                  ENDIF
                ENDDO
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
C---> IMPLICIT DIFFUSION (STANDARD)
!
      IF(OPTDIF.EQ.1) THEN
!
        CALL DIRI01(FD,MTRA2,SEM3D,T3_03,IT1%I,T3_01,T3_02,MESH3D,
     &              KDIR,MSK,MASKPT)
!
C   SOLVES THE LINEAR SYSTEM:
!
        IF(TETADI.GT.0.001D0.OR.SCHCF.EQ.ADV_SUP) THEN
          CALL SOLVE (FD, MTRA2, SEM3D, TRAV3, SLVDIF,
     &                INFO, MESH3D, MTRA1)
        ELSE
          IF(NCSIZE.GT.1) CALL PARCOM(MTRA2%D,2,MESH3D)
          CALL OS('X=Y/Z   ',FD,SEM3D,MTRA2%D,0.D0,2,0.D0,1.D-10)
        ENDIF
!
C---> EXPLICIT DIFFUSION / PARTIAL IMPLICITATION OF 2XDIAGONAL
!
      ELSEIF(OPTDIF.EQ.2) THEN
C---> BACKS UP IF NITERD> 1
        CALL OS( 'X=Y     ' , T3_04 , SEM3D , SVIDE , C )
C---> NITER=1
        CALL OS('X=CY    ',T3_01,MDIFF%D,T3_01,2.D0/SIGMAF )
        CALL OM( 'M=M+D   ',MTRA2,MTRA2,T3_01,C,MESH3D)
C CALL OM( 'M=N     ',MTRA1,MTRA2,MTRA2,C,MESH3D)
C
        CALL MATVEC
     &       ('X=X+CAY ',SEM3D,MDIFF,FN,-1.D0/SIGMAF,MESH3D)
        CALL OS('X=X+CYZ ',SEM3D,FN,MDIFF%D,2.D0/SIGMAF)
C
        CALL DIRI01(FD,MTRA2,SEM3D,T3_03,IT1%I,T3_01,T3_02,MESH3D,
     &            KDIR,MSK,MASKPT)
!
C-->  SOLVES THE LINEAR SYSTEM:
!
        CALL LUMP(T3_01, MTRA2, MESH3D, 1.D0)
        CALL OS( 'X=Y/Z   ' , FD , SEM3D , T3_01 , C )
C
C---> GAUSS-SEIDEL ITERATIONS
C
        IF(NITERD.GE.2) THEN
        DO ITERD = 2,NITERD
          CALL OS( 'X=Y     ' , SEM3D , T3_04 , SVIDE , C )
          CALL MATVEC
     &       ('X=X+CAY ',SEM3D,MDIFF,FD,-1.D0/SIGMAF,MESH3D)
          CALL OS('X=X+CYZ ',SEM3D,FD,MDIFF%D,2.D0/SIGMAF)
C
          CALL DIRI01(FD,MTRA2,SEM3D,T3_03,IT1%I,T3_01,T3_02,MESH3D,
     &              KDIR,MSK,MASKPT)
!
C---> SOLVES THE DIAGONAL LINEAR SYSTEM:
!
          CALL LUMP(T3_01, MTRA2, MESH3D, 1.D0)
          CALL OS( 'X=Y/Z   ' , FD , SEM3D , T3_01 , C )
!
        ENDDO
        ENDIF
C
      ENDIF
!
!=======================================================================
!
C   TREATS THE POINTS WHICH ARE COMPLETELY DRY
C    (BY DEFAULT FORCED TO 0)
!
      IF(MSK.AND.TRBAF.EQ.1) THEN
        DO IPOIN3 = 1,NPOIN3
          IF(MASKPT%R(IPOIN3).LT.0.5D0) FD%R(IPOIN3) = FN%R(IPOIN3)
        ENDDO
      ENDIF
!
!=======================================================================
!
C   THOSE CRUSHED POINTS THAT HAVE NO VOLUME MUST HAVE VALUES EQUAL
C   TO THE FIRST FREE POINT ABOVE (GIVEN BY PLANE IPBOT+1)
!
C   TO AVOID SPURIOUS GRADIENTS... BUT WELL, HEAVY
!
!=======================================================================
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        IF(NCSIZE.GT.1) THEN
          CALL OS('X=Y     ',X=T3_02,Y=VOLU)
C         T3_02 WILL BE THE ASSEMBLED VOLUME
          CALL PARCOM(T3_02,2,MESH3D)
          DO I2D=1,NPOIN2
            IF(IPBOT(I2D).GT.0) THEN
C             VALUE OF THE FIRST FREE POINT IS COPIED BELOW
C             FOR DIRICHLET CASES THIS FREE POINT HAS BEEN GIVEN ABOVE
C             THE FBORF DIRICHLET VALUE
              DO I=0,IPBOT(I2D)-1
                IF(T3_02%R(I2D+I*NPOIN2).LT.1.D-10) THEN
                  FD%R(I2D+I*NPOIN2)=FD%R(I2D+IPBOT(I2D)*NPOIN2)
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ELSE
          DO I2D=1,NPOIN2
            IF(IPBOT(I2D).GT.0) THEN
C             VALUE OF THE FIRST FREE POINT IS COPIED BELOW
C             FOR DIRICHLET CASES THIS FREE POINT HAS BEEN GIVEN ABOVE
C             THE FBORF DIRICHLET VALUE
              DO I=0,IPBOT(I2D)-1
                IF(VOLU%R(I2D+I*NPOIN2).LT.1.D-10) THEN
                  FD%R(I2D+I*NPOIN2)=FD%R(I2D+IPBOT(I2D)*NPOIN2)
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!
C   CLIPS F (IF REQUESTED WITH CLIMIN AND CLIMAX)
!
!=======================================================================
!
      CALL CLIP(FD,FMIN,CLIMIN,FMAX,CLIMAX,0)
!
!=======================================================================
!
      RETURN
      END
C
C#######################################################################
C