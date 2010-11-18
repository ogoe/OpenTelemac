C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOURCE TERMS FOR U & V MOMENTUM EQUATIONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D, INTERFACE_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, CORIOL, CV1, CV2, DELTAR, DENLAW, DT, FCOR, FU2, GRADZSX, GRADZSY, GRAV, IELM2H, IELM3, IKLE3, INCHYD, ISCE, KSCE, LATIT, LONGIT, LT, LV, MASKEL, MESH2D, MESH3, MSK, NELEM2, NELEM3, NETAGE, NORD, NPLAN, NPOIN2, NPOIN3, NREJEU, NTRAC, PRIVE, QSCE, SCHCVI, SCV1, SCV2, SEDI, SMASKEL, SMU, SMV, ST1, ST2, ST3, SURFAC, SVIDE, SVOLU, T1, T2, T3, TA, TRAV2, UN3, USCE, VN3, VOLU, VSCE, W1, W2, W3, X, Y, YASEM3D, Z, Z3, ZS
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::COUROU COUROU@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPTH NPTH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3D_FILES T3D_FILES@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::ADV_LPO ADV_LPO@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_LPO_TF ADV_LPO_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC ADV_NSC@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC_TF ADV_NSC_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_PSI ADV_PSI@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, ATH, BETA0Y, C, CORI, CORIVAR, COSNORD, DEJALU, DR1, DR2, DR3, DRSUDX, DRSUDY, DX1, DX2, DX3, DY1, DY2, DY3, DZ1, DZ123, DZ2, DZ3, DZSUDX, DZSUDY, ERR, FF0, FORMUL, FXH, FYH, I, I1, I2, I3, I3D, I4, I5, I6, IELEM3, IETAGE, IPLAN, IZ, IZM, IZS, NOMX, NOMY, NP, OKX, OKY, OMEGA, OPTFLO, PI, SINNORD, SR1, SR2, SR3, SZ1, SZ2, SZ3, W
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_TRISOU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ALLVEC(), ASSVEC(), FIND_IN_SEL(), OS(), OV(), OVD(), PARCOM(), PLANTE(), VECTOR()
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
!> </td><td> 29/06/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 19/12/2008
!> </td><td> JMH
!> </td><td> WAVE DRIVEN CURRENTS ADDED. SEE IF(COUROU)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td>
!> </td><td> AG (LNHE)
!> </td><td> BUOYANCY TERMS COMPUTED IN PHYSICAL SPACE
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td>
!> </td><td> CGD/SOGREAH
!> </td><td> CORIOLIS FORCE ADDED
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS DU PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>CORIOL
!></td><td>--></td><td>LOGIQUE INDIQUANT SI FORCE DE CORIOLIS
!>    </td></tr>
!>          <tr><td>CV1,CV2
!></td><td><--</td><td>TERMES SOURCES SUR U ET V
!>    </td></tr>
!>          <tr><td>DELTAR
!></td><td>--></td><td>DENSITE RELATIVE
!>    </td></tr>
!>          <tr><td>DENLAW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>FCOR
!></td><td>--></td><td>COEFFICIENT DE CORIOLIS
!>    </td></tr>
!>          <tr><td>FU2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRADZSX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRADZSY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>GRAVITE
!>    </td></tr>
!>          <tr><td>IELM2H
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td>--></td><td>CORRESPONDANCE ENTRE LA NUMEROTATION LOCALE
!>                  ET GLOBALE 3D
!>    </td></tr>
!>          <tr><td>INCHYD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ISCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LATIT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LONGIT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>NUMERO DU PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>LV
!></td><td>--></td><td>LONGUEUR DU VECTEUR POUR LA VECTORISATION
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>MASQUAGE DES ELEMENTS
!>    </td></tr>
!>          <tr><td>MESH2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NELEM3
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NETAGE
!></td><td>--></td><td>NOMBRE D'ETAGES SUR LA VERTICALE
!>    </td></tr>
!>          <tr><td>NORD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS SUR LA VERTICALE
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td>--></td><td>NOMBRE DE TABLEAUX DE DIMENSION NPOIN3
!>                  RESERVES A L'UTILISATEUR
!>    </td></tr>
!>          <tr><td>NREJEU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NOMBRE DE TRACEURS ACTIFS
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAUX RESERVES A L'UTILISATEUR
!>    </td></tr>
!>          <tr><td>QSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SCHCVI
!></td><td>--></td><td>ADVECTION SCHEME ON VELOCITY
!>    </td></tr>
!>          <tr><td>SCV1,SCV2
!></td><td><--</td><td>STRUCTURES ASSOCIEES
!>    </td></tr>
!>          <tr><td>SEDI
!></td><td>--></td><td>LOGIQUE INDIQUANT LA PRESENCE D'UN SEDIMENT
!>    </td></tr>
!>          <tr><td>SMASKEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SMU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SMV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ST1
!></td><td>--></td><td>STRUCTURE ASSOCIEE
!>    </td></tr>
!>          <tr><td>ST2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ST3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES ELEMENTS 2D
!>    </td></tr>
!>          <tr><td>SVIDE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SVOLU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1
!></td><td>--></td><td>TABLEAU DE TRAVAIL PAR POINTS
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TA
!></td><td>--></td><td>TRACEURS
!>    </td></tr>
!>          <tr><td>TRAV2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UN3,VN3
!></td><td>--></td><td>COMPOSANTES HORIZONTALES DE LA VITESSE A TN
!>    </td></tr>
!>          <tr><td>USCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VOLU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W1,W2,W3
!></td><td>--></td><td>TABLEAUX DE TRAVAIL PAR ELEMENTS 3D
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>YASEM3D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZS
!></td><td>--></td><td>COTE PAR RAPPORT A LA SURFACE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TRISOU
     & (CV1, CV2, SCV1, SCV2, UN3, VN3, TA, X, Y, Z, ZS,
     &  DELTAR,MESH3,FCOR,CORIOL,NTRAC,LT,AT,DT,SURFAC,
     &  T1,ST1, W1, W2, W3, SEDI, GRAV, NPOIN3, NELEM3, NPOIN2,
     &  NELEM2, NPLAN, NETAGE, IKLE3, PRIVE, LV, MSK, MASKEL, INCHYD,
     &  SVOLU,VOLU,SVIDE,IELM3,SMASKEL,NREJEU,ISCE,KSCE,QSCE,USCE,VSCE,
     &  IELM2H,GRADZSX,GRADZSY,Z3,TRAV2,FU2,MESH2D, ST2,T2,ST3,T3,
     &  LATIT, LONGIT, NORD,SMU,SMV,YASEM3D,SCHCVI,DENLAW)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS DU PAS DE TEMPS
C| CORIOL         |-->| LOGIQUE INDIQUANT SI FORCE DE CORIOLIS
C| CV1,CV2        |<--| TERMES SOURCES SUR U ET V
C| DELTAR         |-->| DENSITE RELATIVE
C| DENLAW         |---| 
C| DT             |-->| PAS DE TEMPS
C| FCOR           |-->| COEFFICIENT DE CORIOLIS
C| FU2            |---| 
C| GRADZSX        |---| 
C| GRADZSY        |---| 
C| GRAV           |-->| GRAVITE
C| IELM2H         |---| 
C| IELM3          |---| 
C| IKLE3          |-->| CORRESPONDANCE ENTRE LA NUMEROTATION LOCALE
C|                |   | ET GLOBALE 3D
C| INCHYD         |---| 
C| ISCE           |---| 
C| KSCE           |---| 
C| LATIT          |---| 
C| LONGIT         |---| 
C| LT             |-->| NUMERO DU PAS DE TEMPS
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MESH2D         |---| 
C| MESH3          |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
C| NELEM2         |-->| NOMBRE D'ELEMENTS DU MAILLAGE 2D
C| NELEM3         |-->| NOMBRE D'ELEMENTS DU MAILLAGE 3D
C| NETAGE         |-->| NOMBRE D'ETAGES SUR LA VERTICALE
C| NORD           |---| 
C| NPLAN          |-->| NOMBRE DE PLANS SUR LA VERTICALE
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| NPOIN3         |-->| NOMBRE DE POINTS DU MAILLAGE 3D
C| NPRIV          |-->| NOMBRE DE TABLEAUX DE DIMENSION NPOIN3
C|                |   | RESERVES A L'UTILISATEUR
C| NREJEU         |---| 
C| NTRAC          |-->| NOMBRE DE TRACEURS ACTIFS
C| PRIVE          |-->| TABLEAUX RESERVES A L'UTILISATEUR
C| QSCE           |---| 
C| SCHCVI         |-->| ADVECTION SCHEME ON VELOCITY
C| SCV1,SCV2      |<--| STRUCTURES ASSOCIEES
C| SEDI           |-->| LOGIQUE INDIQUANT LA PRESENCE D'UN SEDIMENT
C| SMASKEL        |---| 
C| SMU            |---| 
C| SMV            |---| 
C| ST1            |-->| STRUCTURE ASSOCIEE
C| ST2            |---| 
C| ST3            |---| 
C| SURFAC         |-->| SURFACE DES ELEMENTS 2D
C| SVIDE          |---| 
C| SVOLU          |---| 
C| T1             |-->| TABLEAU DE TRAVAIL PAR POINTS
C| T2             |---| 
C| T3             |---| 
C| TA             |-->| TRACEURS
C| TRAV2          |---| 
C| UN3,VN3        |-->| COMPOSANTES HORIZONTALES DE LA VITESSE A TN
C| USCE           |---| 
C| VOLU           |---| 
C| VSCE           |---| 
C| W1,W2,W3       |-->| TABLEAUX DE TRAVAIL PAR ELEMENTS 3D
C| X,Y,Z          |-->| COORDONNEES DU MAILLAGE 3D
C| YASEM3D        |---| 
C| Z3             |---| 
C| ZS             |-->| COTE PAR RAPPORT A LA SURFACE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE INTERFACE_TELEMAC3D, EX_TRISOU => TRISOU
      USE DECLARATIONS_TELEMAC3D, ONLY : COUROU,NPTH,T3D_FILES,T3DBI1
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN3,NELEM3,NPOIN2,NELEM2
      INTEGER, INTENT(IN) :: NPLAN,NETAGE,NTRAC
      INTEGER, INTENT(IN) :: LV,LT,IELM2H,NREJEU,IELM3,SCHCVI,DENLAW
!
      INTEGER, INTENT(IN) :: IKLE3(NELEM3,6)
!
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELEM3)
      DOUBLE PRECISION, INTENT(IN)    :: LATIT,LONGIT,NORD
!
      DOUBLE PRECISION, INTENT(INOUT) :: CV1(NPOIN3),CV2(NPOIN3)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: SCV1, SCV2
!
      DOUBLE PRECISION, INTENT(IN)    :: UN3(NPOIN3), VN3(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELEM2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN3), Y(NPOIN3), Z(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: ZS(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELEM3,6), W2(NELEM3,6)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELEM3,6)
      DOUBLE PRECISION, DIMENSION(NPOIN2,NPLAN), INTENT(INOUT) :: VOLU
!
      DOUBLE PRECISION, INTENT(INOUT) :: T1(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: T2(NPOIN3), T3(NPOIN3)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ST1, ST2, ST3
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: TA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: PRIVE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: DELTAR
      TYPE(BIEF_MESH)                 :: MESH3
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: GRADZSX
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: GRADZSY
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2D
      TYPE(BIEF_OBJ),  INTENT(IN)     :: Z3
      TYPE (BIEF_OBJ), INTENT(IN)     :: SMASKEL
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: SVIDE
      TYPE (BIEF_OBJ), INTENT(INOUT)  :: TRAV2, FU2
      TYPE(BIEF_OBJ),  INTENT(INOUT)  :: SVOLU,SMU,SMV
!
C                                 * = NSCE
      INTEGER, INTENT(IN) :: ISCE(*),KSCE(*)
      DOUBLE PRECISION, INTENT(IN) :: QSCE(*),USCE(*),VSCE(*)
!
      DOUBLE PRECISION, INTENT(IN) :: GRAV, DT, AT, FCOR
      LOGICAL, INTENT(IN) :: CORIOL, SEDI, MSK, INCHYD
      LOGICAL, INTENT(INOUT) :: YASEM3D
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM3,IPLAN,IETAGE,IZ,IZM,IZS,ERR,NP,I3D
      DOUBLE PRECISION A,OMEGA,PI,COSNORD,SINNORD,CORI,BETA0Y,FF0
      LOGICAL CORIVAR
C
      INTEGER I,OPTFLO
      CHARACTER(LEN=15) FORMUL
!
      INTEGER I1,I2,I3,I4,I5,I6
      DOUBLE PRECISION DX1,DX2,DX3,DY1,DY2,DY3
      DOUBLE PRECISION DZ1,DZ2,DZ3,DR1,DR2,DR3
      DOUBLE PRECISION SZ1,SZ2,SZ3,SR1,SR2,SR3
      DOUBLE PRECISION DZSUDX,DZSUDY,DRSUDX,DRSUDY
      DOUBLE PRECISION DZ123,C,ATH
!
C     FOR WAVE DRIVEN CURRENTS
!
      CHARACTER*16 NOMX,NOMY
      LOGICAL DEJALU,OKX,OKY
      DATA DEJALU /.FALSE./
      REAL, ALLOCATABLE :: W(:)
      TYPE(BIEF_OBJ) :: FXH,FYH
      SAVE FXH,FYH,W
!
!***********************************************************************
!
C INITIALISES
!
C     CALL OS( 'X=C     ' , X=SCV1 , C=0.D0 )
C     CALL OS( 'X=C     ' , X=SCV2 , C=0.D0 )
!
      SCV1%TYPR='0'
      SCV2%TYPR='0'
!
!-----------------------------------------------------------------------
C  BUOYANCY SOURCE TERMS
!-----------------------------------------------------------------------
!
      YASEM3D=.FALSE.
!
      IF(DENLAW.NE.0.AND.NTRAC.GT.0) THEN
!
      SCV1%TYPR='Q'
      SCV2%TYPR='Q'
      CALL OS( 'X=0     ' , X=SCV1 )
      CALL OS( 'X=0     ' , X=SCV2 )
!
C     VOLUME OF TEST FUNCTIONS
!
      CALL VECTOR(ST1, '=', 'MASBAS          ',IELM3, 1.D0,
     &            SVIDE, SVIDE,
     &            SVIDE, SVIDE, SVIDE, SVIDE, MESH3,.FALSE.,SMASKEL)
      IF(NCSIZE.GT.1) CALL PARCOM(ST1,2,MESH3)
!
C     1 : BUOYANCY IN REAL MESH
C     2 : BUOYANCY IN TRANSFORMED MESH
!
C     OPTFLO CHANGED FROM 2 INTO 1 BY JMH ON 03/10/2002
C     ENABLES TREATMENT WITH TETRAHEDRONS
C     WHO CAN TELL WHICH IS BEST WITH PRISMS ?
!
      OPTFLO=1
!
      IF(OPTFLO.EQ.1) THEN
!
      YASEM3D=.FALSE.
!
C - G DENSITY GRADIENTS
C     WITH TREATMENT OF HYDROSTATIC INCONSISTENCIES IF NEEDED
!
      FORMUL='GRADF          '
      IF(INCHYD) FORMUL(6:6)='2'
!
!     ===============================================
C     BETTER FILTERING OF HYDROSTATIC INCONSISTENCIES
!     ===============================================
!
C     3 OR 4 IMPLIES THAT 2 IS ALSO APPLIED
!
C     RECOMMENDED : FILTER 4
!
C     FILTER 3
C     IF(INCHYD) FORMUL(6:6)='3'
C     FILTER 4
C     IF(INCHYD) FORMUL(6:6)='4'
!
!
      CALL VECTOR(ST2, '=',FORMUL//'X',IELM3,-GRAV,DELTAR,SVIDE,
     &            SVIDE,SVIDE,SVIDE,SVIDE,MESH3,MSK,SMASKEL)
!
      CALL VECTOR(ST3, '=',FORMUL//'Y',IELM3,-GRAV,DELTAR,SVIDE,
     &            SVIDE,SVIDE,SVIDE,SVIDE,MESH3,MSK,SMASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(ST2,2,MESH3)
        CALL PARCOM(ST3,2,MESH3)
      ENDIF
!
C NODAL VALUE
!
      CALL OVD('X=Y/Z   ',T2,T2,T1,0.D0,NPOIN3,2,0.D0,1.D-9)
      CALL OVD('X=Y/Z   ',T3,T3,T1,0.D0,NPOIN3,2,0.D0,1.D-9)
!
C SIMPSON INTEGRATION
!
      DO IPLAN = NPLAN, 2, -1
        DO I = 1, NPOIN2
          IZ  =(IPLAN-1)*NPOIN2+I
          IZM =(IPLAN-2)*NPOIN2+I
          A = 0.5D0 * (Z(IZ)-Z(IZM))
          CV1(IZM) = CV1(IZ) + (T2(IZ)+T2(IZM)) * A
          CV2(IZM) = CV2(IZ) + (T3(IZ)+T3(IZM)) * A
        ENDDO
      ENDDO
!
C TERMS WITH FREE SURFACE GRADIENT (NODAL VALUES)
!
      DO IPLAN = 1, NPLAN-1
!
        DO I = 1, NPOIN2
          IZ  = (IPLAN-1)*NPOIN2+I
          IZS = (NPLAN-1)*NPOIN2+I
          A = GRAV*(DELTAR%R(IZ)-DELTAR%R(IZS))
!
          CV1(IZ) = CV1(IZ) + A * GRADZSX%R(I)
          CV2(IZ) = CV2(IZ) + A * GRADZSY%R(I)
!
        ENDDO
!
      ENDDO
!
      ELSEIF(OPTFLO.EQ.2) THEN
!
      YASEM3D = .FALSE.
!
C TRANSFORMED MESH
!
C COMPUTES ZS: OPPOSITE OF WATER DEPTH AT CONSIDERED POINT
!
        I2 = NPOIN3 - NPOIN2 + 1
        I4 = NPOIN3
        DO IPLAN = 1,NPLAN
          I1 = NPOIN2*(IPLAN-1) + 1
          I3 = NPOIN2* IPLAN
          CALL OV( 'X=Y-Z   ', ZS(I1:I3), Z(I1:I3), Z(I2:I4), C, NPOIN2)
        END DO
!
        BYELEMENT: DO IELEM3 = 1 , NELEM3
!
           I1 = IKLE3(IELEM3,1)
           I2 = IKLE3(IELEM3,2)
           I3 = IKLE3(IELEM3,3)
           I4 = IKLE3(IELEM3,4)
           I5 = IKLE3(IELEM3,5)
           I6 = IKLE3(IELEM3,6)
!
           DX1 = X(I3) - X(I2)
           DX2 = X(I1) - X(I3)
           DX3 = X(I2) - X(I1)
           DY1 = Y(I2) - Y(I3)
           DY2 = Y(I3) - Y(I1)
           DY3 = Y(I1) - Y(I2)
!
           DR1 = DELTAR%R(I4) - DELTAR%R(I1)
           DR2 = DELTAR%R(I5) - DELTAR%R(I2)
           DR3 = DELTAR%R(I6) - DELTAR%R(I3)
           DZ1 = ZS(I4) - ZS(I1)
           DZ2 = ZS(I5) - ZS(I2)
           DZ3 = ZS(I6) - ZS(I3)
           DZ123 = DZ1 + DZ2 + DZ3
!
           SZ1 = ZS(I4) + ZS(I1)
           SZ2 = ZS(I5) + ZS(I2)
           SZ3 = ZS(I6) + ZS(I3)
           SR1 = DELTAR%R(I4) + DELTAR%R(I1)
           SR2 = DELTAR%R(I5) + DELTAR%R(I2)
           SR3 = DELTAR%R(I6) + DELTAR%R(I3)
!
           IF(MAX(ZS(I1),ZS(I2),ZS(I3)).GT.
     &        MIN(ZS(I4),ZS(I5),ZS(I6)).AND.INCHYD) THEN
              DR1 = 0.D0
              DR2 = 0.D0
              DR3 = 0.D0
              SR1 = 0.D0
              SR2 = 0.D0
              SR3 = 0.D0
           ENDIF
!
           DZSUDX = SZ1 * DY1 + SZ2 * DY2 + SZ3 * DY3
           DZSUDY = SZ1 * DX1 + SZ2 * DX2 + SZ3 * DX3
           DRSUDX = SR1 * DY1 + SR2 * DY2 + SR3 * DY3
           DRSUDY = SR1 * DX1 + SR2 * DX2 + SR3 * DX3
!
           W1(IELEM3,1) = DR1 * DZSUDX - DZ1 * DRSUDX
           W1(IELEM3,2) = DR2 * DZSUDX - DZ2 * DRSUDX
           W1(IELEM3,3) = DR3 * DZSUDX - DZ3 * DRSUDX
           W2(IELEM3,1) = DR1 * DZSUDY - DZ1 * DRSUDY
           W2(IELEM3,2) = DR2 * DZSUDY - DZ2 * DRSUDY
           W2(IELEM3,3) = DR3 * DZSUDY - DZ3 * DRSUDY
           W3(IELEM3,1) = DZ1 + DZ123
           W3(IELEM3,2) = DZ2 + DZ123
           W3(IELEM3,3) = DZ3 + DZ123
!
        END DO BYELEMENT
!
        IF (NETAGE.NE.1) THEN
!
           BYETAGE: DO IETAGE = NETAGE-1 , 1 , -1
              I2 = NELEM2*IETAGE + 1
              I1 = I2 - NELEM2
              CALL OV('X=X+Y   ' , W1(I1,1) , W1(I2,1) , Z , C , NELEM2)
              CALL OV('X=X+Y   ' , W1(I1,2) , W1(I2,2) , Z , C , NELEM2)
              CALL OV('X=X+Y   ' , W1(I1,3) , W1(I2,3) , Z , C , NELEM2)
              CALL OV('X=X+Y   ' , W2(I1,1) , W2(I2,1) , Z , C , NELEM2)
              CALL OV('X=X+Y   ' , W2(I1,2) , W2(I2,2) , Z , C , NELEM2)
              CALL OV('X=X+Y   ' , W2(I1,3) , W2(I2,3) , Z , C , NELEM2)
           END DO BYETAGE
!
           I2 = NELEM2 + 1
           I1 = NELEM3 - NELEM2
           CALL OV('X=Y     ' , W1(1,4) , W1(I2,1) , Z , C , I1)
           CALL OV('X=Y     ' , W1(1,5) , W1(I2,2) , Z , C , I1)
           CALL OV('X=Y     ' , W1(1,6) , W1(I2,3) , Z , C , I1)
           CALL OV('X=Y     ' , W2(1,4) , W2(I2,1) , Z , C , I1)
           CALL OV('X=Y     ' , W2(1,5) , W2(I2,2) , Z , C , I1)
           CALL OV('X=Y     ' , W2(1,6) , W2(I2,3) , Z , C , I1)
!
        ENDIF
!
        I1 = NELEM3 - NELEM2 + 1
        CALL OV('X=C     ' , W1(I1,4) , Y , Z , 0.D0 , NELEM2)
        CALL OV('X=C     ' , W1(I1,5) , Y , Z , 0.D0 , NELEM2)
        CALL OV('X=C     ' , W1(I1,6) , Y , Z , 0.D0 , NELEM2)
        CALL OV('X=C     ' , W2(I1,4) , Y , Z , 0.D0 , NELEM2)
        CALL OV('X=C     ' , W2(I1,5) , Y , Z , 0.D0 , NELEM2)
        CALL OV('X=C     ' , W2(I1,6) , Y , Z , 0.D0 , NELEM2)
!
        CALL OV('X=XY    ' , W1(1,1) , W3 , Z , C , 3*NELEM3)
        CALL OV('X=XY    ' , W1(1,4) , W3 , Z , C , 3*NELEM3)
        CALL OV('X=XY    ' , W2(1,1) , W3 , Z , C , 3*NELEM3)
        CALL OV('X=XY    ' , W2(1,4) , W3 , Z , C , 3*NELEM3)
!
        DO IETAGE = 1 , NETAGE
          I1 = NELEM2*(IETAGE-1) + 1
          CALL OV('X=XY    ' , W3(I1,1) , SURFAC , Z , C , NELEM2)
          CALL OV('X=XY    ' , W3(I1,2) , SURFAC , Z , C , NELEM2)
          CALL OV('X=XY    ' , W3(I1,3) , SURFAC , Z , C , NELEM2)
        ENDDO
        CALL OV('X=Y     ' , W3(1,4) , W3 , Z , C , 3*NELEM3)
!
        CALL ASSVEC(CV1,IKLE3,NPOIN3,NELEM3,NELEM3,41,W1,.FALSE.,
     &              LV,MSK,MASKEL)
        CALL ASSVEC(CV2,IKLE3,NPOIN3,NELEM3,NELEM3,41,W2,.FALSE.,
     &              LV,MSK,MASKEL)
        CALL ASSVEC(T1,IKLE3,NPOIN3,NELEM3,NELEM3,41,W3,.TRUE. ,
     &              LV,MSK,MASKEL)
!
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(SCV1,2,MESH3)
          CALL PARCOM(SCV2,2,MESH3)
          CALL PARCOM(ST1,2,MESH3)
        ENDIF
!
        CALL OVD('X=CY/Z  ',CV1,CV1,T1,0.25D0*GRAV,NPOIN3,2,0.D0,1D-9)
        CALL OVD('X=CY/Z  ',CV2,CV2,T1,0.25D0*GRAV,NPOIN3,2,0.D0,1D-9)
!
      ENDIF
!
C     IF(NTRAC.GT.0)
      ENDIF
!
!-----------------------------------------------------------------------
C  CORIOLIS FORCE
!
C  NOTE JMH : THERE ARE ADDITIONAL TERMS IF W IS TAKEN INTO ACCOUNT
!
!-----------------------------------------------------------------------
!
      IF(CORIOL) THEN
!
         IF(SCV1%TYPR.EQ.'0') THEN
           CALL OS( 'X=0     ' , X=SCV1 )
           CALL OS( 'X=0     ' , X=SCV2 )
           SCV1%TYPR='Q'
           SCV2%TYPR='Q'
         ENDIF
!
         PI=ACOS(-1.D0)
         OMEGA=2.D0*PI/86164.D0
!
C - NORD IS THE ANGLE BETWEEN NORTH AND THE USER'S Y AXIS
!
         COSNORD=COS(PI*NORD/180.D0)
         SINNORD=SIN(PI*NORD/180.D0)
!
C - IF CORIOLIS FORCE DEPENDS ON Y COORDINATE (DEFAULT)
!
        CORIVAR=.FALSE.
!
        IF(CORIVAR) THEN
!
          FF0=2.D0*OMEGA*SIN(LATIT*PI/180.D0)
!
          DO I=1,NPOIN3
!
C                            6.37D6 : EARTH RADIUS
          BETA0Y=(2.D0*OMEGA/6.37D6)*COS(LATIT*PI/180.D0)
     &                              *(Y(I)*COSNORD-X(I)*SINNORD)
          CORI=FF0+BETA0Y
!
          IF(10.D0*BETA0Y.GE.FF0)THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'TRISOU : APPROX. BETA NON VALIDE'
            ELSE
              WRITE(LU,*) 'TRISOU: BETA APPROX. NOT VALID'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
!
          CV1(I)=CV1(I)+VN3(I)*CORI
          CV2(I)=CV2(I)-UN3(I)*CORI
!
          ENDDO
!
        ELSE
!
          CALL OV('X=X+CY   ',CV1,VN3,VN3, FCOR,NPOIN3)
          CALL OV('X=X+CY   ',CV2,UN3,UN3,-FCOR,NPOIN3)
!
        ENDIF
!
      ENDIF
C
C***********************************************************************
C
C     * WITH WAVE DRIVEN CURRENTS
C       -------------------------
C
C       FORCING TERMS FROM A TOMAWAC RESULTS FILE
C
C       BEWARE :    1. MESHES MUST BE THE SAME
C       ---------
C                   2. TAKES THE LAST TIMESTEP FROM TOMAWAC FILE
C
      IF(COUROU) THEN
C
         IF(.NOT.DEJALU) THEN
C
            ALLOCATE(W(NPOIN2),STAT=ERR)
            IF(ERR.NE.0) THEN
              IF(LNG.EQ.1) THEN
                WRITE(LU,*) 'ERREUR D''ALLOCATION DE W DANS TRISOU'
              ENDIF
              IF(LNG.EQ.2) THEN
                WRITE(LU,*) 'MEMORY ALLOCATION ERROR OF W IN TRISOU'
              ENDIF
            ENDIF
            CALL ALLVEC(1,FXH   ,'FXH   ',11, 1 , 2 )
            CALL ALLVEC(1,FYH   ,'FYH   ',11, 1 , 2 )
C
C           T3DBI1 : BINARY DATA FILE 1
            NOMX='FORCE FX        '
            NOMY='FORCE FY        '
            CALL FIND_IN_SEL(FXH,NOMX,T3D_FILES(T3DBI1)%LU,
     &                       W,OKX,NPTH,NP,ATH)
            CALL FIND_IN_SEL(FYH,NOMY,T3D_FILES(T3DBI1)%LU,
     &                       W,OKY,NPTH,NP,ATH)
C
            IF(.NOT.OKX.OR..NOT.OKY) THEN
              IF(LNG.EQ.1) WRITE(LU,5)
              IF(LNG.EQ.2) WRITE(LU,6)
 5            FORMAT(1X,'TRISOU : FORCE FX OU FY NON TROUVES',/,1X,
     &                  '         DANS LE FICHIER DE HOULE')
 6            FORMAT(1X,'TRISOU: FORCE FX OR FY NOT FOUND',/,1X,
     &                  '        IN THE WAVE RESULTS FILE')
              CALL PLANTE(1)
              STOP
            ENDIF
            IF(NP.NE.NPOIN2) THEN
              IF(LNG.EQ.1) WRITE(LU,95)
              IF(LNG.EQ.2) WRITE(LU,96)
 95           FORMAT(1X,'TRISOU : SIMULATION DES COURANTS DE HOULE.',/,
     &               1X,'LES MAILLAGES HOULE ET COURANTS SONT ',/,
     &               1X,'DIFFERENTS : PAS POSSIBLE POUR LE MOMENT.')
 96           FORMAT(1X,'TRISOU: WAVE DRIVEN CURRENTS MODELLING.',/,
     &               1X,'WAVE AND CURRENT MODELS MESHES ARE ',/,
     &               1X,'DIFFERENT : NOT POSSIBLE AT THE MOMENT.')
C
              CALL PLANTE(1)
              STOP
            ENDIF
C           WRITES OUT TO LISTING
            IF(LNG.EQ.1) WRITE(LU,115) ATH
            IF(LNG.EQ.2) WRITE(LU,116) ATH
115         FORMAT(1X,/,1X,'TRISOU : COURANTS DE HOULE',/,
     &                  1X,'         LECTURE AU TEMPS ',F10.3,/)
116         FORMAT(1X,/,1X,'TRISOU: WAVE DRIVEN CURRENTS MODELLING',/,
     &                  1X,'         READING FILE AT TIME ',F10.3,/)
            DEJALU = .TRUE.
C
         ENDIF
C
C        ADDS TO SOURCE TERMS
C
         IF(SCV1%TYPR.EQ.'0') THEN
           DO I=1,NPOIN2
             DO IPLAN=1,NPLAN
               I3D=((IPLAN-1)*NPOIN2)+I
C              CV1(I3D)=1.5D0*FXH%R(I)  (SOGREAH-PECHON-TEISSON VERSION)
               CV1(I3D)=FXH%R(I)
               CV2(I3D)=FYH%R(I)
             ENDDO
           ENDDO
           SCV1%TYPR='Q'
           SCV2%TYPR='Q'
         ELSE
           DO I=1,NPOIN2
             DO IPLAN=1,NPLAN
               I3D=((IPLAN-1)*NPOIN2)+I
C              CV1(I3D)=CV1(I3D)+1.5D0*FXH%R(I)  (SOGREAH-PECHON-TEISSON VERSION)
               CV1(I3D)=CV1(I3D)+FXH%R(I)
               CV2(I3D)=CV2(I3D)+FYH%R(I)
             ENDDO
           ENDDO
         ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
C TAKES THE VELOCITY OF SOURCES INTO ACCOUNT
!
C NOTE : IF USCE AND VSCE ARE NOT GIVEN, CONSIDERS THAT
C        USCE=UN3 AND VSCE=VN3
!
      IF(NREJEU.GT.0.AND.SCHCVI.NE.ADV_NSC.AND.SCHCVI.NE.ADV_PSI
     &              .AND.SCHCVI.NE.ADV_LPO.AND.SCHCVI.NE.ADV_NSC_TF
     &              .AND.SCHCVI.NE.ADV_LPO_TF) THEN
!
        IF(SCV1%TYPR.EQ.'0') THEN
          CALL OS( 'X=0     ' , X=SCV1 )
          CALL OS( 'X=0     ' , X=SCV2 )
          SCV1%TYPR='Q'
          SCV2%TYPR='Q'
        ENDIF
!
C       WITH DISTRIBUTIVE SCHEMES AND FINITE VOLUME SCHEMES
C       THIS IS DONE DIRECTLY INTO SUBROUTINE MURD3D, AND NOT WITH CV1
!
        DO I=1,NREJEU
        CV1((KSCE(I)-1)*NPOIN2+ISCE(I))=CV1((KSCE(I)-1)*NPOIN2+ISCE(I))
     &      + (USCE(I)-UN3((KSCE(I)-1)*NPOIN2+ISCE(I)))*
     &               QSCE(I)/VOLU(ISCE(I),KSCE(I))
        CV2((KSCE(I)-1)*NPOIN2+ISCE(I))=CV2((KSCE(I)-1)*NPOIN2+ISCE(I))
     &      + (VSCE(I)-VN3((KSCE(I)-1)*NPOIN2+ISCE(I)))*
     &               QSCE(I)/VOLU(ISCE(I),KSCE(I))
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C