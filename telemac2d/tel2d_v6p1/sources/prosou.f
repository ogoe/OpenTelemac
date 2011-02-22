
!>  @brief       PREPARES THE SOURCE TERMS IN THE CONTINUITY EQUATION
!>                AND IN THE DYNAMIC EQUATIONS. ARE TAKEN INTO ACCOUNT :
!><br>              - WIND
!><br>              - CORIOLIS FORCE
!><br>              - TIDAL FORCE
!><br>              - SOURCES AND SINKS
!>  @code
!>    RESPECTIVE TERMS ARE:
!>    ==========================
!>
!>     * WIND
!>       ---------
!>                                 1                         2      2
!>                FU           =  --- * F    * U    * SQRT( U    + V   )
!>                  VENT           H     AIR    AIR          AIR    AIR
!>
!>                                 1                         2      2
!>                FV           =  --- * F    * V    * SQRT( U    + V   )
!>                  VENT           H     AIR    AIR          AIR    AIR
!>
!>           WHERE :
!>                  UAIR   :  WIND VELOCITY ALONG X
!>                  VAIR   :  WIND VELOCITY ALONG Y
!>                  FAIR   :  AIR FRICTION COEFFICIENT
!>
!>     * CORIOLIS FORCE
!>       ---------------------
!>
!>                FU           =  + FCOR * V
!>                  CORIOLIS
!>
!>                FV           =  - FCOR * U
!>                  CORIOLIS
!>
!>           WHERE :
!>                  U       :  FLOW VELOCITY ALONG X
!>                  V       :  FLOW VELOCITY ALONG Y
!>                  FCOR    :  CORIOLIS PARAMETER
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  BOTTOM FRICTION IS TAKEN INTO ACCOUNT IN THE PROPAGATION
!>         THROUGH CALL TO FROTXY, IT IS SEMI-IMPLICIT.

!>  @note  IF SOURCES OR SINKS TERMS ARE ADDED TO THE CONTINUITY EQUATION,
!>         IT IS IDENTIFIED WITH VARIABLE YASMH (SET TO TRUE).

!>  @note  SOURCE TERMS FU AND FV ARE FIRST COMPUTED IN P1.
!>         THEY ARE THEN EXTENDED TO QUASI-BUBBLE IF REQUIRED.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC2D, INTERFACE_TELEMAC2D, M_COUPLING_ESTEL3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, CORIOL, COSLAT, COUROU, DSCE, FAIR, FCOR, FU, FV, GRAV, HN, HWIND, ISCE, LT, MARDAT, MAREE, MARTIM, MASKEL, MESH, MSK, NORD, NPTH, NREJET, NREJEU, NVARCL, OPTSOU, PHI0, SINLAT, SMH, SPHERI, T1, UN, UNSV2D, VARCL, VARCLA, VENT, VN, WINDX, WINDY, YASMH
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::T2D_FILES T2D_FILES@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ATH, DEJALU, ERR, I, IELM1, IELMH, IELMU, IR, N, NOMX, NOMY, NP, NPOIN, OKC, OKX, OKY, PI, W, WD, WROT
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PROSOU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ALLVEC(), CHGDIS(), CPSTVC(), FIND_IN_SEL(), INFILTRATION_GET(), MARAST(), OS(), PARCOM(), PLANTE(), VECTOR(), VUSCE(), VVSCE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D(), THOMPS()

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
!> </td><td> 08/04/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TIME
!>    </td></tr>
!>          <tr><td>CORIOL
!></td><td>--></td><td>PRISE EN COMPTE DES EFFORTS DE CORIOLIS .
!>    </td></tr>
!>          <tr><td>COSLAT
!></td><td>--></td><td>COS DE LA LATITUDE EN COORDONNEES SPHERIQUES
!>    </td></tr>
!>          <tr><td>COUROU
!></td><td>--></td><td>IF YES, WAVE DRIVEN CURRENTS TAKEN INTO ACCOUNT
!>    </td></tr>
!>          <tr><td>FAIR
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT DE L'AIR.
!>    </td></tr>
!>          <tr><td>FCOR
!></td><td>--></td><td>PARAMETRE DE CORIOLIS.
!>    </td></tr>
!>          <tr><td>FU
!></td><td><--</td><td>TERMES SOURCE TRAITE EN P1 SUR L'EQUATION EN U
!>    </td></tr>
!>          <tr><td>FV
!></td><td><--</td><td>TERMES SOURCE TRAITE EN P1 SUR L'EQUATION EN V
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>PESANTEUR.
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEURS D'EAU A TN .
!>    </td></tr>
!>          <tr><td>HWIND
!></td><td>--></td><td>MINIMUM DEPTH FOR TAKING WIND INTO ACCOUNT
!>    </td></tr>
!>          <tr><td>ISCE,DSCE
!></td><td>--></td><td>POINTS SOURCES, DEBITS DE LA SOURCE
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>TIME STEP NUMBER
!>    </td></tr>
!>          <tr><td>MARDAT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MAREE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MARTIM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MAILLAGE
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NORD
!></td><td>--></td><td>DIRECTION DU NORD EN DEGRES PAR RAPPORT A
!>                  L'AXE DES Y (SENS TRIGONOMETRIQUE)
!>    </td></tr>
!>          <tr><td>NPTH
!></td><td>--></td><td>RECORD NUMBER IN THE WAVE CURRENTS FILE
!>    </td></tr>
!>          <tr><td>NREJET
!></td><td>--></td><td>NOMBRE DE POINTS SOURCES
!>    </td></tr>
!>          <tr><td>NREJEU
!></td><td>--></td><td>NOMBRE DE VITESSES DES SOURCES DONNEES
!>                  SI NREJEU=0 ON CONSIDERE QUE LA VITESSE DES
!>                  SOURCES EST EGALE A CELLE DU COURANT.
!>    </td></tr>
!>          <tr><td>NVARCL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTSOU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PHI0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SINLAT
!></td><td>--></td><td>SIN DE LA LATITUDE EN COORDONNEES SPHERIQUES
!>    </td></tr>
!>          <tr><td>SMH
!></td><td><--</td><td>TERME SOURCE DANS L'EQUATION DE CONTINUITE
!>    </td></tr>
!>          <tr><td>SPHERI
!></td><td>--></td><td>=TRUE : COORDONNEES SPHERIQUES
!>    </td></tr>
!>          <tr><td>T1
!></td><td>--></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>UN , VN
!></td><td>--></td><td>COMPOSANTES DES VECTEURS VITESSES A TN.
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VARCL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VARCLA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VENT
!></td><td>--></td><td>PRISE EN COMPTE DES EFFORTS DUS AU VENT .
!>    </td></tr>
!>          <tr><td>WINDX,Y
!></td><td>--></td><td>VITESSE DU VENT EN SURFACE.
!>    </td></tr>
!>          <tr><td>WINDY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YASMH
!></td><td><-></td><td>=TRUE SI SMH NON NUL. AJOUTE UN TERME SOURCE
!>                  IMPLICITE DANS L'EQUATION DU TRACEUR
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PROSOU
     &(FU,FV,SMH,    UN,VN,HN,GRAV,NORD,
     & FAIR,WINDX,WINDY,VENT,HWIND,CORIOL,FCOR,
     & SPHERI,YASMH,COSLAT,SINLAT,AT,LT,
     & NREJET,NREJEU,DSCE,ISCE,T1,MESH,MSK,MASKEL,
     & MAREE,MARDAT,MARTIM,PHI0,OPTSOU,COUROU,NPTH,VARCL,NVARCL,VARCLA,
     & UNSV2D,FXWAVE,FYWAVE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TIME
C| CORIOL         |-->| PRISE EN COMPTE DES EFFORTS DE CORIOLIS .
C| COSLAT         |-->| COS DE LA LATITUDE EN COORDONNEES SPHERIQUES
C| COUROU         |-->| IF YES, WAVE DRIVEN CURRENTS TAKEN INTO ACCOUNT
C| FAIR           |-->| COEFFICIENT DE FROTTEMENT DE L'AIR.
C| FCOR           |-->| PARAMETRE DE CORIOLIS.
C| FU             |<--| TERMES SOURCE TRAITE EN P1 SUR L'EQUATION EN U
C| FV             |<--| TERMES SOURCE TRAITE EN P1 SUR L'EQUATION EN V
C| GRAV           |-->| PESANTEUR.
C| HN             |-->| HAUTEURS D'EAU A TN .
C| HWIND          |-->| MINIMUM DEPTH FOR TAKING WIND INTO ACCOUNT
C| ISCE,DSCE      |-->| POINTS SOURCES, DEBITS DE LA SOURCE
C| LT             |-->| TIME STEP NUMBER
C| MARDAT         |---| 
C| MAREE          |---| 
C| MARTIM         |---| 
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MESH           |-->| MAILLAGE
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NORD           |-->| DIRECTION DU NORD EN DEGRES PAR RAPPORT A
C|                |   | L'AXE DES Y (SENS TRIGONOMETRIQUE)
C| NPTH           |-->| RECORD NUMBER IN THE WAVE CURRENTS FILE
C| NREJET         |-->| NOMBRE DE POINTS SOURCES
C| NREJEU         |-->| NOMBRE DE VITESSES DES SOURCES DONNEES
C|                |   | SI NREJEU=0 ON CONSIDERE QUE LA VITESSE DES
C|                |   | SOURCES EST EGALE A CELLE DU COURANT.
C| NVARCL         |---| 
C| OPTSOU         |---| 
C| PHI0           |---| 
C| SINLAT         |-->| SIN DE LA LATITUDE EN COORDONNEES SPHERIQUES
C| SMH            |<--| TERME SOURCE DANS L'EQUATION DE CONTINUITE
C| SPHERI         |-->| =TRUE : COORDONNEES SPHERIQUES
C| T1             |-->| TABLEAU DE TRAVAIL
C| UN , VN        |-->| COMPOSANTES DES VECTEURS VITESSES A TN.
C| UNSV2D         |---| 
C| VARCL          |---| 
C| VARCLA         |---| 
C| VENT           |-->| PRISE EN COMPTE DES EFFORTS DUS AU VENT .
C| WINDX,Y        |-->| VITESSE DU VENT EN SURFACE.
C| WINDY          |---| 
C| YASMH          |<->| =TRUE SI SMH NON NUL. AJOUTE UN TERME SOURCE
C|                |   | IMPLICITE DANS L'EQUATION DU TRACEUR
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D, ONLY : T2D_FILES,T2DBI1
      USE INTERFACE_TELEMAC2D, EX_PROSOU => PROSOU
C --- JP RENAUD START ---
      USE M_COUPLING_ESTEL3D
C --- JP RENAUD END ---
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     WORKING ARRAYS
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: T1
C
C-----------------------------------------------------------------------
C
C     VECTORS
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FU,FV,SMH,FXWAVE,FYWAVE
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,UN,VN,HN,UNSV2D
      TYPE(BIEF_OBJ), INTENT(IN)    :: WINDX,WINDY,COSLAT,SINLAT
C
C-----------------------------------------------------------------------
C
C     MESH STRUCTURE
C
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C-----------------------------------------------------------------------
C
      INTEGER, INTENT(IN)           :: NVARCL,LT,NREJET,NREJEU,OPTSOU
      INTEGER, INTENT(IN)           :: NPTH
      INTEGER, INTENT(IN)           :: MARDAT(3),MARTIM(3),ISCE(NREJET)
      DOUBLE PRECISION, INTENT(IN)  :: HWIND,AT,FAIR,FCOR,DSCE(NREJET)
      DOUBLE PRECISION, INTENT(IN)  :: GRAV,NORD,PHI0
      CHARACTER(LEN=32), INTENT(IN) :: VARCLA(NVARCL)
      LOGICAL, INTENT(IN)           :: VENT,MAREE,CORIOL,SPHERI,MSK
      LOGICAL, INTENT(IN)           :: COUROU
      LOGICAL, INTENT(INOUT)        :: YASMH
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VARCL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER N,I,IELMU,IELMH,IELM1,NPOIN,IR,ERR,NP
C
      DOUBLE PRECISION PI,WROT,WD,ATH
C
      CHARACTER*16 NOMX,NOMY
      LOGICAL DEJALU,OKX,OKY,OKC
      DATA DEJALU /.FALSE./
      REAL, ALLOCATABLE :: W(:)
      SAVE W
C
      INTRINSIC SQRT,MAX,ACOS
C
C-----------------------------------------------------------------------
C  EXTRACTS X COORDINATES, NUMBER OF POINTS P1
C                          AND P1 ELEMENT OF THE MESH
C-----------------------------------------------------------------------
C
      IELM1 = MESH%X%ELM
      NPOIN = MESH%NPOIN
C
C-----------------------------------------------------------------------
C  INITIALISES
C-----------------------------------------------------------------------
C
      CALL CPSTVC(UN,FU)
      CALL CPSTVC(VN,FV)
      CALL OS( 'X=0     ' , X=FU )
      CALL OS( 'X=0     ' , X=FV )
C
C-----------------------------------------------------------------------
C
C  COMPUTATION WITH WIND
C  ----------------
C
C                               1                         2     2
C              FU           =  --- * F    * U    * SQRT( U   + V    )
C                VENT           H     AIR    AIR          AIR   AIR
C
C
C                               1                         2     2
C              FV           =  --- * F    * V    * SQRT( U   + V    )
C                VENT           H     AIR    AIR          AIR   AIR
C
C
      IF(VENT) THEN
C
C  TEMPORARY TREATMENT OF TIDAL FLATS
C  THE WIND EFFECT IS ONLY CONSIDERED IF THE WATER DEPTH IS
C  GREATER THAN 1 M.
C
C  ASSUMES HERE THAT THE WIND IS GIVEN IN P1
C
        DO 10 N=1,NPOIN
          IF (HN%R(N).GT.HWIND) THEN
            WD = SQRT( WINDX%R(N)**2 + WINDY%R(N)**2 )
            FU%R(N) = FU%R(N) + FAIR * WINDX%R(N) * WD / HN%R(N)
            FV%R(N) = FV%R(N) + FAIR * WINDY%R(N) * WD / HN%R(N)
          ENDIF
10      CONTINUE
C
      ENDIF
C
C***********************************************************************
C
C     * WITH CORIOLIS FORCE
C       --------------------------
C
C                FU           =  + FCOR * V
C                  CORIOLIS
C
C                FV           =  - FCOR * U
C                  CORIOLIS
C
      IF(CORIOL) THEN
C
      PI = ACOS(-1.D0)
C
        IF(SPHERI) THEN
C
          WROT = 2 * PI / 86164.D0
          DO 20 I=1,NPOIN
C           FORMULATION INDEPENDENT OF THE DIRECTION OF NORTH
            FU%R(I) = FU%R(I) + VN%R(I) * 2 * WROT * SINLAT%R(I)
            FV%R(I) = FV%R(I) - UN%R(I) * 2 * WROT * SINLAT%R(I)
20        CONTINUE
C
C         TAKES THE TIDAL FORCE INTO ACCOUNT
C
          IF(MAREE) THEN
            CALL MARAST(MARDAT,MARTIM,PHI0,NPOIN,AT,
     &                  FU%R,FV%R,MESH%X%R,SINLAT%R,COSLAT%R,GRAV)
          ENDIF
C
          IF(LT.EQ.1) THEN
            IF(LNG.EQ.1) WRITE(LU,11)
            IF(LNG.EQ.2) WRITE(LU,12)
          ENDIF
11        FORMAT(1X,'PROSOU : EN COORDONNEES SHERIQUES, LE',/,
     &           1X,'         COEFFICIENT DE CORIOLIS EST',/,
     &           1X,'         CALCULE EN FONCTION DE LA LATITUDE.',/,
     &           1X,'         LE MOT-CLE ''COEFFICIENT DE CORIOLIS''',/,
     &           1X,'         N''EST DONC PAS PRIS EN COMPTE.')
12        FORMAT(1X,'PROSOU : IN SPHERICAL COORDINATES, THE CORIOLIS',/,
     &           1X,'         PARAMETER DEPENDS ON THE LATITUDE.',/,
     &           1X,'         THE KEY WORD ''CORIOLIS COEFFICIENT''',/,
     &           1X,'         IS CONSEQUENTLY IGNORED.')
C
        ELSE
C
          CALL OS( 'X=X+CY  ' , FU , VN , VN ,  FCOR )
          CALL OS( 'X=X+CY  ' , FV , UN , UN , -FCOR )
C
          IF(LT.EQ.1) THEN
            IF(LNG.EQ.1) WRITE(LU,21)
            IF(LNG.EQ.2) WRITE(LU,22)
          ENDIF
21        FORMAT(1X,'PROSOU : EN COORDONNEES CARTESIENNES, LE',/,
     &           1X,'         COEFFICIENT DE CORIOLIS EST LU DANS LE',/,
     &           1X,'         FICHIER DES PARAMETRES ET CORRESPOND',/,
     &           1X,'         AU MOT-CLE ''COEFFICIENT DE CORIOLIS''',/,
     &           1X,'         IL EST ALORS CONSTANT EN ESPACE')
22        FORMAT(1X,'PROSOU : IN CARTESIAN COORDINATES, THE CORIOLIS',/,
     &           1X,'         PARAMETER IS READ IN THE STEERING FILE',/,
     &           1X,'         IT IS THE KEY WORD ''CORIOLIS',/,
     &           1X,'         COEFFICIENT'', IT IS UNIFORM IN SPACE')
C
        ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  THE SECOND MEMBERS ARE PROPERLY DISCRETISED
C
      IELMU=UN%ELM
C
      IF(IELMU.NE.IELM1) THEN
        CALL CHGDIS(FU,IELM1,IELMU,MESH)
        CALL CHGDIS(FV,IELM1,IELMU,MESH)
      ENDIF
C
C-----------------------------------------------------------------------
C
      IELMH=HN%ELM
      CALL CPSTVC(HN,SMH)
      CALL OS( 'X=0     ' , X=SMH )
C
      YASMH = .FALSE.
C
      IF(NREJET.NE.0) THEN
C
C  YASMH BECOMES TRUE
C
      YASMH = .TRUE.
C
C  SOURCE TERMS IN THE CONTINUITY EQUATION
C           AND IN THE MOMENTUM EQUATION:
C
C  BEWARE, SMH IS ALSO USED FOR TRACER
C
C     COMPUTES THE VOLUME OF THE BASES
C     HN HERE IS A DUMMY STRUCTURE
      CALL VECTOR(T1,'=','MASBAS          ',IELMH,
     &            1.D0,HN,HN,HN,HN,HN,HN,MESH,MSK,MASKEL)
C
      IF(NCSIZE.GT.1) CALL PARCOM(T1,2,MESH)
C
      DO I = 1 , NREJET
C
        IR = ISCE(I)
C       THE TEST IS USEFUL IN PARALLEL MODE, WHEN THE POINT SOURCE
C       IS NOT IN THE SUB-DOMAIN
        IF(IR.GT.0) THEN
         IF(OPTSOU.EQ.1) THEN
C          "NORMAL" VERSION
           SMH%R(IR)=SMH%R(IR)+DSCE(I)/T1%R(IR)
         ELSE
C          "DIRAC" VERSION
           SMH%R(IR) = SMH%R(IR)+DSCE(I)
         ENDIF
        ENDIF
C
      ENDDO
C
C-----------------------------------------------------------------------
C
C EXPLICIT TREATMENT OF MOMENTUM CONTRIBUTIONS TO THE SOURCES
C
      IF(NREJEU.GT.0) THEN
C
      DO I = 1 , NREJEU
C
        IR = ISCE(I)
C       THE TEST IS USEFUL IN PARALLEL MODE, WHEN THE POINT SOURCE
C       IS NOT IN THE SUB-DOMAIN
        IF(IR.GT.0) THEN
C       MOMENTUM ADDED BY THE SOURCE
C      -MOMENTUM TAKEN BY THE SOURCE
        FU%R(IR)=FU%R(IR) + (VUSCE(AT,I)-UN%R(IR))*
     &  DSCE(I)/(T1%R(IR)*MAX(HN%R(IR),0.1D0))
        FV%R(IR)=FV%R(IR) + (VVSCE(AT,I)-VN%R(IR))*
     &  DSCE(I)/(T1%R(IR)*MAX(HN%R(IR),0.1D0))
        ENDIF
C
      ENDDO
C
      ENDIF
C
      ENDIF
C
C***********************************************************************
C
C     * WITH WAVE DRIVEN CURRENTS
C       -------------------------------------
C
C                FU        =  FXWAVE
C                  COUROU
C
C                FV        =  FYWAVE
C                  COUROU
C
C       FXWAVE AND FYWAVE ARE TAKEN IN A RESULTS FILE FROM
C       ARTEMIS OR TOMAWAC
C
C       BEWARE   : 1. MESHES MUST BE THE SAME
C       ---------
C
C                  2. STATIONARY FORCING
C
      IF(COUROU) THEN
C
C        WITH NO COUPLING, TAKING THE WAVE STRESSES ONCE FOR ALL
C        IN A BINARY DATA FILE
C
         IF(.NOT.DEJALU.AND..NOT.INCLUS(COUPLING,'TOMAWAC')) THEN
C
            ALLOCATE(W(NPOIN),STAT=ERR)
            IF(ERR.NE.0) THEN
              IF(LNG.EQ.1) THEN
                WRITE(LU,*) 'ERREUR D''ALLOCATION DE W DANS PROSOU'
              ENDIF
              IF(LNG.EQ.2) THEN
                WRITE(LU,*) 'MEMORY ALLOCATION ERROR OF W IN PROSOU'
              ENDIF
            ENDIF
C
C           NBI1 : BINARY DATA FILE 1
            NOMX='FORCE FX        '
            NOMY='FORCE FY        '
            CALL FIND_IN_SEL(FXWAVE,NOMX,T2D_FILES(T2DBI1)%LU,
     &                       W,OKX,NPTH,NP,ATH)
            CALL FIND_IN_SEL(FYWAVE,NOMY,T2D_FILES(T2DBI1)%LU,
     &                       W,OKY,NPTH,NP,ATH)
            IF(.NOT.OKX.OR..NOT.OKY) THEN
C             SECOND TRY (OLD VERSIONS OF ARTEMIS OR TOMAWAC)
              NOMX='FORCE_FX        '
              NOMY='FORCE_FY        '
              CALL FIND_IN_SEL(FXWAVE,NOMX,T2D_FILES(T2DBI1)%LU,
     &                         W,OKX,NPTH,NP,ATH)
              CALL FIND_IN_SEL(FYWAVE,NOMY,T2D_FILES(T2DBI1)%LU,
     &                         W,OKY,NPTH,NP,ATH)
            ENDIF
C           CLANDESTINE VARIABLES FROM TOMAWAC TO SISYPHE
            IF(NVARCL.GT.0) THEN
              DO I=1,NVARCL
              CALL FIND_IN_SEL(VARCL%ADR(I)%P,VARCLA(I)(1:16),
     &                         T2D_FILES(T2DBI1)%LU,
     &                         W,OKC,NPTH,NP,ATH)
              IF(.NOT.OKC) THEN
                IF(LNG.EQ.1) WRITE(LU,7) VARCLA(I)(1:16)
                IF(LNG.EQ.2) WRITE(LU,8) VARCLA(I)(1:16)
7             FORMAT(1X,'PROSOU : VARIABLE CLANDESTINE :',/,1X,A16,/,1X,
     &                  '         NON TROUVEE',/,1X,
     &                  '         DANS LE FICHIER DE HOULE')
8             FORMAT(1X,'PROSOU : CLANDESTINE VARIABLE:',/,1X,A16,/,1X,
     &                  '         NOT FOUND',/,1X,
     &                  '         IN THE WAVE RESULTS FILE')
              CALL PLANTE(1)
              STOP
              ENDIF
              ENDDO
            ENDIF
C
            IF(.NOT.OKX.OR..NOT.OKY) THEN
              IF(LNG.EQ.1) WRITE(LU,5)
              IF(LNG.EQ.2) WRITE(LU,6)
5             FORMAT(1X,'PROSOU : FORCE FX OU FY NON TROUVES',/,1X,
     &                  '         DANS LE FICHIER DE HOULE')
6             FORMAT(1X,'PROSOU: FORCE FX OR FY NOT FOUND',/,1X,
     &                  '         IN THE WAVE RESULTS FILE')
              CALL PLANTE(1)
              STOP
            ENDIF
            IF(NP.NE.NPOIN) THEN
              IF(LNG.EQ.1) WRITE(LU,95)
              IF(LNG.EQ.2) WRITE(LU,96)
 95           FORMAT(1X,'PROSOU : SIMULATION DES COURANTS DE HOULE.',/,
     &               1X,'LES MAILLAGES HOULE ET COURANTS SONT ',/,
     &               1X,'DIFFERENTS : PAS POSSIBLE POUR LE MOMENT.')
 96           FORMAT(1X,'PROSOU: WAVE DRIVEN CURRENTS MODELLING.',/,
     &               1X,'WAVE AND CURRENT MODELS MESHES ARE ',/,
     &               1X,'DIFFERENT : NOT POSSIBLE AT THE MOMENT.')
C
              CALL PLANTE(1)
              STOP
            ENDIF
C           WRITES OUT TO THE LISTING
            IF(LNG.EQ.1) WRITE(LU,115) ATH
            IF(LNG.EQ.2) WRITE(LU,116) ATH
115         FORMAT(1X,/,1X,'PROSOU : COURANTS DE HOULE',/,
     &                  1X,'         LECTURE AU TEMPS ',F10.3,/)
116         FORMAT(1X,/,1X,'PROSOU: WAVE DRIVEN CURRENTS MODELLING',/,
     &                  1X,'         READING FILE AT TIME ',F10.3,/)
            IF(IELMU.NE.IELM1) THEN
              CALL CHGDIS(FXWAVE,IELM1,IELMU,MESH)
              CALL CHGDIS(FYWAVE,IELM1,IELMU,MESH)
            ENDIF
            DEJALU = .TRUE.
C
         ENDIF
C
C        ADDS INTO FU AND FV
C
         IF(INCLUS(COUPLING,'TOMAWAC')) THEN
           IF(IELMU.NE.IELM1) THEN
             CALL CHGDIS(FXWAVE,IELM1,IELMU,MESH)
             CALL CHGDIS(FYWAVE,IELM1,IELMU,MESH)
           ENDIF
         ENDIF
         CALL OS('X=X+Y   ',X=FU,Y=FXWAVE)
         CALL OS('X=X+Y   ',X=FV,Y=FYWAVE)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C
C  TAKES SEEPAGE IN THE SOIL INTO ACCOUNT
C  COMMUNICATES WITH ESTEL-3D
C
C     GETS SOURCE TERM FROM ESTEL-3D TO ACCOUNT FOR SEEPAGE
C     CALLS THE INFILTRATION ROUTINE
C
      CALL INFILTRATION_GET(SMH%R,UNSV2D%R,YASMH)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
