!                    *****************
                     SUBROUTINE CARACT
!                    *****************
!
     & ( U , UTILD , UCONV , VCONV , WCONV , X , Y , ZSTAR ,
     &   T1 , T2 , ZCONV , DX , DY , DZ , Z , SHP , SHZ , SURDET ,
     &   DT , IKLE , IFABOR , ELT , ETA , ITRAV1 , ITRAV2 , IELM ,IELMU,
     &   NELEM , NELMAX , NOMB , NPOIN , NPOIN2 , NDP , NPLAN ,
     &   LV , MSK , MASKEL , MESH , FAC , TEST , STEST,INITLOC)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE ADVECTION EQUATIONS USING THE CHARACTERISTIC
!+                METHOD, FOR A SET OF FUNCTIONS.
!
!note     U AND UTILD CAN NOW BE VECTORS, IN WHICH CASE NOMB WILL BE
!+         EQUAL TO 1.
!
!warning  COMPATIBILITY WITH 3.0 IS NO LONGER ENSURED
!+            BECAUSE OF CALL TO PARCOM
!
!history  J-M HERVOUET (LNHE)     ; ALGIANE FROEHLY (MATMECA)
!+        27/08/08
!+        V5P9
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| PAS DE TEMPS
!| DX,DY,DZ       |---| STOCKAGE DES SOUS-PAS .
!| ELT            |---| NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
!|                |   | CARACTERISTIQUES.
!| ETA            |---| NUMEROS DES ETAGES AU PIED DES COURBES
!|                |   | CARACTERISTIQUES (POUR TEL3D).
!| FAC            |---|
!| IELM           |-->| TYPE D'ELEMENT : 11 : TRIANGLE P1
!|                |   | 21 : QUADRANGLE P1
!|                |   | 41 : PRISME DE TEL3D
!| IELMU          |---|
!| IFABOR         |-->| NUMEROS DES ELEMENTS VOISINS (ATTENTION, POUR
!|                |   | TEL3D, IFABOR EST LE TABLEAU IBOR DE MITRID).
!| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DES ELEMENTS 2D.
!| INITLOC        |---|
!| ITRAV1         |---| TABLEAU DE TRAVAIL ENTIER.
!| ITRAV2         |---| TABLEAU DE TRAVAIL ENTIER.
!| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION.
!| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
!|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE.
!| MESH           |---|
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
!| NDP            |-->| NOMBRE DE POINTS PAR ELEMENT 2D.
!| NELEM          |-->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE 2D.
!| NELMAX         |-->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
!| NOMB           |-->| NOMBRE DE VARIABLES A CONVECTER.
!| NPLAN          |-->| NOMBRE DE PLAN SUIVANT Z (POUR TEL3D).
!| NPOIN          |-->| NOMBRE TOTAL DE POINTS DU MAILLAGE.
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D (POUR TEL3D).
!| SHP            |---| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
!|                |   | COURBES CARACTERISTIQUES.
!| SHZ            |---| COORDONNEES BARYCENTRIQUES SUIVANT Z AU PIED
!|                |   | DES COURBES CARACTERISTIQUES (POUR TEL3D)
!| STEST          |---|
!| SURDET         |-->| 1/DETERMINANT POUR LES ELEMENTS 2D.
!| T1             |---|
!| T2             |---|
!| TEST           |---|
!| U              |-->| VARIABLES A L'ETAPE N .
!| UCONV,VCONV    |-->| COMPOSANTES DES VITESSES DU CONVECTEUR.
!| UTILD          |<--| VARIABLES APRES LA CONVECTION .
!| WCONV          |---|
!| X,Y,ZSTAR      |-->| COORDONNEES DU MAILLAGE .
!| Z              |-->| COTE DANS LE MAILLAGE REEL (POUR TEL3D) .
!| ZCONV          |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CARACT => CARACT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NELEM,NELMAX,NOMB,NPOIN,NPOIN2
      INTEGER, INTENT(IN)             :: NDP,NPLAN,IELM,IELMU,LV
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: U
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: UTILD
      DOUBLE PRECISION, INTENT(IN)    :: UCONV(*),VCONV(*),WCONV(*)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN2,NPLAN),ZSTAR(NPLAN)
      DOUBLE PRECISION, INTENT(OUT)   :: ZCONV(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(OUT)   :: DX(*),DY(*),DZ(NPOIN)
      DOUBLE PRECISION, INTENT(OUT)   :: SHP(NDP,*),SHZ(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM)
      DOUBLE PRECISION, INTENT(IN)    :: DT
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,*)
      INTEGER, INTENT(IN)             :: IFABOR(NELMAX,*)
      INTEGER, INTENT(OUT)            :: ELT(*),ETA(*)
      INTEGER, INTENT(OUT)            :: ITRAV1(*),ITRAV2(*)
      LOGICAL, INTENT(IN)             :: MSK,INITLOC
      DOUBLE PRECISION, INTENT(OUT)   :: TEST(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FAC(NPOIN)
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: STEST
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: T1,T2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NRK,I,NPOINT,NPOINT2,IPOIN,IELMI
      DOUBLE PRECISION C
      LOGICAL QUAD
!
!***********************************************************************
!
! NUMBER OF RUNGE-KUTTA SUB-STEPS BY CROSSED ELEMENT
!
      QUAD = .FALSE.
      NRK = 3
      CALL OV( 'X=Y     ' , T1%R , X , Z , C , NPOIN )
      CALL OV( 'X=Y     ' , T2%R , Y , Z , C , NPOIN )
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1) CALL OV('X=C     ',TEST,Y,Z,1.D0,NPOIN)
!
!-----------------------------------------------------------------------
!
      IF(IELM.EQ.11) THEN
!
! IDENTIFIES IF ONE OF THE ELEMENTS TO ADVECT IS OF P2 DISCRETISATION
!
        DO I=1,U%N
          IF(U%ADR(I)%P%ELM.EQ.13) THEN
            QUAD = .TRUE.
          ENDIF
        ENDDO
!
        IF(QUAD) THEN
          CALL CHGDIS(T1,IELM,13,MESH)
          CALL CHGDIS(T2,IELM,13,MESH)
        ENDIF
!
!-----------------------------------------------------------------------
!
        IF(.NOT.QUAD) THEN
!
!    P1 TRIANGLES
!    ============
!
!       FILLS SHP AND ELT (OPTIMISED)
!
        NPOINT  = NPOIN
        NPOINT2 = NPOIN
        IF(INITLOC) THEN
          CALL GTSH11(SHP,ELT,IKLE,NPOINT,NELEM,NELMAX,MSK,MASKEL)
        ENDIF
!
!       CALLS THE SUBROUTINE TRACING THE CHARATERISTIC CURVES BACK IN TIME
!
        IF(NCSIZE.GT.1) CALL OV('X=C     ',TEST,Y,Z,1.D0,NPOINT)
        CALL CHAR11(UCONV,VCONV,DT,NRK,X,Y,IKLE,
     &              IFABOR,T1%R,T2%R,DX,DY,SHP,ELT,ITRAV1,
     &              NPOINT,NPOINT2,NELEM,NELMAX,SURDET,-1,TEST)
!
!-----------------------------------------------------------------------
!
        ELSEIF(QUAD) THEN
!
!         P2 TRIANGLES FOR ONE OF THE ADVECTED VARIABLES
!         ===============================================
!
!         FILLS SHP AND ELT (OPTIMISED)
!
          NPOINT  = NPOIN+MESH%NSEG
          NPOINT2 = NPOIN+MESH%NSEG
!
!         CASE OF A QUADRATIC TRACER AND A LINEAR VELOCITY (NOT IMPLEMENTED)
!
          IF(IELMU.NE.13)THEN
            IF(LNG.EQ.1) WRITE(LU,21)
            IF(LNG.EQ.2) WRITE(LU,22)
            CALL PLANTE(1)
            STOP
          ENDIF
!
          IF(INITLOC) THEN
            CALL GTSH13(SHP,ELT,IKLE,NPOINT2,NELEM ,
     &                   NELMAX  , MSK , MASKEL )
          ENDIF
!
!         CALLS THE SUBROUTINE TRACING THE CHARACTERISTIC CURVES BACK IN TIME
!
!         CALL CHAR13( UCONV , VCONV , DT    , NRK , X , Y ,
          IF(NCSIZE.GT.1) CALL OV('X=C     ',TEST,Y,Z,1.D0,NPOINT)
          CALL CHAR11( UCONV , VCONV , DT    , NRK , X , Y ,
     &                 IKLE    , IFABOR  ,
     &                 T1%R    , T2%R    , DX    , DY ,SHP,ELT ,ITRAV1,
     &                 NPOINT  , NPOINT2 , NELEM ,
     &                 NELMAX  , SURDET  , -1    , TEST)
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(IELM.EQ.41) THEN
!
!    TELEMAC-3D PRISMS
!    =====================
!
        NPOINT = NPOIN
        NPOINT2 = NPOIN2
        DO I = 1,NPLAN
          CALL OV('X=C     ' ,ZCONV(1,I),Y,Z,ZSTAR(I),NPOIN2)
        ENDDO
!
!      FILLS SHP AND ELT (OPTIMISED)
!
        IF(INITLOC) THEN
          CALL GTSH41( WCONV , SHP , SHZ ,
     &                 ELT , ETA , IKLE , NPOINT2 ,
     &                 NELEM , NPLAN , MSK , MASKEL )
        ENDIF
!
!      CALLS THE SUBROUTINE TRACING THE CHARATERISTIC CURVES BACK IN TIME
!
         IF(NCSIZE.GT.1) CALL OV('X=C     ',TEST,Y,Z,1.D0,NPOINT)
         CALL CHAR41( UCONV , VCONV , WCONV , DT , NRK ,
     &                X , Y , ZSTAR , Z , IKLE , IFABOR , T1%R , T2%R ,
     &                ZCONV , DX , DY , DZ , SHP , SHZ , ELT , ETA ,
     &                ITRAV1 , NPOINT ,
     &                NPOINT2 , NELEM , NPLAN , SURDET , -1 ,
     &                ITRAV2 , TEST )
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,11) IELM
        IF(LNG.EQ.2) WRITE(LU,12) IELM
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!  TEMPORARY
!  TEST = NUMBER OF SUB-DOMAINS TREATING A POINT
!
!-----------------------------------------------------------------------
!
!  ADJUSTS SHP ACCORDING TO WHAT WAS FOUND IN THE OTHER SUB-DOMAINS
!  (GIVEN BY TEST)
!
      IF(NCSIZE.GT.1) THEN
!
        IF(QUAD) THEN
          IF(LNG.EQ.1) WRITE(LU,19)
          IF(LNG.EQ.2) WRITE(LU,20)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        DO IPOIN = 1,NPOIN
!         A CHARACTERISTIC WHICH DID NOT STOP IN THE SUB-DOMAIN
!         WILL GIVE 0
          IF(TEST(IPOIN).LT.0.5D0) THEN
            SHP(1,IPOIN) = 0.D0
            SHP(2,IPOIN) = 0.D0
            SHP(3,IPOIN) = 0.D0
          ENDIF
        ENDDO
!
!       THIS IS JUST TO PRINT A WARNING
!
        CALL PARCOM(STEST,2,MESH)
        DO IPOIN = 1,NPOIN
          IF(TEST(IPOIN).LT.0.5D0) THEN
            IF(LNG.EQ.1) WRITE(LU,13) IPOIN
            IF(LNG.EQ.2) WRITE(LU,14) IPOIN
          ENDIF
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  INTERPOLATES AT THE FOOT OF THE CHARACTERISTICS (IF REQUIRED)
!
      IF(NOMB.GT.0) THEN
!
        IF(U%TYPE.EQ.2.AND.UTILD%TYPE.EQ.2) THEN
!
!         U AND UTILD ARE VECTORS (NOMB THEN EQUALS 1)
!
          CALL INTERP(U%R,UTILD%R,SHP,NDP,SHZ,ETA,ELT,
     &                NPOINT,NPOINT2,NPLAN,IELM,IKLE,NELMAX)
!
          IF(NCSIZE.GT.1) THEN
            IF(QUAD) THEN
              IF(LNG.EQ.1) WRITE(LU,19)
              IF(LNG.EQ.2) WRITE(LU,20)
              CALL PLANTE(1)
              STOP
            ENDIF
!           CHOOSES THE RESULT WITH MAXIMUM
!           ABSOLUTE VALUE
            CALL PARCOM(UTILD,1,MESH)
          ENDIF
!
        ELSEIF(U%TYPE.EQ.4.AND.UTILD%TYPE.EQ.4) THEN
!
!     U AND UTILD ARE VECTOR BLOCKS
!
         IF(U%N.LT.NOMB.OR.UTILD%N.LT.NOMB) THEN
            IF(LNG.EQ.1) WRITE(LU,15) U%N,UTILD%N
            IF(LNG.EQ.2) WRITE(LU,16) U%N,UTILD%N
            CALL PLANTE(1)
            STOP
          ENDIF
!
          DO 61 I=1,NOMB
!
! INTERPOLATES THE VELOCITIES IN PRESENCE OF A QUADRATIC VARIABLE
!
          IF(QUAD) THEN
            IELMI = U%ADR(I)%P%ELM
            NPOINT2=U%ADR(I)%P%DIM1
            CALL INTERP(U%ADR(I)%P%R,UTILD%ADR(I)%P%R,SHP,NDP,SHZ,
     &           ETA,ELT,NPOINT2,NPOINT2,NPLAN,IELMI,IKLE,NELMAX)
!
! INTERPOLATES THE VELOCITIES IN THE OTHER CASES
!
          ELSE
            CALL INTERP(U%ADR(I)%P%R,UTILD%ADR(I)%P%R,SHP,NDP,SHZ,
     &            ETA,ELT,NPOINT,NPOINT2,NPLAN,IELM,IKLE,NELMAX)
          ENDIF
!
          IF(NCSIZE.GT.1) THEN
            IF(QUAD)THEN
              IF(LNG.EQ.1) WRITE(LU,19)
              IF(LNG.EQ.2) WRITE(LU,20)
              CALL PLANTE(1)
              STOP
            ELSE
!             CHOOSES THE RESULT WITH MAXIMUM
!             ABSOLUTE VALUE
              CALL PARCOM(UTILD%ADR(I)%P,1,MESH)
            ENDIF
          ENDIF
!
61        CONTINUE
!
        ELSE
!
          IF(LNG.EQ.1) WRITE(LU,17) U%TYPE,UTILD%TYPE
          IF(LNG.EQ.2) WRITE(LU,18) U%TYPE,UTILD%TYPE
          CALL PLANTE(1)
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
11    FORMAT(1X,'CARACT : TYPE D''ELEMENT INCONNU : ',I6)
12    FORMAT(1X,'CARACT: UNKNOWN TYPE OF ELEMENT : ',I6)
!
13    FORMAT(1X,'CARACT : (PARALLELE) REMONTEE INCOMPLETE POUR : ',I6)
14    FORMAT(1X,'CARACT: (PARALLEL) INCOMPLETE PATH LINE FOR : ',I6)
!
15    FORMAT(1X,'CARACT : MAUVAIS BLOC DES VARIABLES : ',2I6)
16    FORMAT(1X,'CARACT: WRONG BLOCK OF VARIABLES : ',2I6)
!
17    FORMAT(1X,'CARACT : TYPE D''OBJET INCONNU : ',2I6)
18    FORMAT(1X,'CARACT: UNKNOWN TYPE OF OBJECT: ',2I6)
!
19    FORMAT(1X,'CARACT : PARALLELISME NON PREVU EN QUADRATIQUE')
20    FORMAT(1X,'CARACT : PARALLELISM NOT TREATED WITH QUADRATIC ')
!
21    FORMAT(1X,'CARACT : VITESSES LINEAIRES ET TRACEUR QUADRATIQUE')
22    FORMAT(1X,'CARACT : LINEAR VELOCITY AND QUADRATIC TRACER')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
