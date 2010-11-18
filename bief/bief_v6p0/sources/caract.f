C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE ADVECTION EQUATIONS USING THE CHARACTERISTIC
!>                METHOD, FOR A SET OF FUNCTIONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  U AND UTILD CAN NOW BE VECTORS, IN WHICH CASE NOMB WILL BE
!>         EQUAL TO 1.

!>  @warning  COMPATIBILITY WITH 3.0 IS NO LONGER ENSURED
!>            BECAUSE OF CALL TO PARCOM

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DT, DX, DY, DZ, ELT, ETA, FAC, IELM, IELMU, IFABOR, IKLE, INITLOC, ITRAV1, ITRAV2, LV, MASKEL, MESH, MSK, NDP, NELEM, NELMAX, NOMB, NPLAN, NPOIN, NPOIN2, SHP, SHZ, STEST, SURDET, T1, T2, TEST, U, UCONV, UTILD, VCONV, WCONV, X, Y, Z, ZCONV, ZSTAR
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, I, IELMI, IPOIN, NPOINT, NPOINT2, NRK, QUAD
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CARACT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CHAR11(), CHAR41(), CHGDIS(), GTSH11(), GTSH13(), GTSH41(), INTERP(), OV(), PARCOM(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CHARAC()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 27/08/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18; ALGIANE FROEHLY (MATMECA)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>DX,DY,DZ
!></td><td>---</td><td>STOCKAGE DES SOUS-PAS .
!>    </td></tr>
!>          <tr><td>ELT
!></td><td>---</td><td>NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
!>                  CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>ETA
!></td><td>---</td><td>NUMEROS DES ETAGES AU PIED DES COURBES
!>                  CARACTERISTIQUES (POUR TEL3D).
!>    </td></tr>
!>          <tr><td>FAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT : 11 : TRIANGLE P1
!>                  21 : QUADRANGLE P1
!>                  41 : PRISME DE TEL3D
!>    </td></tr>
!>          <tr><td>IELMU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>NUMEROS DES ELEMENTS VOISINS (ATTENTION, POUR
!>                  TEL3D, IFABOR EST LE TABLEAU IBOR DE MITRID).
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DES ELEMENTS 2D.
!>    </td></tr>
!>          <tr><td>INITLOC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ITRAV1
!></td><td>---</td><td>TABLEAU DE TRAVAIL ENTIER.
!>    </td></tr>
!>          <tr><td>ITRAV2
!></td><td>---</td><td>TABLEAU DE TRAVAIL ENTIER.
!>    </td></tr>
!>          <tr><td>LV
!></td><td>--></td><td>LONGUEUR DU VECTEUR POUR LA VECTORISATION.
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NDP
!></td><td>--></td><td>NOMBRE DE POINTS PAR ELEMENT 2D.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NOMB
!></td><td>--></td><td>NOMBRE DE VARIABLES A CONVECTER.
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLAN SUIVANT Z (POUR TEL3D).
!>    </td></tr>
!>          <tr><td>NPLINT
!></td><td>--></td><td>PLAN DE REFERENCE INTERMEDIAIRE (POUR TEL3D).
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE TOTAL DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D (POUR TEL3D).
!>    </td></tr>
!>          <tr><td>SHP
!></td><td>---</td><td>COORDONNEES BARYCENTRIQUES 2D AU PIED DES
!>                  COURBES CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>SHZ
!></td><td>---</td><td>COORDONNEES BARYCENTRIQUES SUIVANT Z AU PIED
!>                  DES COURBES CARACTERISTIQUES (POUR TEL3D)
!>    </td></tr>
!>          <tr><td>STEST
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SURDET
!></td><td>--></td><td>1/DETERMINANT POUR LES ELEMENTS 2D.
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TEST
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U
!></td><td>--></td><td>VARIABLES A L'ETAPE N .
!>    </td></tr>
!>          <tr><td>UCONV,VCONV
!></td><td>--></td><td>COMPOSANTES DES VITESSES DU CONVECTEUR.
!>    </td></tr>
!>          <tr><td>UTILD
!></td><td><--</td><td>VARIABLES APRES LA CONVECTION .
!>    </td></tr>
!>          <tr><td>WCONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X,Y,ZSTAR
!></td><td>--></td><td>COORDONNEES DU MAILLAGE .
!>    </td></tr>
!>          <tr><td>XCONV,YCONV
!></td><td>---</td><td>COORDONNEES AU PIED DES CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>COTE DANS LE MAILLAGE REEL (POUR TEL3D) .
!>    </td></tr>
!>          <tr><td>ZCONV
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CARACT
     & ( U , UTILD , UCONV , VCONV , WCONV , X , Y , ZSTAR ,
     &   T1 , T2 , ZCONV , DX , DY , DZ , Z , SHP , SHZ , SURDET ,
     &   DT , IKLE , IFABOR , ELT , ETA , ITRAV1 , ITRAV2 , IELM ,IELMU,
     &   NELEM , NELMAX , NOMB , NPOIN , NPOIN2 , NDP , NPLAN ,
     &   LV , MSK , MASKEL , MESH , FAC , TEST , STEST,INITLOC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DT             |-->| PAS DE TEMPS
C| DX,DY,DZ       |---| STOCKAGE DES SOUS-PAS .
C| ELT            |---| NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
C|                |   | CARACTERISTIQUES.
C| ETA            |---| NUMEROS DES ETAGES AU PIED DES COURBES
C|                |   | CARACTERISTIQUES (POUR TEL3D).
C| FAC            |---| 
C| IELM           |-->| TYPE D'ELEMENT : 11 : TRIANGLE P1
C|                |   | 21 : QUADRANGLE P1
C|                |   | 41 : PRISME DE TEL3D
C| IELMU          |---| 
C| IFABOR         |-->| NUMEROS DES ELEMENTS VOISINS (ATTENTION, POUR
C|                |   | TEL3D, IFABOR EST LE TABLEAU IBOR DE MITRID).
C| IKLE           |-->| NUMEROS GLOBAUX DES POINTS DES ELEMENTS 2D.
C| INITLOC        |---| 
C| ITRAV1         |---| TABLEAU DE TRAVAIL ENTIER.
C| ITRAV2         |---| TABLEAU DE TRAVAIL ENTIER.
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION.
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE.
C| MESH           |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NDP            |-->| NOMBRE DE POINTS PAR ELEMENT 2D.
C| NELEM          |-->| NOMBRE TOTAL D'ELEMENTS DANS LE MAILLAGE 2D.
C| NELMAX         |-->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
C| NOMB           |-->| NOMBRE DE VARIABLES A CONVECTER.
C| NPLAN          |-->| NOMBRE DE PLAN SUIVANT Z (POUR TEL3D).
C| NPLINT         |-->| PLAN DE REFERENCE INTERMEDIAIRE (POUR TEL3D).
C| NPOIN          |-->| NOMBRE TOTAL DE POINTS DU MAILLAGE.
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D (POUR TEL3D).
C| SHP            |---| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
C|                |   | COURBES CARACTERISTIQUES.
C| SHZ            |---| COORDONNEES BARYCENTRIQUES SUIVANT Z AU PIED
C|                |   | DES COURBES CARACTERISTIQUES (POUR TEL3D)
C| STEST          |---| 
C| SURDET         |-->| 1/DETERMINANT POUR LES ELEMENTS 2D.
C| T1             |---| 
C| T2             |---| 
C| TEST           |---| 
C| U             |-->| VARIABLES A L'ETAPE N .
C| UCONV,VCONV    |-->| COMPOSANTES DES VITESSES DU CONVECTEUR.
C| UTILD          |<--| VARIABLES APRES LA CONVECTION .
C| WCONV          |---| 
C| X,Y,ZSTAR      |-->| COORDONNEES DU MAILLAGE .
C| XCONV,YCONV    |---| COORDONNEES AU PIED DES CARACTERISTIQUES.
C| Z             |-->| COTE DANS LE MAILLAGE REEL (POUR TEL3D) .
C| ZCONV          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CARACT => CARACT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
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
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NRK,I,NPOINT,NPOINT2,IPOIN,IELMI
      DOUBLE PRECISION C
      LOGICAL QUAD
C
C***********************************************************************
C
C NUMBER OF RUNGE-KUTTA SUB-STEPS BY CROSSED ELEMENT
C
      QUAD = .FALSE.
      NRK = 3
      CALL OV( 'X=Y     ' , T1%R , X , Z , C , NPOIN )
      CALL OV( 'X=Y     ' , T2%R , Y , Z , C , NPOIN )
C
C-----------------------------------------------------------------------
C
      IF(NCSIZE.GT.1) CALL OV('X=C     ',TEST,Y,Z,1.D0,NPOIN)
C
C-----------------------------------------------------------------------
C
      IF(IELM.EQ.11) THEN
C
C IDENTIFIES IF ONE OF THE ELEMENTS TO ADVECT IS OF P2 DISCRETISATION
C
        DO I=1,U%N
          IF(U%ADR(I)%P%ELM.EQ.13) THEN
            QUAD = .TRUE.
          ENDIF
        ENDDO
C
        IF(QUAD) THEN
          CALL CHGDIS(T1,IELM,13,MESH)
          CALL CHGDIS(T2,IELM,13,MESH)
        ENDIF
C
C-----------------------------------------------------------------------
C
        IF(.NOT.QUAD) THEN
C
C    P1 TRIANGLES
C    ============
C
C       FILLS SHP AND ELT (OPTIMISED)
C
        NPOINT  = NPOIN
        NPOINT2 = NPOIN
        IF(INITLOC) THEN
          CALL GTSH11(UCONV,VCONV,X,Y,SHP,ELT,IKLE,
     &                ITRAV1,ITRAV2,NPOINT,NELEM,NELMAX,LV,MSK,MASKEL)
        ENDIF
C
C       CALLS THE SUBROUTINE TRACING THE CHARATERISTIC CURVES BACK IN TIME
C
        IF(NCSIZE.GT.1) CALL OV('X=C     ',TEST,Y,Z,1.D0,NPOINT)
        CALL CHAR11(UCONV,VCONV,DT,NRK,X,Y,IKLE,
     &              IFABOR,T1%R,T2%R,DX,DY,SHP,ELT,ITRAV1,
     &              NPOINT,NPOINT2,NELEM,NELMAX,SURDET,-1,TEST)
C
C-----------------------------------------------------------------------
C
        ELSEIF(QUAD) THEN
C
C         P2 TRIANGLES FOR ONE OF THE ADVECTED VARIABLES
C         ===============================================
C
C         FILLS SHP AND ELT (OPTIMISED)
C
          NPOINT  = NPOIN+MESH%NSEG
          NPOINT2 = NPOIN+MESH%NSEG
C
C         CASE OF A QUADRATIC TRACER AND A LINEAR VELOCITY (NOT IMPLEMENTED)
C
          IF(IELMU.NE.13)THEN
            IF(LNG.EQ.1) WRITE(LU,21)
            IF(LNG.EQ.2) WRITE(LU,22)
            CALL PLANTE(1)
            STOP
          ENDIF
C
          IF(INITLOC) THEN
            CALL GTSH13( UCONV , VCONV , X , Y, SHP , ELT , IKLE,
     &                   ITRAV1  , ITRAV2  , NPOINT2, NELEM ,
     &                   NELMAX  , LV ,MSK , MASKEL )
          ENDIF
C
C         CALLS THE SUBROUTINE TRACING THE CHARACTERISTIC CURVES BACK IN TIME
C
C         CALL CHAR13( UCONV , VCONV , DT    , NRK , X , Y ,
          IF(NCSIZE.GT.1) CALL OV('X=C     ',TEST,Y,Z,1.D0,NPOINT)
          CALL CHAR11( UCONV , VCONV , DT    , NRK , X , Y ,
     &                 IKLE    , IFABOR  ,
     &                 T1%R    , T2%R    , DX    , DY ,SHP,ELT ,ITRAV1,
     &                 NPOINT  , NPOINT2 , NELEM ,
     &                 NELMAX  , SURDET  , -1    , TEST)
C
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(IELM.EQ.41) THEN
C
C    TELEMAC-3D PRISMS
C    =====================
C
        NPOINT = NPOIN
        NPOINT2 = NPOIN2
        DO I = 1,NPLAN
          CALL OV('X=C     ' ,ZCONV(1,I),Y,Z,ZSTAR(I),NPOIN2)
        ENDDO
C
C      FILLS SHP AND ELT (OPTIMISED)
C
        IF(INITLOC) THEN
          CALL GTSH41( UCONV , VCONV , WCONV , X , Y , SHP , SHZ ,
     &                 ELT , ETA , IKLE , ITRAV1 , ITRAV2 , NPOINT2 ,
     &                 NELEM , NPLAN , LV , MSK , MASKEL )
        ENDIF
C
C      CALLS THE SUBROUTINE TRACING THE CHARATERISTIC CURVES BACK IN TIME
C
         IF(NCSIZE.GT.1) CALL OV('X=C     ',TEST,Y,Z,1.D0,NPOINT)
         CALL CHAR41( UCONV , VCONV , WCONV , DT , NRK ,
     &                X , Y , ZSTAR , Z , IKLE , IFABOR , T1%R , T2%R ,
     &                ZCONV , DX , DY , DZ , SHP , SHZ , ELT , ETA ,
     &                ITRAV1 , NPOINT ,
     &                NPOINT2 , NELEM , NPLAN , SURDET , -1 ,
     &                ITRAV2 , TEST )
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF(LNG.EQ.1) WRITE(LU,11) IELM
        IF(LNG.EQ.2) WRITE(LU,12) IELM
        CALL PLANTE(1)
        STOP
C
      ENDIF

C
C  TEMPORARY
C  TEST = NUMBER OF SUB-DOMAINS TREATING A POINT
C
C-----------------------------------------------------------------------
C
C  ADJUSTS SHP ACCORDING TO WHAT WAS FOUND IN THE OTHER SUB-DOMAINS
C  (GIVEN BY TEST)
C
      IF(NCSIZE.GT.1) THEN
C
        IF(QUAD) THEN
          IF(LNG.EQ.1) WRITE(LU,19)
          IF(LNG.EQ.2) WRITE(LU,20)
          CALL PLANTE(1)
          STOP
        ENDIF
C
        DO IPOIN = 1,NPOIN
C         A CHARACTERISTIC WHICH DID NOT STOP IN THE SUB-DOMAIN
C         WILL GIVE 0
          IF(TEST(IPOIN).LT.0.5D0) THEN
            SHP(1,IPOIN) = 0.D0
            SHP(2,IPOIN) = 0.D0
            SHP(3,IPOIN) = 0.D0
          ENDIF
        ENDDO
C
C       THIS IS JUST TO PRINT A WARNING
C
        CALL PARCOM(STEST,2,MESH)
        DO IPOIN = 1,NPOIN
          IF(TEST(IPOIN).LT.0.5D0) THEN
            IF(LNG.EQ.1) WRITE(LU,13) IPOIN
            IF(LNG.EQ.2) WRITE(LU,14) IPOIN
          ENDIF
        ENDDO
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  INTERPOLATES AT THE FOOT OF THE CHARACTERISTICS (IF REQUIRED)
C
      IF(NOMB.GT.0) THEN
C
        IF(U%TYPE.EQ.2.AND.UTILD%TYPE.EQ.2) THEN
C
C         U AND UTILD ARE VECTORS (NOMB THEN EQUALS 1)
C
          CALL INTERP(U%R,UTILD%R,SHP,NDP,SHZ,ETA,ELT,
     &                NPOINT,NPOINT2,NPLAN,IELM,IKLE,NELMAX)
C
          IF(NCSIZE.GT.1) THEN
            IF(QUAD) THEN
              IF(LNG.EQ.1) WRITE(LU,19)
              IF(LNG.EQ.2) WRITE(LU,20)
              CALL PLANTE(1)
              STOP
            ENDIF
C           CHOOSES THE RESULT WITH MAXIMUM
C           ABSOLUTE VALUE
            CALL PARCOM(UTILD,1,MESH)
          ENDIF
C
        ELSEIF(U%TYPE.EQ.4.AND.UTILD%TYPE.EQ.4) THEN
C
C     U AND UTILD ARE VECTOR BLOCKS
C
         IF(U%N.LT.NOMB.OR.UTILD%N.LT.NOMB) THEN
            IF(LNG.EQ.1) WRITE(LU,15) U%N,UTILD%N
            IF(LNG.EQ.2) WRITE(LU,16) U%N,UTILD%N
            CALL PLANTE(1)
            STOP
          ENDIF
C
          DO 61 I=1,NOMB
C
C INTERPOLATES THE VELOCITIES IN PRESENCE OF A QUADRATIC VARIABLE
C
          IF(QUAD) THEN
            IELMI = U%ADR(I)%P%ELM
            NPOINT2=U%ADR(I)%P%DIM1
            CALL INTERP(U%ADR(I)%P%R,UTILD%ADR(I)%P%R,SHP,NDP,SHZ,
     &           ETA,ELT,NPOINT2,NPOINT2,NPLAN,IELMI,IKLE,NELMAX)
C
C INTERPOLATES THE VELOCITIES IN THE OTHER CASES
C
          ELSE
            CALL INTERP(U%ADR(I)%P%R,UTILD%ADR(I)%P%R,SHP,NDP,SHZ,
     &            ETA,ELT,NPOINT,NPOINT2,NPLAN,IELM,IKLE,NELMAX)
          ENDIF
C
          IF(NCSIZE.GT.1) THEN
            IF(QUAD)THEN
              IF(LNG.EQ.1) WRITE(LU,19)
              IF(LNG.EQ.2) WRITE(LU,20)
              CALL PLANTE(1)
              STOP
            ELSE
C             CHOOSES THE RESULT WITH MAXIMUM
C             ABSOLUTE VALUE
              CALL PARCOM(UTILD%ADR(I)%P,1,MESH)
            ENDIF
          ENDIF
C
61        CONTINUE
C
        ELSE
C
          IF(LNG.EQ.1) WRITE(LU,17) U%TYPE,UTILD%TYPE
          IF(LNG.EQ.2) WRITE(LU,18) U%TYPE,UTILD%TYPE
          CALL PLANTE(1)
C
        ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
11    FORMAT(1X,'CARACT : TYPE D''ELEMENT INCONNU : ',I6)
12    FORMAT(1X,'CARACT: UNKNOWN TYPE OF ELEMENT : ',I6)
C
13    FORMAT(1X,'CARACT : (PARALLELE) REMONTEE INCOMPLETE POUR : ',I6)
14    FORMAT(1X,'CARACT: (PARALLEL) INCOMPLETE PATH LINE FOR : ',I6)
C
15    FORMAT(1X,'CARACT : MAUVAIS BLOC DES VARIABLES : ',2I6)
16    FORMAT(1X,'CARACT: WRONG BLOCK OF VARIABLES : ',2I6)
C
17    FORMAT(1X,'CARACT : TYPE D''OBJET INCONNU : ',2I6)
18    FORMAT(1X,'CARACT: UNKNOWN TYPE OF OBJECT: ',2I6)
C
19    FORMAT(1X,'CARACT : PARALLELISME NON PREVU EN QUADRATIQUE')
20    FORMAT(1X,'CARACT : PARALLELISM NOT TREATED WITH QUADRATIC ')
C
21    FORMAT(1X,'CARACT : VITESSES LINEAIRES ET TRACEUR QUADRATIQUE')
22    FORMAT(1X,'CARACT : LINEAR VELOCITY AND QUADRATIC TRACER')
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C