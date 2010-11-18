C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE BOUNDARY CONDITIONS FOR TRACER DIFFUSION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CLT, DEBLIQ, FINLIQ, FRTYPE, ICONV, KDDL, KDIR, KENT, KINC, KLOG, KNEU, KP1BOR, KSORT, LIMTRA, LITBOR, MASKEL, MASKTR, MESH, MSK, NBOR, NELBOR, NELMAX, NFRLIQ, NPOIN, NPTFR, TBOR, THOMFR, TN, U, V, XNEBOR, YNEBOR
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
!>    </th><td> DDL, DEP, DIR, IELEM, IFRLIQ, K, K1, K2, NEU, OND, USCALN
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DIFFIN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PARCOM_BORD(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SUSPENSION_COMPUTATION(), TELEMAC2D()

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
!> </td><td> 25/06/2008
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td> MOVED FROM TELEMAC-2D TO ALLOW CALL BY SISYPHE
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CLT
!></td><td><--</td><td>CONDITIONS AUX LIMITES DU TRACEUR POUR
!>                  LE CALCUL (LITBOR MODIFIES).
!>    </td></tr>
!>          <tr><td>DEBLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FINLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FRTYPE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ICONV
!></td><td>--></td><td>OPTION DE CONVECTION : 1) CARACTERISTIQUES
!>                  2) SUPG
!>                  3) HYBRIDE
!>    </td></tr>
!>          <tr><td>KDDL
!></td><td>--></td><td>INDICATEUR DE DEGRE DE LIBERTE
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>--></td><td>INDICATEUR DE POINT DE DIRICHLET
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>--></td><td>INDICATEUR DE POINT D'ENTREE FLUIDE .
!>    </td></tr>
!>          <tr><td>KINC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>INDICATEUR DE PAROI SOLIDE .
!>    </td></tr>
!>          <tr><td>KNEU
!></td><td>--></td><td>INDICATEUR DE POINT DE NEUMANN
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>UN SEGMENT EST SITUE ENTRE LE POINT DE BORD K
!>                  ET KP1BOR(K).
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>--></td><td>INDICATEUR DE POINT DE SORTIE FLUIDE .
!>    </td></tr>
!>          <tr><td>LIMTRA
!></td><td><--</td><td>CONDITIONS AUX LIMITES POUR LA DISPERSION
!>    </td></tr>
!>          <tr><td>LITBOR
!></td><td>--></td><td>TYPES DE CONDITIONS AUX LIMITES DU TRACEUR.
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MASKTR(1,1)
!></td><td><--</td><td>MASQUE VALANT UN POUR LES SEGMENTS DIRICHLET
!>    </td></tr>
!>          <tr><td>MASKTR(1,2)
!></td><td><--</td><td>MASQUE VALANT UN POUR LES SEGMENTS DDL
!>    </td></tr>
!>          <tr><td>MASKTR(1,3)
!></td><td><--</td><td>MASQUE VALANT UN POUR LES SEGMENTS NEUMANN
!>                  (ET ZERO SINON)
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMEROS DES ELEMENTS ADJACENTS AUX BORDS
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NFRLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>DIMENSION DES TABLEAUX .
!>    </td></tr>
!>          <tr><td>TBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>THOMFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTES DU COURANT
!>    </td></tr>
!>          <tr><td>XNEBOR,YNEBOR
!></td><td>--></td><td>COMPOSANTES DE LA NORMALE EXTERIEURE AU
!>                  DOMAINE PAR POINT DE BORD
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DIFFIN
     &(MASKTR,LIMTRA,LITBOR,CLT,U,V,XNEBOR,YNEBOR,NBOR,
     & KP1BOR,NPTFR,KENT,KSORT,KLOG,KINC,KNEU,KDIR,KDDL,
     & ICONV,NELBOR,NPOIN,NELMAX,MSK,MASKEL,
     & NFRLIQ,THOMFR,DEBLIQ,FINLIQ,FRTYPE,TN,TBOR,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CLT            |<--| CONDITIONS AUX LIMITES DU TRACEUR POUR
C|                |   | LE CALCUL (LITBOR MODIFIES).
C| DEBLIQ         |---| 
C| FINLIQ         |---| 
C| FRTYPE         |---| 
C| ICONV          |-->| OPTION DE CONVECTION : 1) CARACTERISTIQUES
C|                |   | 2) SUPG
C|                |   | 3) HYBRIDE
C| KDDL           |-->| INDICATEUR DE DEGRE DE LIBERTE
C| KDIR           |-->| INDICATEUR DE POINT DE DIRICHLET
C| KENT           |-->| INDICATEUR DE POINT D'ENTREE FLUIDE .
C| KINC           |---| 
C| KLOG           |-->| INDICATEUR DE PAROI SOLIDE .
C| KNEU           |-->| INDICATEUR DE POINT DE NEUMANN
C| KP1BOR         |-->| UN SEGMENT EST SITUE ENTRE LE POINT DE BORD K
C|                |   | ET KP1BOR(K).
C| KSORT          |-->| INDICATEUR DE POINT DE SORTIE FLUIDE .
C| LIMTRA         |<--| CONDITIONS AUX LIMITES POUR LA DISPERSION
C| LITBOR         |-->| TYPES DE CONDITIONS AUX LIMITES DU TRACEUR.
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MASKTR(1,1)    |<--| MASQUE VALANT UN POUR LES SEGMENTS DIRICHLET
C| MASKTR(1,2)    |<--| MASQUE VALANT UN POUR LES SEGMENTS DDL
C| MASKTR(1,3)    |<--| MASQUE VALANT UN POUR LES SEGMENTS NEUMANN
C|                |   | (ET ZERO SINON)
C| MESH           |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD
C| NELBOR         |-->| NUMEROS DES ELEMENTS ADJACENTS AUX BORDS
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
C| NFRLIQ         |---| 
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPTFR          |-->| DIMENSION DES TABLEAUX .
C| TBOR           |---| 
C| THOMFR         |---| 
C| TN             |---| 
C| U,V            |-->| COMPOSANTES DU COURANT
C| XNEBOR,YNEBOR  |-->| COMPOSANTES DE LA NORMALE EXTERIEURE AU
C|                |   | DOMAINE PAR POINT DE BORD
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_DIFFIN => DIFFIN
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASKTR,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)    :: TN
      INTEGER, INTENT(IN)           :: NPOIN,NPTFR,NELMAX,ICONV,NFRLIQ
      INTEGER, INTENT(IN)    :: LITBOR(NPTFR),KP1BOR(NPTFR),NBOR(NPTFR)
      INTEGER, INTENT(INOUT) :: LIMTRA(NPTFR),CLT(NPTFR)
      INTEGER, INTENT(IN)    :: KENT,KSORT,KLOG,KDIR,KDDL,KNEU,KINC
      INTEGER, INTENT(IN)    :: NELBOR(NPTFR)
      INTEGER, INTENT(IN)    :: DEBLIQ(NFRLIQ),FINLIQ(NFRLIQ)
      INTEGER, INTENT(IN)    :: FRTYPE(NFRLIQ)
C
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN), V(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(NPTFR), YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: MASKEL(NELMAX)
C
      LOGICAL, INTENT(IN) :: MSK,THOMFR
C
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,K1,K2,IELEM
      INTEGER DIR,DDL,NEU,OND,IFRLIQ
      DOUBLE PRECISION USCALN
      LOGICAL DEP
C
C-----------------------------------------------------------------------
C
      DIR=1
      DDL=2
      NEU=3
      OND=4
C
C CLT CONTAINS ARRAY LITBOR, POSSIBLY MODIFIED ACCORDING TO THE SIGN
C OF U.N ON THE LIQUID BOUNDARIES, WHERE N IS THE OUTGOING NORMAL.
C
       DO K=1,NPTFR
         CLT(K) = LITBOR(K)
C        LOCATES THE LIQUID BOUNDARIES:
         IF(CLT(K).EQ.KENT) THEN
          USCALN = U(NBOR(K))*XNEBOR(K) + V(NBOR(K))*YNEBOR(K)
C         OUTGOING VELOCITY, FREE TRACER
          IF(USCALN.GT.0.D0) CLT(K) = KSORT
         ELSEIF(CLT(K).EQ.KSORT) THEN
          USCALN = U(NBOR(K))*XNEBOR(K) + V(NBOR(K))*YNEBOR(K)
C
C         INCOMING VELOCITY, TRACER IMPOSED AT THE LAST VALUE
          IF(USCALN.LT.0.D0) THEN
            TBOR%R(K)=TN%R(NBOR(K))
            CLT(K) = KENT
          ENDIF
         ENDIF
       ENDDO
C
C
C  BUILDS ARRAY MASKTR ACCORDING TO CLT
C
C  MASKTR EQUALS 1 FOR A SEGMENT OF TYPE NEUMANN, 0 OTHERWISE
C
C  A SEGMENT IS OF TYPE NEUMANN IF AT LEAST ONE OF ITS POINTS
C  IS SPECIFIED AS NEUMANN BY THE USER.
C
C
C     INITIALISES THE MASKS TO 0
C
      CALL OS('X=0     ',MASKTR)
      DO K1 = 1 , NPTFR
        K2 = KP1BOR(K1)
C       K2=K1 => IN PARALLEL MODE, NEXT IN OTHER SUB-DOMAIN
C                                  IN SUCH A CASE THE MASK BY
C                                  SEGMENT SHOULD NOT BE USED
        IF(K2.NE.K1) THEN
          IF(CLT(K1).EQ.KLOG.OR.CLT(K2).EQ.KLOG) THEN
C           SEGMENTS OF TYPE NEUMANN
            MASKTR%ADR(NEU)%P%R(K1)=1.D0
          ELSEIF(CLT(K1).EQ.KENT.AND.CLT(K2).EQ.KSORT) THEN
C           SEGMENTS OF TYPE EXIT
            MASKTR%ADR(DDL)%P%R(K1)=1.D0
          ELSEIF(CLT(K1).EQ.KSORT.OR.CLT(K2).EQ.KSORT) THEN
            MASKTR%ADR(DDL)%P%R(K1)=1.D0
          ELSEIF(CLT(K1).EQ.KSORT.AND.CLT(K2).EQ.KENT) THEN
C           SEGMENTS OF TYPE EXIT
            MASKTR%ADR(DDL)%P%R(K1)=1.D0
          ELSEIF(CLT(K1).EQ.KENT.OR.CLT(K2).EQ.KENT) THEN
            MASKTR%ADR(DIR)%P%R(K1)=1.D0
          ELSEIF(CLT(K1).EQ.KINC.OR.CLT(K2).EQ.KINC) THEN
            MASKTR%ADR(OND)%P%R(K1)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,100)
            IF(LNG.EQ.2) WRITE(LU,101)
100         FORMAT(1X,'DIFFIN : CAS NON PREVU')
101         FORMAT(1X,'DIFFIN : UNEXPECTED CASE')
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ENDDO
C
C     IN PARALLEL MODE, RETRIEVES THE 1S GIVEN BY ANOTHER
C     SUB-DOMAIN (I.E. TAKES THE MAX OF EACH POINT)
C
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM_BORD(MASKTR%ADR(NEU)%P%R,3,MESH)
        CALL PARCOM_BORD(MASKTR%ADR(DDL)%P%R,3,MESH)
        CALL PARCOM_BORD(MASKTR%ADR(DIR)%P%R,3,MESH)
        CALL PARCOM_BORD(MASKTR%ADR(OND)%P%R,3,MESH)
      ENDIF
C
C  POSSIBLE MASKING
C
      IF(MSK) THEN
        DO K1 = 1 , NPTFR
          IELEM=NELBOR(K1)
          IF(IELEM.GT.0) THEN
            MASKTR%ADR(DIR)%P%R(K1) = MASKTR%ADR(DIR)%P%R(K1) *
     &                                                     MASKEL(IELEM)
            MASKTR%ADR(DDL)%P%R(K1) = MASKTR%ADR(DDL)%P%R(K1) *
     &                                                     MASKEL(IELEM)
            MASKTR%ADR(NEU)%P%R(K1) = MASKTR%ADR(NEU)%P%R(K1) *
     &                                                     MASKEL(IELEM)
            MASKTR%ADR(OND)%P%R(K1) = MASKTR%ADR(OND)%P%R(K1) *
     &                                                     MASKEL(IELEM)
          ENDIF
        ENDDO
C
C       IN PARALLEL MODE, RETRIEVES THE 0S GIVEN BY ANOTHER
C       SUB-DOMAIN (I.E. TAKES THE MIN OF EACH POINT)
C
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM_BORD(MASKTR%ADR(NEU)%P%R,4,MESH)
          CALL PARCOM_BORD(MASKTR%ADR(DDL)%P%R,4,MESH)
          CALL PARCOM_BORD(MASKTR%ADR(DIR)%P%R,4,MESH)
          CALL PARCOM_BORD(MASKTR%ADR(OND)%P%R,4,MESH)
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
C FROM PHYSICAL TO TECHNICAL CONDITIONS
C
      DO 4 K=1,NPTFR
C
        IF(CLT(K).EQ.KENT ) THEN
C
C  ENTERING THE DOMAIN: IMPOSED TRACER
C
          LIMTRA(K) = KDIR
C
        ELSEIF(CLT(K).EQ.KSORT) THEN
C
C  LEAVING THE DOMAIN : FREE IF SUPG OR PSI SCHEME,
C                       RESULT OF IMPOSED ADVECTION OTHERWISE
C
          IF(ICONV.EQ.1) THEN
C           SEE DIFFCL : TTILD PUT IN TBOR
            LIMTRA(K) = KDIR
          ELSE
            LIMTRA(K) = KDDL
          ENDIF
C
        ELSEIF(CLT(K).EQ.KLOG ) THEN
C
C  WALL: NEUMANN CONDITIONS (IT'S NOT ACTUALLY USED)
C
          LIMTRA(K) = KNEU
C
        ELSE
C
C  ERROR, UNKNOWN VALUE OF LITBOR
C
          IF(LNG.EQ.1) WRITE(LU,10) K,LITBOR(K)
          IF(LNG.EQ.2) WRITE(LU,12) K,LITBOR(K)
10        FORMAT(1X,'DIFFIN: POINT ',1I6,' LITBOR= ',1I6,' ?????')
12        FORMAT(1X,'DIFFIN: POINT ',1I6,' LITBOR= ',1I6,' ?????')
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
4     CONTINUE
C
C----------------------------------------------------------------------
C
C POST-TREATMENT FOR LIQUID BOUNDARY CONDITIONS (THOMPSON METHOD)
C THE TRACER BOUNDARY CONDITION THEN IS OF TYPE DIRICHLET
C
C WILL NOT WORK IN PARALLEL MODE
C (BUT THOMPSON NEEDS THE CHARACTERISTICS)
C
      IF(NFRLIQ.NE.0.AND.THOMFR) THEN
C
      DO 17 IFRLIQ = 1 , NFRLIQ
        DEP = .FALSE.
        K = DEBLIQ(IFRLIQ)
15      CONTINUE
        IF(FRTYPE(IFRLIQ).EQ.2) LIMTRA(K) = KDIR
        IF(K.EQ.FINLIQ(IFRLIQ).AND.DEP) THEN
          GO TO 16
        ELSE
          DEP=.TRUE.
          K = KP1BOR(K)
          GO TO 15
        ENDIF
16      CONTINUE
17    CONTINUE
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C