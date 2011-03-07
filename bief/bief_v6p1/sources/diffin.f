!                    *****************
                     SUBROUTINE DIFFIN
!                    *****************
!
     &(MASKTR,LIMTRA,LITBOR,CLT,U,V,XNEBOR,YNEBOR,NBOR,
     & KP1BOR,NPTFR,KENT,KSORT,KLOG,KINC,KNEU,KDIR,KDDL,
     & ICONV,NELBOR,NPOIN,NELMAX,MSK,MASKEL,
     & NFRLIQ,THOMFR,DEBLIQ,FINLIQ,FRTYPE,TN,TBOR,MESH)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE BOUNDARY CONDITIONS FOR TRACER DIFFUSION.
!
!history  J-M HERVOUET (LNH)
!+        25/06/2008
!+        V5P9
!+   MOVED FROM TELEMAC-2D TO ALLOW CALL BY SISYPHE
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
!| CLT            |<--| CONDITIONS AUX LIMITES DU TRACEUR POUR
!|                |   | LE CALCUL (LITBOR MODIFIES).
!| DEBLIQ         |---|
!| FINLIQ         |---|
!| FRTYPE         |---|
!| ICONV          |-->| OPTION DE CONVECTION : 1) CARACTERISTIQUES
!|                |   | 2) SUPG
!|                |   | 3) HYBRIDE
!| KDDL           |-->| INDICATEUR DE DEGRE DE LIBERTE
!| KDIR           |-->| INDICATEUR DE POINT DE DIRICHLET
!| KENT           |-->| INDICATEUR DE POINT D'ENTREE FLUIDE .
!| KINC           |---|
!| KLOG           |-->| INDICATEUR DE PAROI SOLIDE .
!| KNEU           |-->| INDICATEUR DE POINT DE NEUMANN
!| KP1BOR         |-->| UN SEGMENT EST SITUE ENTRE LE POINT DE BORD K
!|                |   | ET KP1BOR(K).
!| KSORT          |-->| INDICATEUR DE POINT DE SORTIE FLUIDE .
!| LIMTRA         |<--| CONDITIONS AUX LIMITES POUR LA DISPERSION
!| LITBOR         |-->| TYPES DE CONDITIONS AUX LIMITES DU TRACEUR.
!| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
!|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
!| MESH           |---|
!| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
!| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD
!| NELBOR         |-->| NUMEROS DES ELEMENTS ADJACENTS AUX BORDS
!| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
!| NFRLIQ         |---|
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
!| NPTFR          |-->| DIMENSION DES TABLEAUX .
!| TBOR           |---|
!| THOMFR         |---|
!| TN             |---|
!| U,V            |-->| COMPOSANTES DU COURANT
!| XNEBOR,YNEBOR  |-->| COMPOSANTES DE LA NORMALE EXTERIEURE AU
!|                |   | DOMAINE PAR POINT DE BORD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_DIFFIN => DIFFIN
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASKTR,TBOR
      TYPE(BIEF_OBJ), INTENT(IN)    :: TN
      INTEGER, INTENT(IN)           :: NPOIN,NPTFR,NELMAX,ICONV,NFRLIQ
      INTEGER, INTENT(IN)    :: LITBOR(NPTFR),KP1BOR(NPTFR),NBOR(NPTFR)
      INTEGER, INTENT(INOUT) :: LIMTRA(NPTFR),CLT(NPTFR)
      INTEGER, INTENT(IN)    :: KENT,KSORT,KLOG,KDIR,KDDL,KNEU,KINC
      INTEGER, INTENT(IN)    :: NELBOR(NPTFR)
      INTEGER, INTENT(IN)    :: DEBLIQ(NFRLIQ),FINLIQ(NFRLIQ)
      INTEGER, INTENT(IN)    :: FRTYPE(NFRLIQ)
!
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN), V(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: XNEBOR(NPTFR), YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: MASKEL(NELMAX)
!
      LOGICAL, INTENT(IN) :: MSK,THOMFR
!
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,K1,K2,IELEM
      INTEGER DIR,DDL,NEU,OND,IFRLIQ
      DOUBLE PRECISION USCALN
      LOGICAL DEP
!
!-----------------------------------------------------------------------
!
      DIR=1
      DDL=2
      NEU=3
      OND=4
!
! CLT CONTAINS ARRAY LITBOR, POSSIBLY MODIFIED ACCORDING TO THE SIGN
! OF U.N ON THE LIQUID BOUNDARIES, WHERE N IS THE OUTGOING NORMAL.
!
       DO K=1,NPTFR
         CLT(K) = LITBOR(K)
!        LOCATES THE LIQUID BOUNDARIES:
         IF(CLT(K).EQ.KENT) THEN
          USCALN = U(NBOR(K))*XNEBOR(K) + V(NBOR(K))*YNEBOR(K)
!         OUTGOING VELOCITY, FREE TRACER
          IF(USCALN.GT.0.D0) CLT(K) = KSORT
         ELSEIF(CLT(K).EQ.KSORT) THEN
          USCALN = U(NBOR(K))*XNEBOR(K) + V(NBOR(K))*YNEBOR(K)
!
!         INCOMING VELOCITY, TRACER IMPOSED AT THE LAST VALUE
          IF(USCALN.LT.0.D0) THEN
            TBOR%R(K)=TN%R(NBOR(K))
            CLT(K) = KENT
          ENDIF
         ENDIF
       ENDDO
!
!
!  BUILDS ARRAY MASKTR ACCORDING TO CLT
!
!  MASKTR EQUALS 1 FOR A SEGMENT OF TYPE NEUMANN, 0 OTHERWISE
!
!  A SEGMENT IS OF TYPE NEUMANN IF AT LEAST ONE OF ITS POINTS
!  IS SPECIFIED AS NEUMANN BY THE USER.
!
!
!     INITIALISES THE MASKS TO 0
!
      CALL OS('X=0     ',MASKTR)
      DO K1 = 1 , NPTFR
        K2 = KP1BOR(K1)
!       K2=K1 => IN PARALLEL MODE, NEXT IN OTHER SUB-DOMAIN
!                                  IN SUCH A CASE THE MASK BY
!                                  SEGMENT SHOULD NOT BE USED
        IF(K2.NE.K1) THEN
          IF(CLT(K1).EQ.KLOG.OR.CLT(K2).EQ.KLOG) THEN
!           SEGMENTS OF TYPE NEUMANN
            MASKTR%ADR(NEU)%P%R(K1)=1.D0
          ELSEIF(CLT(K1).EQ.KENT.AND.CLT(K2).EQ.KSORT) THEN
!           SEGMENTS OF TYPE EXIT
            MASKTR%ADR(DDL)%P%R(K1)=1.D0
          ELSEIF(CLT(K1).EQ.KSORT.OR.CLT(K2).EQ.KSORT) THEN
            MASKTR%ADR(DDL)%P%R(K1)=1.D0
          ELSEIF(CLT(K1).EQ.KSORT.AND.CLT(K2).EQ.KENT) THEN
!           SEGMENTS OF TYPE EXIT
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
!
!     IN PARALLEL MODE, RETRIEVES THE 1S GIVEN BY ANOTHER
!     SUB-DOMAIN (I.E. TAKES THE MAX OF EACH POINT)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM_BORD(MASKTR%ADR(NEU)%P%R,3,MESH)
        CALL PARCOM_BORD(MASKTR%ADR(DDL)%P%R,3,MESH)
        CALL PARCOM_BORD(MASKTR%ADR(DIR)%P%R,3,MESH)
        CALL PARCOM_BORD(MASKTR%ADR(OND)%P%R,3,MESH)
      ENDIF
!
!  POSSIBLE MASKING
!
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
!
!       IN PARALLEL MODE, RETRIEVES THE 0S GIVEN BY ANOTHER
!       SUB-DOMAIN (I.E. TAKES THE MIN OF EACH POINT)
!
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM_BORD(MASKTR%ADR(NEU)%P%R,4,MESH)
          CALL PARCOM_BORD(MASKTR%ADR(DDL)%P%R,4,MESH)
          CALL PARCOM_BORD(MASKTR%ADR(DIR)%P%R,4,MESH)
          CALL PARCOM_BORD(MASKTR%ADR(OND)%P%R,4,MESH)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
! FROM PHYSICAL TO TECHNICAL CONDITIONS
!
      DO 4 K=1,NPTFR
!
        IF(CLT(K).EQ.KENT ) THEN
!
!  ENTERING THE DOMAIN: IMPOSED TRACER
!
          LIMTRA(K) = KDIR
!
        ELSEIF(CLT(K).EQ.KSORT) THEN
!
!  LEAVING THE DOMAIN : FREE IF SUPG OR PSI SCHEME,
!                       RESULT OF IMPOSED ADVECTION OTHERWISE
!
          IF(ICONV.EQ.1) THEN
!           SEE DIFFCL : TTILD PUT IN TBOR
            LIMTRA(K) = KDIR
          ELSE
            LIMTRA(K) = KDDL
          ENDIF
!
        ELSEIF(CLT(K).EQ.KLOG ) THEN
!
!  WALL: NEUMANN CONDITIONS (IT'S NOT ACTUALLY USED)
!
          LIMTRA(K) = KNEU
!
        ELSE
!
!  ERROR, UNKNOWN VALUE OF LITBOR
!
          IF(LNG.EQ.1) WRITE(LU,10) K,LITBOR(K)
          IF(LNG.EQ.2) WRITE(LU,12) K,LITBOR(K)
10        FORMAT(1X,'DIFFIN: POINT ',1I6,' LITBOR= ',1I6,' ?????')
12        FORMAT(1X,'DIFFIN: POINT ',1I6,' LITBOR= ',1I6,' ?????')
          CALL PLANTE(1)
          STOP
!
        ENDIF
!
4     CONTINUE
!
!----------------------------------------------------------------------
!
! POST-TREATMENT FOR LIQUID BOUNDARY CONDITIONS (THOMPSON METHOD)
! THE TRACER BOUNDARY CONDITION THEN IS OF TYPE DIRICHLET
!
! WILL NOT WORK IN PARALLEL MODE
! (BUT THOMPSON NEEDS THE CHARACTERISTICS)
!
      IF(NFRLIQ.NE.0.AND.THOMFR) THEN
!
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
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END