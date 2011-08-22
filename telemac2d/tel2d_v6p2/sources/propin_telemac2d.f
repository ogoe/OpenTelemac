!                    ***************************
                     SUBROUTINE PROPIN_TELEMAC2D
!                    ***************************
!
     &(LIMPRO,LIMDIM,MASK,LIUBOR,LIVBOR,LIHBOR,KP1BOR,NBOR,NPTFR,
     & KENT,KENTU,KSORT,KADH,KLOG,KINC,KNEU,KDIR,KDDL,KOND,
     & CLH,CLU,CLV,IELMU,U,V,GRAV,H,LT,NPOIN,NELBOR,NELMAX,MSK,MASKEL,
     & NFRLIQ,THOMFR,NUMLIQ,FRTYPE,XNEBOR,YNEBOR,ENTET)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    1) CHECKS THE COMPATIBILITY OF BOUNDARY CONDITIONS.
!+
!+            2) FILLS ARRAYS LIMPRO AND MASK.
!
!history  J-M HERVOUET (LNHE)
!+        27/06/2008
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
!| CLH            |<->| COPY OF LIHBOR
!| CLU            |<->| COPY OF LIUBOR
!| CLV            |<->| COPY OF LIVBOR
!| ENTET          |-->| IF YES, MESSAGES PRINTED
!| FRTYPE         |-->| TYPE OF TREATMENT FOR LIQUID BOUNDARIES
!| GRAV           |-->| GRAVITY
!| IELMU          |-->| TYPE OF ELEMENT FOR U
!| KADH           |-->| CONVENTION FOR NO SLIP BOUNDARY CONDITION
!| KDDL           |-->| CONVENTION FOR DEGREE OF FREEDOM
!| KDIR           |-->| CONVENTION FOR DIRICHLET POINT
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| KINC           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KNEU           |-->| CONVENTION FOR NEUMANN CONDITION
!| KOND           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!|                |   | (TECHNICAL BOUNDARY CONDITIONS, AS OPPOSED
!|                |   |  TO KINC: PHYSICAL BOUNDARY CONDITION)
!| KP1BOR         |-->| GIVES THE NEXT BOUNDARY POINT IN A CONTOUR
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIMDIM         |-->| FIRST DIMENSION OF LIMPRO
!| LIMPRO         |<--| TYPES OF BOUNDARY CONDITIONS FOR PROPAGATION
!|                |   | PER POINT   :    .1:H  .2:U  .3:V
!|                |   | PER SEGMENT :    .4:H  .5:U  .6:V
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| LT             |-->| CURRENT TIME STEP
!| MASK           |<--| BLOCK OF MASKS FOR SEGMENTS :
!|                |   | MASK(NPTFR,1) : 1. IF KDIR ON U 0. SINON
!|                |   | MASK(NPTFR,2) : 1. IF KDIR ON V 0. SINON
!|                |   | MASK(NPTFR,3) : 1. IF KDDL ON U 0. SINON
!|                |   | MASK(NPTFR,4) : 1. IF KDDL ON V 0. SINON
!|                |   | MASK(NPTFR,5) : 1. IF KNEU ON U 0. SINON
!|                |   | MASK(NPTFR,6) : 1. IF KNEU ON V 0. SINON
!|                |   | MASK(NPTFR,7) : 1. IF KOND 0. SINON
!|                |   | MASK(NPTFR,8) : 1. - MASK( ,5)
!|                |   | MASK(NPTFR,9) : 1. IF H DIRICHLET
!|                |   | MASK(NPTFR,10): 1. IF H NEUMANN
!|                |   | MASK(NPTFR,11): 1. IF H DEGREE OF FREEDOM
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NELBOR         |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT.
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NFRLIQ         |-->| NUMBER OF LIQUID BOUNDARIES
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| THOMFR         |-->| IF YES, THOMPSON BOUNDARY CONDITIONS
!| XNEBOR         |<--| X-COMPONENT OF NORMAL AT NODES
!| YNEBOR         |<--| Y-COMPONENT OF NORMAL AT NODES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NELMAX,NPTFR,KOND,KENTU,LT,NFRLIQ
      INTEGER, INTENT(IN) :: KENT,KSORT,KADH,KLOG,KINC,KNEU,KDIR,KDDL
      INTEGER, INTENT(IN) :: LIMDIM,IELMU
      INTEGER, INTENT(IN) :: NELBOR(NPTFR),LIVBOR(NPTFR),LIHBOR(NPTFR)
      INTEGER, INTENT(IN) :: LIUBOR(NPTFR),FRTYPE(NFRLIQ)
      INTEGER, INTENT(INOUT) :: LIMPRO(LIMDIM,6)
      INTEGER, INTENT(IN) :: KP1BOR(NPTFR),NBOR(NPTFR),NUMLIQ(NFRLIQ)
      INTEGER, INTENT(INOUT) :: CLH(NPTFR),CLU(NPTFR),CLV(NPTFR)
      LOGICAL, INTENT(IN) :: MSK,THOMFR,ENTET
      DOUBLE PRECISION, INTENT(IN)   :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)   :: U(NPOIN),V(NPOIN),H(NPOIN)
      DOUBLE PRECISION, INTENT(IN)   :: GRAV,MASKEL(NELMAX)
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: MASK
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,N,IFRLIQ,IELEM,KP1
!
      INTEGER, PARAMETER :: UDIR    =  1
      INTEGER, PARAMETER :: VDIR    =  2
      INTEGER, PARAMETER :: UDDL    =  3
      INTEGER, PARAMETER :: VDDL    =  4
      INTEGER, PARAMETER :: UNEU    =  5
      INTEGER, PARAMETER :: VNEU    =  6
      INTEGER, PARAMETER :: HOND    =  7
      INTEGER, PARAMETER :: UNONNEU =  8
      INTEGER, PARAMETER :: HDIR    =  9
      INTEGER, PARAMETER :: HNEU    = 10
      INTEGER, PARAMETER :: HDDL    = 11
!
      DOUBLE PRECISION YY,F2,F3
!
      LOGICAL ALERTE1,ALERTE2
!
      INTEGER IGUILT1,IGUILT2
!
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
      DO 1 K=1,NPTFR
        CLH(K) = LIHBOR(K)
        CLU(K) = LIUBOR(K)
        CLV(K) = LIVBOR(K)
1     CONTINUE
!
!     FOR THOMPSON TREATMENT
!
      IF(NFRLIQ.NE.0.AND.THOMFR) THEN
!
        DO K = 1 , NPTFR
          IFRLIQ=NUMLIQ(K)
          IF(IFRLIQ.NE.0) THEN
            IF(FRTYPE(IFRLIQ).EQ.2.AND.H(NBOR(K)).GT.1.D-3) THEN
              CLH(K) = KENT
              CLU(K) = KENTU
              CLV(K) = KENTU
            ENDIF
          ENDIF
        ENDDO
!
      ENDIF
!
!  CHECKS AND MODIFIES THE CONDITIONS (IF REQUIRED) TO AVOID NON
!  PHYSICAL CASES : COMPLETELY FREE EXIT IN RIVER FLOW
!                   INCIDENT WAVE IN SUPERCRITICAL OUTGOING FLOW
!                   INCIDENT WAVE IN SUPERCRITICAL INCOMING FLOW
!
      ALERTE1=.FALSE.
      ALERTE2=.FALSE.
!
      DO 3 K=1,NPTFR
!
        N = NBOR(K)
        F2 = (U(N)**2+V(N)**2) / GRAV / MAX(H(N),1.D-8)
!
!       INCIDENT WAVE IN SUPERCRITICAL OUTGOING FLOW
!       INCIDENT WAVE IN SUPERCRITICAL INCOMING FLOW
!
        IF(CLU(K).EQ.KINC.AND.
     &     CLV(K).EQ.KINC.AND.
     &     F2.GE.1.D0) THEN
          CLU(K) = KSORT
          CLV(K) = KSORT
        ENDIF
!
!       COMPLETELY FREE EXIT IN RIVER FLOW
!
        IF(CLH(K).EQ.KSORT.AND.
     &     CLU(K).EQ.KSORT.AND.
     &     CLV(K).EQ.KSORT.AND.
     &     F2.LE.1.D0) THEN
          CLU(K) = KINC
          CLV(K) = KINC
        ENDIF
!
!       INCOMING FREE VELOCITY
!
        IF(CLU(K).EQ.KSORT.AND.CLV(K).EQ.KSORT) THEN
          F3 = U(N)*XNEBOR(K)+V(N)*YNEBOR(K)
          IF(F3.LE.-1.D-2) THEN
            ALERTE1=.TRUE.
            IGUILT1=K
          ENDIF
        ENDIF
!
!       SUPERCRITICAL INFLOW WITH FREE ELEVATION
!
        IF(CLH(K).EQ.KSORT.AND.F2.GE.1.D0) THEN
          F3 = U(N)*XNEBOR(K)+V(N)*YNEBOR(K)
          IF(F3.LE.-1.D-2) THEN
            ALERTE2=.TRUE.
            IGUILT2=K
          ENDIF
        ENDIF
!
3     CONTINUE
!
      IF(ALERTE1.AND.ENTET) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'PROBLEME MAL POSE, VITESSE LIBRE ENTRANTE'
          WRITE(LU,*) 'PAR EXEMPLE AU POINT DE BORD NUMERO ',IGUILT1
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'ILL-POSED PROBLEM, ENTERING FREE VELOCITY'
          WRITE(LU,*) 'FOR EXAMPLE AT BOUNDARY POINT NUMBER ',IGUILT1
        ENDIF
      ENDIF
!
      IF(ALERTE2.AND.ENTET) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'PROBLEME MAL POSE, HAUTEUR LIBRE'
          WRITE(LU,*) 'SUR FRONTIERE AVEC VITESSE ENTRANTE'
          WRITE(LU,*) 'ET ECOULEMENT TORRENTIEL'
          WRITE(LU,*) 'PAR EXEMPLE AU POINT DE BORD NUMERO ',IGUILT2
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'ILL-POSED PROBLEM, FREE DEPTH'
          WRITE(LU,*) 'ON BOUNDARY WITH ENTERING VELOCITY'
          WRITE(LU,*) 'AND SUPERCRITICAL FLOW'
          WRITE(LU,*) 'FOR EXAMPLE AT BOUNDARY POINT NUMBER ',IGUILT2
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
! INITIALISES THE BOUNDARY CONDITIONS FOR PROPAGATION:
!
!     INITIALISES ALL THE VECTORS OF THE BLOCK TO 0
!
      CALL OS('X=0     ',X=MASK)
!
      DO 4 K=1,NPTFR
!
!     IF THE NODE FOLLOWING K IS NOT IN THE SUB-DOMAIN IN PARALLEL MODE
!     WILL HAVE KP1=K
      KP1=KP1BOR(K)
!
!-----------------------------------------------------------------------
!
!     BOUNDARY CONDITIONS ON ELEVATION
!
      IF(CLH(K).EQ.KENT) THEN
        LIMPRO(K,1) = KDIR
        IF(KP1.NE.K) THEN
          IF(CLH(KP1).EQ.KENT) THEN
            MASK%ADR(HDIR)%P%R(K)=1.D0
          ELSEIF(CLH(KP1).EQ.KLOG) THEN
            MASK%ADR(HNEU)%P%R(K)=1.D0
          ELSEIF(CLH(KP1).EQ.KSORT) THEN
            MASK%ADR(HDDL)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,10) K
            IF(LNG.EQ.2) WRITE(LU,11) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSEIF(CLH(K).EQ.KSORT) THEN
        LIMPRO(K,1) = KDDL
        IF(KP1.NE.K) THEN
          IF(CLH(KP1).EQ.KSORT) THEN
            MASK%ADR(HDDL)%P%R(K)=1.D0
          ELSEIF(CLH(KP1).EQ.KLOG) THEN
            MASK%ADR(HNEU)%P%R(K)=1.D0
          ELSEIF(CLH(KP1).EQ.KENT) THEN
            MASK%ADR(HDDL)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,10) K
            IF(LNG.EQ.2) WRITE(LU,11) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSEIF(CLH(K).EQ.KLOG ) THEN
        LIMPRO(K,1) = KNEU
        IF(KP1.NE.K) MASK%ADR(HNEU)%P%R(K)=1.D0
      ELSE
        IF(LNG.EQ.1) WRITE(LU,10) K
        IF(LNG.EQ.2) WRITE(LU,11) K
        CALL PLANTE(1)
        STOP
      ENDIF
!
!   BOUNDARY CONDITIONS ON U
!
      IF(CLU(K).EQ.KENT.OR.CLU(K).EQ.KENTU) THEN
        LIMPRO(K,2) = KDIR
        IF(KP1.NE.K) THEN
          IF(CLU(KP1).EQ.KENT.OR.CLU(KP1).EQ.KENTU) THEN
            MASK%ADR(UDIR)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KADH) THEN
            MASK%ADR(UDIR)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KLOG) THEN
            MASK%ADR(UNEU)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KINC) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KSORT) THEN
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,20) K
            IF(LNG.EQ.2) WRITE(LU,21) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSEIF(CLU(K).EQ.KADH) THEN
        LIMPRO(K,2) = KDIR
        IF(KP1.NE.K) MASK%ADR(UNEU)%P%R(K)=1.D0
      ELSEIF(CLU(K).EQ.KSORT) THEN
        LIMPRO(K,2) = KDDL
        IF(KP1.NE.K) THEN
          IF(CLU(KP1).EQ.KSORT) THEN
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KLOG) THEN
            MASK%ADR(UNEU)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KINC) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KENT) THEN
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KENTU) THEN
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KADH) THEN
            MASK%ADR(UNEU)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,20) K
            IF(LNG.EQ.2) WRITE(LU,21) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSEIF(CLU(K).EQ.KLOG) THEN
        LIMPRO(K,2) = KDDL
        IF(KP1.NE.K) MASK%ADR(UNEU)%P%R(K)=1.D0
      ELSEIF(CLU(K).EQ.KINC ) THEN
        LIMPRO(K,2) = KDDL
        IF(KP1.NE.K) THEN
          IF(CLU(KP1).EQ.KINC) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KLOG) THEN
            MASK%ADR(UNEU)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KSORT) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KENT) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KENTU) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLU(KP1).EQ.KADH) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,20) K
            IF(LNG.EQ.2) WRITE(LU,21) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSE
        IF(LNG.EQ.1) WRITE(LU,20) K
        IF(LNG.EQ.2) WRITE(LU,21) K
        CALL PLANTE(1)
        STOP
      ENDIF
!
!   BOUNDARY CONDITIONS ON V
!
      IF(CLV(K).EQ.KENT.OR.CLV(K).EQ.KENTU) THEN
        LIMPRO(K,3) = KDIR
        IF(KP1.NE.K) THEN
          IF(CLV(KP1).EQ.KENT.OR.CLV(KP1).EQ.KENTU) THEN
            MASK%ADR(VDIR)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KADH) THEN
            MASK%ADR(VDIR)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KLOG) THEN
            MASK%ADR(VNEU)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KINC) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KSORT) THEN
            MASK%ADR(VDDL)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,30) K
            IF(LNG.EQ.2) WRITE(LU,31) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSEIF(CLV(K).EQ.KADH) THEN
        LIMPRO(K,3) = KDIR
        IF(KP1.NE.K) MASK%ADR(VNEU)%P%R(K)=1.D0
      ELSEIF(CLV(K).EQ.KSORT) THEN
        LIMPRO(K,3) = KDDL
        IF(KP1.NE.K) THEN
          IF(CLV(KP1).EQ.KSORT) THEN
            MASK%ADR(VDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KLOG) THEN
            MASK%ADR(VNEU)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KINC) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KENT) THEN
            MASK%ADR(VDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KENTU) THEN
            MASK%ADR(VDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KADH) THEN
            MASK%ADR(VDDL)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,30) K
            IF(LNG.EQ.2) WRITE(LU,31) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSEIF(CLV(K).EQ.KLOG ) THEN
        LIMPRO(K,3) = KDDL
        IF(KP1.NE.K) MASK%ADR(VNEU)%P%R(K)=1.D0
      ELSEIF(CLV(K).EQ.KINC ) THEN
        LIMPRO(K,3) = KDDL
        IF(KP1.NE.K) THEN
          IF(CLV(KP1).EQ.KINC) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KLOG) THEN
            MASK%ADR(VNEU)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KSORT) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KENT) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KENTU) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSEIF(CLV(KP1).EQ.KADH) THEN
            MASK%ADR(HOND)%P%R(K)=1.D0
            MASK%ADR(UDDL)%P%R(K)=1.D0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,30) K
            IF(LNG.EQ.2) WRITE(LU,31) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ELSE
        IF(LNG.EQ.1) WRITE(LU,30) K
        IF(LNG.EQ.2) WRITE(LU,31) K
        CALL PLANTE(1)
        STOP
      ENDIF
!
4     CONTINUE
!
!-----------------------------------------------------------------------
!
!     LIQUID BOUNDARIES MASK
!
      DO K=1,NPTFR
        KP1=KP1BOR(K)
        IF(KP1.NE.K) MASK%ADR(UNONNEU)%P%R(K)=1.D0-MASK%ADR(UNEU)%P%R(K)
      ENDDO
!
!     DEDUCES ARRAYS LIMPRO (. , 4 5 AND 6) FROM THE MASKS
!
      DO K=1,NPTFR
        IF(MASK%ADR(HDIR)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,4)=KDIR
        ELSEIF(MASK%ADR(HDDL)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,4)=KDDL
        ELSEIF(MASK%ADR(HNEU)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,4)=KNEU
        ELSEIF(MASK%ADR(HOND)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,4)=KOND
        ELSE
          IF(NCSIZE.GT.1) THEN
            LIMPRO(K,4)=0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,10) K
            IF(LNG.EQ.2) WRITE(LU,11) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
        IF(MASK%ADR(UDIR)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,5)=KDIR
        ELSEIF(MASK%ADR(UDDL)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,5)=KDDL
        ELSEIF(MASK%ADR(UNEU)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,5)=KNEU
        ELSEIF(MASK%ADR(HOND)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,5)=KOND
        ELSE
          IF(NCSIZE.GT.1) THEN
            LIMPRO(K,5)=0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,20) K
            IF(LNG.EQ.2) WRITE(LU,21) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
        IF(MASK%ADR(VDIR)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,6)=KDIR
        ELSEIF(MASK%ADR(VDDL)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,6)=KDDL
        ELSEIF(MASK%ADR(VNEU)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,6)=KNEU
        ELSEIF(MASK%ADR(HOND)%P%R(K).GT.0.5D0) THEN
          LIMPRO(K,6)=KOND
        ELSE
          IF(NCSIZE.GT.1) THEN
            LIMPRO(K,6)=0
          ELSE
            IF(LNG.EQ.1) WRITE(LU,30) K
            IF(LNG.EQ.2) WRITE(LU,31) K
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
      ENDDO
!
!     SUPPLEMENT FOR QUADRATIC SPEEDS
!     THE POINT IN THE MIDDLE OF A SEGMENT HAS THE SAME CONDITION AS THE SEGMENT
!
      IF(IELMU.EQ.13) THEN
        DO K=1,NPTFR
          LIMPRO(K+NPTFR,2)=LIMPRO(K,5)
          LIMPRO(K+NPTFR,3)=LIMPRO(K,6)
        ENDDO
      ENDIF
!
!     MASKS USING THE MASK OF THE ELEMENTS
!
      IF(MSK) THEN
        DO K=1,NPTFR
          IELEM=NELBOR(K)
          IF(IELEM.GT.0) THEN
            YY = MASKEL(IELEM)
            MASK%ADR(UDIR   )%P%R(K) = MASK%ADR(UDIR   )%P%R(K) * YY
            MASK%ADR(VDIR   )%P%R(K) = MASK%ADR(VDIR   )%P%R(K) * YY
            MASK%ADR(UDDL   )%P%R(K) = MASK%ADR(UDDL   )%P%R(K) * YY
            MASK%ADR(VDDL   )%P%R(K) = MASK%ADR(VDDL   )%P%R(K) * YY
            MASK%ADR(UNEU   )%P%R(K) = MASK%ADR(UNEU   )%P%R(K) * YY
            MASK%ADR(VNEU   )%P%R(K) = MASK%ADR(VNEU   )%P%R(K) * YY
            MASK%ADR(HOND   )%P%R(K) = MASK%ADR(HOND   )%P%R(K) * YY
            MASK%ADR(HDIR   )%P%R(K) = MASK%ADR(HDIR   )%P%R(K) * YY
            MASK%ADR(HNEU   )%P%R(K) = MASK%ADR(HNEU   )%P%R(K) * YY
            MASK%ADR(HDDL   )%P%R(K) = MASK%ADR(HDDL   )%P%R(K) * YY
            MASK%ADR(UNONNEU)%P%R(K) = MASK%ADR(UNONNEU)%P%R(K) * YY
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
10    FORMAT(1X,'PROPIN : POINT DE BORD',1I5,' CAS NON PREVU SUR H')
11    FORMAT(1X,'PROPIN: BOUNDARY POINT',1I5,' UNKNOWN PARAMETER FOR H')
20    FORMAT(1X,'PROPIN : POINT DE BORD',1I5,' CAS NON PREVU SUR U')
21    FORMAT(1X,'PROPIN: BOUNDARY POINT',1I5,' UNKNOWN PARAMETER FOR U')
30    FORMAT(1X,'PROPIN : POINT DE BORD',1I5,' CAS NON PREVU SUR V')
31    FORMAT(1X,'PROPIN: BOUNDARY POINT',1I5,' UNKNOWN PARAMETER FOR V')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
