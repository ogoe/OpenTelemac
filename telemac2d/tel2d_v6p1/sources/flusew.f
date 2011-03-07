!                    *****************
                     SUBROUTINE FLUSEW
!                    *****************
!
     &(AMINF,UBOR,VBOR,NPOIN,EPS,G,W,
     & XNEBOR,YNEBOR,NPTFR,LIMPRO,NBOR,KDIR,KNEU,KDDL)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief
!
!note     UBOR AND VBOR NOT USED (JMH)
!note     PORTABILITY: CRAY
!
!history  N. GOUTAL
!+        09/09/1994
!+        V5P2
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
!| AMINF          |<--| A-(WINF,NINF).WINF AVEC  /NINF/ = 1
!| EPS            |---| 
!| G              |---| 
!| KDDL           |---| 
!| KDIR           |---| 
!| KNEU           |---| 
!| LIMPRO         |---| 
!| NBOR           |---| 
!| NPOIN          |---| 
!| NPTFR          |---| 
!| UBOR           |---| 
!| VBOR           |---| 
!| W              |---| 
!| XNEBOR         |---| 
!| YNEBOR         |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,KDIR,KNEU,KDDL
      INTEGER, INTENT(IN)             :: LIMPRO(NPTFR,6),NBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: UBOR(NPTFR),VBOR(NPTFR),EPS,G
      DOUBLE PRECISION, INTENT(INOUT) :: AMINF(3,NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IEL,K
!
      DOUBLE PRECISION HI,UI,VI,XN,YN,R1,RLAMB0,HJ,UJ,VJ,R,PI
!
!------
! 1. COMPUTES FLUXES AT THE INFLOW BOUNDARIES
!------
!
      DO 10 K = 1 , NPTFR
!
!     IF H IS FREE OR DISCHARGE IS FREE
      IF(LIMPRO(K,1).EQ.KDDL.OR.LIMPRO(K,2).EQ.KDDL) THEN
      IEL = NBOR(K)
      XN = XNEBOR(K)
      YN = YNEBOR(K)
      HJ = W(1,IEL)
      IF(HJ.GT.EPS) THEN
        UJ = W(2,IEL)/HJ
        VJ = W(3,IEL)/HJ
        R  =  UJ*XN + VJ*YN-2.D0*SQRT(G*HJ)
        R1 =  UJ*XN + VJ*YN+2.D0*SQRT(G*HJ)
!
!       IF DISCHARGE IMPOSED
!
        IF(LIMPRO(K,2).EQ.KDIR) THEN
!
!   Q GIVEN; COMPUTES H FOR A SUBCRITICAL INFLOW BOUNDARY
!
          RLAMB0 = UJ * XN + VJ * YN
          IF ( RLAMB0.LE.0.D0) THEN
            PI = -R+RLAMB0
            HI = (PI**2/4.D0)/G
            UI = AMINF(2,K)/HI
            VI = AMINF(3,K)/HI
            AMINF(1,K) = HI
          ENDIF
        ENDIF
!
!       IF H IMPOSED
!
        IF(LIMPRO(K,1).EQ.KDIR) THEN
!
!   H GIVEN; COMPUTES Q FOR A SUBCRITICAL OUTFLOW BOUNDARY
!
           HI = AMINF(1,K)
!
           RLAMB0 = (UJ * XN) + (VJ * YN)
           IF ( RLAMB0.GE.-0.0001D0) THEN
             UI = (R1-2.D0*SQRT(G*HI))*XN
             VI = (R1-2.D0*SQRT(G*HI))*YN
             AMINF(2,K) = UI*HI
             AMINF(3,K) = VI*HI
           ENDIF
         ENDIF
       ENDIF
!
       ENDIF
!
10    CONTINUE
!
      RETURN
      END