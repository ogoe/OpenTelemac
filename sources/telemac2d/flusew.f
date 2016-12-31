!                      *****************
                       SUBROUTINE FLUSEW
!                      *****************
!
     &(AMINF,NPOIN,EPS,G,W,XNEBOR,YNEBOR,
     & NPTFR,LIMPRO,NBOR,KDIR,KDDL)
!
!***********************************************************************
! TELEMAC2D   V6P3                                           05/15/2013
!***********************************************************************
!
!brief  HOW TO MANAGE INLET AND OUTLET (NOT IMPLEMENTED YET):
!   1- CHECK THE REGIME : FROUDE(FR)>1 OR <1
!   2- FOR INLET:
!        - IF FR<1 ==> Q IMPOSED
!        - IF FR>1 ==> Q AND H IMPOSED
!   3- FOR OUTLET:
!        - IF FR<1 ==> H IMPOSED
!        - IF FR>1 ==> NO CONDITION IS REQUIRED
!
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
!history  R. ATA (EDF-LNHE)
!+        03/15/2011
!+        V6P1
!+    INTRODUCTION OF XSGBOR AND YSGBOR TO BE ADAPTED
!+
!history  R. ATA (EDF-LNHE)
!+        05/15/2013
!+        V6P3
!+    clean and remove unused variables
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AMINF          |<--| IN/OUT VALUES TO BE IMPOSED
!| EPS            |-->| TOLERENCE FOR WATER DEPTH
!| G              |-->| GRAVITY
!| KDDL           |-->| CONVENTION FOR BC (FREE H)
!| KDIR           |-->| CONVENTION FOR BC (DIRICHLET POINT)
!| KNEU           |-->| CONVENTION FOR BC (NEUMANN POINT)
!| LIMPRO         |-->| BC TYPE (PRESCRIBED)
!| NBOR           |-->| GLOBAL NUMBER (INDEX) OF BOUNDARY NODES
!| NPOIN          |-->| TOTAL NUMBER OF NODES
!| NPTFR          |-->| TOTAL NUMBER OF BOUNDARY NODES
!| UBOR           |-->| IMPOSED VELOCITY X-COMPONENENT
!| VBOR           |-->| IMPOSED VELOCITY Y-COMPONENENT
!| W              |-->| (H,HU,HV)
!| XNEBOR         |-->| X-COMPOENENT OF OUTWARD UNIT NORMAL AT POINT
!| YNEBOR         |-->| Y-COMPOENENT OF OUTWARD UNIT NORMAL AT POINT
!| XSGBOR         |-->| X-COMPOENENT OF OUTWARD UNIT NORMAL AT EDGE
!| YSGBOR         |-->| Y-COMPOENENT OF OUTWARD UNIT NORMAL AT EDGE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,KDIR,KDDL
      INTEGER, INTENT(IN)             :: LIMPRO(NPTFR,6),NBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: EPS,G
      DOUBLE PRECISION, INTENT(INOUT) :: AMINF(3,NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IEL,K
      DOUBLE PRECISION HI,UI,VI,XN,YN,R1,RLAMB0,HJ,UJ,VJ,R,PI
!
      DO K = 1 , NPTFR
!
!       IF H IS FREE OR INFLOW IS FREE
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
!       IF IN/OUTFLOW IMPOSED
!
            IF(LIMPRO(K,2).EQ.KDIR) THEN
!
!       Q GIVEN ; COMPUTES H FOR A SUBCRITICAL ENTRY
!
              RLAMB0 = UJ*XN + VJ*YN
              IF ( RLAMB0.LE.0.D0) THEN
                PI = -R+RLAMB0
                HI = (PI**2/4.D0)/G
                UI = AMINF(2,K)/HI
                VI = AMINF(3,K)/HI
                AMINF(1,K) = HI
              ENDIF
            ENDIF
!
!         IF H IMPOSED
!
            IF(LIMPRO(K,1).EQ.KDIR) THEN
!
!        H GIVEN ; COMUTES Q FOR A SUBCRITICAL OUTFLOW
!
              HI = AMINF(1,K)
!
              RLAMB0 = UJ*XN + VJ*YN
              IF (RLAMB0.GE.-0.0001D0) THEN
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
      ENDDO !  K
!
      RETURN
      END
