!                    *****************
                     SUBROUTINE PREDES
!                    *****************
!
     &(LLT,AAT)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+
!+
!
!history  JMH
!+        07/12/2009
!+        V6P0
!+   KS SET TO 0 IF LLT=0
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
!| AAT            |-->| CURRENT TIME (FOR BUILDING SOLUTIONS)
!| LLT            |-->| LOCAL LT (MAY BE LT-1+PERCOU)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: LLT
      DOUBLE PRECISION, INTENT(IN) :: AAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER LTT,I
      LOGICAL IMP,LEO
!
!-----------------------------------------------------------------------
!
!     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
!     FOLLOWING TESTS, WHICH MUST BE THE SAME AS IN DESIMP (BIEF LIBRARY)
!
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LLT/LISPR)*LISPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIL) IMP=.TRUE.
      LTT=(LLT/LEOPR)*LEOPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIG) LEO=.TRUE.
!
!     NO PRINTOUTS REUIRED: LEAVING
      IF (.NOT.(LEO.OR.IMP)) GO TO 1000
!
!=======================================================================
!     COMPUTES SECONDARY VARIABLES
!=======================================================================
!
!     FREE SURFACE: H+ZF
!
      IF((LEO.AND.SORLEO(4)).OR.(IMP.AND.SORIMP(4))) THEN
        CALL OS('X=Y+Z   ',X=Z,Y=HN,Z=ZF)
      ENDIF
!
!     DISCHARGE
!
      IF((LEO.AND.SORLEO(6)).OR.(IMP.AND.SORIMP(6))) THEN
        DO I=1,NPOIN
          Q%R(I)=HN%R(I)*SQRT(U2D%R(I)**2+V2D%R(I)**2)
        ENDDO
      ENDIF
!
!     DISCHARGE ALONG X
!
      IF((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        CALL OS('X=YZ    ',X=QU,Y=U2D,Z=HN)
      ENDIF
!
!     DISCHARGE ALONG Y
!
      IF((LEO.AND.SORLEO(8)).OR.(IMP.AND.SORIMP(8))) THEN
        CALL OS('X=YZ    ',X=QU,Y=V2D,Z=HN)
      ENDIF
!
!=======================================================================
!
!     VARIABLES WHICH ARE NOT INITIALISED AT THE FIRST CALL OF PREDES
!
      IF(LLT.EQ.0) THEN
!       JMH ON 27/11/2009
        IF((LEO.AND.SORLEO(19)).OR.(IMP.AND.SORIMP(19))) THEN
          CALL OS('X=0     ',X=KS)
        ENDIF
      ENDIF
!
!=======================================================================
!
1000  CONTINUE
!
!=======================================================================
!
      RETURN
      END
