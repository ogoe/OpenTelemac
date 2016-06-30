!                    **********************
                     SUBROUTINE CONDIM_SUSP
!                    **********************
!
     &(CS,CS0,NSICLA,X,Y,AT,NPOIN)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    INITIALISES THE SUSPENDED SEDIMENT CONCENTRATION
!+               (CONDIM_SISYPHE.F IS READ EVEN IF CHARR=NO).
!
!history  M. GONZALES DE LINARES
!+        2004
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME
!| CS             |<->| SUSPENDED SEDIMENT CONCENTRATION
!| CS0            |-->| INITIAL CONCENTRATIONS (CONSTANT VALUES)/CLASS
!| NPOIN          |-->| NUMBER OF POINTS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| X              |-->| X-COORDINATE
!| Y              |-->| Y-COORDINATE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NPOIN,NSICLA
      DOUBLE PRECISION,INTENT(IN)   :: AT,CS0(NSICLA)
      DOUBLE PRECISION,INTENT(IN)   :: X(NPOIN),Y(NPOIN)
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: CS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!-----------------------------------------------------------------------
!
!  --------------------------------------------------------------
!  INITIALISES THE ARRAYS THAT HAVE NOT BEEN READ IN THE RESULTS FILE:
!  --------------------------------------------------------------
!
      IF(NSICLA.GT.0) THEN
        DO I=1,NSICLA
          CALL OS('X=C     ',X=CS%ADR(I)%P,C=CS0(I))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
