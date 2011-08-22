!                    ********************
                     LOGICAL FUNCTION EOF
!                    ********************
!
     &(LUNIT)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DETECTS THE END OF A FILE:
!+
!+            IF EOF = .TRUE.  : HAS REACHED END OF FILE,
!+
!+            IF EOF = .FALSE. : CAN CARRY ON.
!
!history
!+        17/08/94
!+        V5P1
!+   ORIGINAL IDEA : ANTOINE YESSAYAN (THANK YOU TONIO)
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
!| LUNIT          |-->| LOGICAL INUT OF FILE TO BE READ
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: LUNIT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      EOF = .TRUE.
!
      READ ( UNIT=LUNIT , ERR=100 , END=100 )
!
      EOF = .FALSE.
!
100   CONTINUE
!
      BACKSPACE ( UNIT = LUNIT )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
