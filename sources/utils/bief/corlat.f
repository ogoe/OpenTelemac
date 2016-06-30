!                    *****************
                     SUBROUTINE CORLAT
!                    *****************
!
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE LATITUDE OF THE POINTS IN THE MESH.
!+
!+            CASE.
!
!warning  USER SUBROUTINE; LINES COMMENTED WITH 'CEX' ARE AN EXAMPLE IN TELEMAC-2D
!code
!+  EXAMPLE : MULTIPLIES BY A CONSTANT
!+
!+    DO I = 1 , NPOIN
!+      X(I) = X(I) * 1.D0
!+      Y(I) = Y(I) * 1.D0
!+    ENDDO
!
!history  J-M HERVOUET
!+        01/03/90
!+        V5P1
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CORLAT => CORLAT
!EX   USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO
!     THE CALLING PROGRAM AND THE NEEDED MODIFICATION
!     BY ADDING USE DECLARATIONS_"NAME OF CALLING CODE"
!     ALL THE DATA STRUCTURE OF THIS CODE IS AVAILABLE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!EX   INTEGER I
!
!-----------------------------------------------------------------------
!
!  EXAMPLE : MULTIPLIES BY A CONSTANT
!
!EX   DO I = 1 , NPOIN
!EX     X(I) = X(I) * 1.D0
!EX     Y(I) = Y(I) * 1.D0
!EX   ENDDO
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CORLAT : PAS DE MODIFICATION DE LA LATITUDE'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CORLAT :NO MODIFICATION OF LATITUDE'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
