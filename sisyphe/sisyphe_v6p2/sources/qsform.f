!                    *****************
                     SUBROUTINE QSFORM
!                    *****************
!
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    ALLOWS THE USER TO CODE THEIR OWN BEDLOAD TRANSPORT
!+                FORMULATION, BEST SUITED TO THEIR APPLICATION.
!
!warning  USER SUBROUTINE; SAND TRANSPORT FORMULA MUST BE CODED BY THE USER
!
!history
!+        20/05/1996
!+
!+
!
!history  F. HUVELIN
!+        **/11/2003
!+        V5P4
!+   MODIFIED
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
      USE DECLARATIONS_SISYPHE
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!  FOLLOWING LINES NEED TO BE COMMENTED OUT
!
      IF(LNG.EQ.1) WRITE(LU,52)
      IF(LNG.EQ.2) WRITE(LU,53)
!
52    FORMAT(/,1X,' STOP :',/
     &     ,1X,' LE TAUX DE TRANSPORT DOIT ETRE
     &       CALCULE DANS QSFORM')
53    FORMAT(/,1X,'SISYPHE IS STOPPED : ',/
     &      ,1X,' SAND TRANSPORT MUST BE CALCULATED IN QSFORM')
      CALL PLANTE(1)
      STOP
!
      RETURN
      END
