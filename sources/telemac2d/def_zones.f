!                    ********************
                     SUBROUTINE DEF_ZONES
!                    ********************
!
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DEFINES ZONES IN THE MESH. THE RESULT MUST BE :
!+
!+            NZONE : THE NUMBER OF ZONES,
!+
!+            ZONE : STRUCTURE OF SIZE NPOIN STATING THE ZONE NUMBER
!+                       OF EVERY POINT.
!
!warning  USER SUBROUTINE; DOES NOTHING BY DEFAULT
!
!history  J-M HERVOUET
!+        17/08/2001
!+        V5P3
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
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     TO BE REMOVED IF IMPLEMENTED FOR A SPECIFIC CASE
!     THIS JUST TO WARN IN CASE A USER FORGETS TO DO IT
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LE SOUS-PROGRAMME DEF_ZONES DOIT ETRE INCLUS DANS'
        WRITE(LU,*) 'LE FICHIER FORTRAN POUR Y DEFINIR LES ZONES'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'SUBROUTINE DEF_ZONES MUST BE INCLUDED'
        WRITE(LU,*) 'IN THE FORTRAN FILE TO DEFINE ZONES'
      ENDIF
      CALL PLANTE(1)
      STOP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     NZONE = ???
!     ZONE%I(I) = ???
!
!-----------------------------------------------------------------------
!
      RETURN
      END
