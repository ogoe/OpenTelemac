!                    *********************
                     SUBROUTINE TOM_CORFON
!                    *********************
!
!
!***********************************************************************
! TOMAWAC   V6P1                                   14/06/2011
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!
!history  F. MARCOS
!+
!+
!+
!
!history  OPTIMER (    )
!+        12/01/2001
!+
!+   TOMAWAC/COWADIS MERGE
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
      USE DECLARATIONS_TOMAWAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!
!EGR---------------------------------MODIF debut
      INTEGER IP
!
      DO IP=1,NPOIN2
        IF (X(IP).GE.(1.56D0).AND.X(IP).LE.(2.78D0)) THEN
          ZF(IP)=-0.75+(30.D0/122.D0)*(X(IP)-1.56D0)
        ELSEIF (X(IP).GT.(2.78D0).AND.X(IP).LE.(5.22D0)) THEN
          ZF(IP)=-0.45D0
        ELSEIF (X(IP).GT.(5.22D0).AND.X(IP).LE.(6.44D0)) THEN
          ZF(IP)=-0.45-(30.D0/122.D0)*(X(IP)-5.22D0)
        ELSE
          ZF(IP)=-0.75D0
        ENDIF
      ENDDO
!EGR---------------------------------MODIF fin
!
      RETURN
      END

