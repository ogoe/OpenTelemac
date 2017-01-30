!                    *****************
                     SUBROUTINE TOM_CORFON
!                    *****************
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
      INTEGER IP
      DOUBLE PRECISION DMIN, XMIN
!
!     tmptmp : permet de passer le fond en real pour comparer avec le fichier geom
      REAL TMPTMP
!
!.....BATHYMETRIE EN PENTE VARIABLE DU CAS-TEST EN CANAL A HOULE BJ15
!.....DE BATTJES ET JANSSEN (1978) - CAS AVEC UNE BARRE.
!     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      DMIN = 0.05D0
      XMIN = 20.D0*DMIN
      DO IP=1,NPOIN2
        IF(X(IP).GE.19.02D0) THEN
           TMPTMP = -0.616
        ELSEIF(X(IP).GE.9.02D0) THEN
            TMPTMP= -0.616 + 0.0495*(19.02-X(IP))
        ELSEIF(X(IP).GE.4.62D0) THEN
           TMPTMP = -0.120 - 0.025*(9.02-X(IP))
        ELSEIF(X(IP).GE.XMIN) THEN
           TMPTMP= -0.230 + 0.05D0*(4.62-X(IP))
        ELSE
            TMPTMP= -DMIN
        ENDIF
        ZF(IP)=TMPTMP
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

