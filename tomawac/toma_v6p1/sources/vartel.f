!                    *****************
                     SUBROUTINE VARTEL
!                    *****************
!
     &( VAR, X, Y, DEPTH, UC, VC, ZREPOS, TRA01, F, NPLAN, NF, NPOIN2)
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    ALLOWS THE USE OF A VARIABLE READ FROM A TELEMAC FILE.
!
!history  F. MARCOS (LNH)
!+        09/06/95
!+        V1P0
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
!| DEPTH          |<->| HAUTEUR D'EAU
!| F              |<->| SPECTRE DE VARIANCE
!| NF             |-->| NOMBRE DE FREQUENCES
!| NPLAN          |-->| NOMBRE DE DIRECTIONS
!| NPOIN2         |-->| NOMBRE DE POINTS 2D
!| TRA01          |<->| TABLEAU DE TRAVAIL
!| UC,VC          |<->| CHAMPS DE COURANT
!| VAR            |-->| VARIABLE RECUPEREE DANS LE FICHIER TELEMAC
!| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE 2D
!| ZREPOS         |<->| COTE INITIALE DU PLAN D'EAU AU REPOS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER  NPOIN2,NPLAN,NF
!
      DOUBLE PRECISION F (NPOIN2,NPLAN,NF) , TRA01(NPOIN2)
      DOUBLE PRECISION X (NPOIN2) , Y (NPOIN2) , DEPTH(NPOIN2)
      DOUBLE PRECISION UC(NPOIN2) , VC(NPOIN2) , VAR(NPOIN2)
      DOUBLE PRECISION ZREPOS
!
!-----------------------------------------------------------------------
!
!     USE THE VARIABLE 'VAR' TO INITIALISES THE ARRAYS
!
!     FOR EXAMPLE :
!
!     CALL OV( 'X=Y     ' , DEPTH , VAR , X , 0.D0 , NPOIN2)
!
      RETURN
      END