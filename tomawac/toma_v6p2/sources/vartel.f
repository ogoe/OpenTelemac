!                    *****************
                     SUBROUTINE VARTEL
!                    *****************
!
     &( VAR, X, Y, DEPTH, UC, VC, ZREPOS, TRA01, F, NPLAN, NF, NPOIN2)
!
!***********************************************************************
! TOMAWAC   V6P1                                   29/06/2011
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
!history  G.MATTAROLO (EDF - LNHE)
!+        29/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEPTH          |<->| WATER DEPTH
!| F              |<->| DIRECTIONAL SPECTRUM
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| TRA01          |<->| WORK TABLE
!| UC             |-->| CURRENT VELOCITY ALONG X AT THE MESH POINTS
!| VC             |-->| CURRENT VELOCITY ALONG Y AT THE MESH POINTS
!| VAR            |-->| VARIABLE VALUES READ FROM TELEMAC FILE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| ZREPOS         |<->| INITIAL STILL WATER LEVEL
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
