!                    ***************
                     FUNCTION FONCRO
!                    ***************
!
     &( X     , B     , N     , A     , XM    )
!
!***********************************************************************
! TOMAWAC   V6P1                                   14/06/2011
!***********************************************************************
!
!brief    COMPUTES THE VALUE OF THE FUNCTION TO BE INTEGRATED
!+                FOR WAVE BREAKING (ROELVINK, 1993).
!
!history  F. BECQ (EDF/DER/LNH)
!+        26/03/96
!+        V1P1
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
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| PARAMETER A OF THE FUNCTION TO BE INTEGRATED
!| B              |-->| PARAMETER B OF THE FUNCTION TO BE INTEGRATED
!| N              |-->| EXPONENT N OF THE FUNCTION TO BE INTEGRATED
!| X              |-->| VALUE AT WHICH THE FUNCTION IS EVALUATED
!| XM             |-->| PARAMETER M OF THE FUNCTION TO BE INTEGRATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!     VARIABLES IN ARGUMENT
!     """""""""""""""""""""
      INTEGER  N
      DOUBLE PRECISION X      , B     , A     , XM    , FONCRO
!
!     LOCAL VARIABLES
!     """"""""""""""""""
      DOUBLE PRECISION AUX
!
!
      AUX   = A*X**XM
      FONCRO= XM*AUX*DEXP(-AUX)*(1.D0-DEXP(-(B*X)**N))
!
      RETURN
      END
