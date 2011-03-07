!                    ***************
                     FUNCTION FONCRO
!                    ***************
!
     &( X     , B     , N     , A     , XM    )
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| A              |-->| PARAMETRE A DE LA FONCTION A EVALUER
!| B              |-->| PARAMETRE B DE LA FONCTION A EVALUER
!| N              |-->| EXPOSANT N  DE LA FONCTION A EVALUER
!| X              |-->| VALEUR A LAQUELLE LA FONCTION EST EVALUEE
!| XM             |-->| PARAMETRE M DE LA FONCTION A EVALUER
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