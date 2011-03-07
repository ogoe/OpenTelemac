!                    *****************
                     SUBROUTINE CQUEUE
!                    *****************
!
     &( NF    , RAISF , TAILF , JFRE  , JBIS  , COEF1 )
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    ADJUSTS FREQUENCY INDICES AND COMPUTES TAIL
!
!note     THE SPECTRUM IS ASSUMED TO BE 0 FOR FREQUENCIES LOWER THAN
!+          THE FIRST DISCRETISED FREQUENCY.
!note   BEYOND THE LAST DISCRETISED FREQUENCY THE SPECTRUM IS
!+          ASSUMED TO DECREASE FOLLOWING A FREQ**(-TAILF) LAW.
!
!history  M. BENOIT
!+        26/06/96
!+        V1P2
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
!| COEF1          |---|
!| JBIS           |---|
!| JFRE           |-->| INDICE FREQUENTIEL DE LA COMPOSANTE FREQ.
!| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
!| RAISF          |-->| RAISON FREQUENTIELLE DE DISCRETISATION
!| TAILF          |-->| FACTEUR DE QUEUE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """""""""""""""""""""
      INTEGER  NF    , JFRE  , JBIS
      DOUBLE PRECISION RAISF , TAILF , COEF1
!
!
      IF (JFRE.GT.NF) THEN
        JBIS = NF
        COEF1= 1.D0/RAISF**(DBLE(JFRE-NF)*TAILF)
      ELSEIF (JFRE.LT.1) THEN
        JBIS = 1
        COEF1= 0.D0
      ELSE
        JBIS = JFRE
        COEF1= 1.D0
      ENDIF
!
      RETURN
      END