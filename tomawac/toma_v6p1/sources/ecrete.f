!                    *****************
                     SUBROUTINE ECRETE
!                    *****************
!
     &( F     , DEPTH , NPOIN2, NPLAN , NF    , PROMIN)
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE VARIANCE SPECTRUM (SETS IT TO 0) AT
!+                ALL THE NODES WHERE THE DEPTH OF WATER IS LESS
!+                THAN PROMIN.
!
!history  M. BENOIT (EDF LNHE)
!+        19/01/2004
!+        V5P4
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
!| DEPTH          |---| 
!| F              |---| 
!| NF             |-->| NOMBRE DE FREQUENCES
!| NPLAN          |-->| NOMBRE DE DIRECTIONS
!| NPOIN2         |-->| NOMBRE DE POINTS 2D
!| PROMIN         |-->| PROFONDEUR MINIMALE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER          NPOIN2 , NPLAN, NF
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF), DEPTH(NPOIN2)
      DOUBLE PRECISION PROMIN
      INTEGER          IP    , JP    , JF
!
      DO IP=1,NPOIN2
        IF (DEPTH(IP).LT.PROMIN) THEN
          DO JF=1,NF
            DO JP=1,NPLAN
              F(IP,JP,JF)=0.0D0
            ENDDO
          ENDDO
        ENDIF
      ENDDO
!
      RETURN
      END