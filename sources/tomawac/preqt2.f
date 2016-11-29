!                    *****************
                     SUBROUTINE PREQT2
!                    *****************
!
     &( TETA  , NPLAN , BDISPB, BDSSPB, NBD , INDI )
!
!***********************************************************************
! TOMAWAC   V6P1                                   23/06/2011
!***********************************************************************
!
!brief    SOURCE TERM RELATED TO NON-LINEAR INTERACTIONS
!+                BETWEEN FREQUENCY TRIPLETS.
!+                DEVELOPED FROM THE BOUSSINESQ EQUATIONS.
!
!history  EDF/DER/LNH
!+        11/06/98
!+        V5P0
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
!+        23/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BDISPB         |-->| LOWER DIRECTIONAL BOUNDARY OF THE SPB MODEL
!|                |   | (TRIADS INTERACTION)
!| BDSSPB         |-->| UPPER DIRECTIONAL BOUNDARY OF THE SPB MODEL
!|                |   | (TRIADS INTERACTION)
!| INDI           |<--| CONFIGURATION INDEX
!| NBD            |<--| NUMBER OF CONFIGURATIONS
!| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
!| TETA           |-->| DISCRETIZED DIRECTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_PREQT2 => PREQT2
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)          :: NPLAN
      INTEGER, INTENT(INOUT)       :: INDI(NPLAN), NBD
      DOUBLE PRECISION, INTENT(IN) :: BDISPB , BDSSPB
      DOUBLE PRECISION, INTENT(IN) :: TETA(NPLAN)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER           IPL
      DOUBLE PRECISION  AP2 , DTETA
!
      INTEGER           NBPL , NBPU, NB1
!
!     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
!
      DTETA = TETA(2)-TETA(1)
      IF(BDSSPB.GE.BDISPB) THEN
        AP2  = (BDISPB-TETA(1))/DTETA
        NBPL = NINT(AP2) + 1
        AP2  = (BDSSPB-TETA(1))/DTETA
        NBPU = NINT(AP2)
        NBD=NBPU-NBPL+1
        DO IPL=1,NBD
          INDI(IPL)=NBPL+IPL-1
        END DO
      ELSE
        AP2  = (BDSSPB-TETA(1))/DTETA
        NBPU = NINT(AP2)  + 1
        AP2  = (BDISPB-TETA(1))/DTETA
        NBPL = NINT(AP2)
        IF(NBPL.GT.NPLAN) THEN
          NBPL = 1
          INDI(1) = 1
          NBD  = NBPU - NBPL + 1
!          ALLOCATE(INDI(1:NBD))
          DO IPL = 2,NBD
            INDI(IPL)=IPL
          END DO
        ELSE
          NB1 = NPLAN - NBPL + 1
          NBD = NB1 + NBPU
!         ALLOCATE(INDI(1:NBD))
          DO IPL = 1,NB1
            INDI(IPL)=NBPL+IPL-1
          END DO
          DO IPL = 1,NBPU
            INDI(IPL+NB1)=IPL
          END DO
        ENDIF
      ENDIF
!     
      RETURN
      END
