!                    *****************
                     SUBROUTINE VISCKO
!                    *****************
!
     &(VISCVI,VISCTA,ROTAT,AK,EP,NTRAC,CMU,DNUVIH,DNUVIV,DNUTAH,DNUTAV)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE TURBULENT VISCOSITY
!+                AND TURBULENT THERMAL DIFFUSIVITY
!+                ACCORDING TO K AND EPSILON.
!
!history  HOLGER WEILBEER   ISEB/UHA
!+        25/11/97
!+        V5P3
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  HOLGER WEILBEER   ISEB/UHA
!+        **/02/01
!+
!+   K-OMEGA
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
!| AK             |-->| TURBULENT ENERGY
!| CMU            |-->| CONSTANT FOR K-EPSILON MODEL
!| DNUTAH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF TRACER
!| DNUTAV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF TRACER
!| DNUVIH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF VELOCITIES
!| DNUVIV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF VELOCITIES
!| EP             |-->| TURBULENT DISSIPATION
!| NTRAC          |-->| NUMBER OF TRACERS
!| ROTAT          |-->| KIND OF L1 NORM OF VORTICITY
!| VISCTA         |<->| TURBULENT VISCOSITY COEFFICIENTS FOR TRACERS
!| VISCVI         |<->| TURBULENT VISCOSITY COEFFICIENTS FOR VELOCITIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NTRAC
      DOUBLE PRECISION, INTENT(IN) :: CMU
      DOUBLE PRECISION, INTENT(IN) :: DNUVIH, DNUVIV
      DOUBLE PRECISION, INTENT(IN) :: DNUTAH, DNUTAV
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: VISCVI, VISCTA
      TYPE(BIEF_OBJ), INTENT(IN)   :: ROTAT, AK, EP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC
!
!***********************************************************************
!
! VISCVI%ADR(1) AND VISCVI%ADR(2) ARE USED AS TEMPORARY WORK FIELDS
!
      CALL OS('X=CY    ',X=VISCVI%ADR(1)%P,Y=AK,C=0.3D0)
      CALL OS('X=CY    ',X=VISCVI%ADR(2)%P,Y=EP,C=0.3D0)
      CALL OS('X=+(Y,Z)',X=VISCVI%ADR(2)%P,Y=VISCVI%ADR(2)%P,Z=ROTAT)
!
      CALL OS('X=CY/Z  ',X=VISCVI%ADR(3)%P,
     &                   Y=VISCVI%ADR(1)%P,Z=VISCVI%ADR(2)%P,C=1.D0)
!
!-----------------------------------------------------------------------
!
      CALL OS('X=Y+C   ',X=VISCVI%ADR(1)%P,Y=VISCVI%ADR(3)%P,C=DNUVIH)
      CALL OS('X=Y     ',X=VISCVI%ADR(2)%P,Y=VISCVI%ADR(1)%P)
      CALL OS('X=X+C   ',X=VISCVI%ADR(3)%P,C=DNUVIV)
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC = 1,NTRAC
!
! OLIVIER: TURBULENT PRANDTL CLOSURE = 1.0
!
          CALL OS('X=CY    ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                       Y=VISCVI%ADR(3)%P,C=1.D0)
          CALL OS('X=Y+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(1)%P,
     &                       Y=VISCTA%ADR(ITRAC)%P%ADR(3)%P,C=DNUTAH)
          CALL OS('X=Y     ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                       Y=VISCTA%ADR(ITRAC)%P%ADR(1)%P)
          CALL OS('X=X+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,C=DNUTAV)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE VISCKO
