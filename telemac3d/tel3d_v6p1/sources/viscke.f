!                    *****************
                     SUBROUTINE VISCKE
!                    *****************
!
     &(VISCVI,VISCTA,AK,EP,NTRAC,CMU,
     & DNUVIH,DNUVIV,DNUTAH,DNUTAV,KMIN,EMIN,ITURBH,ITURBV,PRANDTL)
!
!***********************************************************************
! TELEMAC3D   V6P1                                  21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE TURBULENT VISCOSITY
!+                AND TURBULENT THERMAL DIFFUSIVITY
!+                ACCORDING TO K AND EPSILON.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history
!+
!+        V6P0
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
!| AK             |-->| TURBULENT ENERGY
!| CMU            |-->| CONSTANT FOR K-EPSILON MODEL
!| DNUTAH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF TRACER
!| DNUTAV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF TRACER
!| DNUVIH         |-->| COEFFICIENT FOR HORIZONTAL DIFFUSION OF VELOCITIES
!| DNUVIV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF VELOCITIES
!| EMIN           |-->| MINIMUM VALUE OF EPSILON FOR CLIPPING
!| EP             |-->| TURBULENT DISSIPATION
!| ITURBH         |-->| HORIZONTAL TURBULENCE MODEL (3= K-EPSILON)
!| ITURBV         |-->| VERTICAL TURBULENCE MODEL (3= K-EPSILON)
!| KMIN           |-->| MINIMUM VALUE OF K FOR CLIPPING
!| NTRAC          |-->| NUMBER OF TRACERS
!| PRANDTL        |-->| PRANDTL CONSTANT
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
      INTEGER, INTENT(IN)          :: NTRAC,ITURBH,ITURBV
      DOUBLE PRECISION, INTENT(IN) :: CMU,PRANDTL
      DOUBLE PRECISION, INTENT(IN) :: DNUVIH, DNUVIV
      DOUBLE PRECISION, INTENT(IN) :: DNUTAH, DNUTAV,KMIN,EMIN
      TYPE(BIEF_OBJ), INTENT(INOUT):: VISCVI, VISCTA
      TYPE(BIEF_OBJ), INTENT(IN)   :: AK,EP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC,I,NPOIN3
!
!***********************************************************************
!
      NPOIN3 = AK%DIM1
!
      DO I=1,NPOIN3
!
        IF(EP%R(I).GT.1.1D0*EMIN) THEN
          VISCVI%ADR(3)%P%R(I)=CMU*AK%R(I)**2/EP%R(I)
        ELSE
!         IF EPSILON IS NEAR TO CLIP VALUE, NO TURBULENCE, WHATEVER K
          VISCVI%ADR(3)%P%R(I)=0.D0
        ENDIF
!
      ENDDO
!
!     HORIZONTAL DIFFUSION OF VELOCITIES
!
      IF(ITURBH.EQ.3) THEN
        CALL OS('X=Y+C   ',X=VISCVI%ADR(1)%P,Y=VISCVI%ADR(3)%P,C=DNUVIH)
        CALL OS('X=Y     ',X=VISCVI%ADR(2)%P,Y=VISCVI%ADR(1)%P)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     TRACERS
!
      IF(NTRAC.GT.0) THEN
        IF(ABS(PRANDTL-1.D0).LT.1.D-4) THEN
!         HERE PRANDTL TURBULENT = 1.0
          DO ITRAC = 1,NTRAC
            CALL OS('X=Y+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                         Y=VISCVI%ADR(3)%P,C=DNUTAV)
          ENDDO
        ELSE
          DO ITRAC = 1,NTRAC
            DO I=1,NPOIN3
              VISCTA%ADR(ITRAC)%P%ADR(3)%P%R(I)=
     &        VISCVI%ADR(3)%P%R(I)/PRANDTL + DNUTAV
            ENDDO
          ENDDO
        ENDIF
        IF(ITURBH.EQ.3) THEN
          IF(ABS(PRANDTL-1.D0).LT.1.D-4) THEN
!           HERE PRANDTL TURBULENT = 1.0
            DO ITRAC = 1,NTRAC
              CALL OS('X=Y+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(1)%P,
     &                           Y=VISCVI%ADR(3)%P,C=DNUTAH)
              CALL OS('X=Y     ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                           Y=VISCTA%ADR(ITRAC)%P%ADR(1)%P)
            ENDDO
          ELSE
            DO ITRAC = 1,NTRAC
              DO I=1,NPOIN3
                VISCTA%ADR(ITRAC)%P%ADR(1)%P%R(I)=
     &          VISCVI%ADR(3)%P%R(I)/PRANDTL + DNUTAH
              ENDDO
              CALL OS('X=Y     ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                           Y=VISCTA%ADR(ITRAC)%P%ADR(1)%P)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
!     FINAL VALUE OF VERTICAL DIFFUSION FOR VELOCITIES
!
      CALL OS('X=X+C   ',X=VISCVI%ADR(3)%P,C=DNUVIV)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
