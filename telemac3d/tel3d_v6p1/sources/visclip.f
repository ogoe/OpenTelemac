!                    ******************
                     SUBROUTINE VISCLIP
!                    ******************
!
     & (VISCVI,VISCTA,H,NPLAN,NPOIN3,NPOIN2,NTRAC)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    LIMITS VISCOSITY ON TIDAL FLATS.
!
!history  J.-M. HERVOUET
!+        23/03/2004
!+        V5P5
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
!| H              |---|
!| NPLAN          |-->| NOMBRE DE PLANS DU MAILLAGE 3D
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
!| NPOIN3         |-->| NOMBRE DE POINTS DU MAILLAGE 3D
!| NTRAC          |-->| NOMBRE DE TRACEURS ACTIFS
!| VISCTA         |<--| VISCOSITE DYNAMIQUE DES TRACEURS
!| VISCVI         |<--| VISCOSITE DYNAMIQUE DE LA VITESSE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NPOIN3, NPOIN2, NPLAN
      INTEGER, INTENT(IN)            :: NTRAC
      TYPE (BIEF_OBJ), INTENT(INOUT) :: VISCVI, VISCTA
      TYPE (BIEF_OBJ), INTENT(IN)    :: H
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPLAN,I,ITRAC
      DOUBLE PRECISION HLIM,COR
      DATA HLIM /0.2D0/
!
!***********************************************************************
!
!     LIMITS TURBULENT VISCOSITY ON TIDAL FLATS
!
      DO I=1,NPOIN2
        IF(H%R(I).LT.HLIM) THEN
          COR=(MAX(H%R(I),0.D0)/HLIM)**2
          DO IPLAN=1,NPLAN
            VISCVI%ADR(1)%P%R(I+(IPLAN-1)*NPOIN2)=
     &      VISCVI%ADR(1)%P%R(I+(IPLAN-1)*NPOIN2)*COR
            VISCVI%ADR(2)%P%R(I+(IPLAN-1)*NPOIN2)=
     &      VISCVI%ADR(2)%P%R(I+(IPLAN-1)*NPOIN2)*COR
            VISCVI%ADR(3)%P%R(I+(IPLAN-1)*NPOIN2)=
     &      VISCVI%ADR(3)%P%R(I+(IPLAN-1)*NPOIN2)*COR
          ENDDO
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
!
        DO ITRAC=1,NTRAC
!
          DO I=1,NPOIN2
            IF(H%R(I).LT.HLIM) THEN
              COR=(MAX(H%R(I),0.D0)/HLIM)**2
              DO IPLAN=1,NPLAN
                VISCTA%ADR(ITRAC)%P%ADR(1)%P%R(I+(IPLAN-1)*NPOIN2)=
     &          VISCTA%ADR(ITRAC)%P%ADR(1)%P%R(I+(IPLAN-1)*NPOIN2)*COR
                VISCTA%ADR(ITRAC)%P%ADR(2)%P%R(I+(IPLAN-1)*NPOIN2)=
     &          VISCTA%ADR(ITRAC)%P%ADR(2)%P%R(I+(IPLAN-1)*NPOIN2)*COR
                VISCTA%ADR(ITRAC)%P%ADR(3)%P%R(I+(IPLAN-1)*NPOIN2)=
     &          VISCTA%ADR(ITRAC)%P%ADR(3)%P%R(I+(IPLAN-1)*NPOIN2)*COR
              ENDDO
            ENDIF
          ENDDO
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END