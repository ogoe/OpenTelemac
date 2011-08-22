!                    ************************
                     SUBROUTINE FLUX_EF_VF_3D
!                    ************************
!
     &(FLOW,W2D,W3D,NSEG2D,NSEG3D,NELEM2,NELEM3,MESH2D,MESH3D,INIFLO,
     & IOPT,SENS)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SUMS UP 3D NON-ASSEMBLED FLUXES ON THE VERTICAL
!+                THEN COMPUTES THE 2D SEGMENT FLUXES.
!+
!+           (LEO POSTMA'S METHOD).
!
!history
!+        15/05/2009
!+        V6P0
!+   INSPIRED FROM LEO POSTMA (DELTARES)
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
!| FLOW           |<--| FLUX
!| INIFLO         |-->| IF(YES) FLOW WILL BE INITIALISED AT 0.
!| IOPT           |-->| CHOICE OF THE CONSTANT IN FLUX_EF_VF
!| MESH2D         |-->| 2D MESH
!| MESH3D         |-->| 3D MESH
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NELEM3         |-->| NUMBER OF ELEMENTS IN 3D
!| NSEG2D         |-->| NUMBER OF SEGMENTS IN 2D
!| NSEG3D         |-->| NUMBER OF SEGMENTS IN 3D
!| SENS           |-->| IF 1: HORIZONTAL FLUXES FROM BOTTOM TO TOP
!|                |   | IF 2: HORIZONTAL FLUXES FROM TOP TO BOTTOM
!| W2D            |<--| NON ASSEMBLED FLUXES LEAVING POINTS,PER TRIANGLE
!| W3D            |-->| NON ASSEMBLED FLUXES LEAVING POINTS,PER PRISM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_FLUX_EF_VF_3D => FLUX_EF_VF_3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER, INTENT(IN)             :: NSEG2D,NSEG3D,NELEM2,NELEM3
!                                             *=NSEG2D*NPLAN+NPOIN2*NETAGE
      INTEGER, INTENT(IN)             :: IOPT,SENS
      DOUBLE PRECISION, INTENT(INOUT) :: FLOW(NSEG3D)
      DOUBLE PRECISION, INTENT(IN)    :: W3D(NELEM3,6)
      DOUBLE PRECISION, INTENT(INOUT) :: W2D(NELEM2,3)
      LOGICAL, INTENT(IN)             :: INIFLO
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2D,MESH3D
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER IPLAN,NPLAN,N1,N2,IELEM,ISEG
      NPLAN=NELEM3/NELEM2 + 1
!
!-----------------------------------------------------------------------
!
!     INITIALISES FLOW TO 0.D0
!
      IF(INIFLO) THEN
        DO ISEG = 1,NSEG2D*NPLAN
          FLOW(ISEG) = 0.D0
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(SENS.NE.1.AND.SENS.NE.2) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'FLUX_EF_VF_3D : SENS INCONNU = ',SENS
        IF(LNG.EQ.2) WRITE(LU,*) 'FLUX_EF_VF_3D: UNKNOWN SENS = ',SENS
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(SENS.EQ.2.AND..NOT.INIFLO) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'FLUX_EF_VF_3D : SENS = 2 ET INIFLO = .FALSE.'
          WRITE(LU,*) '                OPTIONS INCOMPATIBLES'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'FLUX_EF_VF_3D: SENS = 2 AND INIFLO = .FALSE.'
          WRITE(LU,*) '               INCOMPATIBLE OPTIONS'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     ADDS FLUXES ON HORIZONTAL SEGMENTS (FOR SEGMENT NUMBERING IN 3D
!                                         SEE STOSEG41 IN BIEF)
!
      DO IPLAN=1,NPLAN
!
!       POINTS 1, 2 AND 3 OF UPPER LEVEL
        IF(IPLAN.EQ.1) THEN
!         FIRST PLANE: ONLY POINTS 1, 2 AND 3 OF UPPER LEVEL
          DO IELEM=1,NELEM2
            W2D(IELEM,1)=W3D(IELEM,1)
            W2D(IELEM,2)=W3D(IELEM,2)
            W2D(IELEM,3)=W3D(IELEM,3)
          ENDDO
        ELSEIF(IPLAN.EQ.NPLAN) THEN
!         LAST PLANE: ONLY POINTS 4, 5 AND 6 OF LOWER LEVEL
          N1=NELEM2*(IPLAN-2)
          DO IELEM=1,NELEM2
            W2D(IELEM,1)=W3D(N1+IELEM,4)
            W2D(IELEM,2)=W3D(N1+IELEM,5)
            W2D(IELEM,3)=W3D(N1+IELEM,6)
          ENDDO
        ELSE
!       INTERMEDIATE PLANE
!         POINTS 4, 5 AND 6 OF LOWER LEVEL + 1, 2, 3 OF UPPER LEVEL
          N1=NELEM2*(IPLAN-2)
          N2=N1+NELEM2
          DO IELEM=1,NELEM2
            W2D(IELEM,1)=W3D(N1+IELEM,4)+W3D(N2+IELEM,1)
            W2D(IELEM,2)=W3D(N1+IELEM,5)+W3D(N2+IELEM,2)
            W2D(IELEM,3)=W3D(N1+IELEM,6)+W3D(N2+IELEM,3)
          ENDDO
        ENDIF
        IF(SENS.EQ.1) THEN
          N1=(IPLAN-1    )*NSEG2D+1
          N2= IPLAN         *NSEG2D
        ELSE
          N1=(NPLAN-IPLAN)*NSEG2D+1
          N2=(1+NPLAN-IPLAN)*NSEG2D
        ENDIF
        CALL FLUX_EF_VF(FLOW(N1:N2),W2D,NSEG2D,NELEM2,
     &                  MESH2D%ELTSEG%I,MESH2D%ORISEG%I,
     &                  MESH2D%IKLE%I,.FALSE.,IOPT)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
