!                    ************************************
                     SUBROUTINE BYPASS_CRUSHED_POINTS_EBE
!                    ************************************
!
     &(VOLU,SVOLU,VOLUN,SVOLUN,FLUX,TRA01,MESH2,MESH3,
     & NPOIN3,NELEM2,NELEM3,NPLAN,IKLE)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BYPASSES FLUXES TO POINTS THAT WILL REMAIN WITH
!+                A ZERO VOLUME.
!+
!+            FLUX IS CONVEYED TO UPPER LAYER THROUGH A VERTICAL.
!+
!+            THIS AVOIDS USELESS ITERATIONS.
!
!warning  HERE FLUXES ARE FROM POINT 2 TO POINT 1.
!+            SEE FLUX3D (HORIZONTAL FLUXES BASED ON FLUINT)
!+            AND PRECON (VERTICAL FLUXES BASED ON WSCONV)
!
!history  J-M HERVOUET (LNHE)
!+        20/04/2010
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
!| FLUX           |<->| FLUXES TO BE CHANGED
!| IKLE           |-->| CONNECTIVITY TABLE
!| MESH2          |<->| 2D MESH
!| MESH3          |<->| 3D MESH
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NELEM3         |-->| NUMBER OF ELEMENTS IN 3D
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| SVOLU          |-->| BIEF_OBJ STRUCTURE, WITH SVOLU%R=VOLU
!| SVOLUN         |-->| BIEF_OBJ STRUCTURE, WITH SVOLUN%R=VOLUN
!| TRA01          |<->| WORK BIEF_OBJ STRUCTURE
!| VOLU           |-->| VOLUME AROUND POINTS AT TIME N+1
!| VOLUN          |-->| VOLUME AROUND POINTS AT TIME N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN3,NELEM3,NELEM2,NPLAN
      INTEGER, INTENT(IN)             :: IKLE(NELEM3,6)
!
      DOUBLE PRECISION, INTENT(IN)    :: VOLUN(NPOIN3),VOLU(NPOIN3)
!
      TYPE(BIEF_OBJ), INTENT(IN)      :: SVOLU,SVOLUN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TRA01
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2,MESH3
!
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(30,NELEM3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM2,IELEM3,I,I1,I2,I3,I4,I5,I6,IPLAN
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION EPS_VOLUME
      DATA EPS_VOLUME /1.D-14/
!
!-----------------------------------------------------------------------
!
!     TRA01=VOLU+VOLUN=0 MEANS THAT BOTH VOLU AND VOLUN ARE EQUAL TO 0
!
      CALL OS('X=Y+Z   ',X=TRA01,Y=SVOLU,Z=SVOLUN)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(TRA01,2,MESH3)
      ENDIF
!
!     GROUPS FLUXES
!
      DO IELEM3=1,NELEM3
        DO I=1,15
          FLUX(I,IELEM3)=FLUX(I,IELEM3)-FLUX(I+15,IELEM3)
        ENDDO
      ENDDO
!
!     BYPASSES FLUXES
!
      DO IELEM2=1,NELEM2
        DO IPLAN=1,NPLAN-1
          IELEM3=IELEM2+(IPLAN-1)*NELEM2
          I1=IKLE(IELEM3,1)
          I2=IKLE(IELEM3,2)
          I3=IKLE(IELEM3,3)
!
          IF(TRA01%R(I1).LT.EPS_VOLUME.OR.
     &       TRA01%R(I2).LT.EPS_VOLUME.OR.
     &       TRA01%R(I3).LT.EPS_VOLUME    ) THEN
!
          I4=IKLE(IELEM3,4)
          I5=IKLE(IELEM3,5)
          I6=IKLE(IELEM3,6)
!
!         CROSSED SEGMENTS
!
!         ISSUED FROM 1
          IF(TRA01%R(I1).LT.EPS_VOLUME.OR.
     &       TRA01%R(I5).LT.EPS_VOLUME) THEN
            FLUX(13,IELEM3)=FLUX(13,IELEM3)+FLUX(04,IELEM3)
            FLUX(03,IELEM3)=FLUX(03,IELEM3)+FLUX(04,IELEM3)
            FLUX(04,IELEM3)=0.D0
          ENDIF
          IF(TRA01%R(I1).LT.EPS_VOLUME.OR.
     &       TRA01%R(I6).LT.EPS_VOLUME) THEN
            FLUX(14,IELEM3)=FLUX(14,IELEM3)+FLUX(05,IELEM3)
            FLUX(03,IELEM3)=FLUX(03,IELEM3)+FLUX(05,IELEM3)
            FLUX(05,IELEM3)=0.D0
          ENDIF
!         ISSUED FROM 2
          IF(TRA01%R(I2).LT.EPS_VOLUME.OR.
     &       TRA01%R(I4).LT.EPS_VOLUME) THEN
            FLUX(13,IELEM3)=FLUX(13,IELEM3)-FLUX(07,IELEM3)
            FLUX(08,IELEM3)=FLUX(08,IELEM3)+FLUX(07,IELEM3)
            FLUX(07,IELEM3)=0.D0
          ENDIF
          IF(TRA01%R(I2).LT.EPS_VOLUME.OR.
     &       TRA01%R(I6).LT.EPS_VOLUME) THEN
            FLUX(15,IELEM3)=FLUX(15,IELEM3)+FLUX(09,IELEM3)
            FLUX(08,IELEM3)=FLUX(08,IELEM3)+FLUX(09,IELEM3)
            FLUX(09,IELEM3)=0.D0
          ENDIF
!         ISSUED FROM 3
          IF(TRA01%R(I3).LT.EPS_VOLUME.OR.
     &       TRA01%R(I4).LT.EPS_VOLUME) THEN
            FLUX(14,IELEM3)=FLUX(14,IELEM3)-FLUX(10,IELEM3)
            FLUX(12,IELEM3)=FLUX(12,IELEM3)+FLUX(10,IELEM3)
            FLUX(10,IELEM3)=0.D0
          ENDIF
          IF(TRA01%R(I3).LT.EPS_VOLUME.OR.
     &       TRA01%R(I5).LT.EPS_VOLUME) THEN
            FLUX(15,IELEM3)=FLUX(15,IELEM3)-FLUX(11,IELEM3)
            FLUX(12,IELEM3)=FLUX(12,IELEM3)+FLUX(11,IELEM3)
            FLUX(11,IELEM3)=0.D0
          ENDIF
!
!         LOWER HORIZONTAL SEGMENTS
!
          IF(TRA01%R(I1).LT.EPS_VOLUME) THEN
!           ISSUED FROM 1
            FLUX(13,IELEM3)=FLUX(13,IELEM3)+FLUX(01,IELEM3)
            FLUX(14,IELEM3)=FLUX(14,IELEM3)+FLUX(02,IELEM3)
            FLUX(03,IELEM3)=FLUX(03,IELEM3)+FLUX(01,IELEM3)
     &                                     +FLUX(02,IELEM3)
            FLUX(08,IELEM3)=FLUX(08,IELEM3)-FLUX(01,IELEM3)
            FLUX(12,IELEM3)=FLUX(12,IELEM3)-FLUX(02,IELEM3)
            FLUX(01,IELEM3)=0.D0
            FLUX(02,IELEM3)=0.D0
          ENDIF
          IF(TRA01%R(I2).LT.EPS_VOLUME) THEN
!           ISSUED FROM 2
            FLUX(13,IELEM3)=FLUX(13,IELEM3)+FLUX(01,IELEM3)
            FLUX(15,IELEM3)=FLUX(15,IELEM3)+FLUX(06,IELEM3)
            FLUX(08,IELEM3)=FLUX(08,IELEM3)-FLUX(01,IELEM3)
     &                                     +FLUX(06,IELEM3)
            FLUX(03,IELEM3)=FLUX(03,IELEM3)+FLUX(01,IELEM3)
            FLUX(12,IELEM3)=FLUX(12,IELEM3)-FLUX(06,IELEM3)
            FLUX(01,IELEM3)=0.D0
            FLUX(06,IELEM3)=0.D0
          ENDIF
          IF(TRA01%R(I3).LT.EPS_VOLUME) THEN
!           ISSUED FROM 3
            FLUX(15,IELEM3)=FLUX(15,IELEM3)+FLUX(06,IELEM3)
            FLUX(14,IELEM3)=FLUX(14,IELEM3)+FLUX(02,IELEM3)
            FLUX(12,IELEM3)=FLUX(12,IELEM3)-FLUX(06,IELEM3)
     &                                     -FLUX(02,IELEM3)
            FLUX(03,IELEM3)=FLUX(03,IELEM3)+FLUX(02,IELEM3)
            FLUX(08,IELEM3)=FLUX(08,IELEM3)+FLUX(06,IELEM3)
            FLUX(06,IELEM3)=0.D0
            FLUX(02,IELEM3)=0.D0
          ENDIF
!
!         THESE PROPERTIES SHOULD BE ENSURED AFTER ASSEMBLING
!         SMALL MASS ERROR IF 3 FOLLOWING LINES ARE DELETED (WHY ?)
!
          IF(TRA01%R(I1).LT.EPS_VOLUME) FLUX(03,IELEM3)=0.D0
          IF(TRA01%R(I2).LT.EPS_VOLUME) FLUX(08,IELEM3)=0.D0
          IF(TRA01%R(I3).LT.EPS_VOLUME) FLUX(12,IELEM3)=0.D0
!
!         UPPER HORIZONTAL SEGMENTS CANNOT BE TREATED
!         NO DEGREE OF FREEDOM LEFT, THEY ARE TRANSFERRED
!         TO UPPER LEVEL
!
          IF(IPLAN.NE.NPLAN-1) THEN
            FLUX(01,IELEM3+NELEM2)=FLUX(01,IELEM3+NELEM2)
     &                            +FLUX(13,IELEM3)
            FLUX(02,IELEM3+NELEM2)=FLUX(02,IELEM3+NELEM2)
     &                            +FLUX(14,IELEM3)
            FLUX(06,IELEM3+NELEM2)=FLUX(06,IELEM3+NELEM2)
     &                            +FLUX(15,IELEM3)
            FLUX(13,IELEM3)=0.D0
            FLUX(14,IELEM3)=0.D0
            FLUX(15,IELEM3)=0.D0
          ENDIF
!
        ELSE
!         NO MORE CRUSHED POINTS ABOVE
          EXIT
        ENDIF
!
        ENDDO
      ENDDO
!
!     UNGROUPS FLUXES
!
      DO IELEM3=1,NELEM3
        DO I=1,15
          IF(FLUX(I,IELEM3).GT.0.D0) THEN
            FLUX(I+15,IELEM3)=0.D0
          ELSE
            FLUX(I+15,IELEM3)=-FLUX(I,IELEM3)
            FLUX(I,IELEM3)=0.D0
          ENDIF
        ENDDO
      ENDDO
!
!=======================================================================
!
      RETURN
      END
