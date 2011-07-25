!                    ******************************
                     SUBROUTINE DREDGESIM_INTERFACE
!                    ******************************
!
     &(OPTION)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    THIS IS THE INTERFACE TO DREDGESIM, CONTAINING ALL
!+                DEPENDENCIES TO DREDGESIM LIBRARIES.
!
!warning  FOR REAL INTERFACING WITH DREDGESIM, COMMENTS "CDSIM"
!+            MUST BE REMOVED
!
!history  J-M HERVOUET (LNHE)
!+        02/02/2009
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
!| OPTION         |-->| 1 : INITIALISATION (CALLED IN SISYPHE)
!|                |   | 2 : CALLED EVERY TIME STEP (FROM
!|                |   | BEDLOAD_POSTTREATMENT)
!|                |   | 3 : END  (CALLED IN SISYPHE)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : DREDGESIM,DT,NPOIN,NSICLA,DZF_GF,
     &                                 ZFCL_C,AVAIL,MESH,SIS_FILES
!DSIM     USE P_SISYPHE_UI, ONLY : INIT_AND_SETUP_DS,CLEAR_SISYDREDGE
!DSIM     USE P_DREDGESIM_UI, ONLY : STOP_DREDGESIM,CLEAR_DREDGESIM
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: OPTION
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=250) :: DREDGEINP,SEDGEO
      DOUBLE PRECISION,ALLOCATABLE :: AVAI_GF(:,:)
      INTEGER I,J
      CHARACTER(LEN=11) EXTENS
      EXTERNAL          EXTENS
!
!-----------------------------------------------------------------------
!
      IF(OPTION.EQ.1) THEN
!
!     INITIALISES
!
        DREDGEINP = ''
        SEDGEO = ''
        IF(NCSIZE.GT.1) THEN
!         INPUT FILE FOR DREDGESIM
          DREDGEINP = TRIM('SISMAF'//EXTENS(NCSIZE-1,IPID))
          SEDGEO = TRIM('SISGEO'//EXTENS(NCSIZE-1,IPID))
        ELSE
          DREDGEINP = 'SISMAF'
          SEDGEO = 'SISGEO'
        ENDIF
!DSIM       CALL INIT_AND_SETUP_DS(SIS_FILES(SISMAF)%LU,DREDGEINP,
!DSIM                              SIS_FILES(SISGEO)%LU,
!DSIM                              SEDGEO,
!DSIM    &                         NCSIZE,IPID,MESH%ILMAX,
!DSIM    &                         MESH%IKP%I,MESH%IKM%I,MESH%NACHB%I,
!DSIM    &                         MESH%INDPU%I,MESH%NHP%I,MESH%NHM%I)
!
      ELSEIF(OPTION.EQ.2) THEN
!
!     CALL FROM WITHIN BEDLOAD_POSTTREATMENT
!
!       ALLOCATES AVAI_GF
        ALLOCATE(AVAI_GF(NPOIN,NSICLA))
!       INITIALISES THE DEPTH TO ADD
        CALL OS('X=0     ',X=DZF_GF)
!DSIM       CALL RUN_DREDGESIM(DT)
!DSIM       AVAI_GF = GET_SM_NODE_SEDIMENT_FRACTION()
        DO J = 1, NPOIN
          IF(DZF_GF%R(J).GT.0.D0) THEN
            DO I = 1, NSICLA
              ZFCL_C%ADR(I)%P%R(J) = ZFCL_C%ADR(I)%P%R(J) +
     &                               DZF_GF%R(J)*AVAI_GF(J,I)
            ENDDO
          ELSE
            DO I = 1, NSICLA
              ZFCL_C%ADR(I)%P%R(J) = ZFCL_C%ADR(I)%P%R(J) +
     &                               DZF_GF%R(J)*AVAIL(J,1,I)
            ENDDO
          ENDIF
        ENDDO
        DEALLOCATE(AVAI_GF)
!
      ELSEIF(OPTION.EQ.3) THEN
!
!     CLOSES
!
!DSIM       CALL STOP_DREDGESIM()
!DSIM       CALL CLEAR_DREDGESIM()
!DSIM       CALL CLEAR_SISYDREDGE()
!
      ELSE
!
!     ERROR
!
        IF(LNG.EQ.1) WRITE(LU,*) 'MAUVAISE OPTION POUR DREDGESIM'
        IF(LNG.EQ.2) WRITE(LU,*) 'BAD OPTION FOR DREDGESIM'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
