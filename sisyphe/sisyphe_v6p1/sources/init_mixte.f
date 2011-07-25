!                    *********************
                     SUBROUTINE INIT_MIXTE
!                    *********************
!
     &(XMVS,NPOIN,AVAIL,NSICLA,ES,ELAY,NCOUCH_TASS,CONC_VASE,
     & MS_SABLE,MS_VASE,ZF,ZR,AVA0)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables   
!+   
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AVA0           |-->| VOLUME PERCENT 
!| AVAIL          |<->| VOLUME PERCENT OF EACH CLASS
!| CONC_VASE      |<->| MUD CONCENTRATION FOR EACH LAYER
!| ELAY           |<->| THICKNESS OF SURFACE LAYER
!| ES             |<->| LAYER THICKNESSES AS DOUBLE PRECISION
!| MS_SABLE       |<->| MASS OF SAND PER LAYER (KG/M2)
!| MS_VASE        |<->| MASS OF MUD PER LAYER (KG/M2)
!| NCOUCH_TASS    |-->| NUMBER OF LAYERS FOR CONSOLIDATION
!| NPOIN          |-->| NUMBER OF POINTS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| XMVS           |-->| WATER DENSITY 
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZR             |-->| NON ERODABLE BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NSICLA,NCOUCH_TASS
      DOUBLE PRECISION, INTENT(IN)    :: XMVS
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,10,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10),ELAY(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: ZR(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_SABLE(NPOIN,10)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(NPOIN,10)
      DOUBLE PRECISION, INTENT(IN)    :: CONC_VASE(10)
      DOUBLE PRECISION, INTENT(IN)   :: AVA0(NSICLA)
!
!-----------------------------------------------------------------------
!     LOCAL VARIABLES
!
      INTEGER I,J
!
      DOUBLE PRECISION EPAI_VASE(10),EPAI_SABLE(10)
      DOUBLE PRECISION DIFF
!
! ------------------------------------------------------------------------
!
!*******INITIAL SEDIMENT COMPOSITION IS IDENTICAL AT EACH NODE
! DEFAULT INITIALISATION: ALL LAYERS ARE EMPTY EXCEPT BED LAYER
! OTHERWISE SET THICKNESS OF THE MUD LAYERS IN EPAI_VASE(I= 1, NCOUCH_TASS-1)
      IF(NCOUCH_TASS.GT.1) THEN
        DO J= 1,NCOUCH_TASS-1
          EPAI_VASE(J) = 0.D0
        ENDDO
!
!       EXAMPLE FOR NCOUCH_TASS= 10
!
!       EPAI_VASE(1)=0.0525D0
!       EPAI_VASE(2)=0.0385D0
!       EPAI_VASE(3)=0.03995D0
!       EPAI_VASE(4)=0.0437D0
!       EPAI_VASE(5)=0.0517D0
!       EPAI_VASE(6)=0.1259D0
!       EPAI_VASE(7)=0.4889D0
!       EPAI_VASE(8)=1.5071D0
!       EPAI_VASE(9)=0.86410D0
!
        DO J=1,NCOUCH_TASS-1
          IF(NSICLA.GT.1) THEN
            EPAI_SABLE(J) = AVA0(1)/AVA0(2)*EPAI_VASE(J)
          ENDIF
        ENDDO
!
      ENDIF
!
!     COMPLETING THE LAST LAYER SO THAT SUM OF LAYERS = ZF-ZR
!
      DO I=1,NPOIN
        ELAY(I)=0.D0
        IF(NCOUCH_TASS.GT.1) THEN
          DO J=1,NCOUCH_TASS-1
            ES(I,J)= EPAI_VASE(J)
            IF(NSICLA.GT.1) THEN
              ES(I,J)= ES(I,J)  + EPAI_SABLE(J)
            ENDIF
            ELAY(I)=ELAY(I)+ES(I,J)
          ENDDO
        ENDIF
        DIFF= (ZF(I)-ZR(I)) - ELAY(I)
        IF(DIFF.GE.0.D0) THEN
          ES(I,NCOUCH_TASS) = DIFF
          ELAY(I) = ZF(I)-ZR(I)
        ELSE
          ES(I,NCOUCH_TASS) = 0.D0
          WRITE(LU,*) 'ERROR IN INIT-MIXTE: THE SUM OF THICKNESS'
          WRITE(LU,*) 'OF BED LAYERS > ERODIBLE BED MATERIAL'
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!     COMPUTING THE INITIAL MASSES OF MUD AND SAND
!
      DO I=1,NPOIN
!
        DO J=1,NCOUCH_TASS
!
!         FILLING VOIDS BETWEEN SAND GRAINS
!
          IF(NSICLA.EQ.1) THEN
!
!           PURE MUD
            MS_VASE(I,J) = ES(I,J)*CONC_VASE(J)
            AVAIL(I,J,1) = 1.D0
!
          ELSE
!
!           IF MIXED
            MS_VASE(I,J) = ES(I,J)*CONC_VASE(J)*AVA0(2)
            MS_SABLE(I,J)= ES(I,J)*XMVS*AVA0(1)
            IF(ES(I,J).GE.1.D-6) THEN
              AVAIL(I,J,1)= AVA0(1)
              AVAIL(I,J,2)= AVA0(2)
            ELSE
              AVAIL(I,J,1)= 0.D0
              AVAIL(I,J,2)= 0.D0
            ENDIF
!
          ENDIF
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
