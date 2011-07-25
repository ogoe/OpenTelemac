!                    ************************
                     SUBROUTINE INIT_SEDIMENT
!                    ************************
!
     &(NSICLA,ELAY,ZF,ZR,NPOIN,AVAIL,FRACSED_GF,AVA0,
     & LGRAFED,CALWC,XMVS,XMVE,GRAV,VCE,XWC,FDM,
     & CALAC,AC,SEDCO,ES,NCOUCH_TASS,CONC_VASE,
     & MS_SABLE,MS_VASE,ACLADM,UNLADM,TOCE_SABLE)
C
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief
!
!history  C. VILLARET (LNHE)
!+        30/12/2008
!+
!+
!
!history  JMH
!+        16/09/2009
!+        V6P0
!+   AVAIL(NPOIN,10,NSICLA)
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
!| AC             |<->| CRITICAL SHIELDS PARAMETER
!| ACLADM         |-->| MEAN DIAMETER OF SEDIMENT
!| AT0            |<->| TIME IN S
!| AVAIL          |<->| VOLUME PERCENT OF EACH CLASS
!| CALAC          |---| ****
!| CALWC          |-->| ****
!| CONC_VASE      |<->| MUD CONCENTRATION FOR EACH LAYER
!| ELAY           |<->| THICKNESS OF SURFACE LAYER
!| ES             |<->| LAYER THICKNESSES AS DOUBLE PRECISION
!| FDM            |-->| DIAMETER DM FOR EACH CLASS 
!| FRACSED_GF     |-->|(A SUPPRIMER)
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| LGRAFED        |-->|(A SUPPRIMER)
!| MS_SABLE       |<->| MASS OF SAND PER LAYER (KG/M2)
!| MS_VASE        |<->| MASS OF MUD PER LAYER (KG/M2)
!| NCOUCH_TASS    |-->| NUMBER OF LAYERS FOR CONSOLIDATION
!| NPOIN          |-->| NUMBER OF POINTS
!| NSICLA         |-->| NUMBER OF SEDIMENT CLASSES
!| SEDCO          |-->| LOGICAL, SEDIMENT COHESIVE OR NOT
!| UNLADM         |-->| MEAN DIAMETER OF ACTIVE STRATUM LAYER
!| VCE            |-->| WATER VISCOSITY
!| XMVE           |-->| FLUID DENSITY 
!| XMVS           |-->| WATER DENSITY 
!| XWC            |-->| SETTLING VELOCITY
!| ZF             |-->| ELEVATION OF BOTTOM
!| ZR             |-->| NON ERODABLE BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE BIEF
      USE INTERFACE_SISYPHE, EX_INIT_SEDIMENT => INIT_SEDIMENT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,           INTENT(IN)     :: NSICLA,NPOIN,NCOUCH_TASS
      TYPE(BIEF_OBJ),    INTENT(INOUT)  :: ELAY,ZF,ZR
      TYPE(BIEF_OBJ), INTENT(INOUT)     :: MS_SABLE, MS_VASE
      TYPE(BIEF_OBJ),    INTENT(INOUT)  :: ACLADM, UNLADM
      LOGICAL,           INTENT(IN)     :: LGRAFED,CALWC
      LOGICAL,           INTENT(IN)     :: CALAC
      DOUBLE PRECISION,  INTENT(IN)     :: XMVS,XMVE,GRAV,VCE
      DOUBLE PRECISION,  INTENT(INOUT)  :: AVA0(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: AVAIL(NPOIN,10,NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: FRACSED_GF(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: FDM(NSICLA),XWC(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: AC(NSICLA),TOCE_SABLE
C
      LOGICAL,           INTENT(IN)     :: SEDCO(NSICLA)
C
C IF SEDCO(1) OR SEDCO(2) = YES --> CONSOLIDATION MODEL
C
C
      DOUBLE PRECISION, INTENT(IN)    :: CONC_VASE(10)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER            :: I,J
      DOUBLE PRECISION   :: DENS,DSTAR
      LOGICAL            :: MIXTE
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
C  ------ BED COMPOSITION
!
        CALL OS('X=Y-Z   ',X=ELAY,Y=ZF,Z=ZR)
!
C     ONLY ONE CLASS
C
      IF(NSICLA.EQ.1) THEN
         DO I=1,NPOIN
          AVAIL(I,1,1) = 1.D0
          ACLADM%R(I) = FDM(1)
        ENDDO
C     PURE MUD ONLY
        IF(SEDCO(1)) CALL INIT_MIXTE(XMVS,NPOIN,AVAIL,NSICLA,ES,
     &                               ELAY%R,NCOUCH_TASS,CONC_VASE,
     &                                  MS_SABLE%R,MS_VASE%R,ZF%R,
     &                                               ZR%R,AVA0)
C
      ELSE
C
C     NON-COHESIVE, MULTI-CLASSES
C
        IF(.NOT.SEDCO(2)) THEN
          CALL INIT_AVAI
C         CALL MEAN_GRAIN_SIZE
C THIS PART CAN BE INTEGRATED INTO INIT_AVAI
          DO J=1,NPOIN
            ACLADM%R(J) = 0.D0
            UNLADM%R(J) = 0.D0
            DO I=1,NSICLA
              IF(AVAIL(J,1,I).GT.0.D0) THEN
                ACLADM%R(J) = ACLADM%R(J) + FDM(I)*AVAIL(J,1,I)
                UNLADM%R(J) = UNLADM%R(J) + FDM(I)*AVAIL(J,2,I)
              ENDIF
            ENDDO
            ACLADM%R(J)=MAX(ACLADM%R(J),0.D0)
            UNLADM%R(J)=MAX(UNLADM%R(J),0.D0)
          ENDDO
        ELSE
C
C        MIXED (so far only 2 classes: NON COHESIVE /COHESIVE)
C  
          MIXTE=.TRUE.      
          CALL INIT_MIXTE(XMVS,NPOIN,AVAIL,NSICLA,ES,ELAY%R,
     &                     NCOUCH_TASS,CONC_VASE,MS_SABLE%R,
     &                     MS_VASE%R,ZF%R,ZR%R,AVA0)
          DO I=1,NPOIN
            ACLADM%R(I) = FDM(1)
          ENDDO
        ENDIF
C
      ENDIF
C
      IF(LGRAFED) THEN
        DO I=1, NSICLA
          FRACSED_GF(I)=AVA0(I)
        ENDDO
      ENDIF
C
C
C ------ SETTLING VELOCITY
C
      IF(.NOT.CALWC) THEN
        DENS = (XMVS - XMVE) / XMVE
        DO I = 1, NSICLA
          CALL VITCHU_SISYPHE(XWC(I),DENS,FDM(I),GRAV,VCE)
        ENDDO
      ENDIF
C
C------ SHIELDS PARAMETER
C
      IF(.NOT.CALAC) THEN
        DENS  = (XMVS - XMVE )/ XMVE
        DO I = 1, NSICLA
          DSTAR = FDM(I)*(GRAV*DENS/VCE**2)**(1.D0/3.D0)
          IF (DSTAR <= 4.D0) THEN
            AC(I) = 0.24*DSTAR**(-1.0D0)
          ELSEIF (DSTAR <= 10.D0) THEN
            AC(I) = 0.14D0*DSTAR**(-0.64D0)
          ELSEIF (DSTAR <= 20.D0) THEN
            AC(I) = 0.04D0*DSTAR**(-0.1D0)
          ELSEIF (DSTAR <= 150.D0) THEN
            AC(I) = 0.013D0*DSTAR**(0.29D0)
          ELSE
            AC(I) = 0.055D0
          ENDIF          
        ENDDO
      ENDIF
C pour les sédiments mixtes (suspension_flux_mixte)
      IF(MIXTE) TOCE_SABLE=AC(1)*FDM(1)*GRAV*(XMVS - XMVE)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
