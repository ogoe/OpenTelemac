!                    ************************
                     SUBROUTINE INIT_SEDIMENT
!                    ************************
!
     &(NSICLA,ELAY,ZF,ZR,NPOIN,AVAIL,FRACSED_GF,AVA0,
     & LGRAFED,CALWC,XMVS,XMVE,GRAV,VCE,XWC,FDM,
     & CALAC,AC,SEDCO,ES,NCOUCH_TASS,CONC_VASE,
     & MS_SABLE,MS_VASE,ACLADM,UNLADM)
!
!***********************************************************************
! SISYPHE   V6P0                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AC             |---| 
!| ACLADM         |---| 
!| AVA0           |-->| 
!| AVAIL          |-->| 
!| CALAC          |---| 
!| CALWC          |-->| 
!| CONC_VASE      |---| 
!| ELAY           |-->| 
!| ES             |---| 
!| FDM            |-->| 
!| FRACSED_GF     |-->| 
!| GRAV           |-->| GRAVITY ACCELERATION
!| LGRAFED        |-->| 
!| MS_SABLE       |---| 
!| MS_VASE        |---| 
!| NCOUCH_TASS    |---| 
!| NPOIN          |-->| NUMBER OF POINTS
!| NSICLA         |-->| NUMBER OF SEDIMENT CLASSES
!| SEDCO          |---| 
!| UNLADM         |---| 
!| VCE            |-->| 
!| XMVE           |-->| 
!| XMVS           |-->| 
!| XWC            |-->| 
!| ZF             |-->| BOTTOM
!| ZR             |-->| NON ERODABLE BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_SISYPHE, EX_INIT_SEDIMENT => INIT_SEDIMENT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
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
      DOUBLE PRECISION,  INTENT(INOUT)  :: AC(NSICLA)
!
      LOGICAL,           INTENT(IN)     :: SEDCO(NSICLA)
!
! IF SEDCO(1) OR SEDCO(2) = YES --> CONSOLIDATION MODEL
!
!
      DOUBLE PRECISION, INTENT(IN)    :: CONC_VASE(10)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER            :: I,J
      DOUBLE PRECISION   :: DENS,DSTAR
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!  ------ BED COMPOSITION
!
        CALL OS('X=Y-Z   ',X=ELAY,Y=ZF,Z=ZR)
!
!     ONLY ONE CLASS
!
      IF(NSICLA.EQ.1) THEN
         DO I=1,NPOIN
          AVAIL(I,1,1) = 1.D0
          ACLADM%R(I) = FDM(1)
        ENDDO
!     PURE MUD ONLY
        IF(SEDCO(1)) CALL INIT_MIXTE(XMVS,NPOIN,AVAIL,NSICLA,ES,
     &                               ELAY%R,NCOUCH_TASS,CONC_VASE,
     &                                  MS_SABLE%R,MS_VASE%R,ZF%R,
     &                                               ZR%R,AVA0)
!
      ELSE
!
!     NON-COHESIVE, MULTI-CLASSES
!
        IF(.NOT.SEDCO(2)) THEN
          CALL INIT_AVAI
!         CALL MEAN_GRAIN_SIZE
! THIS PART CAN BE INTEGRATED INTO INIT_AVAI
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
!        MIXED (NON COHESIVE /COHESIVE)
          CALL INIT_MIXTE(XMVS,NPOIN,AVAIL,NSICLA,ES,ELAY%R,
     &                     NCOUCH_TASS,CONC_VASE,MS_SABLE%R,
     &                     MS_VASE%R,ZF%R,ZR%R,AVA0)
          DO I=1,NPOIN
            ACLADM%R(I) = FDM(1)
          ENDDO
        ENDIF
!
      ENDIF
!
      IF(LGRAFED) THEN
        DO I=1, NSICLA
          FRACSED_GF(I)=AVA0(I)
        ENDDO
      ENDIF
!
!
! ------ SETTLING VELOCITY
!
      IF(.NOT.CALWC) THEN
        DENS = (XMVS - XMVE) / XMVE
        DO I = 1, NSICLA
          CALL VITCHU_SISYPHE(XWC(I),DENS,FDM(I),GRAV,VCE)
        ENDDO
      ENDIF
!
!------ SHIELDS PARAMETER
!
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
!
!-----------------------------------------------------------------------
!
      RETURN
      END