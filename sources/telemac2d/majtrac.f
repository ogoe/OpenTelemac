!                    ******************
                     SUBROUTINE MAJTRAC
!                    ******************
!
     &(NS,NT,DIMT,DLIMT,NSEG,NPTFR,NUBO,
     & X,Y,AIRS,NU,AIRE,HT,HTN,TN,ZF,NBOR,
     & TBOR,FLUTENT,FLUTSOR,SMTR,NORDRE,CMI,JMI,
     & DJXT,DJYT,DXT,DYT,DPX,DPY,DIFT,CVIST,BETA,DSZ,AIRST,HSTOK,
     & HCSTOK,FLUXT,FLUHBOR,MASSOU,DTT,MESH,
     & ELTSEG,IFABOR,VNOCL)
!
!***********************************************************************
! TELEMAC2D   V6P3                                         27/07/2013
!***********************************************************************
!
!brief    UPDATES THE TRACER.
!
!history  INRIA
!+
!+        V5P4
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
!history  R.ATA
!+        27/07/2013
!+        V6P3
!+   Adaptation for new data structure of finite volumes
!+   clean and optimize
!+   parallelism
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AIRE           |-->| ELEMENT AREA
!| AIRS           |-->| CELL AREA
!| AIRST          |-->| AREA OF SUB-TRIANGLES (SECOND ORDER)
!| BETA           |---| COEFFICIENT OF EXTRAPOLATION FOR ORDRE 2
!| CMI            |-->| COORDINATES OF MIDDLE PONTS OF EDGES
!| CVIST          |-->| COEFFICIENT OF DIFFUSION FOR TRACER
!| DIFT           |-->| LOGICAL TELLING IF THERE IS DIFFUSION FOR TRACER OR NOT
!| DIMT           |-->| DIMENSION OF TRACER
!| DJXT,DJYT      |---| GRADIENTS PER TRIANGLES
!| DLIMT          |-->| DIMENSION OF TRACER AT THE BOUNDARY
!| DSZ            |-->| VARIATION OF Z FOR ORDRE 2
!| DTT            |-->| TIME STEP FOR TRACER
!| DXT,DYT        |---| GRADIENTS AT THE NODES
!| FLUHBOR        |-->| MASS FLUX AT THE BOUNDARY
!| FLUTENT,FLUTSOR|<--| TRACER FLUX AT THE INLET AND OUTLET
!| FLUXT          |-->| MASS FLUX OF TRACER
!| HCSTOK         |-->| STOCKED H RECONSTRUCTED FOR ORDRE 2
!| HSTOK          |-->| STOCKED WATER DEPTH
!| HT             |<--| HT AT TIME N+1
!| HTN,TN         |-->| HT AT TIME N
!| JMI            |-->| NUMBER OF THE TRIANGLE IN WHICH IS LOCATED
!|                |   | THE MIDPOINT OF THE INTERFACE
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| NBOR           |-->| GLOBAL NUMBERING OF BOUNDARY POINTS
!| NORDRE         |-->| ORDRE OF THE SCHEME
!| NPTFR          |-->| TOTAL NUMBER OF BOUNDARY NODES
!| NS             |-->| TOTAL NUMBER OF NODES IN THE MESH
!| NSEG           |-->| TOTAL NUMBER OF EDGES
!| NT             |-->| TOTAL NUMBER OF ELEMENTS
!| NU             |-->| NUMBERING OF NODES IN THE TRIANGLES
!| NUBO           |-->| GLOBAL INDICES OF EDGE EXTREMITIES
!| SMTR           |-->| TRACER SOURCE TERMS
!| TBOR           |-->| BOUNDARY CONDITIONS FOR T
!| X,Y            |-->| COORDINATES IF THE NODES
!| ZF             |-->| BATHYMETRY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TELEMAC2D, EX_MAJTRAC => MAJTRAC
      USE BIEF_DEF
      USE DECLARATIONS_TELEMAC2D,ONLY: DEBUG, CET_MT,DST_MT,DSP_MT,
     &                                 DSM_MT,CORRT_MT,GRADI_MT,
     &                                 GRADJ_MT,GRADJI_MT,GRADIJ_MT,
     &                                 DEJA_MT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN) :: DIFT
      INTEGER, INTENT(IN) :: NSEG,NPTFR,NORDRE,DIMT,DLIMT,NS,NT
      INTEGER, INTENT(IN) :: NUBO(2,NSEG),NU(NT,3)
      INTEGER, INTENT(IN) :: NBOR(NPTFR),JMI(*)
      INTEGER, INTENT(IN)             :: ELTSEG(NT,3)
      INTEGER, INTENT(IN)             :: IFABOR(NT,3)
      DOUBLE PRECISION, INTENT(INOUT) :: HT(DIMT),FLUTENT,FLUTSOR
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU
      DOUBLE PRECISION, INTENT(IN)    :: TBOR(DLIMT),DSZ(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: X(NS),Y(NS),AIRS(NS),AIRE(NT)
      DOUBLE PRECISION, INTENT(IN)    :: HTN(DIMT),TN(DIMT),ZF(*)
      DOUBLE PRECISION, INTENT(IN)    :: SMTR(DIMT),DPX(3,NT),DPY(3,NT)
      DOUBLE PRECISION, INTENT(IN)    :: CMI(2,*),AIRST(2,*),CVIST
      DOUBLE PRECISION, INTENT(INOUT) :: DJXT(*),DJYT(*),DXT(*),DYT(*)
      DOUBLE PRECISION, INTENT(INOUT) :: BETA
      DOUBLE PRECISION, INTENT(IN)    :: HSTOK(*),VNOCL(3,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: HCSTOK(2,*),FLUXT(*)
      DOUBLE PRECISION, INTENT(IN)    :: FLUHBOR(*),DTT
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,K,NSG,NUBO1,NUBO2,J,ILIM,ERR,I,IEL
!
      DOUBLE PRECISION ZF1,ZF2,FLUH,FLUT,HI0,HJ0,AIX,AIY,AJX,AJY,AMDS
      DOUBLE PRECISION FLU11,FLU41,UAS41,UAS42,DSZ1,DSZ2
      DOUBLE PRECISION DEMI,PROD_SCAL
!
!     DYNAMIC ARRAY ALLOCATION !!!!!!!!
!
      LOGICAL, ALLOCATABLE ::   YESNO(:)
!
!-----------------------------------------------------------------------
!
      IF(.NOT.DEJA_MT) THEN
        ALLOCATE(CET_MT(NS),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DST_MT(2,NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSP_MT(NS),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSM_MT(NS)    ,STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(CORRT_MT(NS)   ,STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(GRADI_MT(NSEG),GRADJ_MT(NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(GRADIJ_MT(NSEG),GRADJI_MT(NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        GO TO 1002
1001    CONTINUE
        IF(LNG.EQ.1) WRITE(LU,1000) ERR
        IF(LNG.EQ.2) WRITE(LU,2000) ERR
1000    FORMAT(1X,'MAJTRAC : ERREUR A L''ALLOCATION DE MEMOIRE : ',/,1X,
     &         'CODE D''ERREUR : ',1I6)
2000    FORMAT(1X,'MAFTRAC: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &        'ERROR CODE: ',1I6)
        CALL PLANTE(1)
        STOP
1002    CONTINUE
        DEJA_MT=.TRUE.
      ENDIF
      DEMI = 0.5D0
!
!-----------------------------------------------------------------------
!
!  INITIALISES
!
      CET_MT(:)=(/(0.D0,IS=1,NS)/)
      ALLOCATE(YESNO(NSEG),STAT=ERR)
      IF(ERR.GT.0)THEN
        IF(LNG.EQ.1) WRITE(LU,1000) ERR
        IF(LNG.EQ.2) WRITE(LU,2000) ERR
        CALL PLANTE(1)
        STOP
      ENDIF
!
! INITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO
!
!   COMPUTES THE TRACER GRADIENTS BY TRIANGLE AND BY NODE
!   COMPUTES THE DIFFUSION TERM
!
      IF(DIFT.OR.NORDRE.EQ.2) CALL GRADNODT(NS,NT,NU,AIRE,AIRS,
     &HSTOK,TN,DPX,DPY,DJXT,DJYT,DXT,DYT,DIFT,CVIST,CET_MT,DTT,MESH)
!
      IF(NORDRE.EQ.2) THEN
!
!  REBUILDS 2ND ORDER FOR TRACER
!  *************************************
!
!    INITIALIZATION
      DSP_MT(:)  =(/(0.D0,IS=1,NS)/)
      DSM_MT(:)  =(/(0.D0,IS=1,NS)/)
      DST_MT(1,:)=(/(0.D0,IS=1,NSEG)/)
      DST_MT(2,:)=(/(0.D0,IS=1,NSEG)/)
!    INITIALIZATION  OF GRADIENTS
      GRADI_MT(:) =(/(0.D0,IS=1,NSEG)/)
      GRADJ_MT(:) =(/(0.D0,IS=1,NSEG)/)
      GRADIJ_MT(:)=(/(0.D0,IS=1,NSEG)/)
      GRADJI_MT(:)=(/(0.D0,IS=1,NSEG)/)
!
!
      DO IEL=1, NT
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
            NSG = ELTSEG(IEL,I)
!     RECUPERATE JMI
            J   = JMI(NSG) ! THIS THE TRIANGLE IN WHICH IS LOCATED CMI
            IF(NCSIZE.GT.1.AND.J.EQ.0) CYCLE  ! THAT MEANS CMI IS NOT LOCATED IN TRIANGLE J
!
!    RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!     WITH RESPECT TO THE NORMAL
            NUBO1 = NUBO(1,NSG)
            NUBO2 = NUBO(2,NSG)
            PROD_SCAL= ((X(NUBO2)-X(NUBO1))*VNOCL(1,NSG)+
     &                  (Y(NUBO2)-Y(NUBO1))*VNOCL(2,NSG))
            IF(PROD_SCAL.LT.0.D0)THEN
              NUBO1 = NUBO(2,NSG)
              NUBO2 = NUBO(1,NSG)
            ENDIF
!
            ZF1 = ZF(NUBO1)
            ZF2 = ZF(NUBO2)
!
            IF(PROD_SCAL.LT.0.D0)THEN
              DSZ1 = DSZ(2,NSG)
              DSZ2 = DSZ(1,NSG)
            ELSE
              DSZ1 = DSZ(1,NSG)
              DSZ2 = DSZ(2,NSG)
            ENDIF
!
            HI0 = HSTOK(NUBO1)
            HJ0 = HSTOK(NUBO2)
!
!           STICKS TO 1ST ORDER FOR A COVERED EDGE
!
            IF(ZF1.GE. (HJ0+ZF2) .OR. ZF2.GE. (HI0+ZF1)
     &         .OR. 2.*ABS(DSZ1).GE.HI0
     &         .OR. 2.*ABS(DSZ1).GE.HJ0
     &         .OR. 2.*ABS(DSZ2).GE.HI0
     &         .OR. 2.*ABS(DSZ2).GE.HJ0)  THEN
!             DST_MT(1,NSG) =0.D0
!             DST_MT(2,NSG) =0.D0
              CYCLE
            ELSE
!
              AIX         = CMI(1,NSG)-X(NUBO1)
              AIY         = CMI(2,NSG)-Y(NUBO1)
              AJX         = CMI(1,NSG)-X(NUBO2)
              AJY         = CMI(2,NSG)-Y(NUBO2)
!
              GRADI_MT(NSG)  = AIX*DXT(NUBO1) + AIY*DYT(NUBO1)
              GRADJ_MT(NSG)  = AJX*DXT(NUBO2) + AJY*DYT(NUBO2)
              GRADIJ_MT(NSG) = AIX*DJXT(J) + AIY*DJYT(J)
              GRADJI_MT(NSG) = AJX*DJXT(J) + AJY*DJYT(J)
            ENDIF
            YESNO(NSG)=.TRUE.
          ENDIF
        ENDDO
      ENDDO
      IF(NCSIZE.GT.1)THEN      ! NPON,NPLAN,ICOM,IAN , HERE ICOM=1 VALUE WITH MAX | |
        CALL PARCOM2_SEG(GRADI_MT,GRADJ_MT,GRADI_MT,
     &              NSEG,1,2,2,MESH,1,11)
        CALL PARCOM2_SEG(GRADIJ_MT,GRADJI_MT,GRADJI_MT,
     &              NSEG,1,2,2,MESH,1,11)
      ENDIF
!
!    EXTRAPOLATES THE GRADIENTS AND SLOPE LIMITOR
!
      ILIM=2
      BETA=0.3333D0
      DO NSG=1,NSEG
        DST_MT(1,NSG)  = EXLIM (ILIM,BETA,GRADI_MT(NSG),GRADIJ_MT(NSG))
        DST_MT(2,NSG)  = EXLIM (ILIM,BETA,GRADJ_MT(NSG),GRADJI_MT(NSG))
      ENDDO
!
! INITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO
      DO IEL=1, NT
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
            NSG = ELTSEG(IEL,I)
!    RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!     WITH RESPECT TO THE NORMAL
            NUBO1 = NUBO(1,NSG)
            NUBO2 = NUBO(2,NSG)
            PROD_SCAL= ((X(NUBO2)-X(NUBO1))*VNOCL(1,NSG)+
     &                  (Y(NUBO2)-Y(NUBO1))*VNOCL(2,NSG))
            IF(PROD_SCAL.LT.0.D0)THEN
              NUBO1 = NUBO(2,NSG)
              NUBO2 = NUBO(1,NSG)
            ENDIF
!
!           FOR PARALLELILSM
            IF(NCSIZE.GT.1.AND.IFABOR(IEL,I).EQ.-2)THEN ! THIS IS AN INTERFACE EDGE
              IF(DST_MT(1,NSG).GE.0.D0) THEN
                DSP_MT(NUBO1) = DSP_MT(NUBO1) +
     &                   DEMI*AIRST(1,NSG)*HCSTOK(1,NSG)*DST_MT(1,NSG) ! WE CONSIDER ONLY
              ELSE                                                      ! 0.5 AIRST
                DSM_MT(NUBO1) = DSM_MT(NUBO1) -
     &                   DEMI*AIRST(1,NSG)*HCSTOK(1,NSG)*DST_MT(1,NSG) ! PARCOM2 WILL ADD
              ENDIF                                                     ! CONTRIBUTIONS
              IF(DST_MT(2,NSG).GE.0.D0) THEN
                DSP_MT(NUBO2) = DSP_MT(NUBO2) +
     &                   DEMI*AIRST(2,NSG)*HCSTOK(2,NSG)*DST_MT(2,NSG)
              ELSE
                DSM_MT(NUBO2) = DSM_MT(NUBO2) -
     &                   DEMI*AIRST(2,NSG)*HCSTOK(2,NSG)*DST_MT(2,NSG)
              ENDIF
              IF(DST_MT(2,NSG).GE.0.D0) THEN
                DSP_MT(NUBO2) = DSP_MT(NUBO2) +
     &                   DEMI*AIRST(2,NSG)*HCSTOK(2,NSG)*DST_MT(2,NSG)
              ELSE
                DSM_MT(NUBO2) = DSM_MT(NUBO2) -
     &                   DEMI*AIRST(2,NSG)*HCSTOK(2,NSG)*DST_MT(2,NSG)
              ENDIF
            ELSE ! NO PARALLELILSM OR NO INTERFACE EDGE
              IF(DST_MT(1,NSG).GE.0.D0) THEN
                DSP_MT(NUBO1) = DSP_MT(NUBO1) +
     &          AIRST(1,NSG)* HCSTOK(1,NSG)*DST_MT(1,NSG)
              ELSE
                DSM_MT(NUBO1) = DSM_MT(NUBO1) -
     &          AIRST(1,NSG)* HCSTOK(1,NSG)*DST_MT(1,NSG)
              ENDIF
              IF(DST_MT(2,NSG).GE.0.) THEN
                DSP_MT(NUBO2) = DSP_MT(NUBO2) +
     &          AIRST(2,NSG)* HCSTOK(2,NSG)*DST_MT(2,NSG)
              ELSE
                DSM_MT(NUBO2) = DSM_MT(NUBO2) -
     &          AIRST(2,NSG)* HCSTOK(2,NSG)*DST_MT(2,NSG)
              ENDIF
            ENDIF
!
            YESNO(NSG)=.TRUE.
          ENDIF
        ENDDO
      ENDDO
      !  FOR PARALLELILSM
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM2(DSP_MT,DSM_MT,DSM_MT,NS,1,2,2,MESH)
      ENDIF
!
!     COMPUTES THE CORRECTIONS TO ENSURE CONSERVATION OF HT
!                  ***********           ******************
!
      DO IS=1,NS
        CORRT_MT(IS) =  DSM_MT(IS) - DSP_MT(IS)
        AMDS =MAX(DSP_MT(IS),DSM_MT(IS))
        IF(AMDS.GT.0.D0) THEN
          CORRT_MT(IS) = CORRT_MT(IS)/AMDS
        ENDIF
      ENDDO
!
      ENDIF ! ENDIF OF NORDRE.EQ.2
!
!     COMPUTES FLUXES FOR THE INTERNAL INTERFACES
!
!  REINITIALIZATION OF YESNO
      DO I=1,NSEG
        YESNO(I)=.FALSE.
      ENDDO
!
!     LOOP ON GLOBAL LIST OF EDGES
!    ******************************
!
      DO IEL=1, NT
        DO I = 1,3
          IF(.NOT.YESNO(ELTSEG(IEL,I)))THEN
            NSG = ELTSEG(IEL,I)
!
!    RECUPERATE NODES OF THE EDGE WITH THE GOOD ORIENTATION
!     WITH RESPECT TO THE NORMAL
            NUBO1 = NUBO(1,NSG)
            NUBO2 = NUBO(2,NSG)
            PROD_SCAL= ((X(NUBO2)-X(NUBO1))*VNOCL(1,NSG)+
     &                  (Y(NUBO2)-Y(NUBO1))*VNOCL(2,NSG))
            IF(PROD_SCAL.LT.0.D0)THEN
              NUBO1 = NUBO(2,NSG)
              NUBO2 = NUBO(1,NSG)
            ENDIF
!
            UAS41     = TN(NUBO1)
            UAS42     = TN(NUBO2)
!
            FLU11=FLUXT(NSG)
!
            IF (FLU11.GE.0.) THEN
              IF(NORDRE.EQ.2) THEN
                UAS41 = UAS41  + DST_MT(1,NSG) +
     &          MIN(0.D0,CORRT_MT(NUBO1))*MAX(0.D0,DST_MT(1,NSG))+
     &          MAX(0.D0,CORRT_MT(NUBO1))*MAX(0.D0,-DST_MT(1,NSG))
              ENDIF
              FLU41 =  UAS41 * FLU11
            ELSE
              IF(NORDRE.EQ.2) THEN
                UAS42 = UAS42 + DST_MT(2,NSG) +
     &          MIN(0.D0,CORRT_MT(NUBO2))*MAX(0.D0,DST_MT(2,NSG))+
     &          MAX(0.D0,CORRT_MT(NUBO2))*MAX(0.D0,-DST_MT(2,NSG))
              ENDIF
              FLU41 =  UAS42 * FLU11
            ENDIF
!
            CET_MT(NUBO1) = CET_MT(NUBO1) - FLU41
            CET_MT(NUBO2) = CET_MT(NUBO2) + FLU41
!
            YESNO(NSG)=.TRUE.
          ENDIF
        ENDDO
      ENDDO
      !  FOR PARALLELILSM
      IF(NCSIZE.GT.1)THEN
        CALL PARCOM2(CET_MT,CET_MT,CET_MT,NS,1,2,1,MESH)
      ENDIF
!
!     BOUNDARY FLUX
!
      IF(NPTFR.GT.0)THEN  ! USEFUL FOR PARALLEL CASE
        DO K=1,NPTFR
          IS=NBOR(K)
!
          FLUH =FLUHBOR(K)
!
          IF(FLUH.GE.0.D0) THEN
            FLUT= TN(IS)*FLUH
            FLUTSOR = FLUTSOR +FLUT
          ELSE
            FLUT= TBOR(K)*FLUH
            FLUTENT = FLUTENT +FLUT
          ENDIF
!
          CET_MT(IS)  = CET_MT(IS) - FLUT
!
        ENDDO
      ENDIF
!
!     UPDATES HT
!
      DO IS =1,NS
!
        HT(IS)  = HTN(IS) +  (CET_MT(IS)+SMTR(IS))/AIRS(IS)
        MASSOU = MASSOU + SMTR(IS)
!
        IF(HT(IS).LE.1.D-15) HT(IS)=0.D0
!
      ENDDO
!
      DEALLOCATE(YESNO)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
