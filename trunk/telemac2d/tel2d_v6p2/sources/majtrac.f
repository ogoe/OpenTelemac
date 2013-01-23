!                    ******************
                     SUBROUTINE MAJTRAC
!                    ******************
!
     &(NS,NT,DIMT,DLIMT,NSEG,NPTFR,NUBO,
     & X,Y,AIRS,NU,AIRE,HT,HTN,TN,ZF,NBOR,
     & TBOR,FLUTENT,FLUTSOR,SMTR,NORDRE,CMI,JMI,
     & DJXT,DJYT,DXT,DYT,DPX,DPY,DIFT,CVIST,BETA,DSZ,AIRST,HSTOK,
     & HCSTOK,FLUXT,FLUHBOR,MASSOU,DTT)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
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
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL, INTENT(IN) :: DIFT
      INTEGER, INTENT(IN) :: NSEG,NPTFR,NORDRE,DIMT,DLIMT,NS,NT
      INTEGER, INTENT(IN) :: NUBO(2,NSEG),NU(NT,3)
      INTEGER, INTENT(IN) :: NBOR(NPTFR),JMI(*)
      DOUBLE PRECISION, INTENT(INOUT) :: HT(DIMT),FLUTENT,FLUTSOR
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU
      DOUBLE PRECISION, INTENT(IN)    :: TBOR(DLIMT),DSZ(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: X(NS),Y(NS),AIRS(NS),AIRE(NT)
      DOUBLE PRECISION, INTENT(IN)    :: HTN(DIMT),TN(DIMT),ZF(*)
      DOUBLE PRECISION, INTENT(IN)    :: SMTR(DIMT),DPX(3,NT),DPY(3,NT)
      DOUBLE PRECISION, INTENT(IN)    :: CMI(2,*),AIRST(2,*),CVIST
      DOUBLE PRECISION, INTENT(INOUT) :: DJXT(*),DJYT(*),DXT(*),DYT(*)
      DOUBLE PRECISION, INTENT(INOUT) :: BETA
      DOUBLE PRECISION, INTENT(IN)    :: HSTOK(*)
      DOUBLE PRECISION, INTENT(IN)    :: HCSTOK(2,*),FLUXT(*)
      DOUBLE PRECISION, INTENT(IN)    :: FLUHBOR(*),DTT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,K,NSG,NUBO1,NUBO2,J,ILIM,ERR
!
      DOUBLE PRECISION ZF1,ZF2,FLUH,FLUT,HI0,HJ0,AIX,AIY,AJX,AJY,AMDS
      DOUBLE PRECISION GRADI,GRADJ,GRADIJ,GRADJI,FLU11,FLU41,UAS41,UAS42
!
!     DYNAMIC ARRAY ALLOCATION !!!!!!!!
!
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: CET(:),DST(:,:)
      DOUBLE PRECISION, ALLOCATABLE, SAVE :: DSP(:),DSM(:),CORRT(:)
      LOGICAL DEJA
      DATA DEJA/.FALSE./
!
!-----------------------------------------------------------------------
!
      IF(.NOT.DEJA) THEN
        ALLOCATE(CET(NS),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DST(2,NSEG),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSP(NS),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(DSM(NS),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        ALLOCATE(CORRT(NS),STAT=ERR)
        IF(ERR.NE.0) GO TO 1001
        GO TO 1002
1001    CONTINUE
        IF(LNG.EQ.1) WRITE(LU,1000) ERR
        IF(LNG.EQ.2) WRITE(LU,2000) ERR
1000    FORMAT(1X,'MAJTRAC : ERREUR A L''ALLOCATION DE MEMOIRE : ',/,1X,
     &        'CODE D''ERREUR : ',1I6)
2000    FORMAT(1X,'MAJTRAC: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &        'ERROR CODE: ',1I6)
        CALL PLANTE(1)
        STOP
1002    CONTINUE
        DEJA=.TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!  INITIALISES
!
      DO IS=1,NS
        CET(IS) = 0.D0
      ENDDO
!
!   COMPUTES THE TRACER GRADIENTS BY TRIANGLE AND BY NODE
!   COMPUTES THE DIFFUSION TERM
!
      IF(DIFT.OR.NORDRE.EQ.2) CALL GRADNODT(NS,NT,NU,AIRE,AIRS,
     &HSTOK,TN,DPX,DPY,DJXT,DJYT,DXT,DYT,DIFT,CVIST,CET,DTT)
!
      IF(NORDRE.EQ.2) THEN
!
!  REBUILDS 2ND ORDER FOR TRACER
!  *************************************
!
      DO IS=1,NS
        DSP(IS)=0.D0
        DSM(IS)=0.D0
      ENDDO
!
      DO  NSG=1,NSEG
!
         J         = JMI(NSG)
!
         NUBO1     = NUBO(1,NSG)
         NUBO2     = NUBO(2,NSG)
!
         ZF1   =    ZF(NUBO1)
         ZF2   =    ZF(NUBO2)
!
         HI0   =HSTOK(NUBO1)
         HJ0   =HSTOK(NUBO2)
!
!   STICKS TO 1ST ORDER FOR A COVERED EDGE
!
         IF(ZF1.GE. (HJ0+ZF2) .OR. ZF2.GE. (HI0+ZF1)
     &  .OR. 2.*ABS(DSZ(1,NSG)).GE.HI0
     &  .OR. 2.*ABS(DSZ(1,NSG)).GE.HJ0
     &  .OR. 2.*ABS(DSZ(2,NSG)).GE.HI0
     &  .OR. 2.*ABS(DSZ(2,NSG)).GE.HJ0)  THEN
         DST(1,NSG) =0.D0
         DST(2,NSG) =0.D0
        ELSE
!
         AIX       = CMI(1,NSG)-X(NUBO1)
         AIY       = CMI(2,NSG)-Y(NUBO1)
         AJX       = CMI(1,NSG)-X(NUBO2)
         AJY       = CMI(2,NSG)-Y(NUBO2)
!
         GRADI  = AIX*DXT(NUBO1) + AIY*DYT(NUBO1)
         GRADJ  = AJX*DXT(NUBO2) + AJY*DYT(NUBO2)
         GRADIJ  = AIX*DJXT(J) + AIY*DJYT(J)
         GRADJI  = AJX*DJXT(J) + AJY*DJYT(J)
!
!    EXTRAPOLATES THE GRADIENTS AND SLOPE LIMITOR
!
         ILIM=2
         BETA=0.3333D0
         DST(1,NSG)  = EXLIM (ILIM,BETA,GRADI,GRADIJ)
         DST(2,NSG)  = EXLIM (ILIM,BETA,GRADJ,GRADJI)
!
       ENDIF
       ENDDO
!
      DO NSG=1,NSEG
!
         NUBO1     = NUBO(1,NSG)
         NUBO2     = NUBO(2,NSG)
!
         IF(DST(1,NSG).GE.0.D0) THEN
         DSP(NUBO1) = DSP(NUBO1) +
     &  AIRST(1,NSG)* HCSTOK(1,NSG)*DST(1,NSG)
         ELSE
         DSM(NUBO1) = DSM(NUBO1) -
     &  AIRST(1,NSG)* HCSTOK(1,NSG)*DST(1,NSG)
         ENDIF
         IF(DST(2,NSG).GE.0.) THEN
         DSP(NUBO2) = DSP(NUBO2) +
     &  AIRST(2,NSG)* HCSTOK(2,NSG)*DST(2,NSG)
         ELSE
         DSM(NUBO2) = DSM(NUBO2) -
     &  AIRST(2,NSG)* HCSTOK(2,NSG)*DST(2,NSG)
         ENDIF
!
      ENDDO
!
!     COMPUTES THE CORRECTIONS TO ENSURE CONSERVATION OF HT
!                  ***********           ******************
!
      DO IS=1,NS
       CORRT(IS) =  DSM(IS) - DSP(IS)
       AMDS =MAX(DSP(IS),DSM(IS))
        IF(AMDS.GT.0.D0) THEN
        CORRT(IS) = CORRT(IS)/AMDS
        ENDIF
      ENDDO
 12       CONTINUE
!
      ENDIF
!
!     COMPUTES FLUXES FOR THE INTERNAL INTERFACES
!
      DO 500 NSG=1,NSEG
!
         NUBO1     = NUBO(1,NSG)
         NUBO2     = NUBO(2,NSG)
!
         UAS41     = TN(NUBO1)
         UAS42     = TN(NUBO2)
!
         FLU11=FLUXT(NSG)
!
         IF (FLU11.GE.0.) THEN
       IF(NORDRE.GE.2) THEN
          UAS41 = UAS41  + DST(1,NSG) +
     & MIN(0.D0,CORRT(NUBO1))*MAX(0.D0,DST(1,NSG))+
     & MAX(0.D0,CORRT(NUBO1))*MAX(0.D0,-DST(1,NSG))
        ENDIF
         FLU41 =  UAS41 * FLU11
         ELSE
       IF(NORDRE.GE.2) THEN
           UAS42 = UAS42 + DST(2,NSG) +
     & MIN(0.D0,CORRT(NUBO2))*MAX(0.D0,DST(2,NSG))+
     & MAX(0.D0,CORRT(NUBO2))*MAX(0.D0,-DST(2,NSG))
       ENDIF
         FLU41 =  UAS42 * FLU11
        ENDIF
!
         CET(NUBO1) = CET(NUBO1) - FLU41
!
         CET(NUBO2) = CET(NUBO2) + FLU41
!
500   CONTINUE
!
!     BOUNDARY FLUX
!
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
       CET(IS)  = CET(IS) - FLUT
!
      ENDDO
!
!     UPDATES HT
!
      DO  IS =1,NS
!
        HT(IS)  = HTN(IS) +  (CET(IS)+SMTR(IS))/AIRS(IS)
        MASSOU = MASSOU + SMTR(IS)
!
        IF(HT(IS).LE.1.D-15) HT(IS)=0.D0
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
