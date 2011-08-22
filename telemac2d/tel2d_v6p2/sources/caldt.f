!                      ****************
                       SUBROUTINE CALDT
!                      ****************
!
     &(NS,G,H,U,V,DTHAUT,DT,CFL,ICIN,DTVARI,LISTIN)
!
!***********************************************************************
!TELEMAC-2D VERSION 6.1                                 22/11/10
!***********************************************************************
!
!brief  COMPUTES THE TIME STEP UNDER CFL CONDITION
!
!history  INRIA FOR KINETIC SCHEMES
!+
!+        V5P8
!+
!
!history  R. ATA (EDF-LNHE) DT FOR REMAINING SCHEMES
!+        15/03/2010
!+        V6P1
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NS             |-->| TOTAL NUMBER OF NODES 
!| G              |-->| GRAVITY
!| H              |-->| WATER DEPTHS
!| U              |-->| X-COMPONENT OF VELOCITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| DTHAUT         |-->| CHARACTERISTIC LENTH FOR CFL (DX)
!| DT             |<--| TIME STEP 
!| CFL            |-->| CFL
!| ICIN           |-->| WHICH SCHEME (SEE LIST BELOW)
!| DTVARI         |-->| LOGICAL: VARIABLE TIME STEP
!| LISTIN         |-->| LOGICAL: OUTPUT LISTING
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU                                                    
      COMMON/INFO/LNG,LU 
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NS,ICIN
      DOUBLE PRECISION, INTENT(INOUT) :: DT
      DOUBLE PRECISION, INTENT(IN) :: H(NS),U(NS),V(NS),DTHAUT(NS)
      DOUBLE PRECISION, INTENT(IN) :: G,CFL
      LOGICAL, INTENT(IN) :: DTVARI,LISTIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! 
      INTEGER IS
!
      DOUBLE PRECISION RA3,EPSL,SIGMAX ,UA2, UA3, UNORM  
!
!-----------------------------------------------------------------------
!
      IF(ICIN.EQ.1.OR.ICIN.EQ.2) THEN
!
!       KINETIC SCHEME
!
        RA3 = SQRT(1.5D0*G)
!
        DT = 1.E+12
        EPSL = 0.01D0
!
        DO IS=1,NS
          SIGMAX = H(IS)
          UA2    = U(IS)
          UA3    = V(IS)                                       
          UNORM=SQRT(UA2*UA2 + UA3*UA3)
          SIGMAX= MAX(EPSL, RA3*SQRT(SIGMAX) +UNORM )
          DT = MIN(DT, CFL*DTHAUT(IS)/SIGMAX)
        ENDDO
!
      ELSEIF(ICIN.EQ.0.OR.ICIN.EQ.3.OR.ICIN.EQ.4.OR.ICIN.EQ.5) THEN
!
!     SCHEMES OF ROE, ZOKAGOA, TCHAMEN, HLLC AND WAF
!
        DT = 1.E+12
        EPSL = 0.01D0
C
        DO IS=1,NS
          SIGMAX = G*H(IS)
          UA2    = U(IS)
          UA3    = V(IS)                                       
          UNORM=SQRT(UA2*UA2 + UA3*UA3)
          SIGMAX= MAX(EPSL, SQRT(SIGMAX) +UNORM )
!         DTHAUT=|Ci|/Sum(Lij) IS THE CHOICE OF INRIA
!         WE CAN CHANGE FOR MIN(CMI) FOR INSTANCE
          DT = MIN(DT, CFL*DTHAUT(IS)/SIGMAX)
        ENDDO
!
      ELSE
        IF(LNG.EQ.1) WRITE(LU,4010) ICIN
        IF(LNG.EQ.2) WRITE(LU,4020) ICIN
4010    FORMAT(1X,'FLU_TCH : ERREUR DANS LE CHOIX DE ICIN : ',1I6)
4020    FORMAT(1X,'FLU_TCH: ERROR IN THE CHOICE OF ICIN: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!*************************************************************************
!ERROR TREATMENT AND LISTING OUTPUTS
!*************************************************************************
!
      IF(DTVARI.AND.CFL.LT.1.D0) THEN
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,*) 'PAS DE TEMPS : ',DT
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,*) 'TIME-STEP: ',DT
      ELSEIF(DTVARI.AND.CFL.GE.1.D0) THEN
        DT=0.5D0*DT/CFL
        IF(LISTIN.AND.LNG.EQ.1) THEN 
          WRITE(LU,*) 'ATTENTION CFL DEMANDE > 1 !...!'
          WRITE(LU,*) 'PAS DE TEMPS (AVEC CFL = 0.5) : ',DT
        ELSEIF(LISTIN.AND.LNG.EQ.2) THEN 
          WRITE(LU,*) 'WARNING: WANTED CFL >1 !...! '
          WRITE(LU,*) 'TIME-STEP (WITH CFL = 0.5): ',DT
        ENDIF
      ELSEIF(.NOT.DTVARI) THEN
!       INCLUDES THE CASES: NOT DTVARI, ALL CFL (> AND < 1)
        IF(LISTIN.AND.LNG.EQ.1) THEN 
          WRITE(LU,*) 'ATTENTION CFL NON FOURNI !...!'
          WRITE(LU,*) 'PAS DE TEMPS (AVEC CFL = 0.5) : ',DT
        ELSEIF(LISTIN.AND.LNG.EQ.2) THEN 
          WRITE(LU,*) 'WARNING: PLEASE GIVE CFL  !...! '
          WRITE(LU,*) 'TIME-STEP (WITH CFL = 0.5): ',DT
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
