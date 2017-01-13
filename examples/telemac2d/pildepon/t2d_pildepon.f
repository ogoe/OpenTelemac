!                    **************************
                     SUBROUTINE UTIMP_TELEMAC2D
!                    **************************
!
     &(LTL,ATL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WRITES OUT ADDITIONAL OUTPUT REQUIRED BY THE USER.
!
!note     THIS SUBROUTINE IS CALLED IN THE SAME PLACES AS THE
!+                MAIN TELEMAC2D OUTPUT SUBROUTINE (NAMED DESIMP),
!+                I.E. CALLED TWICE:
!+
!note   (1) ONCE PER RUN, WHEN LTL==0, INDEPENDENTLY OF WHETHER
!+             'OUTPUT OF INITIAL CONDITIONS : YES' IS SET OR NOT
!note   (2) EACH TIME STEP JUST AFTER DESIMP-OUTPUT
!
!warning  USER SUBROUTINE; DOES NOTHING BY DEFAULT
!
!history  JACEK A. JANKOWSKI PINXIT, BAW KARLSRUHE, JACEK.JANKOWSKI@BAW.DE
!+        **/08/2003
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
!| ATL            |-->| TIME OF TIME STEP, IN SECONDS
!| GRADEBL        |-->| FIRST TIME STEP FOR GRAPHIC OUTPUTS
!| GRAPRDL        |-->| PERIOD OF GRAPHIC OUTPUTS
!| LISDEBL        |-->| FIRST TIME STEP FOR LISTING OUTPUTS
!| LISPRDL        |-->| PERIOD OF LISTING OUTPUTS
!| LTL            |-->| CURRENT TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN) :: ATL
      INTEGER, INTENT(IN) :: LTL,GRADEBL,GRAPRDL,LISDEBL,LISPRDL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NSEGBOR,ERR,LTT
      INTEGER I,N,ILOCAL,IFILE,IFILEM,ITRAC
      INTEGER ISEG,GLO1,GLO2,NSON_U,NSON_D
      DOUBLE PRECISION X1,Y1,X2,Y2,HM,LONG,DENOM,RELATI
      DOUBLE PRECISION PRESS_F1,PRESS_F2,XI,YI,CIR1,CIR2
      DOUBLE PRECISION GPRDTIME,RESTE, PERDUE
      DOUBLE PRECISION, PARAMETER :: GAMMAW= 9801.D0
      DOUBLE PRECISION, PARAMETER :: EPSS=1.E-10
      INTEGER, ALLOCATABLE :: SEGM1(:),SEGM2(:)
      DOUBLE PRECISION, ALLOCATABLE :: LONGS1(:),LONGS2(:)
!
      INTEGER  P_ISUM
      DOUBLE PRECISION P_DSUM
      EXTERNAL P_ISUM,P_DSUM
!
      SAVE NSON_U, NSON_D
      SAVE SEGM1,SEGM2,LONGS1,LONGS2
!***********************************************************************
! USER OUTPUT
!
      IF(LT.EQ.0) THEN
        ALLOCATE(SEGM1(MESH%NSEG),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR,'UTIMP')
        ALLOCATE(LONGS1(MESH%NSEG),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR,'UTIMP')
        ALLOCATE(SEGM2(MESH%NSEG),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR,'UTIMP')
        ALLOCATE(LONGS2(MESH%NSEG),STAT=ERR)
        CALL CHECK_ALLOCATE(ERR,'UTIMP')
!
        DO ISEG=1, MESH%NSEG
          SEGM1(ISEG)=0
          LONGS1(ISEG)=0.D0
          SEGM2(ISEG)=0
          LONGS2(ISEG)=0.D0
        ENDDO
        NSON_U = 0
        NSON_D = 0
!
! DISTINCTION LINEAR AND QUADRATIC ELEMENTS
!
        IF(U%ELM.EQ.13) THEN
          DO ISEG=1, MESH%NSEG
            GLO1 = MESH%GLOSEG%I(ISEG)
            GLO2 = MESH%GLOSEG%I(ISEG+2*MESH%NSEG)
            X1 = MESH%X%R(GLO1)
            X2 = MESH%X%R(GLO2)
            Y1 = MESH%Y%R(GLO1)
            Y2 = MESH%Y%R(GLO2)
!
! FIND POINTS INSIDE THE CIRCUMFERENCE:
! UPPER CYLINDER
!
            CIR1=(X1+5.D0)**2+(Y1-4.D0)**2
            CIR2=(X2+5.D0)**2+(Y2-4.D0)**2
            IF(CIR1.LE.4.1D0) THEN
              IF(CIR2.LE.4.1D0) THEN
                NSON_U = NSON_U+1
                SEGM1(NSON_U) = ISEG
                LONG=SQRT((X1-X2)**2+(Y1-Y2)**2)
                LONGS1(NSON_U) = LONG
              ENDIF
            ENDIF
! FIND POINTS INSIDE THE CIRCUMFERENCE:
! LOWER CYLINDER
            CIR1=(X1+5.D0)**2+(Y1+4.D0)**2
            CIR2=(X2+5.D0)**2+(Y2+4.D0)**2
            IF(CIR1.LE.4.1D0) THEN
              IF(CIR2.LE.4.1D0) THEN
                NSON_D = NSON_D+1
                SEGM2(NSON_D) = ISEG
                LONG=SQRT((X1-X2)**2+(Y1-Y2)**2)
                LONGS2(NSON_D) = LONG
              ENDIF
            ENDIF
          ENDDO
        ELSEIF(U%ELM.EQ.11.OR.EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
          DO ISEG=1, MESH%NSEG
            GLO1 = MESH%GLOSEG%I(ISEG)
            GLO2 = MESH%GLOSEG%I(ISEG+MESH%NSEG)
            X1 = MESH%X%R(GLO1)
            X2 = MESH%X%R(GLO2)
            Y1 = MESH%Y%R(GLO1)
            Y2 = MESH%Y%R(GLO2)
!
! FIND POINTS INSIDE THE CIRCUMFERENCE:
! UPPER CYLINDER
!
            CIR1=(X1+5.D0)**2+(Y1-4.D0)**2
            CIR2=(X2+5.D0)**2+(Y2-4.D0)**2
            IF(CIR1.LE.4.1D0) THEN
              IF(CIR2.LE.4.1D0) THEN
                NSON_U = NSON_U+1
                SEGM1(NSON_U) = ISEG
                LONG=SQRT((X1-X2)**2+(Y1-Y2)**2)
                LONGS1(NSON_U) = LONG
              ENDIF
            ENDIF
! FIND POINTS INSIDE THE CIRCUMFERENCE:
! LOWER CYLINDER
            CIR1=(X1+5.D0)**2+(Y1+4.D0)**2
            CIR2=(X2+5.D0)**2+(Y2+4.D0)**2
            IF(CIR1.LE.4.1D0) THEN
              IF(CIR2.LE.4.1D0) THEN
                NSON_D = NSON_D+1
                SEGM2(NSON_D) = ISEG
                LONG=SQRT((X1-X2)**2+(Y1-Y2)**2)
                LONGS2(NSON_D) = LONG
              ENDIF
            ENDIF
          ENDDO
        ELSE
          IF(LNG.EQ.1) WRITE(LU,98) U%ELM
          IF(LNG.EQ.2) WRITE(LU,99) U%ELM
98        FORMAT(1X,'UTIMP: IELM=',1I6,' TYPE D''ELEMENT NON PREVU')
99        FORMAT(1X,'UTIMP: IELM=',1I6,
     &          ' TYPE OF ELEMENT NOT AVAILABLE')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF !LT.EQ.0
!
! USING LIST OF SEGMENTS TO COMPUTE THE FORCE ON CYLINDER
!
      IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!
      IF(U%ELM.EQ.13) THEN
        IFILE = T2D_FILES(T2DRFO)%LU
        PRESS_F1=0.D0
        IF(NSON_U.GT.0) THEN
! SAMPLING AFTER t=600s ("REGULAR" SOLUTION)
          IF(AT.GE.600.D0) THEN
            DO I=1,NSON_U
              ISEG=SEGM1(I)
              GLO1=MESH%GLOSEG%I(ISEG)
              GLO2=MESH%GLOSEG%I(ISEG+2*MESH%NSEG)
              HM=(H%R(GLO1)+H%R(GLO2))/2.D0
              PRESS_F1=PRESS_F1+GAMMAW*HM**2*LONGS1(I)/2.D0
            ENDDO
          ENDIF
        ENDIF
        IF(NCSIZE.GT.0) PRESS_F1=P_DSUM(PRESS_F1)
!
        PRESS_F2=0.D0
        IF(NSON_D.GT.0) THEN
          IF(AT.GE.600.D0) THEN
            DO I=1,NSON_D
              ISEG=SEGM2(I)
              GLO1=MESH%GLOSEG%I(ISEG)
              GLO2=MESH%GLOSEG%I(ISEG+2*MESH%NSEG)
              HM=(H%R(GLO1)+H%R(GLO2))/2.D0
              PRESS_F2=PRESS_F2+GAMMAW*HM**2*LONGS2(I)/2.D0
            ENDDO
          ENDIF
        ENDIF
        IF(NCSIZE.GT.0) PRESS_F2=P_DSUM(PRESS_F2)
        IF (AT.GE.600.D0.AND.IPID.EQ.0) THEN
          WRITE (IFILE,*) PRESS_F1, PRESS_F2
        ENDIF
!
      ELSEIF(U%ELM.EQ.11) THEN
        IFILE = T2D_FILES(T2DRFO)%LU
        PRESS_F1=0.D0
        IF(NSON_U.GT.0) THEN
! SAMPLING AFTER t=600s ("REGULAR" SOLUTION)
          IF(AT.GE.600.D0) THEN
            DO I=1,NSON_U
              ISEG=SEGM1(I)
              GLO1=MESH%GLOSEG%I(ISEG)
              GLO2=MESH%GLOSEG%I(ISEG+MESH%NSEG)
              HM=(H%R(GLO1)+H%R(GLO2))/2.D0
              PRESS_F1=PRESS_F1+GAMMAW*HM**2*LONGS1(I)/2.D0
            ENDDO
          ENDIF
        ENDIF
        IF(NCSIZE.GT.0) PRESS_F1=P_DSUM(PRESS_F1)
!
        PRESS_F2=0.D0
        IF(NSON_D.GT.0) THEN
          IF(AT.GE.600.D0) THEN
            DO I=1,NSON_D
              ISEG=SEGM2(I)
              GLO1=MESH%GLOSEG%I(ISEG)
              GLO2=MESH%GLOSEG%I(ISEG+MESH%NSEG)
              HM=(H%R(GLO1)+H%R(GLO2))/2.D0
              PRESS_F2=PRESS_F2+GAMMAW*HM**2*LONGS2(I)/2.D0
            ENDDO
          ENDIF
        ENDIF
        IF(NCSIZE.GT.0) PRESS_F2=P_DSUM(PRESS_F2)
        IF (AT.GE.600.D0.AND.IPID.EQ.0) THEN
          WRITE (IFILE,*) PRESS_F1, PRESS_F2
        ENDIF
      ELSE
        IF(LNG.EQ.1) WRITE(LU,100) U%ELM
        IF(LNG.EQ.2) WRITE(LU,101) U%ELM
100      FORMAT(1X,'UTIMP: IELM=',1I6,' TYPE D''ELEMENT NON PREVU')
101      FORMAT(1X,'UTIMP: IELM=',1I6,
     &        ' TYPE OF ELEMENT NOT AVAILABLE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
! WRITE FINAL MASS BALANCE INTO A FILE
!
      IF(LT.EQ.NIT.AND.IPID.EQ.0) THEN
        IF(NTRAC.GT.0) THEN
          CALL GET_FREE_ID(IFILEM)
! OPEN FILE
          OPEN(UNIT=IFILEM,FILE='../massb_A.txt',STATUS='REPLACE',
     &         ACTION='WRITE',POSITION='APPEND')
          DO ITRAC=1,NTRAC
            PERDUE = MASTR0(ITRAC)+MASTEN(ITRAC)+MASTOU(ITRAC)
     &               -MASTR2(ITRAC)
            DENOM = MAX(ABS(MASTR0(ITRAC)),ABS(MASTR2(ITRAC)),
     &              ABS(MASTEN(ITRAC)),ABS(MASTOU(ITRAC)))
            IF(DENOM.GT.1.D-8) THEN
              RELATI = PERDUE / DENOM
            ENDIF
            IF(ITRAC.NE.NTRAC) THEN
! WRITE IN IT
              WRITE(UNIT=IFILEM,FMT=404) MASTR0(ITRAC),
     &              MASTR2(ITRAC),MASTEN(ITRAC)+MASTOU(ITRAC),
     &              PERDUE,RELATI
404           FORMAT(G16.7,'&',G16.7,'&',G16.7,'&',G16.7,'&',G16.7,'\\')
            ELSE
              WRITE(UNIT=IFILEM,FMT=405) MASTR0(ITRAC),MASTR2(ITRAC),
     &              MASTEN(ITRAC)+MASTOU(ITRAC),PERDUE,
     &              RELATI
405           FORMAT(G16.7,'&',G16.7,'&',G16.7,'&',G16.7,'&',G16.7)
            ENDIF
          ENDDO
          CLOSE(IFILEM)
        ENDIF
      ENDIF
!
      ELSE
!
! FVM
!
! SAMPLING EVERY 0.1 S
        GPRDTIME=0.1D0
        IF(GPRDTIME.LT.EPSS)THEN
          WRITE(LU,*)'ERROR IN UTIMP'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(AT.GE.600.D0)THEN
!
          LTT=CEILING(AT/GPRDTIME)
          RESTE=(LTT*GPRDTIME-AT)/GPRDTIME
          IF(RESTE.LT.EPSS.OR.ABS(RESTE-1.D0).LT.EPSS.OR.
!                                 CASE WHERE RESTE=1
     &      LT.EQ.NIT)THEN
!
! WRITE IN THE FILE
            IFILE = T2D_FILES(T2DRFO)%LU
            PRESS_F1=0.D0
            IF(NSON_U.GT.0) THEN
                DO I=1,NSON_U
                  ISEG=SEGM1(I)
                  GLO1=MESH%GLOSEG%I(ISEG)
                  GLO2=MESH%GLOSEG%I(ISEG+MESH%NSEG)
                  HM=(H%R(GLO1)+H%R(GLO2))/2.D0
                  PRESS_F1=PRESS_F1+GAMMAW*HM**2*LONGS1(I)/2.D0
                ENDDO
            ENDIF
            IF(NCSIZE.GT.0) PRESS_F1=P_DSUM(PRESS_F1)
!
            PRESS_F2=0.D0
            IF(NSON_D.GT.0) THEN
                DO I=1,NSON_D
                  ISEG=SEGM2(I)
                  GLO1=MESH%GLOSEG%I(ISEG)
                  GLO2=MESH%GLOSEG%I(ISEG+MESH%NSEG)
                  HM=(H%R(GLO1)+H%R(GLO2))/2.D0
                  PRESS_F2=PRESS_F2+GAMMAW*HM**2*LONGS2(I)/2.D0
                ENDDO
            ENDIF
            IF(NCSIZE.GT.0) PRESS_F2=P_DSUM(PRESS_F2)
            IF(IPID.EQ.0) WRITE (IFILE,*) PRESS_F1, PRESS_F2
          ENDIF
        ENDIF !AT.GE.600.D0
!
      ENDIF !SAINT-VENANT VF
!
      RETURN
      END SUBROUTINE UTIMP_TELEMAC2D
!
! MODIFICATION OF CALDT FOR (HARDCODED) GPRDTIME: EQUAL TO 0.1
! THIS IS DONE TO AVOID A LARGE RESULT FILE
!                      ****************
                       SUBROUTINE CALDT
!                      ****************
!
     &(NS,G,H,U,V,DTHAUT,DT,AT,TMAX,CFL,ICIN,DTVARI,LISTIN)
!
!***********************************************************************
!TELEMAC-2D VERSION 7.0                                 30/06/13
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
!+
!
!history  R. ATA (EDF-LNHE)
!+        15/01/2013
!+        V6P3
!+   introduce fixed time step
!+   handle very specific cases
!+   parallelization
!
!history  R. ATA (EDF-LNHE) INTRODUCE FIXED TIME STEP
!+        30/06/2013
!+        V6P3
!+  clean and remove unused variables
!
!history  R. ATA (EDF-LNHE) INTRODUCE FIXED TIME STEP
!+        11/01/2016
!+        V7P2
!+  adjust time step to graphical outputs
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NS             |-->| TOTAL NUMBER OF NODES
!| G              |-->| GRAVITY
!| H              |-->| WATER DEPTHS
!| U              |-->| X-COMPONENT OF VELOCITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| DTHAUT         |-->| CHARACTERISTIC LENTH FOR CFL (DX)
!| DT             |<--| TIME STEP
!| AT             |---| CURRENT TIME
!| TMAX           |---| MAX SIMULATION TIME
!| CFL            |-->| CFL
!| ICIN           |-->| WHICH SCHEME (SEE LIST BELOW)
!| DTVARI         |-->| LOGICAL: VARIABLE TIME STEP
!| LISTIN         |-->| LOGICAL: OUTPUT LISTING
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF_DEF, ONLY:NCSIZE
      USE DECLARATIONS_TELEMAC2D,ONLY:DTINI,LEOPRD
      USE INTERFACE_TELEMAC2D, EX_CALDT => CALDT
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NS,ICIN
      DOUBLE PRECISION, INTENT(INOUT) :: DT
      DOUBLE PRECISION, INTENT(IN) :: H(NS),U(NS),V(NS),DTHAUT(NS)
      DOUBLE PRECISION, INTENT(IN) :: G,CFL,AT,TMAX
      LOGICAL, INTENT(IN) :: DTVARI,LISTIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS
      LOGICAL DEJA
      DOUBLE PRECISION RA3,EPSL,SIGMAX,UA2,UA3,UNORM,DTT
      DOUBLE PRECISION RESTE, GPRDTIME
      DOUBLE PRECISION P_DMIN
      EXTERNAL P_DMIN
      INTRINSIC MIN,CEILING
!
!-----------------------------------------------------------------------
!
!   +++++++++++++++++++++++++
!     VARIABLE TIME STEP
      IF(DTVARI)THEN
!   +++++++++++++++++++++++++
      DEJA=.FALSE.
      DT = 1.E+12
      EPSL = 0.01D0
!
      IF(ICIN.EQ.1) THEN
!       KINETIC SCHEME
!
        RA3 = SQRT(1.5D0*G)
        DO IS=1,NS
          IF(H(IS).LT.0.D0.AND.LISTIN.AND..NOT.DEJA)THEN
            WRITE(LU,*) 'CALDT WARNING : NEGATIVE WATER DEPTH'
            WRITE(LU,*) ' SEE NODE:',IS,' FOR EXAMPLE'
            DEJA = .TRUE.
          ELSE
            SIGMAX = ABS(H(IS))
            UA2    = U(IS)
            UA3    = V(IS)
            UNORM=SQRT(UA2*UA2 + UA3*UA3)
            SIGMAX= MAX(EPSL, RA3*SQRT(SIGMAX) +UNORM )
            DT = MIN(DT, CFL*DTHAUT(IS)/SIGMAX)
          ENDIF
        ENDDO
!
      ELSEIF(ICIN.EQ.0.OR.ICIN.EQ.2.OR.ICIN.EQ.3.OR.
     &       ICIN.EQ.4.OR.ICIN.EQ.5) THEN
!     SCHEMES OF ROE, ZOKAGOA, TCHAMEN, HLLC AND WAF
!
        DO IS=1,NS
          IF(H(IS).LT.0.D0.AND.LISTIN.AND..NOT.DEJA)THEN
            WRITE(LU,*) 'CALDT WARNING : NEGATIVE WATER DEPTH'
            WRITE(LU,*) ' SEE NODE:',IS,' FOR EXAMPLE'
            DEJA = .TRUE.
          ELSE
            UA2    = U(IS)
            UA3    = V(IS)
            UNORM=SQRT(UA2*UA2 + UA3*UA3)
            SIGMAX= MAX(EPSL,SQRT(G*ABS(H(IS)))+UNORM)
!           DTHAUT=|Ci|/Sum(Lij)
            DT = MIN(DT, CFL*DTHAUT(IS)/SIGMAX)
          ENDIF
        ENDDO
!
      ELSE
        IF(LNG.EQ.1) WRITE(LU,4010) ICIN
        IF(LNG.EQ.2) WRITE(LU,4020) ICIN
4010    FORMAT(1X,'CALDT: ERREUR DANS LE CHOIX DE ICIN : ',1I6)
4020    FORMAT(1X,'CALDT: ERROR IN THE CHOICE OF ICIN: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
      ENDIF
!
      IF(DTVARI) THEN
!
        IF(TMAX.LT.DT)DT=TMAX !REALLY CRAZY CASES
        DTT=TMAX-AT
        IF(CFL.GE.1.D0) DT=0.9D0*DT/CFL
        IF(DTT.LE.DT.AND.DTT.GT.0.D0)  DT=DTT !LAST TIME STEP
        IF(AT.GT.TMAX) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'CALDT: MAUVAIS CHOIX DE PARAMETRES DE TEMPS '
            WRITE(LU,*)'TEMPS ET TEMPS MAX',AT,TMAX
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*)'CALDT: BAD TIME PARAMETERS'
            WRITE(LU,*)'TIME AND TMAX',AT,TMAX
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSE
!
!       DT NOT VARIABLE
!
        IF(LISTIN.AND.LNG.EQ.1) THEN
          WRITE(LU,*) 'ATTENTION: PAS DE TEMPS FIXE ET CFL NON FOURNI.!'
          WRITE(LU,*) 'PAS DE TEMPS PEUT NE PAS VERIFIER LE CFL : ',DT
        ELSEIF(LISTIN.AND.LNG.EQ.2) THEN
          WRITE(LU,*) 'WARNING: FIXED TIME-STEP AND CFL NOT GIVEN!...! '
          WRITE(LU,*) 'TIME-STEP MAY NOT SATISFY CFL CONDITION: ',DT
        ENDIF
!
      ENDIF
!
!*************************************************************************
!      GRAPHIC OUTPUTS LIKE ASKED BY USER: DT ADATPATION
!*************************************************************************
        GPRDTIME=0.1D0
        IF(GPRDTIME.LT.1.E-12)THEN
          WRITE(LU,*) 'CALDT: PROBLEM WITH PARAMETERS: DTINI,LEOPRD',
     &                 DTINI,LEOPRD
          CALL PLANTE(1)
          STOP
!       CASE WHERE TIME STEP IS BIGGER THEN GRAPHIC OUTPUT (GPRDTIME)
        ELSEIF(GPRDTIME.LT.DT)THEN
          DT=DTINI
          IF(LISTIN)THEN
            WRITE(LU,*) 'WARNING: GRAPHICAL OUTPUT NOT OPTIMIZED: '
            WRITE(LU,*) '   - INITIAL TIME STEP TOO SMALL '
            WRITE(LU,*) '   - AND/OR PERIOD OF GRAPHIC OUTPUT TOO SMALL'
            WRITE(LU,*) 'THIS COULD REDUCE COMPUTATION TIME-STEP'
            WRITE(LU,*) 'AND INCREASE CPU TIME'
          ENDIF
        ENDIF
!       GRAPHIC OUTPUT
        IS=CEILING(AT/GPRDTIME)
        RESTE=IS*GPRDTIME-AT
        IF(RESTE.GT.1.E-13)DT=MIN(RESTE,DT)
!
!       FOR PARALLELISM
!
        IF(NCSIZE.GT.1) DT=P_DMIN(DT)
!
!
!*************************************************************************
!     ERROR TREATMENT AND LISTING OUTPUTS
!*************************************************************************
!
!     CASE DT <=0
!
      IF(DT.LE.1.E-12) THEN
        IF(LISTIN.AND.LNG.EQ.1) THEN
          WRITE(LU,*) 'PAS DE TEMPS NEGATIF OU NUL: ',DT
          WRITE(LU,*) 'PROBABLEMENT A CAUSE D UNE HAUTEUR'
          WRITE(LU,*) 'D EAU NULLE PARTOUT ...'
        ENDIF
        IF(LISTIN.AND.LNG.EQ.2) THEN
          WRITE(LU,*) 'NEGATIVE (OR NIL) TIME-STEP: ',DT
          WRITE(LU,*) 'PROBABLY DUE TO NIL WATER'
          WRITE(LU,*) 'DEPTH EVERYWHERE IN THE DOMAIN ...'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,*) 'PAS DE TEMPS : ',DT
      IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,*) 'TIME-STEP: ',DT
      IF(CFL.GE.1.D0) THEN
        IF(LISTIN.AND.LNG.EQ.1) THEN
          WRITE(LU,*) 'ATTENTION CFL NON FOURNI OU > 1 !...!'
          WRITE(LU,*) 'PAS DE TEMPS (AVEC CFL = 0.9) : ',DT
        ELSEIF(LISTIN.AND.LNG.EQ.2) THEN
          WRITE(LU,*) 'WARNING: CFL NOT GIVEN OR >1 !...! '
          WRITE(LU,*) 'TIME-STEP (WITH CFL = 0.9): ',DT
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

