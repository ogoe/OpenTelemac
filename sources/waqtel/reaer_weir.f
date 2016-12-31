!                   **********************
                     SUBROUTINE REAER_WEIR
!                    **********************
!
     &(FORMRS,H1,H2,ABRS,WATTEMP,EPS,O2SATU,TRUP,TN,
     & ADDTR,WAQPROCESS,IR,NTRAC)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES THE REAERATION AT WEIRS
!
!warning: this is not a source term, this routine changes directly TN
!         questions:
!            1- it changes TN at time tn while it has to be done in tn+1
!            2- it affects the mass conservation !! to adapt bilant
!
!history  R. ATA (LNHE)
!+        02/09/2015
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ADDTR          |-->| NUMBER IF TRACERS ADDED BY WAQPROCESS
!| ABRS           |-->| COEFFICIENT A AND B USED IN RS FORMUL
!| EPS            |-->| TO AVOID DIVISION BY 0
!| FORMRS         |-->| WHICH FROMULA TO COMPUTE RS
!| H1,H2          |-->| WATER DEPTH UPSRTEAM AND DOWNSTREAM WEIR
!| IR             |-->| NODE NUMBER (BELONGS TO WEIR DOWNSTREAM)
!| O2SATU         |-->| O2 SATURATION DENSITY OF WATER (CS)
!| TN             |<--| TRACER AT TIME tn
!| TRUP           |-->| TRACER VALUE UPSTREAM WEIR
!| WAQPROCESS     |-->| WAQ PROCESS
!| WATTEMP        |-->| TEMPERATURE OF WATER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_PARALLEL
      USE INTERFACE_WAQTEL, EX_REAER_WEIR => REAER_WEIR
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)     :: FORMRS,ADDTR,WAQPROCESS,IR
      INTEGER         , INTENT(IN)     :: NTRAC
      DOUBLE PRECISION, INTENT(IN)     :: EPS,H1,H2,ABRS(2),WATTEMP
      DOUBLE PRECISION, INTENT(IN)     :: TRUP,O2SATU
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TN
      INTRINSIC ABS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
      INTEGER           I
      DOUBLE PRECISION  DZ,RSW,TRDO,AB
!
!
      DZ= ABS(H2-H1)
      RSW = 0.D0
!     LETS'S COMPUTE RS IF IT IS NOT TAKEN CONSTANT
      IF(FORMRS.NE.0) THEN
        AB=ABRS(1)*ABRS(2)
!       GAMESON FORMULA 1
        IF(FORMRS.EQ.1)     THEN
          RSW=1.D0+0.5D0*AB*DZ
!       GAMESON FORMULA2
        ELSEIF(FORMRS.EQ.2) THEN
          RSW = 0.11D0*AB*(1.D0+0.046D0*WATTEMP)*DZ
!       WRL FORMULA 1 (NO NEED TO AB ? )
        ELSEIF(FORMRS.EQ.3 )THEN
          RSW = 1.D0+0.69D0*DZ*(1.D0-0.11D0*DZ )
     &       *( 1.D0+0.046D0*WATTEMP)
!       WRL FORMULA 2
        ELSEIF (FORMRS.EQ.4)THEN
          RSW = 1.D0+0.38D0*AB*DZ*(1.D0-0.11D0*DZ)
     &       * (1.D0+0.046D0*WATTEMP )
        ELSE
          IF(LNG.EQ.1)THEN
            WRITE(LU,*)'FORMULE DE RS (REAERATION AU SEUIL)'
            WRITE(LU,*)'INCONNUE  :',FORMRS
            WRITE(LU,*)'LES CHOIX POSSIBLES SONT DE 1 A 4'
          ELSE
            WRITE(LU,*)'FORMULA FOR RS (WEIR REAERATION) '
            WRITE(LU,*)' NOT VALID  :',FORMRS
            WRITE(LU,*)'POSSIBLE CHOICES ARE FROM 1 TO 4'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
!
!       FORCING O2 DENSITY DOWNSTREAM THE WEIR
!
        IF(ABS(RSW).GT.EPS)THEN
          TRDO = O2SATU + (TRUP-O2SATU)/RSW
        ELSE
          WRITE(LU,*)'REAER_WEIR:RSW VERY SMALL',RSW
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(NCSIZE.GT.1)TRDO = P_DMIN(TRDO)+P_DMAX(TRDO)
        IF(WAQPROCESS.EQ.1)THEN
!         O2 PROCESS
          I=NTRAC-ADDTR+1
        ELSEIF(WAQPROCESS.EQ.3)THEN
!         EUTRO PROCESS
          I=NTRAC
        ELSE
          IF(LNG.EQ.1)THEN
            WRITE(LU,*)'REAER_WEIR: PROCESSUS NON CONCERNE '
          ELSE
            WRITE(LU,*)'REAER_WEIR: PROCESS NOT CONCERNED '
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        TN%ADR(I)%P%R(IR)=TRDO
      ENDIF
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!
