!                    *************************
                      SUBROUTINE CALCS3D_EUTRO
!                    *************************
     &  (NPOIN3,NPOIN2,NPLAN,WATTEMP,TN,TEXP,TIMP,RAYEFF,NTRAC,HPROP,
     &   ZPROP,T1,T2_1,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,DEBUG,YATEMP,
     &   IND_T,UN,VN)
!
!
!***********************************************************************
! TELEMAC2D   V7P2                                        21/03/2016
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR EUTRO WAQ 3d PROCESS
!
!history  R. ATA
!+        21/03/2016
!+        V7P2
!+       CREATION (VOID)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEBUG          |-->| IF NE.0 THEN DEBUG MODE
!| DT             |-->| TIME STEP
!| HPROP          |-->| WATER DEPTH AFTER PROPAGATION (2D STRUCTURE)
!| IND_T          |-->| INDEX OF TEMPERATURE IN TRACER TABLE
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| NPOIN2         |-->| TOTAL NUMBER OF MESH NODES
!| NPOIN3         |-->| TOTAL NUMBER OF MESH NODES
!| NTRAC          |-->| OLD NUMBER OF TRACER (BEFORE WAQ RTACERS)
!| RAYEFF         |-->| EFFECT OF SUNSHINE ON ALGAE GROWTH
!| T1,..,T12      |<->| 3D WORKING STRUCTURES
!| T2_1,T2_2      |<->| 2D WORKING STRUCTURES
!| TN             |-->| TRACER STRUCUTRE
!| TEXP           |<--| EXPLICIT SOURCE TERMES
!| VOLU2D         |-->| BASES AREA (NON ASSEMBLED)
!| WATTEMP        |-->| WATER TEMPERATURE
!| YATEMP         |-->| IF TEMPERATURE IS VARIABLE
!| ZPROP          |-->| 3D MESH NODE POSTIONS AFTER PROPAGATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------
!***********************************************************************
      USE BIEF
      USE DECLARATIONS_WAQTEL,ONLY:CMAX,CTOXIC,IK,K520,O2SATU,K2,
     &                            ZSD,I0,KPE,KP,KN,CMORALG,FORMCS,
     &                            TRESPIR,PROPHOC,DTP,PRONITC,K22,WLOR,
     &                            K360,K320,PERNITS,WPOR,WNOR,FORMK2,
     &                            O2PHOTO,K120,O2NITRI,DEMBEN,MEXTINC,
     &                            SECTODAY
      USE INTERFACE_WAQTEL, EX_CALCS3D_EUTRO => CALCS3D_EUTRO
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  U        ! TR ! D  ! VITESSE DE L'EAU                             !
! !  J        ! TR ! D  ! PENTE DE LA LIGNE DE CHARGE                  !
! !  WPOR     ! R  !    ! VITESSE DE SEDIMENTATION DU PHOSPHORE ORGANIQ!
! !  WNOR     ! R  !    ! VITESSE DE SEDIMENTATION DE L AZOTE ORGANIQUE!
! !  CMAX     ! R  !    ! TAUX DE CROISSANCE ALGALE MAXIMUM A 20°C     !
! !  ZSD       ! R  !    ! PROFONDEUR DE SECCHI                         !
! !  KPE      ! R  !    ! COEF D EXTINCTION DU RAY SANS PHYTO          !
! !  BETA     ! R  !    ! COEF DE TURBIDITE VEGETALE                   !
! !  IK       ! R  !    ! PARAMETRE DE CALAGE DE LA FORMULE DE SMITH   !
! !  KP       ! R  !    ! CONSTANTE DE DEMI-SATURATION EN PHOSPHATE    !
! !  KN       ! R  !    ! CONSTANTE DE DEMI-SATURATION EN AZOTE        !
! !  ALPHA    ! R  !    ! COEF 1 DE TOXICITE DE L EAU POUR LES ALGUES  !
! !  ALPHA2   ! R  !    ! COEF 2 DE TOXICITE DE L EAU POUR LES ALGUES  !
! !  RP       ! R  !    ! TAUX DE RESP. DE LA BIOMASSE ALGALE A 20°C   !
! !  PROPHOC       ! R  !    ! PROP DE PHOSPHORE DANS LES CELLULES DU PHYTO !
! !  DTP      ! R  !    ! POURCENT DE PHOSPH DIRECT ASSIM DS PHY MORT  !
! !  K320     ! R  !    ! TAUX DE TRANSFORMATION DU POR EN PO4         !
! !  PRONITC       ! R  !    ! PROP D AZOTE DANS LES CELLULES DU PHYTO      !
! !  PERNITS      ! R  !    ! POURCENT D AZOTE DIRECT ASSIM DS PHY MORT    !
! !  K360     ! R  !    ! TAUX DE TRANSFORMATION DU NOR EN NO3         !
! !  M1       ! R  !    ! COEF 1 DE MORTALITE ALGALE A 20°C            !
! !  M2       ! R  !    ! COEF 2 DE MORTALITE ALGALE A 20°C            !
! !  WLOR     ! R  !    ! VITESSE DE SEDIMENTATION DE LA CHARGE ORGANIQ!
! !  K120     ! R  !    ! CINETIQUE DE DEGRADATION DE LA CHARGE ORGANIQ!
! !  K520     ! R  !    ! CINETIQUE DE NITRIFICATION                   !
! !  F        ! R  !    ! QTTE D O2 PRODUITE PAR PHOTOSYNTHESE         !
! !  N        ! R  !    ! QTTE D O2 CONSOMMEE PAR NITRIFICATION        !
! !  BEN      ! R  !    ! DEMANDE BENTHIQUE A 20°C                     !
! !  K2       ! R  !    ! COEFFICIENT DE REAERATION                    !
! !  FORMK2   ! E  !    ! FORMULE DE CALCUL DE K2                      !
! !  CS       ! R  !    ! CONCENTRATION DE SATURATION EN OXYG DE L'EAU !
! !  FORMCS   ! E  !    ! FORMULE DE CALCUL DE CS                      !
! !  RSw       ! R  !    ! COEFFICIENT DE REAERATION AUX SEUILS         !
! !  FORMRS   ! E  !    ! FORMULE DE CALCUL DE R                       !
! !  ARS      ! R  !    ! COEFFICIENT A DES FORMULES DE CALCUL DE R    !
! !  BRS      ! R  !    ! COEFFICIENT B DES FORMULES DE CALCUL DE R    !
! !  NBSEUI   ! E  !    ! NOMBRE DE SEUILS                             !
! !  XSEUI    ! TR !    ! ABSCISSES DES SEUILS                         !
! !  DZS      ! TR !    ! DELTA Z AUX SEUILS                           !
! !           !    !    !                                              !
! !  IF1      ! TR ! D  ! INDIC DE LECTURE DU FICHIER DES PARAMETRES   !
! !___________!____!____!______________________________________________!
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!               (ENTREE)              (SORTIE)       (ENTREE/SORTIE)
!-----------------------------------------------------------------------
!***********************************************************************
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER          , INTENT(IN   ) :: NPOIN3,NPOIN2,NPLAN
      INTEGER          , INTENT(IN   ) :: NTRAC,DEBUG,IND_T
      LOGICAL          , INTENT(IN   ) :: YATEMP
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,ZPROP,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,TIMP,RAYEFF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T1,T2_1,T3,T4,T5,T6,T7,T8,T9
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T10,T11,T12
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
!
      INTEGER                     :: RANKTR1,RANKTR2,RANKTR3,RANKTR4,I,J
      INTEGER                     :: ERR,RANKTR5,RANKTR6,RANKTR7,RANKTR8
      INTEGER         , PARAMETER :: ADDTR = 8
      DOUBLE PRECISION, PARAMETER :: UNSURVINGT=0.05D0
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-10
      DOUBLE PRECISION, PARAMETER :: CORR1=1.065D0
      DOUBLE PRECISION, PARAMETER :: CORR2=1.0241D0
      DOUBLE PRECISION            :: G2,G3,CC,POWER
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN EUTRO, STEP 0'
!
!     INITIALISATION
!
!     CS IS STOCKED IN T2_1 ==> 2D TABLE
      CALL OS( 'X=0     ',X=T2_1)
!     G2 IS STOCKED IN T6 ==> 3D TABLE
      CALL OS( 'X=0     ',X=T6)
!     CP IS STOCKED IN T7
      CALL OS( 'X=0     ',X=T7)
!     DP IS STOCKED IN T4
      CALL OS( 'X=0     ',X=T4)
!     LNUT IS STOCKED IN T5
      CALL OS( 'X=0     ',X=T5)
!     RN IS STOCKED IN T8
      CALL OS( 'X=0     ',X=T8)
!
!     PRELIMINARY COMPUTATIONS
!
      RANKTR1 = NTRAC-ADDTR+1  ! PHY
      RANKTR2 = RANKTR1+1      ! PO4
      RANKTR3 = RANKTR2+1      ! POR
      RANKTR4 = RANKTR3+1      ! NO3
      RANKTR5 = RANKTR4+1      ! NOR
      RANKTR6 = RANKTR5+1      ! NH4
      RANKTR7 = RANKTR6+1      ! L
      RANKTR8 = NTRAC          ! O2
!
!     G2 IS STOCKED IN T6,WE TAKE INTO ACCOUNT FOR VARIABLE TEMPERATURE
!
      G2 = WATTEMP/20.D0
      IF(YATEMP)THEN
        CALL OS('X=CY    ',X=T6,Y=TN%ADR(IND_T)%P,C=UNSURVINGT)
      ELSE
        CALL OS('X=C     ',X=T6 ,C=G2)
      ENDIF
!
!     COMPUTE G3,BENCOR
!
      POWER = WATTEMP-20.D0
      G3    = (1.047D0)**(POWER)
      DO I=1,NPOIN3
        IF(YATEMP)THEN
          POWER=TN%ADR(IND_T)%P%R(I)-20.D0
          G3=(1.047D0)**(POWER)
        ENDIF
!       CORR2T AND BENCOR STOCKED HERE IN T9,T10
        T9%R(I)=CORR2**POWER
        T10%R(I)=DEMBEN*(CORR1**POWER)
!       G3 IS STOCKED IN T11
        T11%R(I)=G3
      ENDDO
!
!     COMPUTE CS (O2SATU, STOCKED IN T2_1): O2 SATURATION DENSITY OF WATER
!
      IF(.NOT.YATEMP)THEN
        CALL SATUR_O2(O2SATU,FORMCS,WATTEMP,EPS)
        CALL OS('X=C     ',X=T2_1,C=O2SATU       )
      ELSE
        DO I=1,NPOIN2
          J=(NPLAN-1)*NPOIN2+I
          CALL SATUR_O2(T2_1%R(J),FORMCS,TN%ADR(IND_T)%P%R(J),EPS)
        ENDDO
      ENDIF
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 1'
!
!     RAYEFF WITH SMITH FORMULA
!
      CALL RAY_EFFECT(ZSD,TN%ADR(RANKTR1)%P,NPOIN3,MEXTINC,I0,IK,KPE,
     &                RAYEFF,ZPROP,T3,T4)
!
!     COMPUTE LNUT: EFFECTS OF PHOSPHORIOUS AND NITROGENIOUS
!           NUTRIMENTS ON ALGAE GROWTH ==>STOCKED IN T5
!
      CALL NUTEFF(T5%R,TN,NPOIN3,RANKTR2,RANKTR4,KP,KN)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 3'
!
!     RATE OF ALGAE GROWTH: CP (STOCKED IN T7)
!
      CALL ALGAE_GROWTH(T7%R,CMAX,RAYEFF%R,T6,T5%R,CTOXIC(1),NPOIN3)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 4'
!
!     RATE OF ALGAE DISAPPEARANCE DP (STOCKED IN T4) AND MP (STOPCKED IN T12)
!
      CALL ALGAE_DEATH(T4%R,T12%R,CMORALG,TN%ADR(RANKTR1)%P%R,TRESPIR,
     &                 T6,CTOXIC(2),NPOIN3)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 5'
!
!     COMPUTE K2
!
      CALL REAER(FORMK2,K2,K22,NPOIN2,NPLAN,UN,VN,HPROP,EPS)
!
!     COMPUTE RN: PROPORTION OF NITROGEN ASSIMILATED AS NH4(STOCKED IN T8)
!
      CALL OS( 'X=Y+Z   ' ,X=T1,Y=TN%ADR(RANKTR6)%P,Z=TN%ADR(RANKTR4)%P)
      CALL OVD('X=Y/Z   ' ,T8%R,TN%ADR(RANKTR6)%P%R,T1%R,0.D0,
     &          NPOIN3 ,2,0.D0,EPS )
!
!
!     LET'S NOW COMPUTE SOURCE TERMS
!     ------------------------------
!
!     FIRST TRACER [PHY] (RANKTR1)
!
      CALL OS( 'X=Y-Z   ' ,X=T1                 ,Y=T7,Z=T4)
      CALL OS( 'X=YZ    ' ,X=TEXP%ADR(RANKTR1)%P,Y=T1,
     &                     Z=TN%ADR(RANKTR1)%P)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 6'
!
!     SECOND TRACER [PO4] (RANKTR2)
!
      CALL OS( 'X=CY    ' ,X=T1,Y=T4                    ,C=DTP      )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T7                                )
      CALL OS( 'X=CXY   ' ,X=T1,Y=TN%ADR(RANKTR1)%P     ,C=PROPHOC  )
      CALL OS( 'X=CYZ   ' ,X=T3,Y=TN%ADR(RANKTR3)%P,Z=T6,C=K320     )
      CALL OS( 'X=Y+Z   ' ,X=TEXP%ADR(RANKTR2)%P,Y=T1,Z=T3          )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 7'
!
!     THIRD TRACER [POR] (RANKTR3)
!
      G2=PROPHOC*(1.D0-DTP)
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T4,Z=TN%ADR(RANKTR1)%P,C=G2       )
      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(RANKTR3)%P,Y=T1,Z=T3          )
!     SURFACE SOURCES
      DO I=1,NPOIN2
        J=(NPLAN-1)*NPOIN2+I
        TEXP%ADR(RANKTR3)%P%R(J)= TEXP%ADR(RANKTR3)%P%R(J)-
     &          WPOR/MAX(EPS,ZPROP%R(J))
      ENDDO
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 8'
!
!     FOURTH TRACER [NO3] (RANKTR4)
!
      CALL OS( 'X=Y+C   ' ,X=T1,Y=T8                      ,C=-1.D0  )
      CALL OS( 'X=CXYZ  ' ,X=T1,Y=T7,Z=TN%ADR(RANKTR1)%P  ,C=PRONITC)
      CALL OS( 'X=CYZ   ' ,X=T3,Y=TN%ADR(RANKTR6)%P  ,Z=T6,C=K520)
      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(RANKTR4)%P,Y=T3,Z=T1          )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 9'
!
!     FIFTH TRACER [NOR] (RANKTR5)
!
      CC=PRONITC*(1.D0-PERNITS)
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T4,Z=TN%ADR(RANKTR1)%P,C=CC       )
      CALL OS( 'X=CYZ   ' ,X=T3,Y=TN%ADR(RANKTR5)%P  ,Z=T6,C=K360   )
      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(RANKTR5)%P,Y=T1,Z=T3          )
!     surface sources
      DO I=1,NPOIN2
        J=(NPLAN-1)*NPOIN2+I
        TEXP%ADR(RANKTR5)%P%R(J)= TEXP%ADR(RANKTR5)%P%R(J)-
     &          WNOR/MAX(EPS,ZPROP%R(J))
      ENDDO
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 10'
!
!     SIXTH TRACER [NH4] : AMMONIACAL LOAD (RANKTR6)
!
!     IMPLICIT PART
      CALL OS( 'X=CY    ' ,X=TIMP%ADR(RANKTR6)%P,Y=T6,C=K520         )
!     EXPLICIT PART
      CALL OS( 'X=CY    ' ,X=T1,Y=T4                      ,C=PERNITS )
      CALL OS( 'X=YZ    ' ,X=T3,Y=T7,Z=T8                            )
      CALL OS( 'X=C(Y-Z)' ,X=T3,Y=T1,Z=T3                 ,C=PRONITC )
      CALL OS( 'X=XY    ' ,X=T3,Y=TN%ADR(RANKTR1)%P                  )
      CALL OS( 'X=CYZ   ' ,X=T1,Y=TN%ADR(RANKTR5)%P  ,Z=T6,C=K360    )
      CALL OS( 'X=Y+Z   ' ,X=TEXP%ADR(RANKTR6)%P,Y=T1,Z=T3           )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 11'
!
!     SEVENTH TRACER [L]: ORGANIC LOAD (RANKTR7)
!
!     implicit part
      CALL OS( 'X=CY    ' ,X=TIMP%ADR(RANKTR7)%P,Y=T11,C=K120)
!     explicit part
      CALL OS( 'X=CYZ   ' ,X=TEXP%ADR(RANKTR7)%P,Y=T12,
     &                     Z=TN%ADR(RANKTR1)%P,C=O2PHOTO             )
!     surface terms
      DO I=1,NPOIN2
        J=(NPLAN-1)*NPOIN2+I
!       - FLOR/H = WLOR[L]/H
        TEXP%ADR(RANKTR7)%P%R(J)= TEXP%ADR(RANKTR7)%P%R(J)-
     &           WLOR*TN%ADR(RANKTR7)%P%R(J)/MAX(EPS,ZPROP%R(J)      )
      ENDDO
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 12'
!
!     EIGHTH TRACER: DISSOLVED O2 (RANKTR8)
!
      CALL OS( 'X=Y+C   ' ,X=T1                 ,Y=T7,C=-TRESPIR     )
      CALL OS( 'X=CYZ   ' ,X=TEXP%ADR(RANKTR8)%P,Y=T1,
     &                     Z=TN%ADR(RANKTR1)%P,C=O2PHOTO             )
!     -nK520g2[NH4]
      CC=O2NITRI*K520
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T6,Z=TN%ADR(RANKTR6)%P,C=CC        )
      CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(RANKTR8)%P,Y=T1                )
!     K120g3[L]
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T11,Z=TN%ADR(RANKTR7)%P,C=K120)
      CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(RANKTR8)%P,Y=T1                )
!     surface sources
      DO I=1,NPOIN2
        J=(NPLAN-1)*NPOIN2+I
!       K2g4(Cs-[O2]) - BEN/H
        TEXP%ADR(RANKTR8)%P%R(J)= TEXP%ADR(RANKTR8)%P%R(J)+
     &    K2%R(I)*T9%R(J)*MAX((T2_1%R(I)-TN%ADR(RANKTR8)%P%R(J)),0.D0 )
     &   -DEMBEN/MAX(EPS,ZPROP%R(J)                                  )
      ENDDO
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 14'
!
!     CONVERT SECONDS TO DAYS
!     warning: this conversions assumes linearity which is not the case here
!              to check and verify.
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR1)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR2)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR3)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR4)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR5)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR6)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR7)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR8)%P,C=SECTODAY)
!
      CALL OS('X=CX    ',X=TIMP%ADR(RANKTR6)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TIMP%ADR(RANKTR7)%P,C=SECTODAY)
!
!
!     MASS BALANCE: MASS ADDED BY EXPLICIT TERMS
!                   (IMPLICIT PART IS ADDED IN CVDFTR)
!
!     ACTIVATE BIEF_OBJ FOR FURTHER CALCULATIONS
!
      TEXP%ADR(RANKTR1)%P%TYPR='Q'
      TEXP%ADR(RANKTR2)%P%TYPR='Q'
      TEXP%ADR(RANKTR3)%P%TYPR='Q'
      TEXP%ADR(RANKTR4)%P%TYPR='Q'
      TEXP%ADR(RANKTR5)%P%TYPR='Q'
      TEXP%ADR(RANKTR6)%P%TYPR='Q'
      TEXP%ADR(RANKTR7)%P%TYPR='Q'
      TEXP%ADR(RANKTR8)%P%TYPR='Q'
!
      TIMP%ADR(RANKTR6)%P%TYPR='Q'
      TIMP%ADR(RANKTR7)%P%TYPR='Q'
!
!-----------------------------------------------------------------------
!
      RETURN
      END
