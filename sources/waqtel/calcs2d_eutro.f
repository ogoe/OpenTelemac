!                    *************************
                      SUBROUTINE CALCS2D_EUTRO
!                    *************************
     &  (NPOIN,WATTEMP,TN,TEXP,TIMP,RAYEFF,NTRAC,HN,HPROP,T1,T2,T3,T4,
     &   T5,T6,T7,T8,T9,T10,T11,T12,DEBUG,MASSOU,DT,VOLU2D,YATEMP,IND_T,
     &   UN,VN)
!
!
!***********************************************************************
! TELEMAC2D   V7P0                                        21/09/2014
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR EUTRO WAQ PROCESS
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+       CREATION (VOID)
!history  R. ATA
!+        21/09/2015
!+        V7P1
!+       REAL IMPLEMENTATION
!
!history  R. ATA
!+        21/03/2016
!+        V7P2
!+       IMPROVEMENT- REMOVE LOCAL DECLARATIONS
!+       AND ALLOCATIONS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEBUG          |-->| IF NE.0 THEN DEBUG MODE
!| DT             |-->| TIME STEP
!| HPROP          |-->| WATER DEPTH
!| IND_T          |-->| INDEX OF TEMPERATURE IN TRACER TABLE
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| NTRAC          |-->| OLD NUMBER OF TRACER (BEFORE WAQ RTACERS)
!| RAYEFF         |-->| EFFECT OF SUNSHINE ON ALGAE GROWTH
!| T1,..,T12      |<--| WORKING STRUCTURES
!| TN             |-->| TRACER STRUCUTRE
!| TEXP           |<--| EXPLICIT SOURCE TERMES
!| VOLU2D         |-->| BASES AREA (NON ASSEMBLED)
!| WATTEMP        |-->| WATER TEMPERATURE
!| YATEMP         |-->| IF TEMPERATURE IS VARIABLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------
!***********************************************************************
      USE BIEF
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_WAQTEL,ONLY:CMAX,CTOXIC,IK,K520,O2SATU,K2,
     &                            ZSD,I0,KPE,KP,KN,CMORALG,FORMCS,
     &                            TRESPIR,PROPHOC,DTP,PRONITC,K22,WLOR,
     &                            K360,K320,PERNITS,WPOR,WNOR,FORMK2,
     &                            O2PHOTO,K120,O2NITRI,DEMBEN,MEXTINC,
     &                            SECTODAY
      USE INTERFACE_WAQTEL, EX_CALCS2D_EUTRO => CALCS2D_EUTRO
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
! !  RSW       ! R  !    ! COEFFICIENT DE REAERATION AUX SEUILS         !
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
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN   ) :: NPOIN,NTRAC,DEBUG,IND_T
      LOGICAL          , INTENT(IN   ) :: YATEMP
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP,DT
      DOUBLE PRECISION , INTENT(INOUT) :: MASSOU(*)
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,HN,VOLU2D,UN,VN
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TIMP,TEXP,RAYEFF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T7,T8,T9,T10
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T11,T12
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     LOCAL VARIABLES
!
      INTEGER                     :: I,J,ITRAC
      INTEGER                     :: RANKTR1,RANKTR2,RANKTR3,RANKTR4
      INTEGER                     :: ERR,RANKTR5,RANKTR6,RANKTR7,RANKTR8
      INTEGER         , PARAMETER :: ADDTR = 8
      DOUBLE PRECISION, PARAMETER :: UNSURVINGT=0.05D0
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-6
      DOUBLE PRECISION, PARAMETER :: CORR1=1.065D0
      DOUBLE PRECISION, PARAMETER :: CORR2=1.0241D0
      DOUBLE PRECISION            :: G2,G3,CC,POWER
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 0'
!
!     INITIALISATION
!
!     CS IS STOCKED IN T2
      CALL OS( 'X=0     ',X=T2)
      CALL OS( 'X=0     ',X=T3)
!     G2 IS STOCKED IN T6
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
!     G2 IS STOCKED IN T6,WE TAKE INTO ACCOUNT VARIABLE TEMPERATURE
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
      DO I=1,NPOIN
        IF(YATEMP)THEN
          POWER = TN%ADR(IND_T)%P%R(I)-20.D0
          G3    = (1.047D0)**(POWER)
        ENDIF
!       CORR2T AND BENCOR STOCKED HERE IN T9,T10
        T9%R(I) = CORR2**POWER
        T10%R(I)= DEMBEN*(CORR1**POWER)
!       G3 IS STOCKED IN T11
        T11%R(I)=G3
      ENDDO
!
!     COMPUTE CS (O2SATU, STOCKED IN T2)
!
      IF(.NOT.YATEMP)THEN
        CALL SATUR_O2(O2SATU,FORMCS,WATTEMP,EPS)
        CALL OS('X=C     ',X=T2,C=O2SATU       )
      ELSE
        DO I=1,NPOIN
          CALL SATUR_O2(T2%R(I),FORMCS,TN%ADR(IND_T)%P%R(I),EPS)
        ENDDO
      ENDIF
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 1'
!
!     RAYEFF WITH SMITH FORMULA
!
      CALL RAY_EFFECT(ZSD,TN%ADR(RANKTR1)%P,NPOIN,MEXTINC,I0,IK,KPE,
     &                RAYEFF,HPROP,T3,T4)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 2'
!
!     COMPUTE LNUT: EFFECTS OF PHOSPHORIOUS AND NITROGENIOUS
!           NUTRIMENTS ON ALGAE GROWTH ==>STOCKED IN T5
!
      CALL NUTEFF(T5%R,TN,NPOIN,RANKTR2,RANKTR4,KP,KN)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 3'
!
!     RATE OF ALGAE GROWTH: CP (STOCKED IN T7)
!
      CALL ALGAE_GROWTH(T7%R,CMAX,RAYEFF%R,T6,T5%R,CTOXIC(1),NPOIN)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 4'
!
!     RATE OF ALGAE DISAPPEARANCE DP (STOCKED IN T4) AND MP (STOPCKED IN T12)
!
      CALL ALGAE_DEATH(T4%R,T12%R,CMORALG,TN%ADR(RANKTR1)%P%R,TRESPIR,
     &                  T6,CTOXIC(2),NPOIN)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 5'
!
!     COMPUTE K2
!
      CALL REAER(FORMK2,K2,K22,NPOIN,1,UN,VN,HPROP,EPS)
!
!     COMPUTE RS (RSW:DONE IN DIFSOU)
!
!
!     COMPUTE RN: PROPORTION OF NITROGEN ASSIMILATED AS NH4(STOCKED IN T8)
!
      CALL OV( 'X=Y+Z   ' ,T1%R,TN%ADR(RANKTR6)%P%R,TN%ADR(RANKTR4)%P%R,
     &          0.D0,NPOIN)
      CALL OVD('X=Y/Z   ' ,T8%R,TN%ADR(RANKTR6)%P%R,T1%R,0.D0,
     &          NPOIN ,2,0.D0,EPS )
!
!
!     LET'S NOW COMPUTE SOURCE TERMS
!     -------------------------------
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
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T3                                )
      CALL OVD('X=C/Y   ' ,T3%R,HPROP%R,TN%ADR(RANKTR3)%P%R,WPOR,
     &          NPOIN ,2,0.D0,EPS                                   )
      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(RANKTR3)%P,Y=T1,Z=T3          )
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
      G2=PRONITC*(1.D0-PERNITS)
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T4,Z=TN%ADR(RANKTR1)%P,C=G2       )
      CALL OS( 'X=CYZ   ' ,X=T3,Y=TN%ADR(RANKTR5)%P  ,Z=T6,C=K360)
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T3                                )
      CALL OVD('X=CY/Z  ' ,T3%R,TN%ADR(RANKTR5)%P%R,HPROP%R,WNOR,
     &         NPOIN ,2,0.D0,EPS )
      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(RANKTR5)%P,Y=T1,Z=T3          )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 10'
!
!     SIXTH TRACER [NH4] : AMMONIACAL LOAD (RANKTR6)
!
!     IMPLICIT PART
      CALL OS( 'X=CYZ   ' ,X=TIMP%ADR(RANKTR6)%P,Y=T6,Z=HPROP,C=-K520)
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
!     IMPLICIT PART
      CALL OS( 'X=CYZ   ' ,X=TIMP%ADR(RANKTR7)%P,Y=T11,Z=HPROP,C=-K120)
!     EXPLICIT PART
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T12,Z=TN%ADR(RANKTR1)%P,C=O2PHOTO   )
      CALL OVD('X=CY/Z  ' ,T3%R,TN%ADR(RANKTR7)%P%R,HPROP%R,WLOR,
     &          NPOIN ,2,0.D0,EPS                                     )
      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(RANKTR7)%P,Y=T1,Z=T3            )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 12'
!
!     EIGHTH TRACER: DISSOLVED O2 (RANKTR8)
!
      CALL OS( 'X=Y+C   ' ,X=T1                 ,Y=T7,C=-TRESPIR    )
      CALL OS( 'X=CYZ   ' ,X=TEXP%ADR(RANKTR8)%P,Y=T1,
     &                     Z=TN%ADR(RANKTR1)%P,C=O2PHOTO)
!     -nK520g2[NH4]
      CC=O2NITRI*K520
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T6,Z=TN%ADR(RANKTR6)%P,C=CC       )
      CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(RANKTR8)%P,Y=T1               )
!     K120g3[L]
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T11,Z=TN%ADR(RANKTR7)%P,C=K120)
      CALL OS( 'X=X-Y   ' ,X=TEXP%ADR(RANKTR8)%P,Y=T1                )
!     K2g4(Cs-[O2])
      CALL OS( 'X=Y-Z   ' ,X=T1,Y=T2,Z=TN%ADR(RANKTR8)%P             )
      CALL OS( 'X=CXYZ  ' ,X=T1,Y=T9,Z=K2,C=1.D0                     )
      CALL OS( 'X=X+Y   ' ,X=TEXP%ADR(RANKTR8)%P,Y=T1                )
!     -BEN/h
      CALL OVD('X=1/Y   ' ,T3%R,HPROP%R,HPROP%R,0.D0,
     &          NPOIN ,2,0.D0,EPS )
      CALL OS( 'X=X+CY  ' ,X=TEXP%ADR(RANKTR8)%P,Y=T3,C=-DEMBEN      )
!
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 14'
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
      IF(DEBUG.GT.0)WRITE(LU,*)'IN EUTRO, STEP 16'
!
!     MASS BALANCE: MASS ADDED BY EXPLICIT TERMS
!                   (IMPLICIT PART IS ADDED IN CVDFTR)
!
      DO J=1,ADDTR
        ITRAC=NTRAC-ADDTR+J
        MASSOU(ITRAC) = 0.D0
        DO I=1,NPOIN
          MASSOU(ITRAC) = MASSOU(ITRAC)
     &                    + HN%R(I)*TEXP%ADR(ITRAC)%P%R(I)*VOLU2D%R(I)
        ENDDO
        MASSOU(ITRAC)=MASSOU(ITRAC)*DT
        IF(NCSIZE.GT.1) MASSOU(ITRAC)=P_DSUM(MASSOU(ITRAC))
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
