!                    ************************
                      SUBROUTINE CALCS_EUTRO
!                    ************************
     &   (NPOIN,WATTEMP,TN,TEXP,RAYEFF,NTRAC,
     &    HPROP,T1,T2,T3,U,V,DEBUG)
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| NTRAC          |-->| OLD NUMBER OF TRACER (BEFORE WAQ RTACERS)
!| RAYEFF         |-->|  EFFECT OF SUNSHINE ON ALGAE GROWTH
!| TN             |-->| TRACER STRUCUTRE
!| TEXP           |<--| EXPLICIT SOURCE TERMES
!| WATTEMP        |-->| WATER TEMPERATURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------
!***********************************************************************
      USE BIEF
      USE DECLARATIONS_WAQTEL,ONLY:CMAX,CTOXIC,IK,K520,O2SATU,K2,
     &                            PS,I0,BETA,KP,KN,CMORALG,FORMCS,
     &                            TRESPIR,PROPHOC,DTP,PRONITC,K22,WLOR,
     &                            K360,K320,PERNITS,WPOR,WNOR,FORMK2,
     &                            O2PHOTO,K120,O2NITRI,DEMBEN
      USE INTERFACE_WAQTEL, EX_CALCS_EUTRO => CALCS_EUTRO
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  U        ! TR ! D  ! VITESSE DE L'EAU                             !
! !  J        ! TR ! D  ! PENTE DE LA LIGNE DE CHARGE                  !
! !  WPOR     ! R  !    ! VITESSE DE SEDIMENTATION DU PHOSPHORE ORGANIQ!
! !  WNOR     ! R  !    ! VITESSE DE SEDIMENTATION DE L AZOTE ORGANIQUE!
! !  CMAX     ! R  !    ! TAUX DE CROISSANCE ALGALE MAXIMUM A 20°C     !
! !  PS       ! R  !    ! PROFONDEUR DE SECCHI                         !
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
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER          , INTENT(IN   ) :: NPOIN,NTRAC,DEBUG
!      LOGICAL          , INTENT(IN   ) :: YATEMP  ! IF TEMPERATURE IS VARIABLE
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,U,V
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,RAYEFF,T1,T2,T3
!
!     LOCAL VARIABLES
      INTEGER                     :: RANKTR1,RANKTR2,RANKTR3,RANKTR4
      INTEGER                     :: ERR,RANKTR5,RANKTR6,RANKTR7,RANKTR8
      INTEGER         , PARAMETER :: ADDTR = 8
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-6
      DOUBLE PRECISION, PARAMETER :: CORR1=1.065D0
      DOUBLE PRECISION, PARAMETER :: CORR2=1.0241D0
      DOUBLE PRECISION            :: SECTODAY,G2,CC,POWER
      DOUBLE PRECISION, ALLOCATABLE::CP(:),LNUT(:),DP(:),RN(:)
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN EUTRO, STEP 0'
!
!     ALLOCATION AND INITIALISATION
!
      ALLOCATE( CP  (NPOIN),STAT=ERR)
      ALLOCATE( DP  (NPOIN),STAT=ERR)
      ALLOCATE( LNUT(NPOIN),STAT=ERR)
      ALLOCATE( RN  (NPOIN),STAT=ERR)
      IF(ERR.NE.0)GOTO 100
      CALL OV( 'X=C     ' ,CP  ,CP  ,CP  ,0.D0,NPOIN )
      CALL OV( 'X=C     ' ,DP  ,DP  ,DP  ,0.D0,NPOIN )
      CALL OV( 'X=C     ' ,LNUT,LNUT,LNUT,0.D0,NPOIN )
      CALL OV( 'X=C     ' ,RN  ,RN  ,RN  ,0.D0,NPOIN )
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
!     HERE TEMPERATURE IS FIXED WHICH IS NOT PHYSICAL
!     TO INVESTIGATE IF NECESSARY TO USE T VARIABLE !
      POWER=WATTEMP-20.D0
      G2 = 1.05D0**POWER
      SECTODAY=1.D0/86400.D0

!
!     COMPUTE CS (O2SATU): O2 SATURATION DENSITY OF WATER
!
      CALL SATUR_O2(O2SATU,FORMCS,WATTEMP,EPS)
!
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN EUTRO, STEP 1'
!
!     RAYEFF WITH SMITH FORMULA
!
      CALL RAY_EFFECT(PS,TN%ADR(RANKTR1)%P%R,NPOIN,BETA,I0,IK,RAYEFF,
     &                HPROP)
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN EUTRO, STEP 2'
!
!     COMPUTE LNUT: EFFECTS OF PHOSPHORIOUS AND NITROGENIOUS 
!           NUTRIMENTS ON ALGAE GROWTH 
!
      CALL NUTEFF(LNUT,TN,NPOIN,RANKTR2,RANKTR4,KP,KN)
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN EUTRO, STEP 3'
!
!     RATE OF ALGAE GROWTH
!
      CALL ALGAE_GROWTH(CP,CMAX,RAYEFF%R,G2,LNUT,CTOXIC(1),NPOIN)
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN EUTRO, STEP 4'
!
!     RATE OF ALGAE DISAPPEARANCE
!
      CALL ALGAE_DEATH(DP,CMORALG,TN%ADR(RANKTR1)%P%R,TRESPIR,G2,
     &                  CTOXIC(2),NPOIN)
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN EUTRO, STEP 5'
!
!     COMPUTE K2
!
      CALL REAER(FORMK2,K2,K22,NPOIN,U,V,HPROP,EPS)
!
!     COMPUTE RS (RSW:DONE IN DIFSOU)
!
!
!     COMPUTE RN: PROPORTION OF NITROGEN ASSIMILATED AS NH4
!
      CALL OV( 'X=Y+Z   ' ,T1%R,TN%ADR(RANKTR6)%P%R,TN%ADR(RANKTR4)%P%R,
     &          0.D0,NPOIN)
      CALL OVD('X=Y/Z   ' ,RN,TN%ADR(RANKTR6)%P%R,T1%R,0.D0,
     &          NPOIN ,2,0.D0,EPS )
!
!     LET'S NOW COMPUTE SOURCE TERMS
!
!     FIRST TRACER [PHY] (RANKTR1)
      CALL OV( 'X=Y-Z   ' ,T1%R,CP,DP,0.D0,NPOIN)
      CALL OV( 'X=YZ    ' ,TEXP%ADR(RANKTR1)%P%R,T1%R,
     &                     TN%ADR(RANKTR1)%P%R,0.D0,NPOIN)
!
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN EUTRO, STEP 6'
!     SECOND TRACER [PO4] (RANKTR2)
      CALL OV( 'X=CY    ' ,T1%R,DP  ,DP,DTP ,NPOIN)
      CALL OV( 'X=Y-Z   ' ,T1%R,T1%R,CP,0.D0,NPOIN)
      CALL OV( 'X=CYZ   ' ,T1%R,T1%R,TN%ADR(RANKTR1)%P%R,PROPHOC,
     &          NPOIN)
      CALL OV( 'X=CY    ' ,T2%R,TN%ADR(RANKTR3)%P%R,T2%R,K320*G2,
     &          NPOIN)
!    
      CALL OV( 'X=Y+Z   ' ,TEXP%ADR(RANKTR2)%P%R,T1%R,T2%R,0.D0,NPOIN)
!
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN EUTRO, STEP 7'
!     THIRD TRACER [POR] (RANKTR3)
      CALL OV( 'X=CYZ   ' ,T1%R,DP,TN%ADR(RANKTR1)%P%R,
     &          PROPHOC*(1.D0-DTP),NPOIN)
      CALL OV( 'X=X-Y   ' ,T1%R,T2%R,T2%R,0.D0,NPOIN)
      CALL OVD('X=CY/Z  ' ,T2%R,TN%ADR(RANKTR3)%P%R,HPROP%R,WPOR,
     &          NPOIN ,2,0.D0,EPS )
      CALL OV( 'X=Y-Z   ' ,TEXP%ADR(RANKTR3)%P%R,T1%R,T2%R,0.D0,NPOIN)
!
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN EUTRO, STEP 8'
!     FOURTH TRACER [NO3] (RANKTR4)
      CALL OV( 'X=-Y    ' ,T1%R,RN,RN                 ,0.D0    ,NPOIN)
      CALL OV( 'X=X+C   ' ,T1%R,RN,RN                 ,1.D0    ,NPOIN)
      CALL OV( 'X=CXYZ  ' ,T1%R,CP,TN%ADR(RANKTR1)%P%R,-PRONITC,NPOIN)
      CALL OV( 'X=CY    ' ,T2%R,TN%ADR(RANKTR6)%P%R,RN, K520*G2,NPOIN)
!
      CALL OV( 'X=Y+Z   ' ,TEXP%ADR(RANKTR4)%P%R,T1%R,T2%R,0.D0,NPOIN)
      IF(DEBUG.GT.1)WRITE(LU,*)'IN EUTRO, STEP 9'

!     FIFTH TRACER [NOR] (RANKTR5) 
      CALL OV( 'X=CYZ   ' ,T1%R,DP,TN%ADR(RANKTR1)%P%R,
     &          PRONITC*(1.D0-PERNITS),NPOIN)
      CALL OV( 'X=CY    ' ,T2%R,TN%ADR(RANKTR5)%P%R,RN, K360*G2,NPOIN)
      CALL OV( 'X=X-Y   ' ,T1%R,T2%R,T2%R,0.D0,NPOIN)
      CALL OVD('X=CY/Z  ' ,T2%R,TN%ADR(RANKTR5)%P%R,HPROP%R,WNOR,
     &          NPOIN ,2,0.D0,EPS )
!
      CALL OV( 'X=Y-Z   ' ,TEXP%ADR(RANKTR5)%P%R,T1%R,T2%R,0.D0,NPOIN)
!
!     SIXTH TRACER [NH4] : AMMONIACAL LOAD
      CALL OV( 'X=CY    ' ,T1%R,DP  ,DP ,PERNITS,NPOIN)
      CALL OV( 'X=YZ    ' ,T2%R,RN  ,CP ,0.D0  ,NPOIN)
      CALL OV( 'X=C(Y-Z)' ,T1%R,T1%R,T2%R,PRONITC ,NPOIN)
      CALL OV( 'X=XY    ' ,T1%R,T1%R,TN%ADR(RANKTR1)%P%R,0.D0 ,NPOIN)
      CALL OV( 'X=CY    ' ,T2%R,TN%ADR(RANKTR5)%P%R,RN, K360*G2,NPOIN)
      CALL OV( 'X=CY    ' ,T3%R,TN%ADR(RANKTR6)%P%R,RN, K520*G2,NPOIN)
      CALL OV( 'X=X+Y   ' ,T1%R,T2%R,T2%R,0.D0,NPOIN)
      CALL OV( 'X=Y-Z   ' ,TEXP%ADR(RANKTR6)%P%R,T1%R,T3%R,0.D0,NPOIN)     
!
!     SEVENTH TRACER [L]: ORGANIC LOAD
!     COMPUTE MP 
      CALL OV( 'X=CY    ' ,T1%R,TN%ADR(RANKTR1)%P%R,DP,CMORALG(2),NPOIN)
      CC=CMORALG(1)+CTOXIC(2)
      CALL OV( 'X=X+C   ' ,T1%R,DP                 ,DP,CC        ,NPOIN)
!     
      CALL OV( 'X=CYZ   ' ,T1%R,TN%ADR(RANKTR1)%P%R,DP,O2PHOTO   ,NPOIN)
      CC=K120*(1.047D0**POWER)
      CALL OV( 'X=CY    ' ,T2%R,TN%ADR(RANKTR7)%P%R,DP,CC        ,NPOIN)
      CALL OVD('X=CY/Z  ' ,T3%R,TN%ADR(RANKTR7)%P%R,HPROP%R,WLOR,
     &          NPOIN ,2,0.D0,EPS )
!
      CALL OV( 'X=Y-Z   ' ,TEXP%ADR(RANKTR7)%P%R,T1%R,T2%R,0.D0,NPOIN)     
      CALL OV( 'X=X-Y   ' ,TEXP%ADR(RANKTR7)%P%R,T3%R,T3%R,0.D0,NPOIN)     
!
!     EIGHTH TRACER: DISSOLVED O2
      CALL OV( 'X=Y+C   ' ,T1%R,CP,CP,-TRESPIR ,NPOIN)
      CALL OV( 'X=CYZ   ' ,T1%R,T1%R,TN%ADR(RANKTR1)%P%R,O2PHOTO,NPOIN)
      CC=O2NITRI*K520*G2
      CALL OV( 'X=CY    ' ,T3%R,TN%ADR(RANKTR6)%P%R,DP,CC       ,NPOIN)
!     T2 CONTAINS K120G3[L] SO FAR
      CALL OV( 'X=Y-Z   ' ,TEXP%ADR(RANKTR8)%P%R,T1%R,T2%R,0.D0,NPOIN) 
      CALL OV( 'X=X-Y   ' ,TEXP%ADR(RANKTR8)%P%R,T3%R,T3%R,0.D0,NPOIN)
      CC=1.025**POWER
      CALL OV( 'X=CY    ' ,T1%R               ,K2%R,K2%R,CC  ,NPOIN) 
      CALL OV( 'X=Y+C   ' ,T2%R,TN%ADR(RANKTR8)%P%R,CP,-O2SATU,NPOIN)
      CALL OV( 'X=-Y    ' ,T2%R,T2%R               ,T2%R,0.D0 ,NPOIN)
      CALL OV( 'X=YZ    ' ,T3%R,T1%R               ,T2%R,0.D0 ,NPOIN)
      CALL OV( 'X=X-Y   ' ,TEXP%ADR(RANKTR8)%P%R,T3%R,T3%R,0.D0 ,NPOIN)
!   
      CALL OVD('X=1/Y   ' ,T3%R,HPROP%R,HPROP%R,0.D0,
     &          NPOIN ,2,0.D0,EPS )
      CALL OV( 'X=X+CY  ' ,TEXP%ADR(RANKTR8)%P%R,T3%R,T3%R,
     &         -DEMBEN,NPOIN)
!
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN EUTRO, STEP 10'
!     CONVERT SECONDS TO DAYS
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR1)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR2)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR3)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR4)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR5)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR6)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR7)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR8)%P,C=SECTODAY)
!
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN EUTRO, STEP 11'
!
!     DEALLOCATION
!
      DEALLOCATE( CP  ,STAT=ERR)
      DEALLOCATE( DP  ,STAT=ERR)
      DEALLOCATE( LNUT,STAT=ERR)
      DEALLOCATE( RN  ,STAT=ERR)
100   CONTINUE
      IF(ERR.NE.0)THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'CALCS_EUTRO: PROBLEME D ALLOCATION DE VECTEURS'
          WRITE(LU,*) '            PEUT ETRE MEMOIRE INSUFFISANTE'
          WRITE(LU,*) '            LIBERER MEMOIRE ET RECOMMENCER'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'CALCS_EUTRO: PROBLEM WITH ARRAY ALLOCATION'
          WRITE(LU,*) '            COULD BE NOT ENOUGH MEMORY'
          WRITE(LU,*) '            LIBERATE MEMORY AND RESTART'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN EUTRO, STEP 12'
!
!
!-----------------------------------------------------------------------
!
      RETURN
      END
