!                    **********************
                      SUBROUTINE CALCS_BIOMASS
!                    **********************
     &   (NPOIN,WATTEMP,TN,TEXP,RAYEFF,NTRAC,HPROP,T1,T2,DEBUG)
!
!
!***********************************************************************
! TELEMAC2D   V7P0                                        21/09/2014
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR PHYTOPLANKTONIC BIOMASS WAQ PROCESS
!
!history  R. ATA
!+        21/09/2014
!+        V7P0
!+       CREATION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| HPROP          |-->| WATER DEPTH
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| NTRAC          |-->| OLD NUMBER OF TRACER (BEFORE WAQ RTACERS)
!| RAYEFF         |-->| EFFECT OF SUNSHINE ON ALGAE GROWTH
!| T1,T2          |<--| WORKING STRUCTURES
!| TN             |-->| TRACER STRUCUTRE
!| TEXP           |<--| EXPLICIT SOURCE TERMES
!| WATTEMP        |-->| WATER TEMPERATURE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------
!***********************************************************************
      USE BIEF
      USE DECLARATIONS_WAQTEL,ONLY: CMAX,CTOXIC,IK,
     &                              PS,I0,BETA,KP,KN,CMORALG,
     &                              TRESPIR,PROPHOC,DTP,PRONITC,
     &                              K360,K320,PERNITS,WPOR,WNOR
      USE INTERFACE_WAQTEL, EX_CALCS_BIOMASS => CALCS_BIOMASS
!-----------------------------------------------------------------------
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  WPOR     ! R  !    ! VITESSE DE SEDIMENTATION DU PHOSPHORE ORGANIQ!
! !  WNOR     ! R  !    ! VITESSE DE SEDIMENTATION DE L AZOTE ORGANIQUE!
! !  CMAX     ! R  !    ! TAUX DE CROISSANCE ALGALE MAXIMUM A 20° C  !
! !  PS       ! R  !    ! PROFONDEUR DE SECCHI                         !
! !  KPE      ! R  !    ! COEF D EXTINCTION DU RAY SANS PHYTO          !
! !  BETA     ! R  !    ! COEF DE TURBIDITE VEGETALE                   !
! !  IK       ! R  !    ! PARAMETRE DE CALAGE DE LA FORMULE DE SMITH   !
! !  KP       ! R  !    ! CONSTANTE DE DEMI-SATURATION EN PHOSPHATE    !
! !  KN       ! R  !    ! CONSTANTE DE DEMI-SATURATION EN AZOTE        !
! !  ALPHA    ! R  !    ! COEF 1 DE TOXICITE DE L EAU POUR LES ALGUES  !
! !  ALPHA2   ! R  !    ! COEF 2 DE TOXICITE DE L EAU POUR LES ALGUES  !
! !  RP       ! R  !    ! TAUX DE RESP. DE LA BIOMASSE ALGALE A 20° C  !
! !  PROPHOC  ! R  !    ! PROP DE PHOSPHORE DANS LES CELLULES DU PHYTO !
! !  DTP      ! R  !    ! POURCENT DE PHOSPH DIRECT ASSIM DS PHY MORT  !
! !  K320     ! R  !    ! TAUX DE TRANSFORMATION DU POR EN PO4         !
! !  PRONITC  ! R  !    ! PROP D AZOTE DANS LES CELLULES DU PHYTO      !
! !  PERNITS  ! R  !    ! POURCENT D AZOTE DIRECT ASSIM DS PHY MORT    !
! !  K360       ! R  !    ! TAUX DE TRANSFORMATION DU NOR EN NO3         !
! !  M1       ! R  !    ! COEF 1 DE MORTALITE ALGALE A 20° C           !
! !  M2       ! R  !    ! COEF 2 DE MORTALITE ALGALE A 20° C           !
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
!      LOGICAL          , INTENT(IN  ) :: YATEMP  ! IF TEMPERATURE IS VARIABLE
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,RAYEFF,T1,T2
!
!     LOCAL VARIABLES
      INTEGER                     :: RANKTR1,RANKTR2,RANKTR3,RANKTR4
      INTEGER                     :: ERR,RANKTR5
      INTEGER         , PARAMETER :: ADDTR = 5
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION            :: SECTODAY,G1
      DOUBLE PRECISION, ALLOCATABLE::CP(:),LNUT(:),DP(:)
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN BIOMASS, STEP 0'
!
!     ALLOCATION AND INITIALISATION
!
      ALLOCATE( CP  (NPOIN),STAT=ERR)
      ALLOCATE( DP  (NPOIN),STAT=ERR)
      ALLOCATE( LNUT(NPOIN),STAT=ERR)
      IF(ERR.NE.0)GOTO 100
      CALL OV( 'X=C     ' ,CP  ,CP  ,CP  ,0.D0,NPOIN )
      CALL OV( 'X=C     ' ,DP  ,DP  ,DP  ,0.D0,NPOIN )
      CALL OV( 'X=C     ' ,LNUT,LNUT,LNUT,0.D0,NPOIN )
!
!     PRELIMINARY COMPUTATIONS
!
      RANKTR1 = NTRAC-ADDTR+1  ! PHY
      RANKTR2 = RANKTR1+1      ! PO4
      RANKTR3 = RANKTR2+1      ! POR
      RANKTR4 = RANKTR3+1      ! NO3
      RANKTR5 = NTRAC          ! NOR
! 
      G1 = WATTEMP/20.D0
!     HERE TEMPERATURE IS FIXED WHICH IS NOT PHYSICAL
!     TO INVESTIGATE IF NECESSARY TO USE T VARIABLE !
      SECTODAY=1.D0/86400.D0
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN BIOMASS, STEP 1'
!
!     RAYEFF WITH SMITH FORMULA
!
      CALL RAY_EFFECT(PS,TN%ADR(RANKTR1)%P%R,NPOIN,BETA,I0,IK,RAYEFF,
     &                HPROP)
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN BIOMASS, STEP 2'
!
!     COMPUTE LNUT
!
      CALL NUTEFF(LNUT,TN,NPOIN,RANKTR2,RANKTR4,KP,KN)
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN BIOMASS, STEP 3'
!
!     RATE OF ALGAE GROWTH
!
      CALL ALGAE_GROWTH(CP,CMAX,RAYEFF%R,G1,LNUT,CTOXIC(1),NPOIN)
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN BIOMASS, STEP 4'
!
!     RATE OF ALGAE DISAPPEARANCE
!
      CALL ALGAE_DEATH(DP,CMORALG,TN%ADR(RANKTR1)%P%R,TRESPIR,G1,
     &                  CTOXIC(2),NPOIN)
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN BIOMASS, STEP 5'
!
!
!     LET'S NOW COMPUTE SOURCE TERMS
!
!     FIRST TRACER [PHY] (RANKTR1)
      CALL OV( 'X=Y-Z   ' ,T1%R,CP,DP,0.D0,NPOIN)
      CALL OV( 'X=YZ    ' ,TEXP%ADR(RANKTR1)%P%R,T1%R,
     &                     TN%ADR(RANKTR1)%P%R,0.D0,NPOIN)
!
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN BIOMASS, STEP 6'
!     SECOND TRACER [PO4] (RANKTR2)
      CALL OV( 'X=CY    ' ,T1%R,DP  ,DP,DTP ,NPOIN)
      CALL OV( 'X=Y-Z   ' ,T1%R,T1%R,CP,0.D0,NPOIN)
      CALL OV( 'X=CYZ   ' ,T1%R,T1%R,TN%ADR(RANKTR1)%P%R,PROPHOC,
     &          NPOIN)
      CALL OV( 'X=CY    ' ,T2%R,TN%ADR(RANKTR3)%P%R,T2%R,K320*G1,
     &          NPOIN)
!    
      CALL OV( 'X=Y+Z   ' ,TEXP%ADR(RANKTR2)%P%R,T1%R,T2%R,0.D0,NPOIN)
!
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN BIOMASS, STEP 7'
!     THIRD TRACER [POR] (RANKTR3)
      CALL OV( 'X=CYZ   ' ,T1%R,DP,TN%ADR(RANKTR1)%P%R,
     &          PROPHOC*(1.D0-DTP),NPOIN)
      CALL OV( 'X=X-Y   ' ,T1%R,T2%R,T2%R,0.D0,NPOIN)
      CALL OVD('X=CY/Z  ' ,T2%R,TN%ADR(RANKTR3)%P%R,HPROP%R,WPOR,
     &          NPOIN ,2,0.D0,EPS )
      CALL OV( 'X=Y-Z   ' ,TEXP%ADR(RANKTR3)%P%R,T1%R,T2%R,0.D0,NPOIN)
!
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN BIOMASS, STEP 8'
!     FOURTH TRACER [NO3] (RANKTR4)
      CALL OV( 'X=CY    ' ,T1%R,DP  ,DP,PERNITS ,NPOIN)
      CALL OV( 'X=C(Y-Z)' ,T1%R,T1%R,CP,PRONITC,NPOIN)
      CALL OV( 'X=XY    ' ,T1%R,TN%ADR(RANKTR1)%P%R,CP,0.D0,NPOIN)
      CALL OV( 'X=CY    ' ,T2%R,TN%ADR(RANKTR5)%P%R,T2%R,K360*G1,
     &          NPOIN)
!    
      CALL OV( 'X=Y+Z   ' ,TEXP%ADR(RANKTR4)%P%R,T1%R,T2%R,0.D0,NPOIN)
!!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN BIOMASS, STEP 9'

!     FIFTH TRACER [NOR] (RANKTR5) 
      CALL OV( 'X=CYZ   ' ,T1%R,DP,TN%ADR(RANKTR1)%P%R,
     &          PRONITC*(1.D0-PERNITS),NPOIN)
      CALL OV( 'X=X-Y   ' ,T1%R,T2%R,T2%R,0.D0,NPOIN)
      CALL OVD('X=CY/Z  ' ,T2%R,TN%ADR(RANKTR5)%P%R,HPROP%R,WNOR,
     &          NPOIN ,2,0.D0,EPS )
!
      CALL OV( 'X=Y-Z   ' ,TEXP%ADR(RANKTR5)%P%R,T1%R,T2%R,0.D0,NPOIN)
!
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN BIOMASS, STEP 10'
!     CONVERT SECONDS TO DAYS
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR1)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR2)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR3)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR4)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR5)%P,C=SECTODAY)
!
!
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN BIOMASS, STEP 11'
!
!     DEALLOCATION
!
      DEALLOCATE( CP  ,STAT=ERR)
      DEALLOCATE( DP  ,STAT=ERR)
      DEALLOCATE( LNUT,STAT=ERR)
100   CONTINUE
      IF(ERR.NE.0)THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'CALCS_BIOMASS: PROBLEME D ALLOCATION DE VECTEURS'
          WRITE(LU,*) '            PEUT ETRE MEMOIRE INSUFFISANTE'
          WRITE(LU,*) '            LIBERER MEMOIRE ET RECOMMENCER'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'CALCS_BIOMASS: PROBLEM WITH ARRAY ALLOCATION'
          WRITE(LU,*) '            COULD BE NOT ENOUGH MEMORY'
          WRITE(LU,*) '            LIBERATE MEMORY AND RESTART'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!
      IF(DEBUG.GT.1)WRITE(LU,*)'IN BIOMASS, STEP 12'
      RETURN
      END
