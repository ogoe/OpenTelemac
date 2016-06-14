!                     **************************
                      SUBROUTINE CALCS2D_BIOMASS
!                     **************************
     &  (NPOIN,WATTEMP,TN,TEXP,RAYEFF,NTRAC,HN,HPROP,T1,T2,T3,T4,T5,T6,
     &   DEBUG,MASSOU,DT,VOLU2D,YATEMP,IND_T)
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
!| DEBUG          |-->| IF NE.0 THEN DEBUG MODE
!| DT             |-->| TIME STEP
!| HPROP          |-->| WATER DEPTH
!| IND_T          |-->| INDEX OF TEMPERATURE IN TRACER TABLE
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| NTRAC          |-->| OLD NUMBER OF TRACER (BEFORE WAQ RTACERS)
!| RAYEFF         |-->| EFFECT OF SUNSHINE ON ALGAE GROWTH
!| T1,..,T6       |<--| WORKING STRUCTURES
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
      USE DECLARATIONS_WAQTEL,ONLY: CMAX,CTOXIC,IK,ZSD,I0,MEXTINC,KP,KN,
     &                              CMORALG,TRESPIR,PROPHOC,DTP,PRONITC,
     &                              K360,K320,PERNITS,WPOR,WNOR,
     &                              SECTODAY,KPE
      USE INTERFACE_WAQTEL, EX_CALCS2D_BIOMASS => CALCS2D_BIOMASS
!-----------------------------------------------------------------------
! .___________.____.____.______________________________________________.
! !    NOM    !TYPE!MODE!                   ROLE                       !
! !___________!____!____!______________________________________________!
! !  WPOR     ! R  !    ! VITESSE DE SEDIMENTATION DU PHOSPHORE ORGANIQ!
! !  WNOR     ! R  !    ! VITESSE DE SEDIMENTATION DE L AZOTE ORGANIQUE!
! !  CMAX     ! R  !    ! TAUX DE CROISSANCE ALGALE MAXIMUM A 20° C  !
! !  ZSD       ! R  !    ! PROFONDEUR DE SECCHI                         !
 !  KPE     ! R  !    ! COEF DE TURBIDITE VEGETALE                   !
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
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN   ) :: NPOIN,NTRAC,DEBUG,IND_T
      LOGICAL          , INTENT(IN   ) :: YATEMP  
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP,DT
      DOUBLE PRECISION , INTENT(INOUT) :: MASSOU(*)
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,HPROP,HN,VOLU2D
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,RAYEFF,T1,T2,T3,T4,T5,T6
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     LOCAL VARIABLES
      INTEGER                     :: RANKTR1,RANKTR2,RANKTR3,RANKTR4
      INTEGER                     :: RANKTR5,I,J,ITRAC
      INTEGER         , PARAMETER :: ADDTR = 5
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION, PARAMETER :: UNSURVINGT=0.05D0
      DOUBLE PRECISION            :: G1
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 0'
!
!    INITIALISATION
!
!     CP IS STOCKED IN T3 
      CALL OS( 'X=0     ',X=T3)
!     DP IS STOCKED IN T4
      CALL OS( 'X=0     ',X=T4)
!     LNUT IS STOCKED IN T5
      CALL OS( 'X=0     ',X=T5)
!
!     PRELIMINARY COMPUTATIONS
!
      RANKTR1 = NTRAC-4      ! PHY
      RANKTR2 = NTRAC-3      ! PO4
      RANKTR3 = NTRAC-2      ! POR
      RANKTR4 = NTRAC-1      ! NO3
      RANKTR5 = NTRAC        ! NOR
!
!     G1 IS STOCKED IN T6, WE TAKE INTO ACCOUNT VARIABLE TEMPERATURE
!     
      G1 = WATTEMP/20.D0
      IF(YATEMP)THEN
        CALL OS('X=CY    ',X=T6,Y=TN%ADR(IND_T)%P,C=UNSURVINGT)
      ELSE
        CALL OS('X=C     ',X=T6,C=G1)
      ENDIF
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 1'
!
!     RAYEFF WITH SMITH FORMULA
!
      CALL RAY_EFFECT(ZSD,TN%ADR(RANKTR1)%P,NPOIN,MEXTINC,I0,IK,KPE,
     &                RAYEFF,HPROP,T1,T2)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 2'
!
!     COMPUTE LNUT
!
      CALL NUTEFF(T5%R,TN,NPOIN,RANKTR2,RANKTR4,KP,KN)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 3'
!
!     RATE OF ALGAE GROWTH
!
      CALL ALGAE_GROWTH(T3%R,CMAX,RAYEFF%R,T6,T5%R,CTOXIC(1),NPOIN)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 4'
!
!     RATE OF ALGAE DISAPPEARANCE
!
      CALL ALGAE_DEATH(T4%R,T4%R,CMORALG,TN%ADR(RANKTR1)%P%R,TRESPIR,T6,
     &                  CTOXIC(2),NPOIN)
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 5'
!
!
!     LET'S NOW COMPUTE SOURCE TERMS
!
!     FIRST TRACER [PHY] (RANKTR1)
!
      CALL OS( 'X=Y-Z   ' ,X=T1                 ,Y=T3,Z=T4)
      CALL OS( 'X=YZ    ' ,X=TEXP%ADR(RANKTR1)%P,Y=T1,
     &                     Z=TN%ADR(RANKTR1)%P)
!
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 6' 
!
!     SECOND TRACER [PO4] (RANKTR2)
!
      CALL OS( 'X=CY    ' ,X=T1,Y=T4                    ,C=DTP      )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T3                                )
      CALL OS( 'X=CXY   ' ,X=T1,Y=TN%ADR(RANKTR1)%P     ,C=PROPHOC  )
      CALL OS( 'X=CYZ   ' ,X=T2,Y=TN%ADR(RANKTR3)%P,Z=T6,C=K320     )
!    
      CALL OS( 'X=Y+Z   ' ,X=TEXP%ADR(RANKTR2)%P,Y=T1,Z=T2          )
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 7'
!
!     THIRD TRACER [POR] (RANKTR3)
!
      G1=PROPHOC*(1.D0-DTP)
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T4,Z=TN%ADR(RANKTR1)%P,C=G1       )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T2                                )
      CALL OVD('X=CY/Z  ' ,T2%R,TN%ADR(RANKTR3)%P%R,HPROP%R,WPOR,
     &          NPOIN ,2,0.D0,EPS                                   )
      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(RANKTR3)%P,Y=T1,Z=T2          )
!
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 8'
!
!     FOURTH TRACER [NO3] (RANKTR4)
!
      CALL OS( 'X=CY    ' ,X=T1,Y=T4                      ,C=PERNITS)
      CALL OS( 'X=C(Y-Z)' ,X=T1,Y=T1                 ,Z=T3,C=PRONITC)
      CALL OS( 'X=XY    ' ,X=T1,Y=TN%ADR(RANKTR1)%P                 )
      CALL OS( 'X=CYZ   ' ,X=T2,Y=TN%ADR(RANKTR5)%P  ,Z=T6,C=K360   )
!    
      CALL OS( 'X=Y+Z   ' ,X=TEXP%ADR(RANKTR4)%P,Y=T1,Z=T2          )
!!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 9'

!     FIFTH TRACER [NOR] (RANKTR5) 
      G1=PRONITC*(1.D0-PERNITS)
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T4,Z=TN%ADR(RANKTR1)%P,C=G1       )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T2                                )
      CALL OVD('X=CY/Z  ' ,T2%R,TN%ADR(RANKTR5)%P%R,HPROP%R,WNOR,
     &         NPOIN ,2,0.D0,EPS )
!
      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(RANKTR5)%P,Y=T1,Z=T2          )
!
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 10'
!     CONVERT SECONDS TO DAYS
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR1)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR2)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR3)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR4)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR5)%P,C=SECTODAY)
!
!
      IF(DEBUG.GT.0)WRITE(LU,*)'IN BIOMASS, STEP 11'
!
!    
!     MASS BALANCE: MASS ADDED BY EXPLICIT TERMS 
!                   (IMPLICIT PART IS ADDED IN CVDFTR)
!
       DO J=1,ADDTR
         ITRAC=NTRAC-J+1
         MASSOU(ITRAC) = 0.D0
         DO I=1,NPOIN
           MASSOU(ITRAC)= MASSOU(ITRAC)
     &                  + HN%R(I)*TEXP%ADR(ITRAC)%P%R(I)*VOLU2D%R(I)
         ENDDO
         MASSOU(ITRAC)=MASSOU(ITRAC)*DT
         IF(NCSIZE.GT.0) MASSOU(ITRAC)=P_DSUM(MASSOU(ITRAC))
       ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
