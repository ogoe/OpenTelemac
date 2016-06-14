!                     **************************
                      SUBROUTINE CALCS3D_BIOMASS
!                     **************************
     &  (NPOIN3,NPOIN2,NPLAN,WATTEMP,TN,TEXP,RAYEFF,NTRAC,ZPROP,
     &   T1,T2,T3,T4,T5,T6,DEBUG,YATEMP,IND_T)
!
!
!***********************************************************************
! TELEMAC2D   V7P3                                        21/03/2016
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR PHYTOPLANKTONIC BIOMASS WAQ PROCESS
!
!history  R. ATA
!+        21/03/2016
!+        V7P2
!+       CREATION
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEBUG          |-->| IF NE.0 THEN DEBUG MODE
!| DT             |-->| TIME STEP
!| IND_T          |-->| INDEX OF TEMPERATURE IN TRACER TABLE
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| NPOIN2         |-->| TOTAL NUMBER OF MESH NODES
!| NPOIN3         |-->| TOTAL NUMBER OF MESH NODES
!| NTRAC          |-->| OLD NUMBER OF TRACER (BEFORE WAQ RTACERS)
!| RAYEFF         |-->| EFFECT OF SUNSHINE ON ALGAE GROWTH
!| T1,..,T6       |<->| 3D WORKING STRUCTURES
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
      USE INTERFACE_PARALLEL
      USE DECLARATIONS_WAQTEL,ONLY: CMAX,CTOXIC,IK,ZSD,I0,MEXTINC,KP,KN,
     &                              CMORALG,TRESPIR,PROPHOC,DTP,PRONITC,
     &                              K360,K320,PERNITS,WPOR,WNOR,
     &                              SECTODAY,KPE
      USE INTERFACE_WAQTEL, EX_CALCS3D_BIOMASS => CALCS3D_BIOMASS
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
      INTEGER          , INTENT(IN   ) :: NPOIN2,NPOIN3,NTRAC
      INTEGER          , INTENT(IN   ) :: DEBUG,IND_T,NPLAN
      LOGICAL          , INTENT(IN   ) :: YATEMP  
      DOUBLE PRECISION , INTENT(IN   ) :: WATTEMP
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TN,ZPROP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TEXP,RAYEFF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: T1,T2,T3,T4,T5,T6
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     LOCAL VARIABLES
      INTEGER                     :: RANKTR1,RANKTR2,RANKTR3,RANKTR4
      INTEGER                     :: RANKTR5,I,J
      INTEGER         , PARAMETER :: ADDTR = 5
      DOUBLE PRECISION, PARAMETER :: EPS=1.D-3
      DOUBLE PRECISION, PARAMETER :: UNSURVINGT=0.05D0
      DOUBLE PRECISION            :: G1
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!    INITIALISATION
!
!     CP IS STOCKED IN T3 ==>3D TABLE
      CALL OS( 'X=0     ',X=T3)
!     DP IS STOCKED IN T4 ==> 3D TABLE
      CALL OS( 'X=0     ',X=T4)
!     LNUT IS STOCKED IN T5 ==> 3D TABLE
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
!     RAYEFF WITH SMITH FORMULA
!
      CALL RAY_EFFECT(ZSD,TN%ADR(RANKTR1)%P,NPOIN3,MEXTINC,I0,IK,KPE,
     &                RAYEFF,ZPROP,T1,T2)
!
!     COMPUTE LNUT
!
      CALL NUTEFF(T5%R,TN,NPOIN3,RANKTR2,RANKTR4,KP,KN)
!
!     RATE OF ALGAE GROWTH
!
      CALL ALGAE_GROWTH(T3%R,CMAX,RAYEFF%R,T6,T5%R,CTOXIC(1),NPOIN3)
!
!     RATE OF ALGAE DISAPPEARANCE
!
      CALL ALGAE_DEATH(T4%R,T4%R,CMORALG,TN%ADR(RANKTR1)%P%R,TRESPIR,T6,
     &                  CTOXIC(2),NPOIN3)
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
!     SECOND TRACER [PO4] (RANKTR2)
!
      CALL OS( 'X=CY    ' ,X=T1,Y=T4                    ,C=DTP      )
      CALL OS( 'X=X-Y   ' ,X=T1,Y=T3                                )
      CALL OS( 'X=CXY   ' ,X=T1,Y=TN%ADR(RANKTR1)%P     ,C=PROPHOC  )
      CALL OS( 'X=CYZ   ' ,X=T2,Y=TN%ADR(RANKTR3)%P,Z=T6,C=K320     )
      CALL OS( 'X=Y+Z   ' ,X=TEXP%ADR(RANKTR2)%P,Y=T1,Z=T2          )
!
!     THIRD TRACER [POR] (RANKTR3)
!
      G1=PROPHOC*(1.D0-DTP)
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T4,Z=TN%ADR(RANKTR1)%P,C=G1       )
      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(RANKTR3)%P,Y=T1,Z=T2          )
!     SURFACE SOURCES
      DO I=1,NPOIN2
        J=(NPLAN-1)*NPOIN2+I
        TEXP%ADR(RANKTR3)%P%R(J)= TEXP%ADR(RANKTR3)%P%R(J)-
     &          WPOR/MAX(EPS,ZPROP%R(J))
      ENDDO
!
!     FOURTH TRACER [NO3] (RANKTR4)
!
      CALL OS( 'X=CY    ' ,X=T1,Y=T4                      ,C=PERNITS)
      CALL OS( 'X=C(Y-Z)' ,X=T1,Y=T1                 ,Z=T3,C=PRONITC)
      CALL OS( 'X=XY    ' ,X=T1,Y=TN%ADR(RANKTR1)%P                 )
      CALL OS( 'X=CYZ   ' ,X=T2,Y=TN%ADR(RANKTR5)%P  ,Z=T6,C=K360   )
!    
      CALL OS( 'X=Y+Z   ' ,X=TEXP%ADR(RANKTR4)%P,Y=T1,Z=T2          )
!
!     FIFTH TRACER [NOR] (RANKTR5) 
!
      G1=PRONITC*(1.D0-PERNITS)
      CALL OS( 'X=CYZ   ' ,X=T1,Y=T4,Z=TN%ADR(RANKTR1)%P,C=G1       )
      CALL OS( 'X=Y-Z   ' ,X=TEXP%ADR(RANKTR5)%P,Y=T1,Z=T2          )
      DO I=1,NPOIN2
        J=(NPLAN-1)*NPOIN2+I
        TEXP%ADR(RANKTR5)%P%R(J)= TEXP%ADR(RANKTR5)%P%R(J)-
     &          WNOR/MAX(EPS,ZPROP%R(J))
      ENDDO
!
!     CONVERT SECONDS TO DAYS
!
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR1)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR2)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR3)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR4)%P,C=SECTODAY)
      CALL OS('X=CX    ',X=TEXP%ADR(RANKTR5)%P,C=SECTODAY)
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
!
!-----------------------------------------------------------------------
!
      RETURN
      END
