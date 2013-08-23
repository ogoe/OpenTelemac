                        SUBROUTINE LECR3D
!                       *****************
!
     &(AT,Z,U,V,W,NPOIN3,NPOIN2,NPLAN,NRES,BINRES,RB,NVA3,TAB,varsub)
!
!***********************************************************************
! POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
! FORTRAN90
!***********************************************************************
!
!     FONCTION  : LIT LES ENREGISTREMENTS 3D D'UN PAS DE TEMPS
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !   AT           !<-- ! TEMPS CORRESPONDANT AU PAS TRAITE            !
! !   U,V,W        !<-- ! COMPOSANTES 3D DE LA VITESSE                 !
! !   TA,TP        !<-- ! CONCENTRATIONS DES TRACEURS                  !
! !   NUX,NUY,NUZ  !<-- ! COEFFICIENTS DE VISCOSITE POUR LES VITESSES  !
! !   NAX,NAY,NAZ  !<-- ! COEFFICIENTS DE VISCOSITE POUR LES TR.ACTIFS !
! !   NPX,NPY,NPZ  !<-- ! COEFFICIENTS DE VISCOSITE POUR LES TR.PASSIFS!
! !   RI           !<-- ! NOMBRE DE RICHARDSON                         !
! !   AK,EP        !<-- ! VARIABLES DU MODELE K-EPSILON                !
! !   RHO          !<-- ! ECARTS RELATIFS DE DENSITE                   !
! !   H            !<-- ! HAUTEUR D'EAU                                !
! !   Z            !<-- ! COTES DES NOEUDS                             !
! !   ZSTAR        ! -->! COTES RELATIVES DES NOEUDS                   !
! !   NPOIN2       ! -->! NOMBRE DE POINTS DU MAILLAGE 2D              !
! !   NPOIN3       ! -->! NOMBRE DE POINTS DU MAILLAGE 3D              !
! !   NRES         ! -->! NUMERO DE CANAL DU FICHIER DE RESULTAT 3D    !
! !   NPLAN        ! -->! NOMBRE DE PLANS                              !
! !   NPLINT       ! -->! NUMERO DU PLAN DE CHANGEMENT DE TRANSFORMATION
! !   NTRAC        ! -->! NOMBRE DE TRACEURS ACTIFS                    !
! !   NTRPA        ! -->! NOMBRE DE TRACEURS PASSIFS                   !
! !   SORG3D       ! -->! INDICATEUR DES VARIABLES ENREGISTREES        !
! !   R4           ! -->! TABLEAU DE REELS SIMPLE PRECISION POUR LIT   !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
! SOUS-PROGRAMME APPELE PAR : POSTEL3D
! SOUS-PROGRAMME APPELES : LIT
!
!**********************************************************************
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NPOIN3,NPOIN2,NPLAN,NRES
      INTEGER NVA3
!      INTEGER , INTENT(IN) :: NELEM3
      CHARACTER*3 BINRES
!
      DOUBLE PRECISION , INTENT(INOUT) :: AT
      DOUBLE PRECISION , INTENT(INOUT) :: U(NPOIN3)
      DOUBLE PRECISION , INTENT(INOUT) :: V(NPOIN3)
      DOUBLE PRECISION , INTENT(INOUT) :: W(NPOIN3)
      DOUBLE PRECISION , INTENT(INOUT) :: Z(NPOIN2,NPLAN)
      LOGICAL , INTENT(IN) :: VARSUB
      TYPE (BIEF_OBJ) , INTENT(INOUT) :: TAB
!
      INTEGER I,ISTAT
      DOUBLE PRECISION XB(2)
      INTEGER IB(2)
      CHARACTER(LEN=1) CB
      REAL RB(NPOIN3)
!
!***********************************************************************
!
!
! LECTURE DU TEMPS DU DEBUT DU CALCUL
!
      CALL LIT(XB,RB,IB,CB,1,'R4',NRES,BINRES,ISTAT)
      AT=XB(1)
!
! LECTURE DES VITESSES U,V ET W
!
      CALL LIT(Z,RB,IB,CB,NPOIN3,'R4',NRES,BINRES,ISTAT)
      CALL LIT(U,RB,IB,CB,NPOIN3,'R4',NRES,BINRES,ISTAT)
      CALL LIT(V,RB,IB,CB,NPOIN3,'R4',NRES,BINRES,ISTAT)
      CALL LIT(W,RB,IB,CB,NPOIN3,'R4',NRES,BINRES,ISTAT)
!
      IF (NVA3.GT.4) THEN
      DO I=1,NVA3-4
        CALL LIT(TAB%ADR(I)%P%R,RB,IB,CB,NPOIN3,'R4',NRES,BINRES,ISTAT)
      ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END