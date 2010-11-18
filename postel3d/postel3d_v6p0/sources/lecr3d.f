                        SUBROUTINE LECR3D
C                       *****************
C
     *(AT,Z,U,V,W,
     * NPOIN3,NPOIN2,NPLAN,NRES,BINRES,
     * RB,NVA3,TAB,varsub)
c     * RB,NVA3,TAB,varsub,
c     * ws,wscon2,fluin2,hn,nelem3)
C
C***********************************************************************
C POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
C FORTRAN90
C***********************************************************************
C
C     FONCTION  : LIT LES ENREGISTREMENTS 3D D'UN PAS DE TEMPS
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !   AT           !<-- ! TEMPS CORRESPONDANT AU PAS TRAITE            !
C !   U,V,W        !<-- ! COMPOSANTES 3D DE LA VITESSE                 !
C !   TA,TP        !<-- ! CONCENTRATIONS DES TRACEURS                  !
C !   NUX,NUY,NUZ  !<-- ! COEFFICIENTS DE VISCOSITE POUR LES VITESSES  !
C !   NAX,NAY,NAZ  !<-- ! COEFFICIENTS DE VISCOSITE POUR LES TR.ACTIFS !
C !   NPX,NPY,NPZ  !<-- ! COEFFICIENTS DE VISCOSITE POUR LES TR.PASSIFS!
C !   RI           !<-- ! NOMBRE DE RICHARDSON                         !
C !   AK,EP        !<-- ! VARIABLES DU MODELE K-EPSILON                !
C !   RHO          !<-- ! ECARTS RELATIFS DE DENSITE                   !
C !   H            !<-- ! HAUTEUR D'EAU                                !
C !   Z            !<-- ! COTES DES NOEUDS                             !
C !   ZSTAR        ! -->! COTES RELATIVES DES NOEUDS                   !
C !   NPOIN2       ! -->! NOMBRE DE POINTS DU MAILLAGE 2D              !
C !   NPOIN3       ! -->! NOMBRE DE POINTS DU MAILLAGE 3D              !
C !   NRES         ! -->! NUMERO DE CANAL DU FICHIER DE RESULTAT 3D    !
C !   NPLAN        ! -->! NOMBRE DE PLANS                              !
C !   NPLINT       ! -->! NUMERO DU PLAN DE CHANGEMENT DE TRANSFORMATION
C !   NTRAC        ! -->! NOMBRE DE TRACEURS ACTIFS                    !
C !   NTRPA        ! -->! NOMBRE DE TRACEURS PASSIFS                   !
C !   SORG3D       ! -->! INDICATEUR DES VARIABLES ENREGISTREES        !
C !   R4           ! -->! TABLEAU DE REELS SIMPLE PRECISION POUR LIT   !
C !________________!____!______________________________________________!
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C SOUS-PROGRAMME APPELE PAR : POSTEL3D
C SOUS-PROGRAMME APPELES : LIT
C
C**********************************************************************
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPOIN3,NPOIN2,NPLAN,NRES
      INTEGER NVA3
c      INTEGER , INTENT(IN) :: NELEM3
      CHARACTER*3 BINRES
C
      DOUBLE PRECISION , INTENT(INOUT) :: AT
      DOUBLE PRECISION , INTENT(INOUT) :: U(NPOIN3)
      DOUBLE PRECISION , INTENT(INOUT) :: V(NPOIN3)
      DOUBLE PRECISION , INTENT(INOUT) :: W(NPOIN3)
      DOUBLE PRECISION , INTENT(INOUT) :: Z(NPOIN2,NPLAN)
c      DOUBLE PRECISION , INTENT(INOUT) :: WS(NPOIN3),WSCON2(NPOIN3)
c      DOUBLE PRECISION , INTENT(INOUT) :: FLUIN2(6*NELEM3)
c      DOUBLE PRECISION , INTENT(INOUT) :: HN(NPOIN2)
      LOGICAL , INTENT(IN) :: VARSUB
      TYPE (BIEF_OBJ) , INTENT(INOUT) :: TAB
C
C
C
C
      INTEGER I,ISTAT
      DOUBLE PRECISION XB(2)
      INTEGER IB(2)
      CHARACTER(LEN=1) CB
      REAL RB(NPOIN3)
C
C***********************************************************************
C
C
C LECTURE DU TEMPS DU DEBUT DU CALCUL
C
      CALL LIT(XB,RB,IB,CB,1,'R4',NRES,BINRES,ISTAT)
      AT=XB(1)
C
C LECTURE DES VITESSES U,V ET W
C
      CALL LIT(Z,RB,IB,CB,NPOIN3,'R4',NRES,BINRES,ISTAT)
      CALL LIT(U,RB,IB,CB,NPOIN3,'R4',NRES,BINRES,ISTAT)
      CALL LIT(V,RB,IB,CB,NPOIN3,'R4',NRES,BINRES,ISTAT)
      CALL LIT(W,RB,IB,CB,NPOIN3,'R4',NRES,BINRES,ISTAT)
c
      IF (NVA3.GT.4) THEN
      DO I=1,NVA3-4
        CALL LIT(TAB%ADR(I)%P%R,RB,IB,CB,NPOIN3,'R4',NRES,BINRES,ISTAT)
      ENDDO
      ENDIF
C-----------------------------------------------------------------------
C
      if (varsub) then
        READ(NRES)
        READ(NRES)
        READ(NRES)
        READ(NRES)
      endif
c      if (varsub) then
c        CALL LIT(WS,RB,IB,CB,NPOIN3,'R4',NRES,BINRES,ISTAT)
c        CALL LIT(WSCON2,RB,IB,CB,NPOIN3,'R4',NRES,BINRES,ISTAT)
c        CALL LIT(FLUIN2,RB,IB,CB,6*NELEM3,'R4',NRES,BINRES,ISTAT)
c        CALL LIT(HN,RB,IB,CB,NPOIN2,'R4',NRES,BINRES,ISTAT)
c      endif
C-----------------------------------------------------------------------
C
      RETURN
      END
