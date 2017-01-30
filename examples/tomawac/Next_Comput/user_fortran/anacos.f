!                       *****************
                        SUBROUTINE ANACOS
!                       *****************
!
     &( UC    , VC    , X     , Y     , NPOIN2 )
!
!***********************************************************************
!  TOMAWAC VERSION 5.2    07/06/01
!***********************************************************************
!
!     FONCTION  : PERMET LA SPECIFICATION D'UN COURANT ANALYTIQUE
!
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !    UC,VC       !<-- ! COMPOSANTES DU CHAMP DE COURANT              !
! !    X,Y         ! -->! COORDONNEES DES POINTS DU MAILLAGE 2D        !
! !    NPOIN2      ! -->! NOMBRE DE POINTS 2D                          !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
!  APPELE PAR : CONDIW
!
!  SOUS-PROGRAMME APPELE : NEANT
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!.....VARIABLES TRANSMISES
!     """"""""""""""""""""
      INTEGER  NPOIN2
      DOUBLE PRECISION X (NPOIN2) , Y (NPOIN2)
      DOUBLE PRECISION UC(NPOIN2) , VC(NPOIN2)
!
!MB--------------------------------------Modif debut
!
!.....VARIABLES LOCALES
!     """""""""""""""""
      INTEGER  II
      DOUBLE PRECISION R0, RR, B, R1, UM, UU1, UR
!
!
      R0=10.D3
      B=0.3
      R1=R0/2.D0*(1+SQRT(1-2*B**2))
      UM=1.D0
      UU1=UM*EXP(-((R1-R0)/B/R0)**2)
!
      DO II=1,NPOIN2
        RR=SQRT(X(II)**2+Y(II)**2)
        IF (RR.LE.R1) THEN
          UR=UU1*RR/R1
        ELSE
          UR=UM*EXP(-((RR-R0)/(B*R0))**2)
        ENDIF
        IF (RR.EQ.0.D0) THEN
          UC(II)=0.D0
          VC(II)=0.D0
        ELSE
          UC(II)=UR*Y(II)/RR
          VC(II)=-UR*X(II)/RR
        ENDIF
      ENDDO
!MB--------------------------------------Modif fin
!
      RETURN
      END

