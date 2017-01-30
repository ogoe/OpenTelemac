!                       *****************
                        SUBROUTINE ANAVEN
!                       *****************
!
     &( UV    , VV    , X     , Y     , NPOIN2, AT    , DDC   , VX_CTE,
     &  VY_CTE)
!
!***********************************************************************
!  TOMAWAC VERSION 1.0    07/06/95       M. BENOIT (LNH) 30 87 72 66
!***********************************************************************
!
!     FONCTION  : PERMET LA SPECIFICATION D'UN VENT ANALYTIQUE
!                 (EVENTUELLEMENT VARIABLE EN TEMPS)
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !    UV,VV       !<-- ! COMPOSANTES DU CHAMP DE VENT INITIAL         !
! !    X,Y         ! -->! COORDONNEES DES POINTS DU MAILLAGE 2D        !
! !    NPOIN2      ! -->! NOMBRE DE POINTS 2D                          !
! !    AT          !<-- ! TEMPS DU CALCUL                              !
! !    DDC         !<-- ! DATE DE DEBUT DU CALCUL                      !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
!  APPELE PAR : CONDIW,SEMIMP
!
!  SOUS-PROGRAMME APPELE : NEANT
!
!***********************************************************************
!
      IMPLICIT NONE
!
!.....VARIABLES TRANSMISES
!     """"""""""""""""""""
      INTEGER  NPOIN2
      DOUBLE PRECISION AT    , DDC   , VX_CTE, VY_CTE
      DOUBLE PRECISION X (NPOIN2) , Y (NPOIN2)
      DOUBLE PRECISION UV(NPOIN2) , VV(NPOIN2)
!
!MB-----------------------------------Modif debut
!
!.....VARIABLES LOCALES
!     """""""""""""""""
      INTEGER  IP
      DOUBLE PRECISION UCONST, VCONST, VITES , DIR_1 , DIR_2 , CNVR
!
!.....VITES  : vitesse du vent en m/s (constante ici)
!.....DIRR_1 : direction initiale du vent (en degres, par rapport au Nord)
!.....DIRR_2 : direction finale   du vent (en degres, par rapport au Nord)
!              NB : il s'agit de directions vers ou souffle le vent.
      VITES=20.D0
      DIR_1=90.D0
      DIR_2=30.D0
!
      CNVR=3.141592654D0/180.D0
!
      IF (AT.LT.28790.D0) THEN
        UCONST=VITES*SIN(DIR_1*CNVR)
        VCONST=VITES*COS(DIR_1*CNVR)
      ELSE
        UCONST=VITES*SIN(DIR_2*CNVR)
        VCONST=VITES*COS(DIR_2*CNVR)
      ENDIF
!
      DO IP=1,NPOIN2
        UV(IP)=UCONST
        VV(IP)=VCONST
      ENDDO
!MB-----------------------------------Modif fin
!
      RETURN
      END

