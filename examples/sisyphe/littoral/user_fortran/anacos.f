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
!                 (! STATIONNAIRE !)
!
!     FUNCTION  : SPECIFICATION OF AN ANALYTICAL CURRENT
!                 (! STATIONNARY !)
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
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)             ::  NPOIN2
      DOUBLE PRECISION, INTENT(IN)    ::  X (NPOIN2) , Y (NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) ::  UC(NPOIN2) , VC(NPOIN2)
!
!.....VARIABLES LOCALES
!     """""""""""""""""
      INTEGER  IP
      DOUBLE PRECISION UCONST, VCONST
!
!
      UCONST=1.0D0
      VCONST=1.0D0
!
      DO 100 IP=1,NPOIN2
        UC(IP)=UCONST
        VC(IP)=VCONST
  100 CONTINUE
!
      RETURN
      END

