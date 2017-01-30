!                       *****************
                        SUBROUTINE CORRXY
!                       *****************
     & (X,Y,NPOIN)
!
!***********************************************************************
! PROGICIEL : BIEF 5.0          01/03/90    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE CORRXY
!
!  FUNCTION  : MODIFICATION OF THE COORDINATES OF THE POINTS IN THE MESH
!
!              LINES WITH AN INITIAL CEX ARE AN EXAMPLE
!              WITH TELEMAC-2D
!
!              THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO
!              THE CALLING PROGRAM AND THE NEEDED MODIFICATION
!              BY ADDING USE DECLARATIONS_"NAME OF CALLING CODE"
!              ALL THE DATA STRUCTURE OF THIS CODE IS
!              AVAILABLE
!
!-----------------------------------------------------------------------
!  ARGUMENTS USED IN THE EXAMPLE
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |    X,Y         | -->|  COORDONNEES DU MAILLAGE .                   |
! |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE                |
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT :
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
! APPELE PAR : INBIEF
!
! SOUS-PROGRAMME APPELE : NEANT
!
!***********************************************************************
!
      USE BIEF, EX_CORRXY => CORRXY
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN),Y(NPOIN)
!     xr et yr : permet de passer les coor en real pour comparer avec le fichier geom
      REAL XR,YR
!
!MB----------------------------------------Modif debut
      INTEGER IP
!
      DO IP=1,NPOIN
        XR=X(IP)*0.05
        X(IP)=XR

        YR=Y(IP)*0.12
        Y(IP)=YR
      ENDDO
!MB----------------------------------------Modif fin
!
      RETURN
      END

