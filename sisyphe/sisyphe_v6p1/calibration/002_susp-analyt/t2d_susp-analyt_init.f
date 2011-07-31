C                       *****************
                        SUBROUTINE CORRXY
C                       *****************
     * (X,Y,NPOIN)
C
C***********************************************************************
C BIEF VERSION 5.6  17/10/05  EMILE RAZAFINDRAKOTO (LNHE) 01 30 87 74 03
C***********************************************************************
C
C  USER SUBROUTINE CORRXY
C
C  FUNCTION  : MODIFICATION OF THE COORDINATES OF THE POINTS IN THE MESH
C
C
C              THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO
C              THE CALLING PROGRAM AND THE NEEDED MODIFICATION
C              BY ADDING USE DECLARATIONS_"NAME OF CALLING CODE"
C              ALL THE DATA STRUCTURE OF THIS CODE IS
C              AVAILABLE
C
C              BEWARE : DO NOT DO ROTATIONS THAT WILL CHANGE THE
C                       NUMBERING OF LIQUID BOUNDARIES
C                       
C
C
C-----------------------------------------------------------------------
C  ARGUMENTS USED IN THE EXAMPLE 
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |    X,Y         | -->|  COORDONNEES DU MAILLAGE .                   |
C |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE                |
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT :
C PROGRAMMES APPELES : RIEN EN STANDARD
C
C***********************************************************************
C
C APPELE PAR : INBIEF
C
C SOUS-PROGRAMME APPELE : NEANT
C
C***********************************************************************
C
      USE BIEF, EX_CORRXY => CORRXY
C
C
C     OTHER DATA ARE AVAILABLE WITH THE DECLARATIONS OF EVERY PROGRAM
C
C     USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN),Y(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
       INTEGER I
C
C-----------------------------------------------------------------------
C
C  EXAMPLE : MULTIPLICATION BY A CONSTANT (SCALING THE MESH)
C            CHANGING OF THE ORIGIN
C
      DO I = 1 , NPOIN
         X(I) = X(I) /10.D0
         Y(I) = Y(I)
      ENDDO
C
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)'CORRXY (BIEF) : PAS DE MODIFICATION DES COORDONNEES'
        WRITE(LU,*)
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*)'CORRXY (BIEF):NO MODIFICATION OF COORDINATES'
        WRITE(LU,*)
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END


!!
!
!     MODIFICATIONS POUR CE CAS : CHERCHER "SOLUTION ANALYTIQUE"
!                                 2 BLOCS DIFFERENTS DE MODIFICATIONS
!
!
