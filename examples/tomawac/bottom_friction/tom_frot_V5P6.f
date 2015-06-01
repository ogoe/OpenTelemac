


!======================================================================C
!  Fichier FORTRAN de cas-test de validation de TOMAWAC V5P6.          C
!                                                                      C
!  Nom du cas : FROTTEMENT SUR LE FOND (CAS D'UN FOND PLAT)            C
!               EXECUTION DANS L'UN OU L'AUTRE DES MODES DE TOMAWAC.   C
!                                                                      C
!  Ce fichier contient les subroutines suivantes :                     C
!   TOM_CORFON : affectation de la cote du fond a -5m partout.             C
!   CORRXY : modification des coordonnees du maillage lu sur fichier.  C
!   SPELIM : modification du calcul de OM0 en mode ex-COWADIS pour     C
!            retrouver les memes resultats qu'en V1P0 de COWADIS.      C
!                                                                      C
!   Faire une recherche de CMB pour trouver les parties modifiees.     C
!                                                                      C
!                          Michel BENOIT (EDF R&D LNHE)   22/12/2005   C
!======================================================================C
!
!                       *****************
                        SUBROUTINE TOM_CORFON
!                       *****************
!
!***********************************************************************
! PROGICIEL : COWADIS           26/07/99           F.MARCOS
!***********************************************************************
!
!  USER SUBROUTINE TOM_CORFON
!
!  FONCTION  : MODIFICATION DE LA TOPOGRAPHIE
!  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
!
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT :
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!MB---------------------------------MODIF debut
      INTEGER IP
!
      DO IP=1,NPOIN2
        ZF(IP) = -5.0D0
      ENDDO
!MB---------------------------------MODIF fin
!
      RETURN
      END
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
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN),Y(NPOIN)
!
!MB---------------------------------MODIF debut
      INTEGER IP
!
      DO IP=1,NPOIN
        X(IP)=X(IP)*2.0D0
        Y(IP)=Y(IP)*0.5D0
      ENDDO
!MB---------------------------------MODIF fin
!
      RETURN
      END
