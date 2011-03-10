C======================================================================C
C  Fichier FORTRAN de cas-test de validation de TOMAWAC V5P6.          C
C                                                                      C
C  Nom du cas : CAS-TEST EXPERIMENTAL 15 DE BATTJES ET JANSSEN (1978)  C
C               EXECUTION DANS L'UN OU L'AUTRE DES MODES DE TOMAWAC.   C
C                                                                      C
C  Ce fichier contient les subroutines suivantes :                     C
C   CORFON : affectation de la bathymetrie de la barre du cas-test.    C
C   CORRXY : modification des coordonnees du maillage lu sur fichier.  C
C                                                                      C
C   Faire une recherche de CMB pour trouver les parties modifiees.     C
C                                                                      C
C                          Michel BENOIT (EDF R&D LNHE)   22/12/2005   C
C======================================================================C
C                       *****************
                        SUBROUTINE CORFON
C                       *****************
C
C***********************************************************************
C PROGICIEL : COWADIS           26/07/99           F.MARCOS
C***********************************************************************
C
C  USER SUBROUTINE CORFON 
C
C  FONCTION  : MODIFICATION DE LA TOPOGRAPHIE
C  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
C
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT :
C PROGRAMMES APPELES : RIEN EN STANDARD
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TOMAWAC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
CMB----------------------------------------Modif debut
      INTEGER IP
      DOUBLE PRECISION DMIN, XMIN
C                                                                       
C.....BATHYMETRIE EN PENTE VARIABLE DU CAS-TEST EN CANAL A HOULE BJ15
C.....DE BATTJES ET JANSSEN (1978) - CAS AVEC UNE BARRE.
C     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      DMIN = 0.05D0
      XMIN = 20.D0*DMIN
      DO IP=1,NPOIN2
        IF(X(IP).GE.19.02D0) THEN
           ZF(IP) = -0.616D0
        ELSEIF(X(IP).GE.9.02D0) THEN
           ZF(IP) = -0.616D0 + 0.0495D0*(19.02D0-X(IP))
        ELSEIF(X(IP).GE.4.62D0) THEN
           ZF(IP) = -0.120D0 - 0.025D0*(9.02D0-X(IP))
        ELSEIF(X(IP).GE.XMIN) THEN
           ZF(IP) = -0.230D0 + 0.05D0*(4.62D0-X(IP))
        ELSE
           ZF(IP) = -DMIN
        ENDIF
      ENDDO
CMB----------------------------------------Modif fin
C
      RETURN
      END                  
 
C                       *****************
                        SUBROUTINE CORRXY
C                       *****************
     * (X,Y,NPOIN)
C
C***********************************************************************
C PROGICIEL : BIEF 5.0          01/03/90    J-M HERVOUET
C***********************************************************************
C
C  USER SUBROUTINE CORRXY
C
C  FUNCTION  : MODIFICATION OF THE COORDINATES OF THE POINTS IN THE MESH
C
C              LINES WITH AN INITIAL CEX ARE AN EXAMPLE
C              WITH TELEMAC-2D
C
C              THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO
C              THE CALLING PROGRAM AND THE NEEDED MODIFICATION
C              BY ADDING USE DECLARATIONS_"NAME OF CALLING CODE"
C              ALL THE DATA STRUCTURE OF THIS CODE IS
C              AVAILABLE
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
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN),Y(NPOIN)
C
CMB----------------------------------------Modif debut
      INTEGER IP
C
      DO IP=1,NPOIN
        X(IP)=X(IP)*0.05D0
        Y(IP)=Y(IP)*0.12D0
      ENDDO
CMB----------------------------------------Modif fin
C
      RETURN                                                            
      END
