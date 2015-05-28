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
!                    *****************
                     SUBROUTINE TOM_CORFON
!                    *****************
!
!
!***********************************************************************
! TOMAWAC   V6P1                                   14/06/2011
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!
!history  F. MARCOS
!+
!+
!+
!
!history  OPTIMER (    )
!+        12/01/2001
!+
!+   TOMAWAC/COWADIS MERGE
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TOMAWAC
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
c
      INTEGER IP
      DOUBLE PRECISION DMIN, XMIN
c
c     tmptmp : permet de passer le fond en real pour comparer avec le fichier geom
      real tmptmp
C                                                                       
C.....BATHYMETRIE EN PENTE VARIABLE DU CAS-TEST EN CANAL A HOULE BJ15
C.....DE BATTJES ET JANSSEN (1978) - CAS AVEC UNE BARRE.
C     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      DMIN = 0.05D0
      XMIN = 20.D0*DMIN
      DO IP=1,NPOIN2
        IF(X(IP).GE.19.02D0) THEN
           tmptmp = -0.616
        ELSEIF(X(IP).GE.9.02D0) THEN
            tmptmp= -0.616 + 0.0495*(19.02-X(IP))
        ELSEIF(X(IP).GE.4.62D0) THEN
           tmptmp = -0.120 - 0.025*(9.02-X(IP))
        ELSEIF(X(IP).GE.XMIN) THEN
           tmptmp= -0.230 + 0.05D0*(4.62-X(IP))
        ELSE
            tmptmp= -DMIN
        ENDIF
        ZF(IP)=tmptmp
      ENDDO
!
!-----------------------------------------------------------------------
!
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
c     xr et yr : permet de passer les coor en real pour comparer avec le fichier geom
      real xr,yr
C
CMB----------------------------------------Modif debut
      INTEGER IP
C
      DO IP=1,NPOIN
        Xr=X(IP)*0.05
        X(IP)=Xr

        Yr=Y(IP)*0.12
        Y(IP)=yr
      ENDDO
CMB----------------------------------------Modif fin
C
      RETURN                                                            
      END
