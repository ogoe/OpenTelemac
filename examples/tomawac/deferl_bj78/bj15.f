!======================================================================C
!  Fichier FORTRAN de cas-test de validation de TOMAWAC V5P6.          C
!                                                                      C
!  Nom du cas : CAS-TEST EXPERIMENTAL 15 DE BATTJES ET JANSSEN (1978)  C
!               EXECUTION DANS L'UN OU L'AUTRE DES MODES DE TOMAWAC.   C
!                                                                      C
!  Ce fichier contient les subroutines suivantes :                     C
!   CORFON : affectation de la bathymetrie de la barre du cas-test.    C
!   CORRXY : modification des coordonnees du maillage lu sur fichier.  C
!                                                                      C
!   Faire une recherche de CMB pour trouver les parties modifiees.     C
!                                                                      C
!                          Michel BENOIT (EDF R&D LNHE)   22/12/2005   C
!======================================================================C
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
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
      INTEGER IP
      DOUBLE PRECISION DMIN, XMIN
!
!     tmptmp : permet de passer le fond en real pour comparer avec le fichier geom
      REAL TMPTMP
!
!.....BATHYMETRIE EN PENTE VARIABLE DU CAS-TEST EN CANAL A HOULE BJ15
!.....DE BATTJES ET JANSSEN (1978) - CAS AVEC UNE BARRE.
!     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      DMIN = 0.05D0
      XMIN = 20.D0*DMIN
      DO IP=1,NPOIN2
        IF(X(IP).GE.19.02D0) THEN
           TMPTMP = -0.616
        ELSEIF(X(IP).GE.9.02D0) THEN
            TMPTMP= -0.616 + 0.0495*(19.02-X(IP))
        ELSEIF(X(IP).GE.4.62D0) THEN
           TMPTMP = -0.120 - 0.025*(9.02-X(IP))
        ELSEIF(X(IP).GE.XMIN) THEN
           TMPTMP= -0.230 + 0.05D0*(4.62-X(IP))
        ELSE
            TMPTMP= -DMIN
        ENDIF
        ZF(IP)=TMPTMP
      ENDDO
!
!-----------------------------------------------------------------------
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
