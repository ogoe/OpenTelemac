C======================================================================C
C  Fichier FORTRAN de cas-test de validation de TOMAWAC V5P6.          C
C                                                                      C
C  Nom du cas : CAS DE VENT TOURNANT DE 60 DEG. (SANS PROPAGATION).    C
C               EXECUTION EN MODE TROISIEME GENERATION.                C
C                                                                      C
C  Ce fichier contient les subroutines suivantes :                     C
C   ANAVEN : affectation de la vitesse du vent (variable en temps)     C
C                                                                      C
C   Faire une recherche de CMB pour trouver les parties modifiees.     C
C                                                                      C
C                          Michel BENOIT (EDF R&D LNHE)   10/12/2004   C
C======================================================================C
C
C                       *****************
                        SUBROUTINE ANAVEN
C                       *****************
C
     *( UV    , VV    , X     , Y     , NPOIN2, AT    , DDC   , VX_CTE,
     *  VY_CTE) 
C
C***********************************************************************
C  TOMAWAC VERSION 1.0    07/06/95       M. BENOIT (LNH) 30 87 72 66
C***********************************************************************
C
C     FONCTION  : PERMET LA SPECIFICATION D'UN VENT ANALYTIQUE
C                 (EVENTUELLEMENT VARIABLE EN TEMPS) 
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !    UV,VV       !<-- ! COMPOSANTES DU CHAMP DE VENT INITIAL         !
C !    X,Y         ! -->! COORDONNEES DES POINTS DU MAILLAGE 2D        !
C !    NPOIN2      ! -->! NOMBRE DE POINTS 2D                          !
C !    AT          !<-- ! TEMPS DU CALCUL                              !
C !    DDC         !<-- ! DATE DE DEBUT DU CALCUL                      !
C !________________!____!______________________________________________!
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C  APPELE PAR : CONDIW,SEMIMP
C
C  SOUS-PROGRAMME APPELE : NEANT
C
C***********************************************************************
C
      IMPLICIT NONE
C
C.....VARIABLES TRANSMISES
C     """"""""""""""""""""
      INTEGER  NPOIN2
      DOUBLE PRECISION AT    , DDC   , VX_CTE, VY_CTE
      DOUBLE PRECISION X (NPOIN2) , Y (NPOIN2)
      DOUBLE PRECISION UV(NPOIN2) , VV(NPOIN2)
C
CMB-----------------------------------Modif debut
C
C.....VARIABLES LOCALES
C     """""""""""""""""
      INTEGER  IP
      DOUBLE PRECISION UCONST, VCONST, VITES , DIR_1 , DIR_2 , CNVR
C
C.....VITES  : vitesse du vent en m/s (constante ici)
C.....DIRR_1 : direction initiale du vent (en degres, par rapport au Nord)
C.....DIRR_2 : direction finale   du vent (en degres, par rapport au Nord)
C              NB : il s'agit de directions vers ou souffle le vent.
      VITES=20.D0
      DIR_1=90.D0
      DIR_2=30.D0
C
      CNVR=3.141592654D0/180.D0
C
      IF (AT.LT.28790.D0) THEN
        UCONST=VITES*SIN(DIR_1*CNVR)
        VCONST=VITES*COS(DIR_1*CNVR)
      ELSE
        UCONST=VITES*SIN(DIR_2*CNVR)
        VCONST=VITES*COS(DIR_2*CNVR)
      ENDIF
C
      DO IP=1,NPOIN2
        UV(IP)=UCONST
        VV(IP)=VCONST
      ENDDO
CMB-----------------------------------Modif fin
C
      RETURN
      END
