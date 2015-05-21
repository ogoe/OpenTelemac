!                       *****************
                        SUBROUTINE PRE2DH
!                       *****************
!
     &(X,Y,IKLES,IPOBO,NPOIN2,NELEM2,NC2DH,NCOU,TITCAS,
     & NVAR,NTRAC,NTRPA,FFORMAT,NVA3,TEXTLU)
!
!***********************************************************************
! POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
! FORTRAN90
!***********************************************************************
!
!     FONCTION  : PREPARATION DES FICHIERS DES COUPES HORIZONTALES
!                      CONVENTIONS DU LOGICIEL SELAFIN.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! !      NOM       !MODE!                   ROLE                       !
! !________________!____!______________________________________________!
! !   X,Y          ! -->! COORDONNEES DU MAILLAGE CURVILIGNE           !
! !   IKLES        ! -->! TABLE DE CONNECTIVITE                        !
! !   IPOBO        ! -->! INDICATEUR DE LA NATURE DES POINTS           !
! !   NPOIN2       ! -->! NOMBRE DE POINTS DU MAILLAGE 2D              !
! !   NELEM2       ! -->! NOMBRE D'ELEMENTS DU MAILLAGE 2D             !
! !   NC2DH        ! -->! NOMBRE DE COUPES HORIZONTALES                !
! !   NCOU         ! -->! NUMERO DE CANAL - 1 DE LA PREMIERE COUPE     !
! !   TITCAS       ! -->! TITRE A PORTER SUR CHAQUE COUPE              !
! !   NVAR         ! -->! NOMBRE DE VARIABLES ENREGISTREES             !
! !   NTRAC        ! -->! NOMBRE DE TRACEURS ACTIFS                    !
! !   NTRPA        ! -->! NOMBRE DE TRACEURS PASSIFS                   !
! !   SORG3D       ! -->! INDICATEUR DES VARIABLES ENREGISTREES        !
! !   BINCOU       ! -->! STANDARD DE BINAIRE POUR LES COUPES          !
! !________________!____!______________________________________________!
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : POSTEL3D
! SOUS-PROGRAMME APPELES : ECRDEB , ECRI2
!
!***********************************************************************
!
!     - DOCUMENTATION : NOTICE SELAFIN
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_HERMES
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NPOIN2,NELEM2,NC2DH,NCOU,NVAR(1),NTRAC,NTRPA
!
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2)
!
      INTEGER , INTENT(INOUT) :: IKLES(3,NELEM2),IPOBO(NPOIN2)
      INTEGER IC,J,CANAL,IBID(1),ISTAT
      INTEGER NVA3
      INTEGER N
!
      CHARACTER*72 TITCAS
      CHARACTER*32 TEXTLU(100)
      CHARACTER*8 FFORMAT
!
      INTEGER :: DATE(3), TIME(3), IERR
!-----------------------------------------------------------------------
      N=0
!
!
!-----------------------------------------------------------------------
!
!  POUR CHAQUE COUPE HORIZONTALE FAIRE :
!
      DO IC = 1,NC2DH
!
        CANAL = NCOU-1+IC
!
!     OUVERTURE DU FICHIER + ENREGISTREMENT DES PREMIERS PARAMETRES
!     -------------------------------------------------------------
!
        CALL ECRDEB(CANAL,FFORMAT,TITCAS,NVA3,NTRAC,NTRPA,.TRUE.,
     &               TEXTLU,IC,N)
!
!     ENREGISTREMENT DES AUTRES PARAMETRES DE L'ENTETE
!     ------------------------------------------------
!
        DATE = (/0,0,0/)
        TIME = (/0,0,0/)
        CALL SET_MESH(FFORMAT,CANAL,2,TRIANGLE_ELT_TYPE,3,0,0,
     &                NELEM2,NPOIN2,IKLES,IPOBO,IPOBO,X,Y,0,
     &                DATE,TIME,IERR)
        CALL CHECK_CALL(IERR,'PRED2H:SET_MESH')
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
