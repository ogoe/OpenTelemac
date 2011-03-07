!                       *****************
                        SUBROUTINE PRE2DH
!                       *****************
!
     &(X,Y,IKLES,IPOBO,NPOIN2,NELEM2,NC2DH,NCOU,TITCAS,
     & NVAR,NTRAC,NTRPA,BINCOU,nva3,textlu)
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
!***********************************************************************
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NPOIN2,NELEM2,NC2DH,NCOU,NVAR(1),NTRAC,NTRPA
!
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2)
!
      INTEGER NBV(2),IB(10),I(4)
      INTEGER , INTENT(INOUT) :: IKLES(3,NELEM2),IPOBO(NPOIN2)
      INTEGER IC,J,CANAL,IBID(1),ISTAT
      INTEGER NVA3
      INTEGER N
!
      CHARACTER*72 TITCAS
      CHARACTER*32 TEXTLU(100)
      CHARACTER*3  BINCOU
!
      CHARACTER(LEN=2) CB
      DOUBLE PRECISION XB(2)
!-----------------------------------------------------------------------
      N=0
!
!  NOMBRE DE VARIABLES EN SORTIE :
!  (ON NE SORT PAS LES VARIABLES QUI SERVENT A SUBIEF-3D)
!
      NBV(1) = NVA3
      NBV(2) = 0
!
!  4 PARAMETRES INDISPENSABLES :
!
      I(1) = NELEM2
      I(2) = NPOIN2
      I(3) = 3
      I(4) = 1
!
!  LISTE DE FUTURS PARAMETRES DEJA PREVUS.(SEUL LES PREMIERS SERVENT)
!
      DO 10 J = 1,10
         IB(J) = 0
10    CONTINUE
!   ECRITURE ECLATEE DES RESULTATS (CONVENTION SELAFIN)
      IB(1) = 1
!
!-----------------------------------------------------------------------
!
!  POUR CHAQUE COUPE HORIZONTALE FAIRE :
!
      DO 20 IC = 1,NC2DH
!
         CANAL = NCOU
!
!     OUVERTURE DU FICHIER + ENREGISTREMENT DES PREMIERS PARAMETRES
!     -------------------------------------------------------------
!
         CALL ECRDEB(NCOU-1+IC,BINCOU,TITCAS,NBV,NTRAC,NTRPA,.TRUE.,
     &               TEXTLU,IC,N)
!
!     ENREGISTREMENT DES AUTRES PARAMETRES DE L'ENTETE
!     ------------------------------------------------
!
         CALL ECRI2(XB, IB  ,CB,      10, 'I',NCOU-1+IC,BINCOU,ISTAT)
         CALL ECRI2(XB, I   ,CB,       4, 'I',NCOU-1+IC,BINCOU,ISTAT)
         CALL ECRI2(XB,IKLES,CB,3*NELEM2, 'I',NCOU-1+IC,BINCOU,ISTAT)
         CALL ECRI2(XB,IPOBO,CB,  NPOIN2, 'I',NCOU-1+IC,BINCOU,ISTAT)
         CALL ECRI2( X, IBID,CB,  NPOIN2,'R4',NCOU-1+IC,BINCOU,ISTAT)
         CALL ECRI2( Y, IBID,CB,  NPOIN2,'R4',NCOU-1+IC,BINCOU,ISTAT)
!
20    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END