C                       *****************
                        SUBROUTINE PRE2DH
C                       *****************
C
     *(X,Y,IKLES,IPOBO,NPOIN2,NELEM2,NC2DH,NCOU,TITCAS,
     * NVAR,NTRAC,NTRPA,BINCOU,nva3,textlu)
C
C***********************************************************************
C POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
C FORTRAN90
C***********************************************************************
C
C     FONCTION  : PREPARATION DES FICHIERS DES COUPES HORIZONTALES
C                      CONVENTIONS DU LOGICIEL SELAFIN.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !   X,Y          ! -->! COORDONNEES DU MAILLAGE CURVILIGNE           !
C !   IKLES        ! -->! TABLE DE CONNECTIVITE                        !
C !   IPOBO        ! -->! INDICATEUR DE LA NATURE DES POINTS           !
C !   NPOIN2       ! -->! NOMBRE DE POINTS DU MAILLAGE 2D              !
C !   NELEM2       ! -->! NOMBRE D'ELEMENTS DU MAILLAGE 2D             !
C !   NC2DH        ! -->! NOMBRE DE COUPES HORIZONTALES                !
C !   NCOU         ! -->! NUMERO DE CANAL - 1 DE LA PREMIERE COUPE     !
C !   TITCAS       ! -->! TITRE A PORTER SUR CHAQUE COUPE              !
C !   NVAR         ! -->! NOMBRE DE VARIABLES ENREGISTREES             !
C !   NTRAC        ! -->! NOMBRE DE TRACEURS ACTIFS                    !
C !   NTRPA        ! -->! NOMBRE DE TRACEURS PASSIFS                   !
C !   SORG3D       ! -->! INDICATEUR DES VARIABLES ENREGISTREES        !
C !   BINCOU       ! -->! STANDARD DE BINAIRE POUR LES COUPES          !
C !________________!____!______________________________________________!
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C SOUS-PROGRAMME APPELE PAR : POSTEL3D
C SOUS-PROGRAMME APPELES : ECRDEB , ECRI2
C
C***********************************************************************
C
C     - DOCUMENTATION : NOTICE SELAFIN
C
C***********************************************************************
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPOIN2,NELEM2,NC2DH,NCOU,NVAR(1),NTRAC,NTRPA
C
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2)
C
      INTEGER NBV(2),IB(10),I(4)
      INTEGER , INTENT(INOUT) :: IKLES(3,NELEM2),IPOBO(NPOIN2)
      INTEGER IC,J,CANAL,IBID(1),ISTAT
      INTEGER NVA3
      INTEGER N
C
      CHARACTER*72 TITCAS
      CHARACTER*32 TEXTLU(100)
      CHARACTER*3  BINCOU
C
      CHARACTER(LEN=2) CB
      DOUBLE PRECISION XB(2)
C-----------------------------------------------------------------------
      N=0
C
C  NOMBRE DE VARIABLES EN SORTIE :
C  (ON NE SORT PAS LES VARIABLES QUI SERVENT A SUBIEF-3D)
C
      NBV(1) = NVA3
      NBV(2) = 0
C
C  4 PARAMETRES INDISPENSABLES :
C
      I(1) = NELEM2
      I(2) = NPOIN2
      I(3) = 3
      I(4) = 1
C
C  LISTE DE FUTURS PARAMETRES DEJA PREVUS.(SEUL LES PREMIERS SERVENT)
C
      DO 10 J = 1,10
         IB(J) = 0
10    CONTINUE
C   ECRITURE ECLATEE DES RESULTATS (CONVENTION SELAFIN)
      IB(1) = 1
C
C-----------------------------------------------------------------------
C
C  POUR CHAQUE COUPE HORIZONTALE FAIRE :
C
      DO 20 IC = 1,NC2DH
C
         CANAL = NCOU
C
C     OUVERTURE DU FICHIER + ENREGISTREMENT DES PREMIERS PARAMETRES
C     -------------------------------------------------------------
C
         CALL ECRDEB(NCOU-1+IC,BINCOU,TITCAS,NBV,NTRAC,NTRPA,.TRUE.,
     *               TEXTLU,IC,N)
C
C     ENREGISTREMENT DES AUTRES PARAMETRES DE L'ENTETE
C     ------------------------------------------------
C
         CALL ECRI2(XB, IB  ,CB,      10, 'I',NCOU-1+IC,BINCOU,ISTAT)
         CALL ECRI2(XB, I   ,CB,       4, 'I',NCOU-1+IC,BINCOU,ISTAT)
         CALL ECRI2(XB,IKLES,CB,3*NELEM2, 'I',NCOU-1+IC,BINCOU,ISTAT)
         CALL ECRI2(XB,IPOBO,CB,  NPOIN2, 'I',NCOU-1+IC,BINCOU,ISTAT)
         CALL ECRI2( X, IBID,CB,  NPOIN2,'R4',NCOU-1+IC,BINCOU,ISTAT)
         CALL ECRI2( Y, IBID,CB,  NPOIN2,'R4',NCOU-1+IC,BINCOU,ISTAT)
C
20    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
