C                       ***************************
                        DOUBLE PRECISION FUNCTION Q
C                       ***************************
C
     *( I )
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.2    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C FONCTION  : DONNE LA VALEUR DU DEBIT POUR TOUTES LES ENTREES A DEBIT
C             IMPOSE.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |   AT           | -->| TEMPS AUQUEL ON DONNE LE DEBIT               |
C |   I            | -->| RANG DE LA FRONTIERE A DEBIT IMPOSE          |
C |                |    | (1 S'IL N'Y EN A QU'UNE)                     |
C |   DEBIT        | -->| TABLEAU DES DEBITS IMPOSES.                  |
C |                |    | (LU DANS LE FICHIER DES PARAMETRES)          |
C |   HAUTEUR D'EAU| -->| TABLEAU DES HAUTEURS D'EAU.                  |
C |   NPOIN        | -->| NOMBRE DE POINTS DU TABLEAU DES HAUTEURS     |
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C  APPELE PAR : BORD
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN) :: I
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER*8 FCT
      INTEGER N
      LOGICAL DEJA,OK(MAXFRO)
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
C
C     FIRST CALL, OK INITIALISED TO .TRUE.
C
      IF(.NOT.DEJA) THEN
        DO N=1,MAXFRO
          OK(N)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
C
C     IF FILE OF LIQUID BOUNDARIES EXISTING, ATTEMPT TO FIND
C     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
C                      IF  NO, OK SET     TO .FALSE.
C
      IF(OK(I).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
C
C       FCT WILL BE Q(1), Q(2), ETC, Q(99), DEPENDING ON I
        FCT(1:2)='Q('
        IF(I.LT.10) THEN 
          WRITE(FCT(3:3),FMT='(I1)') I
          FCT(4:8)=')    '
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(3:4),FMT='(I2)') I
          FCT(5:8)=')   '
        ELSE
          WRITE(LU,*) 'Q NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP 
        ENDIF
        CALL READ_FIC_FRLIQ(Q,FCT,AT,T2D_FILES(T2DIMP)%LU,ENTET,OK(I)) 
C
      ENDIF
C
      IF(.NOT.OK(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
C 
C     PROGRAMMABLE PART                              
C     Q IS TAKEN IN THE PARAMETER FILE, BUT MAY BE CHANGED 
                                                             
         IF(AT.LE.600.D0) THEN
           Q = DEBIT(I) * AT/600.D0
         ELSE
           Q = DEBIT(I)
         ENDIF             
C 
      ENDIF          
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C                       *****************
                        SUBROUTINE CORSTR
C                       *****************
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.2    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C      FONCTION: CORRECTION DU COEFFICIENT DE FROTTEMENT SUR LE FOND
C                QUAND IL EST VARIABLE EN TEMPS.
C
C      CE SOUS-PROGRAMME EST SIMPLEMENT UN MODELE
C      IL DOIT ETRE REMPLI PAR L'UTILISATEUR
C
C
C 
C
C-----------------------------------------------------------------------
C  EXAMPLE OF POSSIBLE ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |    CHESTR      |<-- |  COEFFICIENT DE FROTTEMENT                   |
C |    X,Y         | -->|  COORDONNEE DU MAILLAGE .                    |
C |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE                |
C |    PRIVE       | -->|  TABLEAU DE TRAVAIL DEFINI DANS PRINCI       |
C |    ZF          | -->|  COTE DU FOND                                |
C |    KFROT       | -->|  LOI DE FROTTEMENT (LINEAIRE,CHEZY,STRICKLER)|
C |    FFON        | -->|  COEFFICIENT DE FROTTEMENT ASSOCIE A LA LOI  |
C |    H           | -->|  HAUTEUR D'EAU.
C |    AT          | -->|  TIME.
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C  APPELE PAR : TELMAC
C
C  SOUS-PROGRAMME APPELE :
C
C**********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER :: I
C
C-----------------------------------------------------------------------

      IF (LT == 1) THEN
         DO I = 1, NPOIN
            IF ( ABS(CHESTR%R(I) - 0.025D0) < 1.D-7 ) THEN
               CHESTR%R(I) = 0.020D0
            END IF
         ENDDO
      END IF

C-----------------------------------------------------------------------
C
      RETURN
      END
