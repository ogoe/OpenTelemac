!                       ***************************
                        DOUBLE PRECISION FUNCTION Q
!                       ***************************
!
     &( I )
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.2    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
! FONCTION  : DONNE LA VALEUR DU DEBIT POUR TOUTES LES ENTREES A DEBIT
!             IMPOSE.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |   AT           | -->| TEMPS AUQUEL ON DONNE LE DEBIT               |
! |   I            | -->| RANG DE LA FRONTIERE A DEBIT IMPOSE          |
! |                |    | (1 S'IL N'Y EN A QU'UNE)                     |
! |   DEBIT        | -->| TABLEAU DES DEBITS IMPOSES.                  |
! |                |    | (LU DANS LE FICHIER DES PARAMETRES)          |
! |   HAUTEUR D'EAU| -->| TABLEAU DES HAUTEURS D'EAU.                  |
! |   NPOIN        | -->| NOMBRE DE POINTS DU TABLEAU DES HAUTEURS     |
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
!  APPELE PAR : BORD
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN) :: I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER*8 FCT
      INTEGER N
      LOGICAL DEJA,OK(MAXFRO)
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
!
!     FIRST CALL, OK INITIALISED TO .TRUE.
!
      IF(.NOT.DEJA) THEN
        DO N=1,MAXFRO
          OK(N)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
!
!     IF FILE OF LIQUID BOUNDARIES EXISTING, ATTEMPT TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK SET     TO .FALSE.
!
      IF(OK(I).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE Q(1), Q(2), ETC, Q(99), DEPENDING ON I
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
!
      ENDIF
!
      IF(.NOT.OK(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
! 
!     PROGRAMMABLE PART                              
!     Q IS TAKEN IN THE PARAMETER FILE, BUT MAY BE CHANGED 
                                                             
        IF(AT.LE.600.D0) THEN
          Q = DEBIT(I) * AT/600.D0
        ELSE
          Q = DEBIT(I)
        ENDIF             
! 
      ENDIF          
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *****************
                        SUBROUTINE CORSTR
!                       *****************
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.2    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
!
!***********************************************************************
!
!      FONCTION: CORRECTION DU COEFFICIENT DE FROTTEMENT SUR LE FOND
!                QUAND IL EST VARIABLE EN TEMPS.
!
!      CE SOUS-PROGRAMME EST SIMPLEMENT UN MODELE
!      IL DOIT ETRE REMPLI PAR L'UTILISATEUR
!
!
! 
!
!-----------------------------------------------------------------------
!  EXAMPLE OF POSSIBLE ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |    CHESTR      |<-- |  COEFFICIENT DE FROTTEMENT                   |
! |    X,Y         | -->|  COORDONNEE DU MAILLAGE .                    |
! |    NPOIN       | -->|  NOMBRE DE POINTS DU MAILLAGE                |
! |    PRIVE       | -->|  TABLEAU DE TRAVAIL DEFINI DANS PRINCI       |
! |    ZF          | -->|  COTE DU FOND                                |
! |    KFROT       | -->|  LOI DE FROTTEMENT (LINEAIRE,CHEZY,STRICKLER)|
! |    FFON        | -->|  COEFFICIENT DE FROTTEMENT ASSOCIE A LA LOI  |
! |    H           | -->|  HAUTEUR D'EAU.
! |    AT          | -->|  TIME.
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
!  APPELE PAR : TELMAC
!
!  SOUS-PROGRAMME APPELE :
!
!**********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER :: I
!
!-----------------------------------------------------------------------

      IF (LT == 1) THEN
        DO I = 1, NPOIN
           IF ( ABS(CHESTR%R(I) - 0.025D0) < 1.D-7 ) THEN
              CHESTR%R(I) = 0.020D0
           END IF
        ENDDO
      END IF

!-----------------------------------------------------------------------
!
      RETURN
      END
