!                       ***************************
                        DOUBLE PRECISION FUNCTION Q
!                       ***************************
!
     &( I )
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.0    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
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
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN) :: I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=9) FCT
      INTEGER N
!
!     IF FILE OF LIQUID BOUNDARIES EXISTING, ATTEMPT TO FIND
!     THE VALUE IN IT. IF YES, OKQ REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OKQ SET     TO .FALSE.
!
      IF(OKQ(I).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
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
          STOP 'Q NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
        ENDIF
        CALL READ_FIC_FRLIQ(Q,FCT,AT,T2D_FILES(T2DIMP)%LU,
     &                      ENTET,OKSL(I))
!
      ENDIF
!
      IF(.NOT.OKQ(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
!
!     PROGRAMMABLE PART
!     Q IS TAKEN IN THE PARAMETER FILE, BUT MAY BE CHANGED
!
        Q = DEBIT(I)
        IF (I.EQ.2) THEN
            IF (AT.LE.36000.D0) THEN
                Q = AT - 26000.D0
            ELSE
                Q = DEBIT(I)
            ENDIF
        ELSEIF (I.EQ.3) THEN
            IF (AT.LE.30800.D0) THEN
                Q= -0.05D0*AT+1550.D0
            ELSE
                Q = DEBIT(I)
            ENDIF
        ELSEIF (I.EQ.4) THEN
            IF (AT.LE.29500.D0) THEN
                Q= -1.D0*AT+28000.D0
            ELSE
                Q = DEBIT(I)
            ENDIF
        ELSEIF (I.EQ.5) THEN
            IF (AT.LE.29500.D0) THEN
                Q= AT-28000.D0
            ELSE
                Q = DEBIT(I)
            ENDIF
        ELSEIF (I.EQ.6) THEN
            IF (AT.LE.35500.D0) THEN
                Q= AT-27000.D0
            ELSE
                Q = DEBIT(I)
            ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

