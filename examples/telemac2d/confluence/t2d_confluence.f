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
          STOP 'Q NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
        ENDIF
        CALL READ_FIC_FRLIQ(Q,FCT,AT,T2D_FILES(T2DIMP)%LU,ENTET,OK(I)) 
!
      ENDIF
!
      IF(.NOT.OK(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
! 
!     PROGRAMMABLE PART                              
!     Q IS TAKEN IN THE PARAMETER FILE, BUT MAY BE CHANGED 
!                                                                                                                                             
      Q = DEBIT(I) * MIN( AT/10.D0 , 1.D0 )
! 
      ENDIF          
!
!-----------------------------------------------------------------------
!
      RETURN
      END

       
