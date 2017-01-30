!                       ****************************
                        DOUBLE PRECISION FUNCTION SL
!                       ****************************
!
!
     &( I , N )
!
!***********************************************************************
!  TELEMAC 2D VERSION 7.1
!***********************************************************************
!
! FONCTION  : DONNE LA VALEUR DE LA COTE DE LA SURFACE LIBRE POUR TOUTES
!             LES ENTREES A COTE IMPOSEE.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |   I            | -->| RANG DE LA FRONTIERE A COTE IMPOSEE
! |                |    | (1 S'IL N'Y EN A QU'UNE)
! |   N            | -->| NUMERO GLOBAL DU POINT
! |________________|____|_______________________________________________
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
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      DOUBLE PRECISION A,WPLG,PER, PI, T0, WH, WD, K, C, G
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: I,N
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=9) FCT
      INTEGER J
!
!-----------------------------------------------------------------------
!
!     IF FILE OF LIQUID BOUNDARIES EXISTING, ATTEMPT TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK SET     TO .FALSE.
!
      IF(OKSL(I).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
!
!       FCT WILL BE SL(1), SL(2), ETC, SL(99), DEPENDING ON I
        FCT(1:3)='SL('
        IF(I.LT.10) THEN
          WRITE(FCT(4:4),FMT='(I1)') I
          FCT(5:8)=')   '
        ELSEIF(I.LT.100) THEN
          WRITE(FCT(4:5),FMT='(I2)') I
          FCT(6:8)=')  '
        ELSE
          STOP 'SL NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
        ENDIF
        CALL READ_FIC_FRLIQ(SL,FCT,AT,T2D_FILES(T2DIMP)%LU,
     &                      ENTET,OKSL(I))
!
      ENDIF
!
      IF(.NOT.OKSL(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
!
!     PROGRAMMABLE PART
!     SL IS TAKEN IN THE PARAMETER FILE, BUT MAY BE CHANGED
!
        PI = 3.141592653589D0
        T0 = 10.D0
        WD=0.32D0
        WH=WD*0.045D0
!
        K=SQRT(3*WH/(4*WD**3))
        G=9.8D0
        C=SQRT(G*(WD+WH))
        SL= WD+ WH*(1/((COSH(K*(C*AT-T0)))**2))

      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

