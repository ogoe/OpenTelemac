!                       ****************************
                        DOUBLE PRECISION FUNCTION SL
!                       ****************************
!
!
     &( I , N )
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.1    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
!
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
      USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: I,N
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION :: FLXCR
      COMMON/FLUX/FLXCR
      DOUBLE PRECISION Q1CR, Q2CR, Z1CR, Z2CR
      CHARACTER(LEN=9) FCT
      DOUBLE PRECISION CLIMIT
      EXTERNAL CLIMIT
      INTEGER ID
!
!-----------------------------------------------------------------------
!
!     IF FILE OF LIQUID BOUNDARIES EXISTING, ATTEMPT TO FIND
!     THE VALUE IN IT. IF YES, OKSL REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OKSL SET     TO .FALSE.
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
          WRITE(LU,*) 'SL NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
          CALL PLANTE(1)
          STOP
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
        ID = T2D_FILES(T2DFO1)%LU
        REWIND ID
        SL = COTE(I)
        IF(I.EQ.7) THEN
          IF(AT.LE.35400.D0) THEN
            SL = 0.0005D0*AT+41.3D0
          ELSE
            READ(ID,200) Q2CR, Z2CR
            Q1CR = 3900.D0
            Z1CR = 58.50D0
            DO WHILE (Q2CR.LT.FLXCR)
              Q1CR = Q2CR
              Z1CR = Z2CR
              READ(ID,200) Q2CR, Z2CR
            ENDDO
 200        FORMAT((F10.2,F5.2))
            SL = CLIMIT(Q1CR,Q2CR,Z1CR,Z2CR)
          ENDIF
        ENDIF
        IF(I.EQ.1) THEN
          IF(AT.LE.42860.D0) THEN
            SL = 0.0005D0*AT+16.7D0
          ELSE
            SL = COTE(I)
          ENDIF
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END

