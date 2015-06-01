!                       *****************
                        SUBROUTINE CORFON
!                       *****************
!
!***********************************************************************
! PROGICIEL : TELEMAC-2D 5.0          01/03/90    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE CORFON
!
!  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
!
!
!-----------------------------------------------------------------------
!  ARGUMENTS USED IN THE EXAMPLE
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |      ZF        |<-->| FOND A MODIFIER.
! |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
! |      A         |<-- | MATRICE
! |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
! |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
! |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
! |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
! |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT :
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU,IPOIN, I
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! Debut CR
! seuil passe usiniere
!      INTEGER boucleCR5,nbathyCR5
!      PARAMETER (nbathyCR5=25)
!      INTEGER numeroCR5(nbathyCR5)
!      DOUBLE PRECISION fondCR5(nbathyCR5)
!      DATA numeroCR5 / 41161,44127,43449,44059,41060,44195,44126,44128,
!     &44221,44170,43752,43976,43448,44238,43447,44095,43975,43977,44006,
!     &44239,9866,44153,43918,44007,13267/
!      DATA fondCR5  / 52.50D0, 52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,
!     &52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,
!     &52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,
!     &52.50D0,52.50D0,52.50D0 /
! seuil passe navigable
      INTEGER BOUCLECR6,NBATHYCR6
      PARAMETER (NBATHYCR6=55)
      INTEGER NUMEROCR6(NBATHYCR6)
      DOUBLE PRECISION FONDCR6(NBATHYCR6)
      DATA NUMEROCR6 / 44437, 44438, 44366, 44324, 44326, 43373, 44473,
     &43748, 44325, 43747, 44442, 44415, 44385, 44386, 44373, 39586,
     &44432,43713,44367,7277,44377,44393,44433,
     &44464,44302,43720,44388,43715,44459,43714,44322,44387,44389,
     &44458,44449,29578,44431,43721,44472,38636, 44358, 44384, 44289,
     &44290, 44423, 43485, 44372, 43840, 44291, 43792, 44402, 44371,
     &44330, 44331, 44346/
      DATA FONDCR6  / 66.40D0, 66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0, 66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0 /
! seuil passe est
      INTEGER BOUCLECR7,NBATHYCR7
      PARAMETER (NBATHYCR7=21)
      INTEGER NUMEROCR7(NBATHYCR7)
      DOUBLE PRECISION FONDCR7(NBATHYCR7)
      DATA NUMEROCR7 / 43910, 43890, 43504, 3801,43886,41708,43838,
     &43839,43887,10707, 43692,17328,43780,43691,43602,42060,43477,
     &13973, 43570, 43476, 43475 /
      DATA FONDCR7  / 66.40D0, 66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0/
! Fin CR
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
!
!-----------------------------------------------------------------------
!
!  MODIFICATION DU FOND DE CERTAINS POINTS DE LA FRONTIERE
!
!  LISSAGES EVENTUELS DU FOND
!
      IF(LISFON.GT.0) THEN
!
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!      IF(LNG.EQ.1) THEN
!        IF(LISFON.EQ.0) THEN
!          WRITE(LU,*)
!          WRITE(LU,*) 'CORFON (TELEMAC2D) : PAS DE MODIFICATION DU FOND'
!          WRITE(LU,*)
!        ELSE
!          WRITE(LU,*)
!          WRITE(LU,*) 'CORFON (TELEMAC2D) : ',LISFON,' LISSAGES DU FOND'
!          WRITE(LU,*)
!        ENDIF
!      ENDIF
!      IF(LNG.EQ.2) THEN
!        IF(LISFON.EQ.0) THEN
!          WRITE(LU,*)
!          WRITE(LU,*) 'CORFON (TELEMAC2D): NO MODIFICATION OF BOTTOM'
!          WRITE(LU,*)
!        ELSE
!          WRITE(LU,*)
!          WRITE(LU,*) 'CORFON (TELEMAC2D): ',LISFON,' BOTTOM SMOOTHINGS'
!          WRITE(LU,*)
!        ENDIF
!      ENDIF
!
!-----------------------------------------------------------------------
!
! Debut CR
!
!
      IF ((AT.GE.30000.D0) .AND. (AT.LE.31315.D0)) THEN
        DO BOUCLECR6=1,NBATHYCR6
          ZF%R(GLOBAL_TO_LOCAL_POINT(NUMEROCR6(BOUCLECR6),MESH))=
     &                                               0.01D0*AT-246.75D0
        ENDDO
      ELSEIF (AT.GT.31315.D0) THEN
        DO BOUCLECR6=1,NBATHYCR6
          ZF%R(GLOBAL_TO_LOCAL_POINT(NUMEROCR6(BOUCLECR6),MESH))=
     &                                               FONDCR6(BOUCLECR6)
        ENDDO
      ENDIF
!
      IF ((AT.GE.30000.D0) .AND. (AT.LE.31190.D0)) THEN
        DO BOUCLECR7=1,NBATHYCR7
          ZF%R(GLOBAL_TO_LOCAL_POINT(NUMEROCR7(BOUCLECR7),MESH))=
     &                                                0.01D0*AT-245.5D0
        ENDDO
      ELSEIF (AT.GT.31190.D0) THEN
        DO BOUCLECR7=1,NBATHYCR7
          ZF%R(GLOBAL_TO_LOCAL_POINT(NUMEROCR7(BOUCLECR7),MESH))=
     &                                               FONDCR7(BOUCLECR7)
        ENDDO
      ENDIF
!
! Fin CR
      RETURN
      END
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
!     PRINT * , 'I=',I,' Q=',Q
      RETURN
      END
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
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      DOUBLE PRECISION :: FLXCR
      COMMON/FLUX/FLXCR
      DOUBLE PRECISION Q1CR, Q2CR, Z1CR, Z2CR
      DOUBLE PRECISION CLIMIT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: I,N
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER*8 FCT
      INTEGER J
      LOGICAL DEJA,OK(MAXFRO)
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
!
!     FIRST CALL, OK INITIALISED TO .TRUE.
!
      IF(.NOT.DEJA) THEN
        DO J=1,MAXFRO
          OK(J)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
!
!-----------------------------------------------------------------------
!
!     IF FILE OF LIQUID BOUNDARIES EXISTING, ATTEMPT TO FIND
!     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
!                      IF  NO, OK SET     TO .FALSE.
!
      IF(OK(I).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
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
        CALL READ_FIC_FRLIQ(SL,FCT,AT,T2D_FILES(T2DIMP)%LU,ENTET,OK(I))
!
      ENDIF
!
      IF(.NOT.OK(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
!
!     PROGRAMMABLE PART
!     SL IS TAKEN IN THE PARAMETER FILE, BUT MAY BE CHANGED
!
        REWIND 26
        SL = COTE(I)
        IF (I.EQ.7) THEN
            IF (AT.LE.35400.D0) THEN
                SL = 0.0005D0*AT+41.3D0
            ELSE
                READ(26,200) Q2CR, Z2CR
                Q1CR = 3900.D0
                Z1CR = 58.50D0
                DO WHILE (Q2CR.LT.FLXCR)
                Q1CR = Q2CR
                Z1CR = Z2CR
                READ(26,200) Q2CR, Z2CR
                ENDDO
 200   FORMAT((F10.2,F5.2))
                SL = CLIMIT(Q1CR,Q2CR,Z1CR,Z2CR)
!                print *, Q1CR, Q2CR, Z1CR, Z2CR, SL
            ENDIF
        ENDIF
        IF (I.EQ.1) THEN
            IF (AT.LE.42860.D0) THEN
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
!                       ********************************
                        DOUBLE PRECISION FUNCTION CLIMIT
!                       ********************************
!
!
     &( Q1CR, Q2CR, X1, X2)
!
      IMPLICIT NONE
!
      DOUBLE PRECISION Q1CR, Q2CR, X1, X2
      DOUBLE PRECISION :: FLXCR
      COMMON/FLUX/FLXCR
!
!
!-----------------------------------------------------------------------
!
      CLIMIT = (X2 - X1)/(Q2CR - Q1CR) * ( FLXCR - Q1CR) + X1

!
!-----------------------------------------------------------------------
!
      RETURN
      END
