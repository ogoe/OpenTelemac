C                       *****************
                        SUBROUTINE CORFON
C                       *****************
C
C***********************************************************************
C PROGICIEL : TELEMAC-2D 5.0          01/03/90    J-M HERVOUET
C***********************************************************************
C
C  USER SUBROUTINE CORFON
C
C  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
C
C
C-----------------------------------------------------------------------
C  ARGUMENTS USED IN THE EXAMPLE 
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |      ZF        |<-->| FOND A MODIFIER.
C |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
C |      A         |<-- | MATRICE
C |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
C |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
C |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
C |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
C |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT :
C PROGRAMMES APPELES : RIEN EN STANDARD
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU,IPOIN, I
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C Debut CR
C seuil passe usiniere
C      INTEGER boucleCR5,nbathyCR5
C      PARAMETER (nbathyCR5=25)
C      INTEGER numeroCR5(nbathyCR5)
C      DOUBLE PRECISION fondCR5(nbathyCR5)
C      DATA numeroCR5 / 41161,44127,43449,44059,41060,44195,44126,44128,
C     &44221,44170,43752,43976,43448,44238,43447,44095,43975,43977,44006,
C     &44239,9866,44153,43918,44007,13267/
C      DATA fondCR5  / 52.50D0, 52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,
C     &52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,
C     &52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,52.50D0,
C     &52.50D0,52.50D0,52.50D0 /     
C seuil passe navigable
      INTEGER boucleCR6,nbathyCR6
      PARAMETER (nbathyCR6=55)
      INTEGER numeroCR6(nbathyCR6)
      DOUBLE PRECISION fondCR6(nbathyCR6)
      DATA numeroCR6 / 44437, 44438, 44366, 44324, 44326, 43373, 44473,
     &43748, 44325, 43747, 44442, 44415, 44385, 44386, 44373, 39586,
     &44432,43713,44367,7277,44377,44393,44433,
     &44464,44302,43720,44388,43715,44459,43714,44322,44387,44389,
     &44458,44449,29578,44431,43721,44472,38636, 44358, 44384, 44289, 
     &44290, 44423, 43485, 44372, 43840, 44291, 43792, 44402, 44371, 
     &44330, 44331, 44346/
      DATA fondCR6  / 66.40D0, 66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0, 66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0 /     
C seuil passe est
      INTEGER boucleCR7,nbathyCR7
      PARAMETER (nbathyCR7=21)
      INTEGER numeroCR7(nbathyCR7)
      DOUBLE PRECISION fondCR7(nbathyCR7)
      DATA numeroCR7 / 43910, 43890, 43504, 3801,43886,41708,43838,
     &43839,43887,10707, 43692,17328,43780,43691,43602,42060,43477,
     &13973, 43570, 43476, 43475 /
      DATA fondCR7  / 66.40D0, 66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,
     &66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0,66.40D0/    
C Fin CR
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
C
C-----------------------------------------------------------------------
C
C  MODIFICATION DU FOND DE CERTAINS POINTS DE LA FRONTIERE
C         
C  LISSAGES EVENTUELS DU FOND
C
      IF(LISFON.GT.0) THEN
C
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     *              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C      IF(LNG.EQ.1) THEN
C        IF(LISFON.EQ.0) THEN
C          WRITE(LU,*)
C          WRITE(LU,*) 'CORFON (TELEMAC2D) : PAS DE MODIFICATION DU FOND'
C          WRITE(LU,*)
C        ELSE
C          WRITE(LU,*)
C          WRITE(LU,*) 'CORFON (TELEMAC2D) : ',LISFON,' LISSAGES DU FOND'
C          WRITE(LU,*)
C        ENDIF
C      ENDIF
C      IF(LNG.EQ.2) THEN
C        IF(LISFON.EQ.0) THEN
C          WRITE(LU,*)
C          WRITE(LU,*) 'CORFON (TELEMAC2D): NO MODIFICATION OF BOTTOM'
C          WRITE(LU,*)
C        ELSE
C          WRITE(LU,*)
C          WRITE(LU,*) 'CORFON (TELEMAC2D): ',LISFON,' BOTTOM SMOOTHINGS'
C          WRITE(LU,*)
C        ENDIF
C      ENDIF
C
C-----------------------------------------------------------------------
C
C Debut CR
C
C
      IF ((AT.GE.30000.D0) .AND. (AT.LE.31315.D0)) THEN
      DO 115 boucleCR6=1,nbathyCR6
         ZF%R(MESH%KNOGL%I(numeroCR6(boucleCR6)))=0.01D0*AT-246.75D0    
 115  CONTINUE
      ELSEIF (AT.GT.31315.D0) THEN
      DO 116 boucleCR6=1,nbathyCR6
         ZF%R(MESH%KNOGL%I(numeroCR6(boucleCR6)))=fondCR6(boucleCR6)
 116  CONTINUE
      ENDIF
C
      IF ((AT.GE.30000.D0) .AND. (AT.LE.31190.D0)) THEN
      DO 117 boucleCR7=1,nbathyCR7
         ZF%R(MESH%KNOGL%I(numeroCR7(boucleCR7)))=0.01D0*AT-245.5D0
 117  CONTINUE
      ELSEIF (AT.GT.31190.D0) THEN
      DO 118 boucleCR7=1,nbathyCR7
         ZF%R(MESH%KNOGL%I(numeroCR7(boucleCR7)))=fondCR7(boucleCR7)
 118  CONTINUE
      ENDIF
C   
C Fin CR
      RETURN
      END      
C                       ***************************
                        DOUBLE PRECISION FUNCTION Q
C                       ***************************
C
     *( I )
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.0    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
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
          STOP 'Q NOT PROGRAMMED FOR MORE THAN 99 BOUNDARIES'
        ENDIF
        CALL READ_FIC_FRLIQ(Q,FCT,AT,T2D_FILES(T2DIMP)%LU,ENTET,OK(I)) 
C
      ENDIF
C
      IF(.NOT.OK(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
C 
C     PROGRAMMABLE PART                              
C     Q IS TAKEN IN THE PARAMETER FILE, BUT MAY BE CHANGED 
C                                                                                                                                            
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
C 
C-----------------------------------------------------------------------
C
C     PRINT * , 'I=',I,' Q=',Q
      RETURN
      END
C                       ****************************
                        DOUBLE PRECISION FUNCTION SL
C                       ****************************
C
C
     *( I , N )
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.1    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
C
C***********************************************************************
C
C FONCTION  : DONNE LA VALEUR DE LA COTE DE LA SURFACE LIBRE POUR TOUTES
C             LES ENTREES A COTE IMPOSEE.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |   I            | -->| RANG DE LA FRONTIERE A COTE IMPOSEE
C |                |    | (1 S'IL N'Y EN A QU'UNE)
C |   N            | -->| NUMERO GLOBAL DU POINT
C |________________|____|_______________________________________________
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
      DOUBLE PRECISION :: FLXCR
      COMMON/FLUX/FLXCR
      DOUBLE PRECISION Q1CR, Q2CR, Z1CR, Z2CR
      DOUBLE PRECISION CLIMIT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: I,N
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER*8 FCT
      INTEGER J
      LOGICAL DEJA,OK(MAXFRO)
      DATA    DEJA /.FALSE./
      SAVE    OK,DEJA
C
C     FIRST CALL, OK INITIALISED TO .TRUE.
C
      IF(.NOT.DEJA) THEN
        DO J=1,MAXFRO
          OK(J)=.TRUE.
        ENDDO
        DEJA=.TRUE.
      ENDIF
C
C-----------------------------------------------------------------------
C
C     IF FILE OF LIQUID BOUNDARIES EXISTING, ATTEMPT TO FIND
C     THE VALUE IN IT. IF YES, OK REMAINS TO .TRUE. FOR NEXT CALLS
C                      IF  NO, OK SET     TO .FALSE.
C
      IF(OK(I).AND.T2D_FILES(T2DIMP)%NAME(1:1).NE.' ') THEN
C
C       FCT WILL BE SL(1), SL(2), ETC, SL(99), DEPENDING ON I
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
C
      ENDIF
C
      IF(.NOT.OK(I).OR.T2D_FILES(T2DIMP)%NAME(1:1).EQ.' ') THEN
C 
C     PROGRAMMABLE PART                              
C     SL IS TAKEN IN THE PARAMETER FILE, BUT MAY BE CHANGED 
C                                                                             
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
C                print *, Q1CR, Q2CR, Z1CR, Z2CR, SL
            ENDIF
        ENDIF
        IF (I.EQ.1) THEN            
            IF (AT.LE.42860.D0) THEN
                SL = 0.0005D0*AT+16.7D0
            ELSE 
            	SL = COTE(I)
            ENDIF
        ENDIF
C 
      ENDIF           
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C                       ********************************
                        DOUBLE PRECISION FUNCTION CLIMIT
C                       ********************************
C
C
     *( Q1CR, Q2CR, X1, X2)
C
      IMPLICIT NONE
C
      DOUBLE PRECISION Q1CR, Q2CR, X1, X2
      DOUBLE PRECISION :: FLXCR
      COMMON/FLUX/FLXCR
C
C
C-----------------------------------------------------------------------
C
      CLIMIT = (X2 - X1)/(Q2CR - Q1CR) * ( FLXCR - Q1CR) + X1

C
C-----------------------------------------------------------------------
C
      RETURN
      END
