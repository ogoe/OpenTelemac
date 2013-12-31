C                       *****************
                        SUBROUTINE CORFON
C                       *****************
C
C***********************************************************************
C TELEMAC 2D VERSION 5.1          01/03/90    J-M HERVOUET
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
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
      INTEGER IM,JM,I,J,I_L,J_L
      DOUBLE PRECISION EIKON
C
C-----------------------------------------------------------------------
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
      IM = 47
      JM = 10
C                                                                         
C  VARIANTE FOND EN PENTE RECTILIGNE + CUVETTE                            
C                                                                         
      DO I=1,IM 
      DO J=1,JM 
C       PENTE RECTILIGNE                                                       
        I_L = GLOBAL_TO_LOCAL_POINT(I,MESH)
        J_L = GLOBAL_TO_LOCAL_POINT(J,MESH)
        ZF%R(I_L+(J_L-1)*IM)=-0.6D0+0.46D0*FLOAT(I_L-1)/FLOAT(IM-1) 
C       BOSSE GAUSSIENNE            
        IF(I_L.GT.9.AND.I_L.LT.29) THEN
          EIKON = -(I_L-19)**2/20.D0 
          ZF%R(I_L+(J_L-1)*IM) = ZF%R(I_L+(J_L-1)*IM) + 0.1D0*EXP(EIKON)
        ENDIF                                                 
      ENDDO                                                            
      ENDDO    
C
C-----------------------------------------------------------------------
C
      RETURN
      END                  
C                       ***************************
                        DOUBLE PRECISION FUNCTION Q
C                       ***************************
C
     *( I )
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.1    17/08/94    J-M HERVOUET (LNH) 30 87 80 18
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
C                                                                                                                                             
C       Q = DEBIT(I)
        Q = -5.D0*HN%R(236)/0.6D0
C 
      ENDIF          
C
C-----------------------------------------------------------------------
C
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
      DOUBLE PRECISION PI,OMEGA,PERIODE,A
      DATA PI/3.141592653589D0/
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
        SL = COTE(I)
C       DESCENTE DE LA COTE 0 A -0.55 EN 300 S
        PERIODE=600.D0
        OMEGA=2*PI/PERIODE
        A=0.55D0/2.D0
        SL=A*(COS(OMEGA*AT)-1.D0)
C 
      ENDIF           
C
C-----------------------------------------------------------------------
C
      RETURN
      END
 
