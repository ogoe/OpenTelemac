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
      DOUBLE PRECISION RAMPE1,RAMPE2
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
      RAMPE1 =  1800.D0                                                   
      RAMPE2 =  800.D0                                                    
C                                                                         
C TEST  POUR INITIALISER LE DEBIT PARTIE DE MONTEE DE CRUE                
C                                                                         
C                                                                         
      Q = 1950.D0 * MIN(1.D0,AT/RAMPE1) +1.D0                           
C                                                                         
C TEST  POUR INITIALISER LE DEBIT PARTIE STATIONNAIRE                     
C                                                                         
      IF(AT.GT.1800.D0.AND.AT.LT.3000.D0) THEN                             
        Q = 1950.D0                                                   
      ENDIF                                                               
C                                                                         
C TEST POUR INITIALISER LE DEBIT PARTIE DE DECRUE                         
C                                                                         
      IF(AT.GE.3000.D0.AND.AT.LT.3800.D0) THEN                             
        Q = 1950.D0*(1.D0 - MIN(1.D0,(AT-3000.D0)/RAMPE2))             
      ENDIF                                                                                                                                                               
        Q = DEBIT(I)                                                            
C 
      ENDIF          
C
C-----------------------------------------------------------------------
C
      RETURN
      END      
C                       *****************
                        SUBROUTINE CONDIN
C                       *****************
C
C***********************************************************************
C TELEMAC 2D VERSION 5.1         19/08/98  J-M HERVOUET TEL: 30 87 80 18
C
C***********************************************************************
C
C     FONCTION  : INITIALISATION DES GRANDEURS PHYSIQUES H, U, V ETC
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |                | -- |  
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
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
      INTEGER K,ITRAC
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C  
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DU TEMPS
C
      AT = 0.D0
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DES VITESSES : VITESSES NULLES
C
      CALL OS( 'X=C     ' , U , U , U , 0.D0 )
      CALL OS( 'X=C     ' , V , V , V , 0.D0 )
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DE H , LA HAUTEUR D'EAU
C
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     *   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0 )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0 )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     *       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H  , H , COTINI )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0   )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     *       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  )
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     *       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , HAUTIN )
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     *       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     *       CDTINI(1:07).EQ.'SPECIAL') THEN
C  ZONE A MODIFIER                                                      
        DO K = 1,NPOIN                                                
          H%R(K) = 0.D0                                                     
          IF(ZF%R(K).LE.5.D0) THEN                                       
            H%R(K) = 5.D0 - ZF%R(K)                                          
          ENDIF                                                           
        ENDDO                                          
C  FIN DE LA ZONE A MODIFIER      
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIN: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C   INITIALISATION DU TRACEUR
C
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL OS( 'X=C     ' , X=T%ADR(ITRAC)%P , C=TRAC0(ITRAC) )
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
C INITIALISATION DE LA VISCOSITE
C
      CALL OS( 'X=C     ' , VISC , VISC , VISC , PROPNU )
C
C-----------------------------------------------------------------------
C
      RETURN
      END           
