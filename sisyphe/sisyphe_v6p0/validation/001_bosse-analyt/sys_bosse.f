
C                       *************************
                        SUBROUTINE NOMVAR_SISYPHE
C                       *************************
C
     *( TEXTE ,TEXTPR , MNEMO , NSICLA , UNIT )
C
C***********************************************************************
C SISYPHE VERSION 6.0                             E. PELTIER    11/09/95
C                                                 C. LENORMANT
C                                                 J.-M. HERVOUET
C                                                 M. GONZALES DE LINARES
C                                                 (2003) 
C                                                 C.VILLARET (2003)
C
C NOTE JMH (03/11/2009) : MODIFIED AFTER JACEK JANKOWSKI DEVELOPMENTS
C                         FOR RESTARTS WITH GRADED SEDIMENTS
C                          
C COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT   
C***********************************************************************
C
C FONCTION  :  FIXE LES NOMS DES VARIABLES DU CODE POUR LES FICHIERS
C              DE RESULTAT ET DE GEOMETRIE.
C
C FUNCTION  :  GIVES THE NAMES OF VARIABLES FOR THE OUTPUT AND GEOMETRY 
C              FILES
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |   TEXTE        |<-- | NAMES OF VARIABLES (PRINTOUT)
C |   TEXTPR       |<-- | NAMES OF VARIABLES (INPUT)
C |   MNEMO        |<-- | SYMBOLS TO SPECIFY THE VARIABLES FOR OUTPUT 
C |                |    | IN THE STEERING FILE 
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C CALLED BY SUBROUTINE : PREDAT
C**********************************************************************
C
      USE DECLARATIONS_SISYPHE, ONLY : MAXVAR,NSICLM,NLAYMAX,NOMBLAY,
     *                                 NPRIV
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)         :: NSICLA
      CHARACTER*8, INTENT(INOUT)  :: MNEMO(MAXVAR)
      CHARACTER*32, INTENT(INOUT) :: TEXTE(MAXVAR),TEXTPR(MAXVAR)
      LOGICAL, INTENT(IN)         :: UNIT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,J,K,ADD
C
      CHARACTER(LEN=32) TEXTE_AVAI(NLAYMAX*NSICLM),TEXTE_QS(NSICLM)
      CHARACTER(LEN=32) TEXTE_CS(NSICLM),TEXTE_QSC(NSICLM)
      CHARACTER(LEN=32) TEXTE_QSS(NSICLM),TEXTE_ES(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_AVAI(NLAYMAX*NSICLM),MNEMO_QS(NSICLM)
      CHARACTER(LEN=8)  MNEMO_CS(NSICLM),MNEMO_ES(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_QSC(NSICLM),MNEMO_QSS(NSICLM)
      CHARACTER(LEN=2)  CLA
      CHARACTER(LEN=1)  LAY
C
C-----------------------------------------------------------------------
C
      ADD=26+MAX(4,NPRIV)+NSICLA*(NOMBLAY+4)+NOMBLAY
      IF(ADD.GT.MAXVAR) THEN
        IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'NOMVAR_SISYPHE : MAXVAR DOIT VALOIR AU MOINS ',ADD
        ENDIF
        IF(LNG.EQ.2) THEN
         WRITE(LU,*) 'NOMVAR_SISYPHE: MAXVAR SHOULD BE AT LEAST ',ADD
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF 
C
C-----------------------------------------------------------------------
C     2 3RD FRACTION MEANS FRACTION OF SEDIMENT OF CLASS 3 IN 2ND LAYER
C
      IF(NOMBLAY.GT.9.OR.NSICLA.GT.99) THEN
        WRITE (LU,*) 'REPROGRAM NOMVAR_SISYPHE DUE TO CONSTANT FORMATS' 
        CALL PLANTE(1)
        STOP 
      ENDIF
C
      DO I=1,NSICLA
        DO J=1,NOMBLAY  
          K=(I-1)*NOMBLAY+J
          WRITE(LAY,'(I1)') J
          IF(I.LT.10) THEN 
            WRITE(CLA,'(I1)') I
          ELSE
            WRITE(CLA,'(I2)') I
          ENDIF
          TEXTE_AVAI(K) = TRIM('FRAC LAY '//LAY//' CL '//CLA)
          MNEMO_AVAI(K) = TRIM(LAY//'A'//CLA)
        ENDDO
      ENDDO 
C
      DO J=1,NSICLA
        IF(J<10) THEN 
          WRITE(CLA,'(I1)') J
        ELSE
          WRITE(CLA,'(I2)') J
        ENDIF
        TEXTE_QS(J)  = TRIM('QS CLASS '//CLA)
        TEXTE_QSC(J) = TRIM('QS BEDLOAD CL'//CLA)
        TEXTE_QSS(J) = TRIM('QS SUSP. CL'//CLA)
        IF(UNIT) THEN
          TEXTE_CS(J) = TRIM('CONC MAS CL'//CLA)
          TEXTE_CS(J)(17:19) = 'G/L'
        ELSE
          TEXTE_CS(J) = TRIM('CONC VOL CL'//CLA)
        ENDIF
        MNEMO_QS(J)  = TRIM('QS'//CLA)
        MNEMO_QSC(J) = TRIM('QSBL'//CLA) 
        MNEMO_QSS(J) = TRIM('QSS'//CLA) 
        MNEMO_CS(J)  = TRIM('CS'//CLA)        
      ENDDO
C
      DO K=1,NOMBLAY
        WRITE(LAY,'(I1)') K
CV        TEXTE_ES(K)(1:16)  = 'LAY. '//LAY//' THICKNESS'
        TEXTE_ES(K)(1:16)  = 'LAYER'//LAY//' THICKNESS'
        TEXTE_ES(K)(17:32) = 'M               '
        MNEMO_ES(K) = LAY//'ES     ' 
      ENDDO 
C
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.2) THEN
C
C       ENGLISH VERSION
C
        TEXTE(01) = 'VELOCITY U      M/S             '
        TEXTE(02) = 'VELOCITY V      M/S             '
        TEXTE(03) = 'WATER DEPTH     M               '
        TEXTE(04) = 'FREE SURFACE    M               '
        TEXTE(05) = 'BOTTOM          M               '
        TEXTE(06) = 'FLOWRATE Q      M3/S/M          '
        TEXTE(07) = 'FLOWRATE QX     M3/S/M          '
        TEXTE(08) = 'FLOWRATE QY     M3/S/M          '
        TEXTE(09) = 'RIGID BED       M               '
        TEXTE(10) = 'FRICTION COEFT                  '
        TEXTE(11) = 'BOTTOM FRICTION N/M2            '
        TEXTE(12) = 'WAVE HEIGHT HM0 M               '
        TEXTE(13) = 'PEAK PERIOD TPR5S               '
        TEXTE(14) = 'MEAN DIRECTION  DEG             '      
        TEXTE(15) = 'SOLID DISCH     M2/S            '
        TEXTE(16) = 'SOLID DISCH X   M2/S            '
        TEXTE(17) = 'SOLID DISCH Y   M2/S            '
        TEXTE(18) = 'EVOLUTION       M               '
        TEXTE(19) = 'RUGOSITE TOTALE M               '
        TEXTE(20) = 'FROT. PEAU MU                   '
C
        TEXTE(21+NSICLA*(NOMBLAY+2))='QS BEDLOAD      M2/S            '
        TEXTE(22+NSICLA*(NOMBLAY+2))='QS BEDLOAD X    M2/S            '
        TEXTE(23+NSICLA*(NOMBLAY+2))='QS BEDLOAD Y    M2/S            '
        TEXTE(24+NSICLA*(NOMBLAY+2))='QS SUSPENSION   M2/S            '
        TEXTE(25+NSICLA*(NOMBLAY+2))='QS SUSPENSION X M2/S            '
        TEXTE(26+NSICLA*(NOMBLAY+2))='QS SUSPENSION Y M2/S            '
C
      ELSE
C
C       FRENCH VERSION
C
        TEXTE(01)  = 'VITESSE U       M/S             '
        TEXTE(02)  = 'VITESSE V       M/S             '
        TEXTE(03)  = 'HAUTEUR D''EAU   M              '
        TEXTE(04)  = 'SURFACE LIBRE   M               '
        TEXTE(05)  = 'FOND            M               '
        TEXTE(06)  = 'DEBIT           M3/S/M          '
        TEXTE(07)  = 'DEBIT QX        M3/S/M          '
        TEXTE(08)  = 'DEBIT QY        M3/S/M          '
        TEXTE(09)  = 'FOND RIGIDE     M               '
        TEXTE(10)  = 'COEFT FROTTEMENT                '
        TEXTE(11)  = 'FROTTEMENT TOB  N/M2            '
        TEXTE(12)  = 'HAUTEUR HM0     M               '
        TEXTE(13)  = 'PERIODE PIC TPR5S               '
        TEXTE(14)  = 'DIRECTION MOY   DEG             '
        TEXTE(15)  = 'DEBIT SOLIDE    M2/S            '
        TEXTE(16)  = 'DEBIT SOLIDE X  M2/S            '
        TEXTE(17)  = 'DEBIT SOLIDE Y  M2/S            '
        TEXTE(18)  = 'EVOLUTION       M               '
        TEXTE(19)  = 'TOTAL BED ROUGH.M               '
        TEXTE(20)  = 'SKIN FRICTION MU                '
C
        ADD=NSICLA*(NOMBLAY+2)
        TEXTE(21+ADD)='QS CHARRIAGE    M2/S            '
        TEXTE(22+ADD)='QS CHARRIAGE X  M2/S            '
        TEXTE(23+ADD)='QS CHARRIAGE Y  M2/S            '
        TEXTE(24+ADD)='QS SUSPENSION   M2/S            '
        TEXTE(25+ADD)='QS SUSPENSION X M2/S            '
        TEXTE(26+ADD)='QS SUSPENSION Y M2/S            '
C       
      ENDIF
C
C     AVAIL: ALL LAYERS OF CLASS 1, THEN ALL LAYERS OF CLASS 2, ETC.
C            SAME ORDER THAT IN POINT_SISYPHE
C
      DO J=1,NOMBLAY
        DO I=1,NSICLA
          TEXTE(20+(I-1)*NOMBLAY+J) = TEXTE_AVAI((I-1)*NOMBLAY+J)
          MNEMO(20+(I-1)*NOMBLAY+J) = MNEMO_AVAI((I-1)*NOMBLAY+J)
        ENDDO
      ENDDO
C
      DO I=1,NSICLA
        TEXTE(20+I+NOMBLAY*NSICLA)     = TEXTE_QS(I)
        MNEMO(20+I+NOMBLAY*NSICLA)     = MNEMO_QS(I)
        TEXTE(20+I+(NOMBLAY+1)*NSICLA) = TEXTE_CS(I)
        MNEMO(20+I+(NOMBLAY+1)*NSICLA) = MNEMO_CS(I)
        TEXTE(26+I+NSICLA*(NOMBLAY+2)) = TEXTE_QSC(I)
        MNEMO(26+I+NSICLA*(NOMBLAY+2)) = MNEMO_QSC(I)
        TEXTE(26+I+NSICLA*(NOMBLAY+3)) = TEXTE_QSS(I)
        MNEMO(26+I+NSICLA*(NOMBLAY+3)) = MNEMO_QSS(I)
      ENDDO
C
      DO I=1,NOMBLAY
        TEXTE(26+I+NSICLA*(NOMBLAY+4)) = TEXTE_ES(I)
        MNEMO(26+I+NSICLA*(NOMBLAY+4)) = MNEMO_ES(I)
      ENDDO
C
C rajouter nomvar
C        TEXTE(27+NSICLA*6)='FOND ANALYTIQUE M               ' 
      ADD=NSICLA*(NOMBLAY+4)+NOMBLAY
C      TEXTE(27+ADD)='PRIVE 1                         '
      TEXTE(27+ADD)='FOND ANALYTIQUE M               ' 
      TEXTE(28+ADD)='PRIVE 2                         '
      TEXTE(29+ADD)='PRIVE 3                         '
      TEXTE(30+ADD)='PRIVE 4                         '
C     NPRIV MAY BE GREATER THAN 4
C     TEXTE(31+ADD)='PRIVE 5                         '
C
      DO I=1,30+NSICLA*(NOMBLAY+4)+NOMBLAY
        TEXTPR(I)=TEXTE(I)
      ENDDO
C
C-----------------------------------------------------------------------
C
C     OTHER NAMES OF PRINTOUT VARIABLES (STEERING FILE)
C
C     VELOCITY U
      MNEMO(1)   = 'U       '
C     VELOCITY V
      MNEMO(2)   = 'V       '
C     WATER DEPTH
      MNEMO(3)   = 'H       '
C     FREE SURFACE 
      MNEMO(4)   = 'S       '
C     BOTTOM
      MNEMO(5)   = 'B       '
C     SCALAR FLOW RATE
      MNEMO(6)   = 'Q       '
C     SCALAR FLOW RATE X
      MNEMO(7)   = 'I       '
C     SCALAR FLOW RATE Y
      MNEMO(8)   = 'J       '
C     RIGID BED
      MNEMO(9)   = 'R       '
C     FRICTION COEFFICIENT
      MNEMO(10)   = 'CHESTR  '
C     MEAN BOTTOM FRICTION
      MNEMO(11)   = 'TOB     '
C     WAVE HEIGHT
      MNEMO(12)   = 'W       '
C     PEAK PERIOD
      MNEMO(13)   = 'X       '
C     WAVE DIRECTION
      MNEMO(14)   = 'THETAW  '
C     SOLID DISCHARGE  
      MNEMO(15)   = 'M       '
C     SOLID DISCHARGE X
      MNEMO(16)   = 'N       '
C     SOLID DISCHARGE Y
      MNEMO(17)   = 'P       '
C     EVOLUTION
      MNEMO(18)   = 'E       '
C     KS
      MNEMO(19)   = 'KS      '
C     MU
      MNEMO(20)   = 'MU      '
C
      MNEMO(21+NSICLA*(NOMBLAY+2)) = 'QSBL    '
      MNEMO(22+NSICLA*(NOMBLAY+2)) = 'QSBLX   '
      MNEMO(23+NSICLA*(NOMBLAY+2)) = 'QSBLY   '
      MNEMO(24+NSICLA*(NOMBLAY+2)) = 'QSSUSP  '
      MNEMO(25+NSICLA*(NOMBLAY+2)) = 'QSSUSPX '
      MNEMO(26+NSICLA*(NOMBLAY+2)) = 'QSSUSPY '
C
      ADD=NSICLA*(NOMBLAY+4)+NOMBLAY
      MNEMO(27+ADD) = 'A       '
      MNEMO(28+ADD) = 'G       '
      MNEMO(29+ADD) = 'L       '
      MNEMO(30+ADD) = 'O       '
C     THE NUMBER OF PRIVATE ARRAYS IS A KEY-WORD
C     MNEMO(31+ADD) = '????????'
C
C----------------------------
C
      ADD=NSICLA*(NOMBLAY+4)+NOMBLAY+26+MAX(NPRIV,4)
      IF(ADD.LT.MAXVAR) THEN
        DO I=ADD+1,MAXVAR
          MNEMO(I) =' '
          TEXTE(I) =' '
          TEXTPR(I)=' '
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C                       *************************
                        SUBROUTINE CONDIM_SISYPHE
C                       *************************
C
     * (U      , V   , QU    , QV  , H   , ZF , Z ,
     *  ESOMT  , THETAW   , Q     , HW  , TW  ,
     *  X      , Y   , NPOIN , AT  , PMAREE)
C
C***********************************************************************
C SISYPHE VERSION 5.3                             E. PELTIER    11/09/95
C                                                 C. LENORMANT
C                                                 J.-M. HERVOUET
C                                                
C COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT      
C***********************************************************************
C
C     FONCTION  : VALEURS IMPOSEES
C                         - DU DEBIT VECTORIEL    QU, QV
C                         - DE LA HAUTEUR D'EAU   H
C                         - DE LA COTE DU FOND    ZF
C                         - DE LA SURFACE LIBRE   Z
C                         - DE L'EVOLUTION TOTALE ESOMT
C                         - DU DEBIT              Q
C                         - DE LA HAUTEUR DE HOULE HW
C                         - DE LA PERIODE DE HOULE TW
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |   U , V        |<-- | COORDONNEES DES VECTEURS VITESSE
C |   QU , QV      |<-- | DEBIT VECTORIEL SUIVANT X ET SUIVANT Y
C |   H            |<-->| HAUTEUR D'EAU
C |   ZF           |<-->| COTE DU FOND
C |   Z            |<-->| COTE DE SURFACE LIBRE
C |   ESOMT        |<-->| EVOLUTION TOTALE DES FONDS
C |   C            |<-->| CELERITE
C |   Q            |<-->| DEBIT
C |   HW           | -->| HAUTEUR DE HOULE
C |   TW           | -->| PERIODE DE HOULE
C |   X,Y          | -->| COORDONNEES DU MAILLAGE
C |   NPOIN        | -->| NOMBRE DE POINTS DU MAILLAGE
C |   AT           | -->| TEMPS
C |   PMAREE       | -->| PERIODE DE LA MAREE
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C PROGRAMME APPELANT : SISYPH
C PROGRAMMES APPELES : 
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPOIN , I
C
      DOUBLE PRECISION   U(NPOIN) , V(NPOIN)     , H(NPOIN)
      DOUBLE PRECISION   QU(NPOIN), QV(NPOIN)    , Q(NPOIN)
      DOUBLE PRECISION   ESOMT(NPOIN)
      DOUBLE PRECISION   HW(NPOIN), TW(NPOIN), THETAW(NPOIN)
      DOUBLE PRECISION   Z(NPOIN) , ZF(NPOIN)
      DOUBLE PRECISION   X(NPOIN) , Y(NPOIN)
      DOUBLE PRECISION   AT       , PMAREE 
C
      DOUBLE PRECISION   PI
C-----------------------------------------------------------------------
C
C  --------------------------------------------------------------
C  INITIALISATION DES TABLEAUX NON LUS DANS LE FICHIER RESULTATS:
C  --------------------------------------------------------------
C
      DO 1 I=1,NPOIN                                                            
         QU(I)=.25D0                                                             
         Q(I)=.25D0                                                                                                                      
         QV(I)=0.D0                                                             
         Z(I)=.6D0                                                              
         ZF(I)=0.D0                                                             
1     CONTINUE                                                                  
      PI=3.1415926D0                                                               
      DO 2 I=1,NPOIN                                                            
         IF (X(I) .GE. 2.D0 .AND. X(I) .LE. 10.D0) THEN                        
           ZF(I)=0.1D0*SIN(PI*(X(I)-2.D0)/8.D0)**2                                                                           
         ENDIF                                                                 
         H(I)=Z(I)-ZF(I) 
C 25/01/2007
C Definition du U2D, V2D 
C    
         U(I)=QU(I)/H(I)
         V(I)=QV(I)/H(I)                                                       
2     CONTINUE 
C
C-----------------------------------------------------------------------
C
      RETURN
      END SUBROUTINE CONDIM_SISYPHE
C                       **************************
                        SUBROUTINE CARACTERISTIQUE
C                       **************************
C
     *(X,Y,NPOIN,HFINAL,TEMPS)
C
C----------------------------------------------------------------
C
      INTEGER, PARAMETER :: NN = 1600
      DOUBLE PRECISION   X(NPOIN),Y(NPOIN),HFINAL(NPOIN)
      DOUBLE PRECISION   ZF0(NN), H0(NN)
      DOUBLE PRECISION   DIST,DIST1,DIST2
      DOUBLE PRECISION   XFICTIF(NN)
      DOUBLE PRECISION   XNEW(NN)
      INTEGER            NPOIN,I,II,compteur,QUOT,RESTE,KK
      DOUBLE PRECISION   GRAV,D,S,STRICKLER,CFROT,DEBIT,pi,PAS
      DOUBLE PRECISION   K1,K2,K,MOYENNE_H,TEMPS,DX
C
      DX=0.01D0
      PI= 4.D0*ATAN(1.D0)
C       
      DO II=1,NN 
        XFICTIF(II) = (II-1)*DX
      ENDDO
      DO II=1,NN 
        H0(II)=0.D0 
        ZF0(II)=0.D0                                                         
        IF(XFICTIF(II).GE. 2.D0 .AND.
     *     XFICTIF(II).LE.10.D0) THEN                        
          ZF0(II)=0.1D0*SIN(PI*(XFICTIF(II)-2.D0)/8.D0)**2                                                                           
        ENDIF                                                                 
        H0(II)=0.6D0-ZF0(II)  
      ENDDO
      DO II=1,NN
        XNEW(II)=0.D0 
        IF(H0(II).GE.1.D0) H0(II)=0
      ENDDO
C
C INITIALISATION DES VARIABLES
C----------------------------------------------------------------
C
      DO I=1,NPOIN
         HFINAL(I)=0.D0
      ENDDO
C      
C  CALCUL DE LA HAUTEUR D'EAU MOYENNE
C----------------------------------------------------------------
C
      MOYENNE_H = 0.D0
      DO I=1,NN
         MOYENNE_H = MOYENNE_H + H0(I)
      ENDDO
      MOYENNE_H = MOYENNE_H / NN
C
C  PARAMETRES ET CONSTANTES
C----------------------------------------------------------------
C
      GRAV = 9.81D0
      D = 0.000150D0
      S = 2.65D0
      STRICKLER = 50.D0
      CFROT = 2.D0*GRAV/(STRICKLER**2*MOYENNE_H**(1.D0/3.D0))
      DEBIT = 0.25D0
      K1 = SQRT(GRAV*(S-1)*D**3)
      K2 = CFROT/(2*GRAV*(S-1)*D)
C
      K=1.6D0*0.5D0*K1*K2**(5.D0/2.D0)*Debit**5/CFROT
C
C  CREATION DE LA SOLUTION PAR METHODE DES CARACTERISTIQUES
C----------------------------------------------------------------
C     
      DO I=1,NN
        XNEW(I) = XFICTIF(I) + K*TEMPS/H0(I)**6
      ENDDO
C
C  INTERPOLATION AVEC L'ANCIEN AXE DES ABSCISSES
C----------------------------------------------------------------
C
      COMPTEUR=0
      DO I=1,NPOIN
        COMPTEUR=0
        DO J=1,NN-1
          DIST =XNEW(J+1)-XNEW(J)
          DIST1=XNEW(J+1)-X(I)
          DIST2=X(I)-XNEW(J)
          IF(DIST1.GE.0 .AND. DIST2.GE.0 .AND.compteur.EQ.0) THEN
            HFINAL(I)=0.6D0-(DIST1*H0(J+1)+DIST2*H0(J))/DIST
            COMPTEUR=COMPTEUR+1
          ENDIF
          IF(COMPTEUR.EQ.0) HFINAL(I)=0.D0
        ENDDO
      ENDDO
C
C----------------------------------------------------------------
C     
      RETURN     
      END
C                       *****************
                        SUBROUTINE PREDES
C                       *****************
C
     *(LLT,AAT)
C
C***********************************************************************
C SISYPHE VERSION 6.0                             E. PELTIER    11/09/95
C                                                 C. LENORMANT
C                                                 J.-M. HERVOUET
C 
C
C JMH 07/12/2009: KS SET TO 0 IF LLT=0
C                                               
C COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT   
C***********************************************************************
C
C     FONCTION  : PREPARATION DE VARIABLES QUI SERONT ECRITES SUR
C                 LE FICHIER DE RESULTATS OU SUR LE LISTING.
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |      LLT       |--> | LOCAL LT (MAY BE LT-1+PERCOU) 
C |      AAT       |--> | CURRENT TIME (FOR BUILDING SOLUTIONS)
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C     - PROGRAMME APPELANT : SISYPH  
C     - SOUS-PROGRAMMES APPELES : OVD,OV
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_SISYPHE
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)          :: LLT
      DOUBLE PRECISION, INTENT(IN) :: AAT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C     
      DOUBLE PRECISION BID 
      INTEGER LTT,IN      
      LOGICAL IMP,LEO
C
C-----------------------------------------------------------------------
C
C     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
C     FOLLOWING TESTS, WHICH MUST BE THE SAME THAN IN DESIMP (BIEF LIBRARY)
C
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LLT/LISPR)*LISPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIL) IMP=.TRUE.
      LTT=(LLT/LEOPR)*LEOPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIG) LEO=.TRUE.
C
C     PAS D'IMPRESSION, PAS DE SORTIE SUR FICHIER, ON RESSORT
      IF (.NOT.(LEO.OR.IMP)) GO TO 1000
C
C=======================================================================
C ECRITURE DES VARIABLES CALCULEES
C=======================================================================
C
C     VITESSE U:    U=QU/H
C
      IF ((LEO.AND.SORLEO(1)).OR.(IMP.AND.SORIMP(1))) THEN
       CALL OS( 'X=Y/Z   ' , T1 , QU , HN , 0.D0 , 2 , 0.D0 , HMIN )
      ENDIF
C
C     VITESSE V:    V=QV/H
C
      IF ((LEO.AND.SORLEO(2)).OR.(IMP.AND.SORIMP(2))) THEN
       CALL OS( 'X=Y/Z   ' , T2 , QV , HN , 0.D0 , 2 , 0.D0 , HMIN )         
      ENDIF
C
C     CELERITE C:   (GRAV*H)**0.5
C
      IF ((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
         CALL CPSTVC(HN,T3)
         DO 50 IN=1, NPOIN
            T3%R(IN)= SQRT (GRAV*HN%R(IN))
50       CONTINUE
      ENDIF
C
C     SURFACE LIBRE Z: H+ZF
C
      IF ((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
         CALL OS('X=Y+Z   ',Z,HN,ZF, BID )
      ENDIF
C
C     FROUDE F: ((QU**2+QV**2)/(GRAV*H**3))**0.5
C
      IF ((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        CALL OS( 'X=Y/Z   ' , T1 , QU , HN , 0.D0 , 2 , 0.D0 , HMIN )
        CALL OS( 'X=Y/Z   ' , T2 , QV , HN , 0.D0 , 2 , 0.D0 , HMIN ) 
        CALL CPSTVC(QU,T4) 
        DO 10 IN=1,NPOIN
           T4%R(IN)= T1%R(IN)**2+T2%R(IN)**2
10      CONTINUE       
        CALL OS( 'X=Y/Z   ' , T4 , T4 , HN , 0.D0 , 2 , 0.D0 , HMIN )  
        DO 110 IN=1,NPOIN
          T4%R(IN)=SQRT(T4%R(IN)/GRAV)        
110     CONTINUE
      ENDIF
C
C=======================================================================
C
C     VARIABLES WHICH ARE NOT INITIALISED AT THE FIRST CALL OF PREDES
C
      IF(LLT.EQ.0) THEN
C       JMH ON 27/11/2009
        IF((LEO.AND.SORLEO(19)).OR.(IMP.AND.SORIMP(19))) THEN
          CALL OS('X=0     ',X=KS)
        ENDIF
      ENDIF
C
C=======================================================================
C
C  SOLUTION ANALYTIQUE POUR LE FOND (PREMIER TABLEAU PRIVE)
C
      IF((LEO.AND.SORLEO(27+(NOMBLAY+4)*NSICLA+NOMBLAY)).OR.
     *   (IMP.AND.SORIMP(27+(NOMBLAY+4)*NSICLA+NOMBLAY))) THEN
        CALL CARACTERISTIQUE(MESH%X%R,MESH%Y%R,NPOIN,
     *                                             PRIVE%ADR(1)%P%R,AAT)
      ENDIF
C
C=======================================================================
C
1000  CONTINUE
C
C=======================================================================
C
      RETURN
      END 

