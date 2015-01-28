!                    *************************
                     SUBROUTINE NOMVAR_SISYPHE
!                    *************************
!
     &(TEXTE,TEXTPR,MNEMO,NSICLA,UNIT,MAXVAR,NPRIV,NOMBLAY)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    GIVES THE VARIABLE NAMES FOR THE RESULTS AND
!+                GEOMETRY FILES.
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/95
!+
!+
!
!history  M. GONZALES DE LINARES; C.VILLARET
!+        2003
!+
!+
!
!history  JMH
!+        03/11/2009
!+        V6P0
!+   MODIFIED AFTER JACEK JANKOWSKI DEVELOPMENTS
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  CV + JMH (LNHE)
!+        17/01/2012
!+        V6P2
!+   Adaptation to greater numbers of layers and classes (up to 99 each)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MAXVAR         |-->| MAXIMUM NUMBER OF OUTPUT VARIABLES
!| MNEMO          |<--| SYMBOLS TO SPECIFY THE VARIABLES FOR OUTPUT
!|                |   | IN THE STEERING FILE
!| NOMBLAY        |-->| NUMBER OF LAYERS
!| NPRIV          |-->| NUMBER OF PRIVATE ARRAYS
!| NSICLA         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| NSICLM         |-->| NUMBER OF SIZE CLASSES FOR BED MATERIALS
!| TEXTE          |<--| NAMES OF VARIABLES (PRINTOUT)
!| TEXTPR         |<--| NAMES OF VARIABLES (INPUT)
!| UNIT           |-->| LOGICAL, FILE NUMBER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE, ONLY : NSICLM,NLAYMAX
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)         :: NSICLA,MAXVAR,NPRIV,NOMBLAY
      CHARACTER*8, INTENT(INOUT)  :: MNEMO(MAXVAR)
      CHARACTER*32, INTENT(INOUT) :: TEXTE(MAXVAR),TEXTPR(MAXVAR)
      LOGICAL, INTENT(IN)         :: UNIT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K,ADD
!
!     LOCAL ARRAYS DIMENSIONED WITH FIXED PARAMETERS FROM
!     DECLARATIONS_SISYPHE. IT IS NOT A HIDDEN DYNAMIC ALLOCATION   
!
      CHARACTER(LEN=32) TEXTE_QS(NSICLM)
      CHARACTER(LEN=32) TEXTE_CS(NSICLM),TEXTE_QSC(NSICLM)
      CHARACTER(LEN=32) TEXTE_QSS(NSICLM),TEXTE_ES(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_QS(NSICLM)
      CHARACTER(LEN=8)  MNEMO_CS(NSICLM),MNEMO_ES(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_QSC(NSICLM),MNEMO_QSS(NSICLM)
      CHARACTER(LEN=2)  CLA,LAY
! V6P2
      CHARACTER(LEN=32) TEXTE_CONC(NLAYMAX)
      CHARACTER(LEN=8)  MNEMO_CONC(NLAYMAX)
!-----------------------------------------------------------------------
!
!      ADD=27+MAX(4,NPRIV)+NSICLA*(NOMBLAY+4)+NOMBLAY
!
      ADD=27+MAX(4,NPRIV)+NSICLA*(NOMBLAY+4)+2*NOMBLAY
!
      IF(ADD.GT.MAXVAR) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'NOMVAR_SISYPHE : MAXVAR DOIT VALOIR AU MOINS ',
     &                ADD
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'NOMVAR_SISYPHE: MAXVAR SHOULD BE AT LEAST ',
     &                ADD
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     2 3RD FRACTION MEANS FRACTION OF SEDIMENT OF CLASS 3 IN 2ND LAYER
!
      DO I=1,NSICLA
        DO J=1,NOMBLAY
          IF(J.LT.10) THEN
            WRITE(LAY,'(I1)') J
          ELSEIF(J.LT.100) THEN
            WRITE(LAY,'(I2)') J
          ELSE
            WRITE (LU,*) 'NOMVAR_SISYPHE: NOT IMPLEMENTED FOR ',NOMBLAY
            WRITE (LU,*) '                LAYERS'
            CALL PLANTE(1)
            STOP          
          ENDIF
          IF(I.LT.10) THEN
            WRITE(CLA,'(I1)') I
          ELSEIF(I.LT.100) THEN
            WRITE(CLA,'(I2)') I
          ELSE
            WRITE (LU,*) 'NOMVAR_SISYPHE: NOT IMPLEMENTED FOR ',NSICLA
            WRITE (LU,*) '                CLASSES'
            CALL PLANTE(1)
            STOP
          ENDIF
!         AVAIL: ALL LAYERS OF CLASS 1, THEN ALL LAYERS OF CLASS 2, ETC.
!         SAME ORDER AS IN POINT_SISYPHE
!         BEWARE, 21 IS DUE TO OTHER VARIABLES BEFORE, SEE BELOW
          TEXTE(21+(I-1)*NOMBLAY+J)=TRIM('FRACLAY '//LAY//' CL '//CLA)
          MNEMO(21+(I-1)*NOMBLAY+J)=TRIM(LAY)//'A'//CLA
        ENDDO
      ENDDO
!
      DO J=1,NSICLA
        IF(J.LT.10) THEN
          WRITE(CLA,'(I1)') J
        ELSEIF(J.LT.100) THEN
          WRITE(CLA,'(I2)') J
        ELSE
          WRITE (LU,*) 'NOMVAR_SISYPHE: NOT IMPLEMENTED FOR ',NSICLA
          WRITE (LU,*) '                CLASSES'
          CALL PLANTE(1)
          STOP        
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
!
      DO K=1,NOMBLAY
        IF(K.LT.10) THEN
          WRITE(LAY,'(I1)') K
          MNEMO_ES(K) = TRIM(LAY)//'ES     '
        ELSEIF(K.LT.100) THEN
          WRITE(LAY,'(I2)') K
          MNEMO_ES(K) = TRIM(LAY)//'ES    '
        ELSE
          WRITE (LU,*) 'NOMVAR_SISYPHE: NOT IMPLEMENTED FOR ',NOMBLAY
          WRITE (LU,*) '                LAYERS'
          CALL PLANTE(1)
          STOP            
        ENDIF   
        TEXTE_ES(K)(1:16)  = 'LAYER'//LAY//' THICKNESS'
        TEXTE_ES(K)(17:32) = 'M               '
      ENDDO
!V6P2
      DO K=1,NOMBLAY
        IF(K.LT.10) THEN
          WRITE(LAY,'(I1)') K
          MNEMO_CONC(K) = TRIM(LAY)//'CONC     '
        ELSEIF(K.LT.100) THEN
          WRITE(LAY,'(I2)') K
          MNEMO_CONC(K) = TRIM(LAY)//'CONC    '
        ELSE
          WRITE (LU,*) 'NOMVAR_SISYPHE: NOT IMPLEMENTED FOR ',NOMBLAY
          WRITE (LU,*) '                LAYERS'
          CALL PLANTE(1)
          STOP            
        ENDIF   
        TEXTE_CONC(K)(1:12)  = 'LAYER'//LAY//' CONC'
        TEXTE_CONC(K)(17:32) = 'KG/L            '
      ENDDO

!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.2) THEN
!
!       ENGLISH VERSION
!
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
        TEXTE(11) = 'BED SHEAR STRESSN/M2        '
        TEXTE(12) = 'WAVE HEIGHT HM0 M               '
        TEXTE(13) = 'PEAK PERIOD TPR5S               '
        TEXTE(14) = 'MEAN DIRECTION  DEG             '
        TEXTE(15) = 'SOLID DISCH     M2/S            '
        TEXTE(16) = 'SOLID DISCH X   M2/S            '
        TEXTE(17) = 'SOLID DISCH Y   M2/S            '
        TEXTE(18) = 'EVOLUTION       M               '
        TEXTE(19) = 'RUGOSITE TOTALE M               '
        TEXTE(20) = 'FROT. PEAU MU                   '
        TEXTE(21) = 'MEAN DIAMETER M                 '
!
!       THIS IS DONE ABOVE
!
!       DO J=1,NOMBLAY
!         DO I=1,NSICLA
!           TEXTE(21+(I-1)*NOMBLAY+J) = ...
!           MNEMO(21+(I-1)*NOMBLAY+J) = ...
!         ENDDO
!       ENDDO
!
        DO I=1,NSICLA
          TEXTE(21+I+NOMBLAY*NSICLA)     = TEXTE_QS(I)
          MNEMO(21+I+NOMBLAY*NSICLA)     = MNEMO_QS(I)
          TEXTE(21+I+(NOMBLAY+1)*NSICLA) = TEXTE_CS(I)
          MNEMO(21+I+(NOMBLAY+1)*NSICLA) = MNEMO_CS(I)
        ENDDO
!
        ADD=NSICLA*(NOMBLAY+2)
        TEXTE(22+ADD)='QS BEDLOAD      M2/S            '
        TEXTE(23+ADD)='QS BEDLOAD X    M2/S            '
        TEXTE(24+ADD)='QS BEDLOAD Y    M2/S            '
        TEXTE(25+ADD)='QS SUSPENSION   M2/S            '
        TEXTE(26+ADD)='QS SUSPENSION X M2/S            '
        TEXTE(27+ADD)='QS SUSPENSION Y M2/S            '
!
      ELSE
!
!       FRENCH VERSION
!
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
        TEXTE(19)  = 'RUGOSITE TOTALE.M               '
        TEXTE(20)  = 'CORR FROTT PEAU MU              '
        TEXTE(21)  = 'DIAMETRE MOYEN  M               '
!
!       THIS IS DONE ABOVE
!
!       DO J=1,NOMBLAY
!         DO I=1,NSICLA
!           TEXTE(21+(I-1)*NOMBLAY+J) = ...
!           MNEMO(21+(I-1)*NOMBLAY+J) = ...
!         ENDDO
!       ENDDO
!
        DO I=1,NSICLA
          TEXTE(21+I+NOMBLAY*NSICLA)     = TEXTE_QS(I)
          MNEMO(21+I+NOMBLAY*NSICLA)     = MNEMO_QS(I)
          TEXTE(21+I+(NOMBLAY+1)*NSICLA) = TEXTE_CS(I)
          MNEMO(21+I+(NOMBLAY+1)*NSICLA) = MNEMO_CS(I)
        ENDDO
!
        ADD=NSICLA*(NOMBLAY+2)
        TEXTE(22+ADD)='QS CHARRIAGE    M2/S            '
        TEXTE(23+ADD)='QS CHARRIAGE X  M2/S            '
        TEXTE(24+ADD)='QS CHARRIAGE Y  M2/S            '
        TEXTE(25+ADD)='QS SUSPENSION   M2/S            '
        TEXTE(26+ADD)='QS SUSPENSION X M2/S            '
        TEXTE(27+ADD)='QS SUSPENSION Y M2/S            '
!
      ENDIF
!
      DO I=1,NSICLA
        TEXTE(27+I+NSICLA*(NOMBLAY+2)) = TEXTE_QSC(I)
        MNEMO(27+I+NSICLA*(NOMBLAY+2)) = MNEMO_QSC(I)
        TEXTE(27+I+NSICLA*(NOMBLAY+3)) = TEXTE_QSS(I)
        MNEMO(27+I+NSICLA*(NOMBLAY+3)) = MNEMO_QSS(I)
      ENDDO
!
      DO I=1,NOMBLAY
        TEXTE(27+I+NSICLA*(NOMBLAY+4)) = TEXTE_ES(I)
        MNEMO(27+I+NSICLA*(NOMBLAY+4)) = MNEMO_ES(I)
      ENDDO
! V6P2
      DO I=1,NOMBLAY
        TEXTE(27+I+NSICLA*(NOMBLAY+4)+NOMBLAY) = TEXTE_CONC(I)
        MNEMO(27+I+NSICLA*(NOMBLAY+4)+NOMBLAY) = MNEMO_CONC(I)
      ENDDO
!
!     ADD=NSICLA*(NOMBLAY+4)+NOMBLAY
      ADD=NSICLA*(NOMBLAY+4)+2*NOMBLAY
! ... V6P2
!
      TEXTE(28+ADD)='FOND ANALYTIQUE M               '
      TEXTE(29+ADD)='PRIVE 2                         '
      TEXTE(30+ADD)='PRIVE 3                         '
      TEXTE(31+ADD)='PRIVE 4                         '
!     NPRIV MAY BE GREATER THAN 4
!     TEXTE(31+ADD)='PRIVE 5                         '
!
      DO I=1,31+NSICLA*(NOMBLAY+4)+2*NOMBLAY
        TEXTPR(I)=TEXTE(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
!     OTHER NAMES FOR OUTPUT VARIABLES (STEERING FILE)
!
!     VELOCITY U
      MNEMO(1)   = 'U       '
!     VELOCITY V
      MNEMO(2)   = 'V       '
!     WATER DEPTH
      MNEMO(3)   = 'H       '
!     FREE SURFACE
      MNEMO(4)   = 'S       '
!     BOTTOM
      MNEMO(5)   = 'B       '
!     SCALAR FLOW RATE
      MNEMO(6)   = 'Q       '
!     SCALAR FLOW RATE X
      MNEMO(7)   = 'I       '
!     SCALAR FLOW RATE Y
      MNEMO(8)   = 'J       '
!     RIGID BED
      MNEMO(9)   = 'R       '
!     FRICTION COEFFICIENT
      MNEMO(10)   = 'CHESTR  '
!     MEAN BOTTOM FRICTION
      MNEMO(11)   = 'TOB     '
!     WAVE HEIGHT
      MNEMO(12)   = 'W       '
!     PEAK PERIOD
      MNEMO(13)   = 'X       '
!     WAVE DIRECTION
      MNEMO(14)   = 'THETAW  '
!     SOLID DISCHARGE
      MNEMO(15)   = 'M       '
!     SOLID DISCHARGE X
      MNEMO(16)   = 'N       '
!     SOLID DISCHARGE Y
      MNEMO(17)   = 'P       '
!     EVOLUTION
      MNEMO(18)   = 'E       '
!     KS
      MNEMO(19)   = 'KS      '
!     MU
      MNEMO(20)   = 'MU      '
!     D50
      MNEMO(21)   = 'D50     '
!
      MNEMO(22+NSICLA*(NOMBLAY+2)) = 'QSBL    '
      MNEMO(23+NSICLA*(NOMBLAY+2)) = 'QSBLX   '
      MNEMO(24+NSICLA*(NOMBLAY+2)) = 'QSBLY   '
      MNEMO(25+NSICLA*(NOMBLAY+2)) = 'QSSUSP  '
      MNEMO(26+NSICLA*(NOMBLAY+2)) = 'QSSUSPX '
      MNEMO(27+NSICLA*(NOMBLAY+2)) = 'QSSUSPY '
!CV
!V6P2      ADD=NSICLA*(NOMBLAY+4)+NOMBLAY
      ADD=NSICLA*(NOMBLAY+4)+2*NOMBLAY
      MNEMO(28+ADD) = 'A       '
      MNEMO(29+ADD) = 'G       '
      MNEMO(30+ADD) = 'L       '
      MNEMO(31+ADD) = 'O       '
!     THE NUMBER OF PRIVATE ARRAYS IS A KEYWORD
!     MNEMO(31+ADD) = '????????'
!
!----------------------------
!CV V6P2 
!      ADD=NSICLA*(NOMBLAY+4)+NOMBLAY+27+MAX(NPRIV,4)
      ADD=NSICLA*(NOMBLAY+4)+2*NOMBLAY+27+MAX(NPRIV,4)
      IF(ADD.LT.MAXVAR) THEN
        DO I=ADD+1,MAXVAR
          MNEMO(I) =' '
          TEXTE(I) =' '
          TEXTPR(I)=' '
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                       *************************
                        SUBROUTINE CONDIM_SISYPHE
!                       *************************
!
     & (U      , V   , QU    , QV  , H   , ZF , Z ,
     &  ESOMT  , THETAWR  , Q     , HWR  , TWR  ,
     &  X      , Y   , NPOIN , AT  , PMAREE)
!
!***********************************************************************
! SISYPHE VERSION 5.3                             E. PELTIER    11/09/95
!                                                 C. LENORMANT
!                                                 J.-M. HERVOUET
!                                                
! COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT      
!***********************************************************************
!
!     FONCTION  : VALEURS IMPOSEES
!                         - DU DEBIT VECTORIEL    QU, QV
!                         - DE LA HAUTEUR D'EAU   H
!                         - DE LA COTE DU FOND    ZF
!                         - DE LA SURFACE LIBRE   Z
!                         - DE L'EVOLUTION TOTALE ESOMT
!                         - DU DEBIT              Q
!                         - DE LA HAUTEUR DE HOULE HW
!                         - DE LA PERIODE DE HOULE TW
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   U , V        |<-- | COORDONNEES DES VECTEURS VITESSE
! |   QU , QV      |<-- | DEBIT VECTORIEL SUIVANT X ET SUIVANT Y
! |   H            |<-->| HAUTEUR D'EAU
! |   ZF           |<-->| COTE DU FOND
! |   Z            |<-->| COTE DE SURFACE LIBRE
! |   ESOMT        |<-->| EVOLUTION TOTALE DES FONDS
! |   C            |<-->| CELERITE
! |   Q            |<-->| DEBIT
! |   HW           | -->| HAUTEUR DE HOULE
! |   TW           | -->| PERIODE DE HOULE
! |   X,Y          | -->| COORDONNEES DU MAILLAGE
! |   NPOIN        | -->| NOMBRE DE POINTS DU MAILLAGE
! |   AT           | -->| TEMPS
! |   PMAREE       | -->| PERIODE DE LA MAREE
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! PROGRAMME APPELANT : SISYPH
! PROGRAMMES APPELES : 
!***********************************************************************
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)::NPOIN
!
      DOUBLE PRECISION, INTENT(IN):: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN):: AT , PMAREE
! SEDIMENT
      DOUBLE PRECISION, INTENT(INOUT) ::  ZF(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT)::  ESOMT(NPOIN)
! HYDRODYNAMICS
      DOUBLE PRECISION, INTENT(INOUT):: Z(NPOIN) , H(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT):: U(NPOIN) , V(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT)::QU(NPOIN), QV(NPOIN), Q(NPOIN)
! WAVES
      DOUBLE PRECISION, INTENT (INOUT):: HWR(NPOIN) , TWR(NPOIN)
      DOUBLE PRECISION, INTENT (INOUT):: THETAWR(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION   PI
!
!-----------------------------------------------------------------------
!
!
!  --------------------------------------------------------------
!  INITIALISATION DES TABLEAUX NON LUS DANS LE FICHIER RESULTATS:
!  --------------------------------------------------------------
!
      DO I=1,NPOIN
        QU(I)=.25D0
        Q(I)=.25D0
        QV(I)=0.D0
        Z(I)=.6D0
        ZF(I)=0.D0
      ENDDO    
      PI=3.1415926D0
      DO I=1,NPOIN
        IF (X(I) .GE. 2.D0 .AND. X(I) .LE. 10.D0) THEN
          ZF(I)=0.1D0*SIN(PI*(X(I)-2.D0)/8.D0)**2    
        ENDIF                                       
        H(I)=Z(I)-ZF(I) 
! 25/01/2007
! Definition du U2D, V2D 
!    
        U(I)=QU(I)/H(I)
        V(I)=QV(I)/H(I)                            
      ENDDO 
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CONDIM_SISYPHE
!                       **************************
                        SUBROUTINE CARACTERISTIQUE
!                       **************************
!
     &(X,Y,NPOIN,HFINAL,TEMPS)
!
!----------------------------------------------------------------
!
      IMPLICIT NONE
      INTEGER, PARAMETER :: NN = 1600
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION   X(NPOIN),Y(NPOIN),HFINAL(NPOIN)
      DOUBLE PRECISION   ZF0(NN), H0(NN)
      DOUBLE PRECISION   DIST,DIST1,DIST2
      DOUBLE PRECISION   XFICTIF(NN)
      DOUBLE PRECISION   XNEW(NN)
      INTEGER            I,J,II,COMPTEUR,QUOT,RESTE,KK
      DOUBLE PRECISION   GRAV,D,S,STRICKLER,CFROT,DEBIT,PI,PAS
      DOUBLE PRECISION   K1,K2,K,MOYENNE_H,TEMPS,DX
!
      DX=0.01D0
      PI= 4.D0*ATAN(1.D0)
!       
      DO II=1,NN 
        XFICTIF(II) = (II-1)*DX
      ENDDO
      DO II=1,NN 
        H0(II)=0.D0 
        ZF0(II)=0.D0   
        IF(XFICTIF(II).GE. 2.D0 .AND.
     &     XFICTIF(II).LE.10.D0) THEN                        
          ZF0(II)=0.1D0*SIN(PI*(XFICTIF(II)-2.D0)/8.D0)**2 
        ENDIF                                             
        H0(II)=0.6D0-ZF0(II)  
      ENDDO
      DO II=1,NN
        XNEW(II)=0.D0 
        IF(H0(II).GE.1.D0) H0(II)=0
      ENDDO
!
! INITIALISATION DES VARIABLES
!----------------------------------------------------------------
!
      DO I=1,NPOIN
        HFINAL(I)=0.D0
      ENDDO
!      
!  CALCUL DE LA HAUTEUR D'EAU MOYENNE
!----------------------------------------------------------------
!
      MOYENNE_H = 0.D0
      DO I=1,NN
        MOYENNE_H = MOYENNE_H + H0(I)
      ENDDO
      MOYENNE_H = MOYENNE_H / NN
!
!  PARAMETRES ET CONSTANTES
!----------------------------------------------------------------
!
      GRAV = 9.81D0
      D = 0.000150D0
      S = 2.65D0
      STRICKLER = 50.D0
      CFROT = 2.D0*GRAV/(STRICKLER**2*MOYENNE_H**(1.D0/3.D0))
      DEBIT = 0.25D0
      K1 = SQRT(GRAV*(S-1)*D**3)
      K2 = CFROT/(2*GRAV*(S-1)*D)
! XKV= 1.6;  N=1-1/XKV=0.375
      K=1.6D0*0.5D0*K1*K2**(5.D0/2.D0)*DEBIT**5/CFROT
!
!  CREATION DE LA SOLUTION PAR METHODE DES CARACTERISTIQUES
!----------------------------------------------------------------
!     
      DO I=1,NN
        XNEW(I) = XFICTIF(I) + K*TEMPS/H0(I)**6
      ENDDO
!
!  INTERPOLATION AVEC L'ANCIEN AXE DES ABSCISSES
!----------------------------------------------------------------
!
      COMPTEUR=0
      DO I=1,NPOIN
        COMPTEUR=0
        DO J=1,NN-1
          DIST =XNEW(J+1)-XNEW(J)
          DIST1=XNEW(J+1)-X(I)
          DIST2=X(I)-XNEW(J)
          IF(DIST1.GE.0 .AND. DIST2.GE.0 .AND.COMPTEUR.EQ.0) THEN
            HFINAL(I)=0.6D0-(DIST1*H0(J+1)+DIST2*H0(J))/DIST
            COMPTEUR=COMPTEUR+1
          ENDIF
          IF(COMPTEUR.EQ.0) HFINAL(I)=0.D0
        ENDDO
      ENDDO
!
!----------------------------------------------------------------
!     
      RETURN     
      END
!                       *****************
                        SUBROUTINE PREDES
!                       *****************
!
     &(LLT,AAT)
!
!***********************************************************************
! SISYPHE VERSION 6.0                             E. PELTIER    11/09/95
!                                                 C. LENORMANT
!                                                 J.-M. HERVOUET
! 
!
! JMH 07/12/2009: KS SET TO 0 IF LLT=0
!                                               
! COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT   
!***********************************************************************
!
!     FONCTION  : PREPARATION DE VARIABLES QUI SERONT ECRITES SUR
!                 LE FICHIER DE RESULTATS OU SUR LE LISTING.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |      LLT       |--> | LOCAL LT (MAY BE LT-1+PERCOU) 
! |      AAT       |--> | CURRENT TIME (FOR BUILDING SOLUTIONS)
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
!     - PROGRAMME APPELANT : SISYPH  
!     - SOUS-PROGRAMMES APPELES : OVD,OV
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: LLT
      DOUBLE PRECISION, INTENT(IN) :: AAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     
      DOUBLE PRECISION BID 
      INTEGER LTT,IN      
      LOGICAL IMP,LEO
!
!-----------------------------------------------------------------------
!
!     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
!     FOLLOWING TESTS, WHICH MUST BE THE SAME THAN IN DESIMP (BIEF LIBRARY)
!
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LLT/LISPR)*LISPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIL) IMP=.TRUE.
      LTT=(LLT/LEOPR)*LEOPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIG) LEO=.TRUE.
!
!     PAS D'IMPRESSION, PAS DE SORTIE SUR FICHIER, ON RESSORT
      IF (.NOT.(LEO.OR.IMP)) GO TO 1000
!
!=======================================================================
! ECRITURE DES VARIABLES CALCULEES
!=======================================================================
!
!     VITESSE U:    U=QU/H
!
      IF ((LEO.AND.SORLEO(1)).OR.(IMP.AND.SORIMP(1))) THEN
        CALL OS( 'X=Y/Z   ' , T1 , QU , HN , 0.D0 , 2 , 0.D0 , HMIN )
      ENDIF
!
!     VITESSE V:    V=QV/H
!
      IF ((LEO.AND.SORLEO(2)).OR.(IMP.AND.SORIMP(2))) THEN
        CALL OS( 'X=Y/Z   ' , T2 , QV , HN , 0.D0 , 2 , 0.D0 , HMIN )
      ENDIF
!
!     CELERITE C:   (GRAV*H)**0.5
!
      IF ((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
        CALL CPSTVC(HN,T3)
        DO IN=1, NPOIN
          T3%R(IN)= SQRT (GRAV*HN%R(IN))
        ENDDO
      ENDIF
!
!     SURFACE LIBRE Z: H+ZF
!
      IF ((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
        CALL OS('X=Y+Z   ',Z,HN,ZF, BID )
      ENDIF
!
!     FROUDE F: ((QU**2+QV**2)/(GRAV*H**3))**0.5
!
      IF ((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        CALL OS( 'X=Y/Z   ' , T1 , QU , HN , 0.D0 , 2 , 0.D0 , HMIN )
        CALL OS( 'X=Y/Z   ' , T2 , QV , HN , 0.D0 , 2 , 0.D0 , HMIN ) 
        CALL CPSTVC(QU,T4) 
        DO IN=1,NPOIN
          T4%R(IN)= T1%R(IN)**2+T2%R(IN)**2
        ENDDO       
        CALL OS( 'X=Y/Z   ' , T4 , T4 , HN , 0.D0 , 2 , 0.D0 , HMIN )  
        DO IN=1,NPOIN
          T4%R(IN)=SQRT(T4%R(IN)/GRAV)        
        ENDDO
      ENDIF
!
!=======================================================================
!
!     VARIABLES WHICH ARE NOT INITIALISED AT THE FIRST CALL OF PREDES
!
      IF(LLT.EQ.0) THEN
!       JMH ON 27/11/2009
        IF((LEO.AND.SORLEO(19)).OR.(IMP.AND.SORIMP(19))) THEN
          CALL OS('X=0     ',X=KS)
        ENDIF
      ENDIF
!
!=======================================================================
!
!  SOLUTION ANALYTIQUE POUR LE FOND (PREMIER TABLEAU PRIVE)
!
!  CV    IF((LEO.AND.SORLEO(27+(NOMBLAY+4)*NSICLA+NOMBLAY)).OR.
!  CV  *   (IMP.AND.SORIMP(27+(NOMBLAY+4)*NSICLA+NOMBLAY))) THEN
      IF(LEO.OR.IMP) THEN
        CALL CARACTERISTIQUE(MESH%X%R,MESH%Y%R,NPOIN,
     &                                    PRIVE%ADR(1)%P%R,AAT)
      ENDIF
!
!=======================================================================
!
1000  CONTINUE
!
!=======================================================================
!
      RETURN
      END 

