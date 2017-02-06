C                       *****************
                        SUBROUTINE LIMWAC
C                       *****************
     *(F     , FBOR  , LIFBOR, NPTFR , NPLAN , NF    ,  TETA , FREQ  ,
     * NPOIN2, NBOR  , AT    , LT    , DDC   , LIMSPE, FPMAXL, FETCHL,
     * SIGMAL, SIGMBL, GAMMAL, FPICL , HM0L  , APHILL, TETA1L, SPRE1L,
     * TETA2L, SPRE2L, XLAMDL, X ,Y  , KENT  , KSORT , NFO1  , NBI1  ,
     * BINBI1, UV    , VV    , SPEULI, VENT  , GRAVIT, DEUPI , 
     * PRIVE , NPRIV , SPEC  , FRA   , DEPTH , FRABL )
C
C***********************************************************************
C TOMAWAC   V1.0            01/02/95        F. MARCOS  (LNH) 30 87 72 66
C***********************************************************************
C
C      FONCTION:
C      =========
C
C    CONDITIONS AUX LIMITES
C
C    ATTENTION
C    PAR DEFAUT, ON DUPLIQUE SUR L'ENSEMBLE DES DIRECTIONS ET DES
C    FREQUENCES LA CONDITION A LA LIMITE DONNEE DANS LE FICHIER DYNAM
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !    F           ! -->!  DENSITE SPECTRALE                           !
C !    FBOR        !<-->!  DENSITE SPECTRALE AU BORD                   !
C !    LIFBOR      ! -->!  TYPE DE CONDITION LIMITE SUR F              !
C !    NPTFR       ! -->!  NOMBRE DE POINTS FRONTIERE 2D               !
C !    NPLAN       ! -->!  NOMBRE DE DIRECTIONS                        !
C !    NF          ! -->!  NOMBRE DE FREQUENCES                        !
C !    TETA        ! -->! DIRECTIONS DE PROPAGATION                    !
C !    FREQ        ! -->! FREQUENCES DISCRETISEES                      !
C !    NPOIN2      ! -->!  NOMBRE DE POINTS 2D                         !
C !    NBOR        ! -->!  NUMEROTATION DES POINTS DE BORD 2D          !
C !    AT          ! -->!  TEMPS                                       !
C !    LT          ! -->!  NUMERO DU PAS DE TEMPS                      !
C !    DDC         ! -->!  DATE DU DEBUT DU CALCUL                     !
C !    X           ! -->!  ABSCISSES DES POINTS 2D                     !
C !    Y           ! -->!  ORDONNEES DES POINTS 2D                     !
C !    KENT        ! -->!  C.L. INDIQUANT UNE FRONTIERE MARITIME       !
C !    KSORT       ! -->!  C.L. INDIQUANT UNE FRONTIERE SOLIDE         !
C !    NFO1        ! -->!  NUMERO DU FICHIER FORMATE UTILISATEUR       !
C !    NBI1        ! -->!  NUMERO DU FICHIER BINAIRE UTILISATEUR       !
C !    BINBI1      ! -->!  BINAIRE DU FICHIER BINAIRE UTILISATEUR      !
C !    PRIVE       ! -->!  TABLEAU DE L'UTILISATEUR                    !
C !    NPRIV       ! -->!  DIMENSION DU TABLEAU PRIVE                  !
C !________________!____!______________________________________________!
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C SOUS-PROGRAMME APPELE PAR : WAC
C
C***********************************************************************
C
      IMPLICIT NONE
C
!      INTEGER LNG,LU
!      COMMON/INFO/ LNG,LU
C
      INTEGER NPCL
      PARAMETER (NPCL=21)
C
      INTEGER NPLAN,NF,NPOIN2,NPTFR,LT,NPRIV
C
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF),X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION FBOR(NPTFR,NPLAN,NF),TETA(NPLAN),FREQ(NF)
      DOUBLE PRECISION UV(NPOIN2),VV(NPOIN2), SPEC(NF), FRA(NPLAN)
      DOUBLE PRECISION PRIVE(NPOIN2,NPRIV),DDC,DEPTH(NPOIN2),FRABL
      DOUBLE PRECISION HM0L,FPICL,GAMMAL,SIGMAL,SIGMBL,APHILL,FETCHL
      DOUBLE PRECISION FPMAXL,TETA1L,SPRE1L,TETA2L,SPRE2L,XLAMDL
      DOUBLE PRECISION GRAVIT,DEUPI
      DOUBLE PRECISION C,ATT,COEF,FCL1,FCL2
C
      DOUBLE PRECISION AT
C
      LOGICAL SPEULI, VENT
C
      INTEGER NBOR(NPTFR),LIFBOR(NPTFR),NFO1,NBI1,IP,I
      INTEGER KENT,KSORT,IFF,IPLAN,IPTFR,LIMSPE
      INTEGER IFRM,IFRP,NBP,NBM,NENR,NPCLI,NPB(NPCL)
C
      CHARACTER*3 BINBI1
C
      DOUBLE PRECISION AT1,AT2,COEF2
      DOUBLE PRECISION CL1(12,25,NPCL),CL2(12,25,NPCL)
C
C
      SAVE AT1,AT2,CL1,CL2,NPB,NENR,NPCLI
C
C***********************************************************************
C
C   MODIFICATION EVENTUELLE DU TYPE DE CONDITION A LA LIMITE
C
C   A REMPLIR PAR L'UTILISATEUR
C
C   LIFBOR(IPTFR)=KENT OU KSORT
C
      IF(LIMSPE.EQ.0 .AND. .NOT.SPEULI) RETURN
C
C     AU PREMIER PASSAGE, ON CALCULE LE SPECTRE AUX LIMITES
C     ----------------------------------------------------------------
      IF (LT.EQ.0) THEN
         REWIND NFO1
         READ(NFO1,1000) NPCLI
!         PRINT*,'NPCLI=',NPCLI
         IF (NPCLI.NE.0) THEN
         READ(NFO1,2000) (NPB(I),I=1,NPCLI)
         READ(NFO1,3000) AT1
         ATT=AT1
         CALL TEMP(AT1,ATT,DDC)
         IF (AT1.GT.AT) THEN
             PRINT*,'ERREUR DEMARAGE LECTURE',AT1,AT
             CALL PLANTE(0)
          ENDIF
!         PRINT*,'AT1 :',AT1
50       CONTINUE
         DO 40 IP=1,NPCLI
           READ(NFO1,4000) (CL1(I,1,IP),I=1,300)
40       CONTINUE
         READ(NFO1,3000) AT2
         ATT=AT2
         CALL TEMP(AT2,ATT,DDC)
         IF (AT2.LT.AT) THEN
            AT1=AT2
            GOTO 50
         ENDIF
         DO 60 IP=1,NPCLI
           READ(NFO1,4000) (CL2(I,1,IP),I=1,300)
60       CONTINUE
!         PRINT*,'AT2 :',AT2
         NENR=2
         ENDIF
      ELSE
         IF (NPCLI.NE.0) THEN
            IF (AT.GT.AT2) THEN
            AT1=AT2
            CALL OV('X=Y     ', CL1 , CL2 , CL1 , C , 300*NPCLI)
            PRINT*,'NOUVEL ENREGISTREMENT LIMWAC'
C
C	    CALL LIT(AT2,Z,I,CAR,1,'I ',NBI1,BINBI1,ISTAT)
C	    CALL LIT(X,Z,NPB,CAR,NPCL,'I ',NBI1,BINBI1,ISTAT)
C	    NPB(21)=520
C	    DO 99 I=1,NENR
C	    CALL LIT(AT2,Z,I,CAR,1,'R4',NBI1,BINBI1,ISTAT)
C	 DO 98 IP=1,NPCLI
C	 CALL LIT(CL2(1,1,IP),BID,I,CAR,300,'R4',NBI1,BINBI1,ISTAT)
98       CONTINUE
99          CONTINUE
C
C	    CALL LIT(AT2,Z,I,CAR,1,'R4',NBI1,BINBI1,ISTAT)
            READ(NFO1,3000) AT2
!            print*,'AT',AT2
            NENR=NENR+1
            ATT=AT2
            CALL TEMP(AT2,ATT,DDC)
            IF (AT2.LT.AT) THEN
               PRINT*,'LIMWAC : ON SAUTE 2 ENREGISTREMENT',AT,AT2
               CALL PLANTE(0)
            ENDIF
 !           PRINT*,'AT1 :',AT1
 !           PRINT*,'AT2 :',AT2
 !           PRINT*,'NENR :',NENR,NPCLI
            DO 70 IP=1,NPCLI
C	CALL LIT(CL2(1,1,IP),BID,I,CAR,300,'R4',NBI1,BINBI1,ISTAT)
           READ(NFO1,4000) (CL2(I,1,IP),I=1,300)
70          CONTINUE
         ENDIF
      ENDIF
C
      COEF=(AT-AT1)/(AT2-AT1)
      DO 5 IP=1,NPCLI-1
        IFRP=NPCLI-IP+1
        IFRM=NPCLI-IP
        NBP=NPB(IFRP)
        NBM=NPB(IFRM)
        IF (NBP.GT.NBM) NBM=NPTFR
        DO 10 IPTFR=NBP,NBM
          DO 20 IPLAN=1,NPLAN
            DO 30 IFF=1,NF
              IF (LIFBOR(IPTFR).EQ.KENT) THEN
                COEF2=REAL(IPTFR-NBM)/REAL(NBP-NBM)
                FCL1=CL1(IPLAN,IFF,IFRM)+COEF2*
     *              (CL1(IPLAN,IFF,IFRP)-CL1(IPLAN,IFF,IFRM))
                FCL2=CL2(IPLAN,IFF,IFRM)+COEF2*
     *              (CL2(IPLAN,IFF,IFRP)-CL2(IPLAN,IFF,IFRM))
                F(NBOR(IPTFR),IPLAN,IFF)=FCL1+(FCL2-FCL1)*COEF
              ENDIF
30          CONTINUE
20        CONTINUE
10      CONTINUE
5     CONTINUE
      ENDIF
1000  FORMAT(I5)
2000  FORMAT(21I5)
3000  FORMAT(f9.1)
4000  FORMAT(5G16.7)
C
      RETURN
      END
C
C                       ***************
                        SUBROUTINE TEMP
C                       ***************
C
     *(TV ,DAT,DDC)
C
C***********************************************************************
C  TOMAWAC VERSION 1.0    01/02/95        F.MARCOS     (LNH) 30 87 72 66
C***********************************************************************
C
C   FONCTION : CE SOUS-PROGRAMME CALCULE LE TEMPS EN SECONDE
C              ENTRE LES DATES DAT ET DDC
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C !      NOM       !MODE!                   ROLE                       !
C !________________!____!______________________________________________!
C !    TV          !<-- !  ECART DE TEMPS EN SECONDES                  !
C !    DAT         ! -->!  DATE D'UN ENREGISTREMENT DES VENTS          !
C !    DDC         ! -->!  DATE DU DEBUT DU CALCUL                     !
C !________________!____!______________________________________________!
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR : LECVEN
C
C SOUS-PROGRAMME APPELE : AUCUN
C
C***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER ADC,MDC,JDC,HDC,MNDC,ADT,MDT,JDT,HDT,MNDT
      INTEGER NJDM(12)
      DOUBLE PRECISION TV,DDC,DAT
C
C-----------------------------------------------------------------------
C
      DATA NJDM /0,31,59,90,120,151,181,212,243,273,304,334/
C       ON NE TRAITE PAS LES ANNEES BISSEXTILES !!
C
C  DECODAGE DE LA DATE DU DEBUT DU CALCUL
C
      ADC=INT(DDC*1.D-8)
      MDC=INT(DDC*1.D-6)
      JDC=INT(DDC*1.D-4)
      HDC=INT(DDC*1.D-2)
      MNDC=INT(DDC-100.D0*HDC)
      HDC =HDC-100*JDC
      JDC =JDC-100*MDC
      MDC =MDC-100*ADC
C
C  DECODAGE DE LA DATE DE L'ENREGISTREMENT DU VENT
C
      ADT=INT(DAT*1.D-8)
      MDT=INT(DAT*1.D-6)
      JDT=INT(DAT*1.D-4)
      HDT=INT(DAT*1.D-2)
      MNDT=INT(DAT-100.D0*HDT)
      HDT =HDT-100*JDT
      JDT =JDT-100*MDT
      MDT =MDT-100*ADT
C
      TV=((((ADT-ADC)*365+(JDT+NJDM(MDT)-JDC-NJDM(MDC)))*24 +
     *     HDT-HDC)*60 + MNDT-MNDC)*60
C
      RETURN
      END
