C                       *****************
                        SUBROUTINE TOM_CORFON
C                       *****************
C
C***********************************************************************
C PROGICIEL : TOMAWAC                          F. MARCOS
C FUSION TOMAWAC/COWADIS       12/01/01        OPTIMER (02 98 44 24 51)                 
C***********************************************************************
C
C  USER SUBROUTINE CORFON
C
C  FONCTION  : MODIFICATION DE LA TOPOGRAPHIE
C  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
C
C-----------------------------------------------------------------------
C  ARGUMENTS TO USE
C .________________.____._______________________________________________
C |      NAME      |MODE|                 FUNCTION
C |________________|____|_______________________________________________
C |      ZF        |<-->| BOTTOM
C |      X,Y       |<-->| MESH COORDINATES 
C |      NPOIN2    | -->| NUMBER OF POINTS IN THE MESH
C |      LISFON    | -->| NUMBER OF BOTTOM SMOOTHINGS
C |      T1,2      |<-->| WORKING TABLES
C |      W1        |<-->| WORKING TABLE
C |________________|____|_______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT :
C PROGRAMMES APPELES : RIEN EN STANDARD
C
C***********************************************************************
C
      USE BIEF
      USE DECLARATIONS_TOMAWAC
C
      IMPLICIT NONE
C
!      INTEGER LNG,LU
!      COMMON/INFO/LNG,LU
C
      INTEGER K
C
      DO K=1,NPOIN2
        IF (ZF(K).LT.-10.D0) THEN
          ZF(K)=ZF(K)
        ELSE
          ZF(K)=-10.D0
       ENDIF
      ENDDO
C     
      RETURN
      END
C                       *****************
                        SUBROUTINE CORRXY(X,Y,NPOIN)
C                       *****************
C
C***********************************************************************
C PROGICIEL : BIEF 5.0          01/03/90    J-M HERVOUET
C***********************************************************************
C
C  USER SUBROUTINE CORRXY
C
C  FUNCTION  : MODIFICATION OF THE COORDINATES OF THE POINTS IN THE MESH
C
C              LINES WITH AN INITIAL CEX ARE AN EXAMPLE
C              WITH TELEMAC-2D
C
C              THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO
C              THE CALLING PROGRAM AND THE NEEDED MODIFICATION
C              BY ADDING USE DECLARATIONS_"NAME OF CALLING CODE"
C              ALL THE DATA STRUCTURE OF THIS CODE IS
C              AVAILABLE
C
C-----------------------------------------------------------------------
C  ARGUMENTS USED IN THE EXAMPLE 
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |    X,Y         | -->|  COORDONNEES DU MAILLAGE .                   |
C |    NPOIN2       | -->|  NOMBRE DE POINTS DU MAILLAGE                |
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C
C PROGRAMME APPELANT :
C PROGRAMMES APPELES : RIEN EN STANDARD
C
C***********************************************************************
C
C APPELE PAR : INBIEF
C
C SOUS-PROGRAMME APPELE : NEANT
C
C***********************************************************************
C
      USE BIEF, EX_CORRXY => CORRXY
C
      USE DECLARATIONS_SPECIAL
C
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NPOIN
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN),Y(NPOIN)
!      INTEGER LNG,LU
!      COMMON/INFO/LNG,LU
      INTEGER I
      DOUBLE PRECISION R,TG23P,PI
C
C                                                                       
      PI=3.1415926D0
      R=6400.D3
      TG23P=TAN(23.D0*PI/60.D0)
      DO I=1,NPOIN
        X(I)=X(I)*180.D0/R/PI
        Y(I)=360.D0/PI*ATAN(EXP(Y(I)/R)*TG23P)-90.D0
      ENDDO
C
      RETURN
      END
C                       *****************
                        SUBROUTINE LIMWAC
C                       *****************
     *(F     , FBOR  , LIFBOR, NPTFR , NPLAN , NF    ,  TETA , FREQ  ,
     * NPOIN2, NBOR  , AT    , LT    , DDC   , LIMSPE, FPMAXL, FETCHL,
     * SIGMAL, SIGMBL, GAMMAL, FPICL , HM0L  , APHILL, TETA1L, SPRE1L,
     * TETA2L, SPRE2L, XLAMDL, X ,Y  , KENT  , KSORT , NFO1  , NBI1  ,
     * BINBI1, UV    , VV    , SPEULI, VENT  ,  VENSTA, GRAVIT,
     * PRIVE , NPRIV , SPEC  , FRA   , DEPTH , FRABL, BOUNDARY_COLOUR )
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
      INTEGER, INTENT(IN)   :: NPTFR,NPLAN,NF,NPOIN2,LT,NPRIV
      INTEGER, INTENT(IN)   :: BOUNDARY_COLOUR(NPTFR)
      LOGICAL, INTENT(IN)   :: SPEULI, VENT, VENSTA
      INTEGER NPCL
      PARAMETER (NPCL=21)
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
      INTEGER NBOR(NPTFR),LIFBOR(NPTFR),NFO1,NBI1,IP,I
      INTEGER KENT,KSORT,IFF,IPLAN,IPTFR,LIMSPE
      INTEGER IFRM,IFRP,NBP,NBM,NENR,NPCLI,NPB(NPCL)
C
      CHARACTER*8 BINBI1
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
            READ(NFO1,4000) ((CL1(I,IFF,IP),I=1,NPLAN),IFF=1,NF)
40       CONTINUE
         READ(NFO1,3000) AT2
         ATT=AT2
         CALL TEMP(AT2,ATT,DDC)
         IF (AT2.LT.AT) THEN
            AT1=AT2
            GOTO 50
         ENDIF
         DO 60 IP=1,NPCLI
            READ(NFO1,4000) ((CL2(I,IFF,IP),I=1,NPLAN),IFF=1,NF)
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
C      CALL LIT(CL2(1,1,IP),BID,I,CAR,300,'R4',NBI1,BINBI1,ISTAT)
               READ(NFO1,4000) ((CL2(I,IFF,IP),I=1,NPLAN),IFF=1,NF)
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
c$$$C                       *****************
c$$$                        SUBROUTINE NOUDON
c$$$C                       *****************
c$$$C
c$$$     *(UV , VV , X  , Y  , NPOIN, NDON , BINDON, NBOR, NPTFR,
c$$$     * AT , DDC, TV1, TV2, NP   , XRELV, YRELV , UR  , VR   ,
c$$$     * TRA, U1 , V1 , U2 , V2   , INDIC, CHDON , NVAR)
c$$$C
c$$$C***********************************************************************
c$$$C  TOMAWAC VERSION 5.0
c$$$C***********************************************************************
c$$$C
c$$$C   FONCTION : CE SOUS-PROGRAMME CALCULE LA VALEUR DU VENT OU
c$$$C              DU COURANT A L'INSTANT COURANT
c$$$C              SUR LE MAILLAGE DE CALCUL
c$$$C             (INSPIRE DE LA ROUTINE FOND DE TELEMAC 2D)
c$$$C
c$$$C-----------------------------------------------------------------------
c$$$C                             ARGUMENTS
c$$$C .________________.____.______________________________________________.
c$$$C !      NOM       !MODE!                   ROLE                       !
c$$$C !________________!____!______________________________________________!
c$$$C !    UV,VV       !<-- !  DONNEE AUX NOEUDS DU MAILLAGE               !
c$$$C !    X,Y         ! -->!  COORDONNEES DU MAILLAGE                     !
c$$$C !    NPOIN       ! -->!  NOMBRE DE POINTS DU MAILLAGE                !
c$$$C !    NDON        ! -->!  NUMERO D'UNITE LOGIQUE DU FICHIER DE DONNEES!
c$$$C !    BINDON      ! -->!  BINAIRE DU FICHIER DE DONNEES               !
c$$$C !    NBOR        ! -->!  NUMEROTATION DES POINTS FRONTIERE           !
c$$$C !    NPTFR       ! -->!  NOMBRE DE  POINTS FRONTIERE                 !
c$$$C !    AT          ! -->!  TEMPS                                       !
c$$$C !    DDC         ! -->!  DATE DU DEBUT DU CALCUL                     !
c$$$C !    TV1         !<-->!  TEMPS DU CHAMPS DE DONNEES 1                !
c$$$C !    TV2         !<-->!  TEMPS DU CHAMPS DE DONNEES 2                !
c$$$C !    NP          !<-->!  NOMBRE DE POINTS DU MAILLAGE DES DONNEES    !
c$$$C !    XRELV       !<-- !  TABLEAU DES ABSCISSES DES POINTS RELEVES    !
c$$$C !    YRELV       !<-- !  TABLEAU DES ORDONNEES DES POINTS RELEVES    !
c$$$C !    UR,VR       !<-->!  TABLEAU DES COURANTS RELEVES                !
c$$$C !    U1,V1,U2,V2 !<-->!  DONNEES AUX NOEUDS DU MAILLAGE              !
c$$$C !    INDIC       ! -->!  TYPE DE FORMAT DE LECTURE                   !
c$$$C !________________!____!______________________________________________!
c$$$C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
c$$$C
c$$$C-----------------------------------------------------------------------
c$$$C
c$$$C APPELE PAR : SEMIMP
c$$$C
c$$$C SOUS-PROGRAMME APPELE : FASP
c$$$C
c$$$C***********************************************************************
c$$$C
c$$$                        USE DECLARATIONS_TOMAWAC, only: lng,lu
c$$$                        IMPLICIT NONE
c$$$C
c$$$!      INTEGER LNG,LU
c$$$!      COMMON/INFO/ LNG,LU
c$$$C
c$$$      INTEGER NP,NDON,NPOIN,NPTFR,INDIC,I,ISTAT,NVAR
c$$$C
c$$$      INTEGER NBOR(NPTFR,2),ID(2)
c$$$C
c$$$      DOUBLE PRECISION X(NPOIN),Y(NPOIN)
c$$$      DOUBLE PRECISION UV(NPOIN),VV(NPOIN),UR(NP),VR(NP)
c$$$      DOUBLE PRECISION U1(NPOIN),V1(NPOIN),U2(NPOIN),V2(NPOIN)
c$$$      DOUBLE PRECISION XRELV(NP),YRELV(NP),TRA(NP)
c$$$      DOUBLE PRECISION AT,TV1,TV2
c$$$      DOUBLE PRECISION DDC,DAT2,DAT2B(1),Z(1),C,COEF
c$$$C
c$$$      CHARACTER*3 BINDON, C1
c$$$      CHARACTER*7 CHDON
c$$$C-----------------------------------------------------------------------
c$$$C
c$$$      IF (AT.GT.TV2) THEN
c$$$C
c$$$C       ----------------------------------------------------------------
c$$$C        ON CHANGE D'ENREGISTREMENT : 2->1 ET ON LIT UN NOUVEAU 2
c$$$C       ----------------------------------------------------------------
c$$$        TV1=TV2
c$$$        CALL OV('X=Y     ', U1 , U2 , U1 , C , NPOIN)
c$$$        CALL OV('X=Y     ', V1 , V2 , V1 , C , NPOIN)
c$$$C
c$$$        IF(LNG.EQ.1) THEN
c$$$          WRITE(LU,*) '   NOUDON : LECTURE D''UN NOUVEL ENREGISTREMENT'
c$$$        ELSE
c$$$          WRITE(LU,*) '   NOUDON : READING A NEW RECORDING'
c$$$        ENDIF
c$$$C
c$$$        IF (INDIC.EQ.1) THEN
c$$$C
c$$$C     ------------------------------------------------------------------
c$$$C          FICHIER DIFFERENCES FINIES FORMATTE DU TYPE WAM CYCLE 4
c$$$C     ------------------------------------------------------------------
c$$$ 90        CONTINUE
c$$$C          LECTURE : DATE DE L'ENREGISTREMENT
c$$$           READ(NDON,*,END=100,ERR=100) DAT2
c$$$           CALL TEMP(TV2,DAT2,DDC)
c$$$C          LECTURE : DONNEES
c$$$           READ(NDON,*,END=100,ERR=100)
c$$$           READ(NDON,20,END=100,ERR=100)
c$$$     *       (UR(I),I=1,NP)
c$$$           READ(NDON,*,END=100,ERR=100)
c$$$           READ(NDON,20,END=100,ERR=100)
c$$$     *       (VR(I),I=1,NP)
c$$$C
c$$$           IF (TV2.LT.AT) THEN
c$$$             IF(LNG.EQ.1) THEN
c$$$               WRITE(LU,*) ' NOUDON : ON SAUTE 1 ENREGISTREMENT ..'
c$$$             ELSE
c$$$               WRITE(LU,*) ' NOUDON : JUMP OF 1 RECORDED DATA SERIES'
c$$$             ENDIF
c$$$             TV1=TV2
c$$$             CALL FASP(X,Y,U1,NPOIN,XRELV,YRELV,UR,NP,NBOR,NPTFR,0.D0)
c$$$             CALL FASP(X,Y,V1,NPOIN,XRELV,YRELV,VR,NP,NBOR,NPTFR,0.D0)
c$$$             GOTO 90
c$$$           ENDIF
c$$$C
c$$$           CALL FASP(X,Y,U2,NPOIN,XRELV,YRELV,UR,NP,NBOR,NPTFR,0.D0)
c$$$           CALL FASP(X,Y,V2,NPOIN,XRELV,YRELV,VR,NP,NBOR,NPTFR,0.D0)
c$$$C
c$$$        ELSEIF (INDIC.EQ.3) THEN
c$$$C
c$$$C     ------------------------------------------------------------------
c$$$C       FICHIER SELAFIN DU TYPE TELEMAC
c$$$C     ------------------------------------------------------------------
c$$$C
c$$$        ID(1)=1
c$$$        ID(2)=2
c$$$ 95     CONTINUE
c$$$C       LECTURE : DATE DE L'ENREGISTREMENT
c$$$        CALL LIT(DAT2B,Z,I,C1,1,'R4',NDON,BINDON,ISTAT)
c$$$        IF(CHDON(1:1).EQ.'C') THEN
c$$$         TV2=DAT2B(1)
c$$$        ELSE
c$$$         DAT2=DAT2B(1)*1.D2
c$$$         CALL TEMP(TV2,DAT2,DDC)
c$$$        ENDIF
c$$$C       LECTURE : DONNEES
c$$$       DO I =1,NVAR
c$$$        IF(I.EQ.ID(1)) THEN
c$$$         CALL LIT(UR,TRA,I,C1,NP,'R4',NDON,BINDON,ISTAT)
c$$$        ELSEIF(I.EQ.ID(2)) THEN
c$$$         CALL LIT(VR,TRA,I,C1,NP,'R4',NDON,BINDON,ISTAT)
c$$$        ELSE
c$$$         READ(NDON)
c$$$        ENDIF
c$$$       ENDDO
c$$$C
c$$$        IF (TV2.LT.AT) THEN
c$$$          IF(LNG.EQ.1) THEN
c$$$            WRITE(LU,*) ' NOUDON : ON SAUTE 1 ENREGISTREMENT ..'
c$$$          ELSE
c$$$            WRITE(LU,*) ' NOUDON : JUMP OF 1 RECORDED DATA SERIES'
c$$$          ENDIF
c$$$          TV1=TV2
c$$$C         INTERPOLATION SPATIALE DES DONNEES
c$$$C          CALL FASP(X,Y,U1,NPOIN,XRELV,YRELV,UR,NP,NBOR,NPTFR,0.D0)
c$$$C          CALL FASP(X,Y,V1,NPOIN,XRELV,YRELV,VR,NP,NBOR,NPTFR,0.D0)
c$$$          U1=UR
c$$$          V1=VR
c$$$          GOTO 95
c$$$        ENDIF
c$$$C
c$$$        WRITE(LU,*) 'T',CHDON,'1:',TV1
c$$$        WRITE(LU,*) 'T',CHDON,'2:',TV2
c$$$C
c$$$C       INTERPOLATION SPATIALE DES DONNEES
c$$$C        CALL FASP(X,Y,U2,NPOIN,XRELV,YRELV,UR,NP,NBOR,NPTFR,0.D0)
c$$$C        CALL FASP(X,Y,V2,NPOIN,XRELV,YRELV,VR,NP,NBOR,NPTFR,0.D0)
c$$$         U2=UR
c$$$         V2=VR
c$$$C
c$$$        ELSEIF (INDIC.EQ.4) THEN
c$$$C
c$$$C     ------------------------------------------------------------------
c$$$C        LECTURE D'UN FORMAT DEFINI PAR L'UTILISATEUR
c$$$C     ------------------------------------------------------------------
c$$$C
c$$$          IF(CHDON(1:1).EQ.'C') THEN
c$$$          CALL COUUTI
c$$$     *    (X,Y,NPOIN,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
c$$$     *     NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NP)
c$$$          ELSE
c$$$          CALL VENUTI
c$$$     *    (X,Y,NPOIN,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
c$$$     *     NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NP)
c$$$          ENDIF
c$$$C
c$$$C
c$$$        ELSE
c$$$C
c$$$        WRITE(LU,*) '************************************************'
c$$$        IF(LNG.EQ.1) THEN
c$$$         WRITE(LU,*) 'NOUDON : INDICATEUR DE FORMAT INCONNU : ',INDIC
c$$$        ELSE
c$$$          WRITE(LU,*)'NOUDON : UNKNOWN INDICATOR OF FORMAT : ',INDIC
c$$$        ENDIF
c$$$        WRITE(LU,*) '************************************************'
c$$$        CALL PLANTE(0)
c$$$        ENDIF
c$$$C
c$$$      ENDIF
c$$$C
c$$$C       --------------------------------------------------------------
c$$$C          INTERPOLATION
c$$$C       --------------------------------------------------------------
c$$$C
c$$$      COEF=(AT-TV1)/(TV2-TV1)
c$$$      DO 60 I=1,NPOIN
c$$$         UV(I)=(U2(I)-U1(I))*COEF+U1(I)
c$$$         VV(I)=(V2(I)-V1(I))*COEF+V1(I)
c$$$60    CONTINUE
c$$$C
c$$$C-----------------------------------------------------------------------
c$$$C
c$$$C     FORMATS
c$$$C
c$$$20    FORMAT (10F6.2)
c$$$C
c$$$      RETURN
c$$$C
c$$$C     EN CAS DE PROBLEME DE LECTURE ...
c$$$C
c$$$100   CONTINUE
c$$$      WRITE(LU,*)'*********************************************'
c$$$      IF (LNG.EQ.1) THEN
c$$$         WRITE(LU,*)'  ERREUR A LA LECTURE DU FICHIER DE DONNEES  '
c$$$         WRITE(LU,*)'      OU FIN DE FICHIER PREMATUREE           '
c$$$      ELSE
c$$$         WRITE(LU,*)'  ERROR WHILE READING DATA FILE '
c$$$         WRITE(LU,*)'    OR UNEXPECTED END OF FILE           '
c$$$      ENDIF
c$$$      WRITE(LU,*)'*********************************************'
c$$$      CALL PLANTE(0)
c$$$C
c$$$      RETURN
c$$$      END
c$$$C                       *****************
c$$$                        SUBROUTINE LECDOI
c$$$C                       *****************
c$$$C
c$$$     *( UD , VD  , X  , Y  , NPOIN2, NDON , BINDON, NBOR , NPTFR, 
c$$$     *  AT , DDC , TV1, TV2, NP   , XRELV, YRELV , UR   , VR   ,
c$$$     *  TRA, U1  , V1 , U2 , V2   , INDIC, NPMAX , CHDON, NVAR )
c$$$C
c$$$C***********************************************************************
c$$$C  TOMAWAC VERSION 5.0
c$$$C***********************************************************************
c$$$C
c$$$C   FONCTION : CE SOUS-PROGRAMME PROJETE LA VALEUR DES DONNEES
c$$$C              DE COURANT OU DE VENT
c$$$C              SUR LE MAILLAGE DE CALCUL ET INTERPOLE
c$$$C              A L'INSTANT INITIAL
c$$$C        (INSPIRE ENTRE AUTRES DE LA ROUTINE FOND DE TELEMAC 2D)
c$$$C
c$$$C-----------------------------------------------------------------------
c$$$C                             ARGUMENTS
c$$$C .________________.____.______________________________________________.
c$$$C !      NOM       !MODE!                   ROLE                       !
c$$$C !________________!____!______________________________________________!
c$$$C !    UD,VD       !<-- !  DONNEE AUX NOEUDS DU MAILLAGE               !
c$$$C !    X,Y         ! -->!  COORDONNEES DU MAILLAGE                     !
c$$$C !    NPOIN2      ! -->!  NOMBRE DE POINTS DU MAILLAGE                !
c$$$C !    NDON        ! -->!  NUMERO D'UNITE LOGIQUE DU FICHIER DE DONNEES!
c$$$C !    BINDON      ! -->!  BINAIRE DU FICHIER DES DONNEES              !
c$$$C !    NBOR        ! -->!  NUMEROTATION DES POINTS FRONTIERE           !
c$$$C !    NPTFR       ! -->!  NOMBRE DE  POINTS FRONTIERE                 !
c$$$C !    AT          ! -->!  TEMPS                                       !
c$$$C !    DDC         ! -->!  DATE DU DEBUT DU CALCUL                     !
c$$$C !    TV1         !<-->!  TEMPS DU CHAMPS DE DONNEES 1                !
c$$$C !    TV2         !<-->!  TEMPS DU CHAMPS DE DONNEES 2                !
c$$$C !    NP          !<-->!  NOMBRE DE POINTS RELEVES                    !
c$$$C !    XRELV       !<-->!  TABLEAU DES ABSCISSES DES POINTS RELEVES    !
c$$$C !    YRELV       !<-->!  TABLEAU DES ORDONNEES DES POINTS RELEVES    !
c$$$C !    UR,VR       !<-->!  TABLEAU DES DONNEES RELEVEES                !
c$$$C !    U1,V1,U2,V2 !<-->!  DONNEES AUX NOEUDS DU MAILLAGE A TV1 ET TV2 !
c$$$C !    INDIC       ! -->!  TYPE DE FORMAT DE LECTURE                   !
c$$$C !    NPMAX       ! -->!  NOMBRE DE POINTS RELEVES MAXIMUM            !
c$$$C !________________!____!______________________________________________!
c$$$C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
c$$$C
c$$$C-----------------------------------------------------------------------
c$$$C
c$$$C APPELE PAR : CONDIW
c$$$C
c$$$C SOUS-PROGRAMME APPELE : COUUTI, VENUTI, FASP
c$$$C
c$$$C***********************************************************************
c$$$C
c$$$                        USE DECLARATIONS_TOMAWAC, only: lng,lu
c$$$                        IMPLICIT NONE
c$$$C
c$$$!      INTEGER LNG,LU
c$$$!      COMMON/INFO/ LNG,LU
c$$$C
c$$$      INTEGER NP,NDON,NPOIN2,NPTFR,INDIC,NCOL,NLIG,BID,I,J
c$$$      INTEGER NVAR,ISTAT,IB(10),ID(2)
c$$$C
c$$$      INTEGER NPMAX,NBOR(NPTFR,2)
c$$$C
c$$$      DOUBLE PRECISION X(NPOIN2)    , Y(NPOIN2)
c$$$      DOUBLE PRECISION UD(NPOIN2)   , VD(NPOIN2)
c$$$      DOUBLE PRECISION XRELV(NPMAX) , YRELV(NPMAX)
c$$$      DOUBLE PRECISION UR(NPMAX)    , VR(NPMAX), TRA(NPMAX)
c$$$      DOUBLE PRECISION U1(NPOIN2)   , V1(NPOIN2)
c$$$      DOUBLE PRECISION U2(NPOIN2)   , V2(NPOIN2)
c$$$      DOUBLE PRECISION XMAX,XMIN,YMAX,YMIN,DX,DY,AT,TV1,TV2
c$$$      DOUBLE PRECISION DDC,DAT1,DAT2,COEF,Z(1),ATT, ATB(1)
c$$$C
c$$$      CHARACTER*3  BINDON,C
c$$$      CHARACTER*7  CHDON
c$$$      CHARACTER*72 TITCAS
c$$$      CHARACTER*32 TEXTE(10)
c$$$C
c$$$C
c$$$C
c$$$C-----------------------------------------------------------------------
c$$$C        LECTURE DES POINTS RELEVES SUR UNITE LOGIQUE NDON
c$$$C-----------------------------------------------------------------------
c$$$C
c$$$      IF (INDIC.EQ.1) THEN
c$$$C
c$$$C      -----------------------------------------------------------------
c$$$C      FORMAT WAM DIFFERENCES FINIES + INTERPOLATION AUX POINTS
c$$$C                 DU MAILLAGE
c$$$C      -----------------------------------------------------------------
c$$$C
c$$$       REWIND NDON
c$$$C
c$$$       READ(NDON,10,END=100,ERR=100)
c$$$     *      NCOL,NLIG,YMIN,YMAX,XMIN,XMAX,BID,BID
c$$$       DX=(XMAX-XMIN)/REAL(NCOL-1)
c$$$       DY=(YMAX-YMIN)/REAL(NLIG-1)
c$$$       NP=NCOL*NLIG
c$$$       IF(LNG.EQ.1) THEN
c$$$        WRITE(LU,*) '--------------------------------------------------'
c$$$        WRITE(LU,*) 'LECDOI : LECTURE DU FICHIER DE ',CHDON
c$$$        WRITE(LU,*) '         NOMBRE DE LIGNES   : ',NLIG
c$$$        WRITE(LU,*) '         NOMBRE DE COLONNES :',NCOL
c$$$        WRITE(LU,*) '         ABSCISSE OU LONGITUDE MINIMALE : ',XMIN
c$$$        WRITE(LU,*) '         ABSCISSE OU LONGITUDE MAXIMALE : ',XMAX
c$$$        WRITE(LU,*) '         ORDONNEE OU LATITUDE MINIMALE  : ',YMIN
c$$$        WRITE(LU,*) '         ORDONNEE OU LATITUDE MAXIMALE  : ',YMAX
c$$$        IF (NP.GT.NPMAX) THEN
c$$$         WRITE(LU,*) '*************************************************'
c$$$         WRITE(LU,*) ' LA DIMENSION PREVUE PAR DEFAUT POUR LE TABLEAU '
c$$$         WRITE(LU,*) ' DE DONNEES :',NPMAX,' EST TROP FAIBLE POUR '
c$$$         WRITE(LU,*) ' CONTENIR LA TOTALITE DES DONNEES :',NP
c$$$         WRITE(LU,*) '*************************************************'
c$$$         CALL PLANTE(0)
c$$$        ENDIF
c$$$       ELSE
c$$$        WRITE(LU,*) '--------------------------------------------------'
c$$$        WRITE(LU,*)'LECDOI : READING OF THE ',CHDON,' DATA FILE '
c$$$        WRITE(LU,*)'         NUMBER OF LINES   : ',NLIG
c$$$        WRITE(LU,*)'         NUMBER OF COLUMNS : ',NCOL
c$$$        WRITE(LU,*)'         MINIMAL ABSCISSAE : ',XMIN
c$$$        WRITE(LU,*)'         MAXIMAL ABSCISSAE : ',XMAX
c$$$        WRITE(LU,*)'         MINIMAL ORDINATES : ',YMIN
c$$$        WRITE(LU,*)'         MAXIMAL ORDINATES : ',YMAX
c$$$        IF (NP.GT.NPMAX) THEN
c$$$         WRITE(LU,*) '*************************************************'
c$$$         WRITE(LU,*) ' THE DEFAULT DIMENSION ALLOWED FOR THE ARRAY OF '
c$$$         WRITE(LU,*) ' DATA :',NPMAX,' IS TOO LOW TO HOLD'
c$$$         WRITE(LU,*) ' ALL THE DATA :',NP
c$$$         WRITE(LU,*) '*************************************************'
c$$$         CALL PLANTE(0)
c$$$        ENDIF
c$$$       ENDIF
c$$$C      LECTURE DE LA DATE DU PREMIER ENREGISTREMENT DE DONNEES
c$$$       READ(NDON,*) DAT1
c$$$       CALL TEMP(TV1,DAT1,DDC)
c$$$       IF (TV1.GT.AT) THEN
c$$$        WRITE(LU,*) '*************************************************'
c$$$        IF(LNG.EQ.1) THEN
c$$$         WRITE(LU,*) ' LE PREMIER ENREGISTREMENT DU FICHIER DE ',CHDON
c$$$         WRITE(LU,*) '   ',DAT1,' EST POSTERIEUR AU TEMPS '
c$$$         WRITE(LU,*) '   DU DEBUT DU CALCUL',DDC
c$$$        ELSE
c$$$         WRITE(LU,*) ' THE FIRST RECORDING OF THE ',CHDON,' FILE '
c$$$         WRITE(LU,*) '   ',DAT1,' IS OLDER THAN THE DEGINNING '
c$$$         WRITE(LU,*) '   OF THE COMPUTATION',DDC
c$$$        ENDIF
c$$$        WRITE(LU,*) '*************************************************'
c$$$        CALL PLANTE(0)
c$$$       ENDIF
c$$$C
c$$$       DO 50 I=1,NCOL
c$$$          DO 40 J=1,NLIG
c$$$                XRELV((I-1)*NLIG+J)=XMIN+DX*(I-1)
c$$$                YRELV((I-1)*NLIG+J)=YMIN+DY*(J-1)
c$$$40        CONTINUE
c$$$50     CONTINUE
c$$$C
c$$$90     CONTINUE
c$$$       READ(NDON,*,END=100,ERR=100)
c$$$       READ(NDON,20,END=100,ERR=100)
c$$$     *       (UR(I),I=1,NP)
c$$$       READ(NDON,*)
c$$$       READ(NDON,20,END=100,ERR=100)
c$$$     *       (VR(I),I=1,NP)
c$$$       CALL OV( 'X=C     ' , U1 , Y , U1 , 0.D0 , NPOIN2)
c$$$       CALL OV( 'X=C     ' , V1 , Y , V1 , 0.D0 , NPOIN2)       
c$$$C
c$$$       READ(NDON,*) DAT2
c$$$       CALL TEMP(TV2,DAT2,DDC)
c$$$       IF (TV2.LT.AT) THEN
c$$$         TV1=TV2
c$$$         GOTO 90
c$$$       ENDIF
c$$$       CALL FASP(X,Y,U1,NPOIN2,XRELV,YRELV,UR,NP,NBOR,NPTFR,0.D0)
c$$$       CALL FASP(X,Y,V1,NPOIN2,XRELV,YRELV,VR,NP,NBOR,NPTFR,0.D0)
c$$$C
c$$$       READ(NDON,*,END=100,ERR=100)
c$$$       READ(NDON,20,END=100,ERR=100)
c$$$     *      (UR(I),I=1,NP)
c$$$       READ(NDON,*,END=100,ERR=100)
c$$$       READ(NDON,20,END=100,ERR=100)
c$$$     *      (VR(I),I=1,NP)
c$$$       CALL FASP(X,Y,U2,NPOIN2,XRELV,YRELV,UR,NP,NBOR,NPTFR,0.D0)
c$$$       CALL FASP(X,Y,V2,NPOIN2,XRELV,YRELV,VR,NP,NBOR,NPTFR,0.D0)
c$$$C
c$$$C
c$$$      ELSEIF (INDIC.EQ.3) THEN
c$$$C
c$$$C      -----------------------------------------------------------------
c$$$C      FORMAT TELEMAC,
c$$$C      LES VARIABLES 1 ET 2 SONT LES COMPOSANTES X ET Y DU VENT
c$$$C      -----------------------------------------------------------------
c$$$C
c$$$       REWIND NDON
c$$$       ID(1)=1
c$$$       ID(2)=2
c$$$C
c$$$C      LECTURE DU TITRE
c$$$C
c$$$       CALL LIT(X,Z,IB,TITCAS,72,'CH',NDON,BINDON,ISTAT)
c$$$C
c$$$C      LECTURE DU NOMBRE DE VARIABLES ET DE LEURS NOMS
c$$$C
c$$$       CALL LIT(X,Z,IB,C,2,'I ',NDON,BINDON,ISTAT)
c$$$       NVAR=IB(1)
c$$$       DO 80 I=1,NVAR
c$$$          CALL LIT(X,Z,IB,TEXTE(I),32,'CH',NDON,BINDON,ISTAT)
c$$$80     CONTINUE
c$$$C
c$$$C      VARIABLES FORMAT ET GEOMETRIE
c$$$C
c$$$       CALL LIT(X,Z,IB,C,10,'I ',NDON,BINDON,ISTAT)
c$$$       CALL LIT(X,Z,IB,C, 4,'I ',NDON,BINDON,ISTAT)
c$$$       NP=IB(2)
c$$$       WRITE(LU,*) '--------------------------------------------'
c$$$       IF (LNG.EQ.1) THEN
c$$$        WRITE(LU,*)'LECDOI : LECTURE DU FICHIER TELEMAC'
c$$$        WRITE(LU,*) '         TITRE DU CAS LU : ',TITCAS
c$$$        WRITE(LU,*)'         NOMBRE DE POINTS   : ',NP
c$$$       ELSE
c$$$        WRITE(LU,*)'LECDOI : READING OF TELEMAC DATA FILE '
c$$$        WRITE(LU,*) '         FILE TITLE : ',TITCAS
c$$$        WRITE(LU,*)'         NUMBER OF POINTS   : ',NP
c$$$       ENDIF
c$$$       WRITE(LU,*) '--------------------------------------------'
c$$$       IF (NP.GT.NPMAX) THEN
c$$$        WRITE(LU,*) '**************************************************'
c$$$        IF(LNG.EQ.1) THEN
c$$$         WRITE(LU,*)
c$$$     *             ' LA DIMENSION PREVUE PAR DEFAUT POUR LE TABLEAU DE'
c$$$         WRITE(LU,*)
c$$$     *             ' DONNEES :',NPMAX,' EST TROP FAIBLE POUR CONTENIR'
c$$$         WRITE(LU,*) ' LA TOTALITE DES DONNEES :',NCOL*NLIG
c$$$        ELSE
c$$$         WRITE(LU,*) ' THE DEFAULT DIMENSION ALLOWED FOR THE ARRAY OF '
c$$$         WRITE(LU,*) ' DATA :',NPMAX,' IS TOO LOW TO HOLD'
c$$$         WRITE(LU,*) ' ALL THE DATA :',NP
c$$$        ENDIF
c$$$        WRITE(LU,*) '**************************************************'
c$$$        CALL PLANTE(0)
c$$$       ENDIF
c$$$C      TABLEAU ENTIER IKLE
c$$$       READ(NDON)
c$$$C      TABLEAU ENTIER IPOBO
c$$$       READ(NDON)
c$$$C
c$$$C      X ET Y
c$$$C
c$$$       CALL LIT(XRELV,TRA,IB,C,NP,'R4',NDON,BINDON,ISTAT)
c$$$       CALL LIT(YRELV,TRA,IB,C,NP,'R4',NDON,BINDON,ISTAT)
c$$$C
c$$$C      PAS DE TEMPS ET VARIABLES
c$$$C
c$$$       CALL LIT(ATB,Z,IB,C,1,'R4',NDON,BINDON,ISTAT)
c$$$       IF(CHDON(1:1).EQ.'C') THEN
c$$$        TV1=ATB(1)
c$$$       ELSE
c$$$        ATT=ATB(1)*1.D2
c$$$        CALL TEMP(TV1,ATT,DDC)
c$$$       ENDIF
c$$$       IF (TV1.GT.AT) THEN
c$$$        WRITE(LU,*) '*************************************************'
c$$$        IF(LNG.EQ.1) THEN
c$$$         WRITE(LU,*) ' LE PREMIER ENREGISTREMENT DU FICHIER DE ',CHDON
c$$$         WRITE(LU,*) '   ',ATT,' EST POSTERIEUR AU TEMPS '
c$$$         WRITE(LU,*) '   DU DEBUT DU CALCUL',DDC
c$$$        ELSE
c$$$         WRITE(LU,*) ' THE FIRST RECORDING OF THE ',CHDON,' FILE '
c$$$         WRITE(LU,*) '   ',ATT,' IS OLDER THAN THE DEGINNING '
c$$$         WRITE(LU,*) '   OF THE COMPUTATION',DDC
c$$$        ENDIF
c$$$        WRITE(LU,*) '*************************************************'
c$$$        CALL PLANTE(0)
c$$$       ENDIF
c$$$C
c$$$110    CONTINUE
c$$$       DO I =1,NVAR
c$$$        IF(I.EQ.ID(1)) THEN
c$$$         CALL LIT(UR,TRA,IB,C,NP,'R4',NDON,BINDON,ISTAT)
c$$$        ELSEIF(I.EQ.ID(2)) THEN
c$$$         CALL LIT(VR,TRA,IB,C,NP,'R4',NDON,BINDON,ISTAT)
c$$$        ELSE
c$$$         READ(NDON)
c$$$        ENDIF
c$$$       ENDDO
c$$$C
c$$$       CALL LIT(ATB,Z,IB,C,1,'R4',NDON,BINDON,ISTAT)
c$$$       IF(CHDON(1:1).EQ.'C') THEN
c$$$        TV2=ATB(1)
c$$$       ELSE
c$$$        ATT=ATB(1)*1.D2
c$$$        CALL TEMP(TV2,ATT,DDC)
c$$$       ENDIF
c$$$       IF (TV2.LT.AT) THEN
c$$$        TV1=TV2
c$$$        GOTO 110
c$$$       ENDIF
c$$$C
c$$$C      ITERPOLATION SPATIALE DES DONNEES AU TEMPS TV1
c$$$C       CALL FASP(X,Y,U1,NPOIN2,XRELV,YRELV,UR,NP,NBOR,NPTFR,0.D0)
c$$$C       CALL FASP(X,Y,V1,NPOIN2,XRELV,YRELV,VR,NP,NBOR,NPTFR,0.D0)
c$$$       U1=UR
c$$$       V1=VR
c$$$C
c$$$       DO I =1,NVAR
c$$$        IF(I.EQ.ID(1)) THEN
c$$$         CALL LIT(UR,TRA,IB,C,NP,'R4',NDON,BINDON,ISTAT)
c$$$        ELSEIF(I.EQ.ID(2)) THEN
c$$$         CALL LIT(VR,TRA,IB,C,NP,'R4',NDON,BINDON,ISTAT)
c$$$        ELSE
c$$$         READ(NDON)
c$$$        ENDIF
c$$$       ENDDO
c$$$       WRITE(LU,*) 'T',CHDON,'1:',TV1
c$$$       WRITE(LU,*) 'T',CHDON,'2:',TV2
c$$$C
c$$$C      ITERPOLATION SPATIALE DES DONNEES AU TEMPS TV2
c$$$C       CALL FASP(X,Y,U2,NPOIN2,XRELV,YRELV,UR,NP,NBOR,NPTFR,0.D0)
c$$$C       CALL FASP(X,Y,V2,NPOIN2,XRELV,YRELV,VR,NP,NBOR,NPTFR,0.D0)
c$$$       U2=UR
c$$$       V2=VR
c$$$C
c$$$C
c$$$      ELSEIF (INDIC.EQ.4) THEN
c$$$C       LECTURE D'UN FORMAT DEFINI PAR L'UTILISATEUR
c$$$        IF(CHDON(1:1).EQ.'C') THEN
c$$$C         LECTURE D'UN CHAMP DE COURANT
c$$$              CALL COUUTI
c$$$     *    (X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
c$$$     *     NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NPMAX)
c$$$        ELSEIF(CHDON(1:1).EQ.'V' .OR. CHDON(1:1).EQ.'W') THEN
c$$$C         LECTURE D'UN CHAMP DE VENT
c$$$          CALL VENUTI
c$$$     *    (X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
c$$$     *     NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NPMAX)
c$$$        ELSE
c$$$          IF(LNG.EQ.1) THEN
c$$$            WRITE(LU,*) 'LE TYPE DE DONNEES A LIRE EST INCONNU'
c$$$          ELSE
c$$$            WRITE(LU,*) 'UNKNOWN DATA'
c$$$          ENDIF
c$$$            CALL PLANTE(0)
c$$$        ENDIF
c$$$C
c$$$      ELSE
c$$$        WRITE(LU,*) '************************************************'
c$$$        IF(LNG.EQ.1) THEN
c$$$        WRITE(LU,*) 'LECDOI : INDICATEUR DE FORMAT INCONNU : ',INDIC
c$$$        WRITE(LU,*) '         POUR LE FICHIER DE ',CHDON
c$$$        ELSE
c$$$          WRITE(LU,*)'LECDOI : UNKNOWN INDICATOR OF FORMAT : ',INDIC
c$$$          WRITE(LU,*)'         FOR THE ',CHDON,' DATA FILE '
c$$$        ENDIF
c$$$        WRITE(LU,*) '************************************************'
c$$$        CALL PLANTE(0)
c$$$      ENDIF
c$$$C
c$$$C-----------------------------------------------------------------------
c$$$C   INTERPOLATION TEMPORELLE DES DONNEES
c$$$C-----------------------------------------------------------------------
c$$$C
c$$$      COEF=(AT-TV1)/(TV2-TV1)
c$$$      DO 120 I=1,NPOIN2
c$$$         UD(I)=(U2(I)-U1(I))*COEF+U1(I)
c$$$         VD(I)=(V2(I)-V1(I))*COEF+V1(I)
c$$$120   CONTINUE
c$$$C
c$$$C-----------------------------------------------------------------------
c$$$C
c$$$C     FORMATS
c$$$C
c$$$10    FORMAT (2I4,4F9.3,2I2)
c$$$20    FORMAT (10F6.2)
c$$$C
c$$$      RETURN
c$$$C
c$$$C     EN CAS DE PROBLEME DE LECTURE ...
c$$$C
c$$$100   CONTINUE
c$$$      WRITE(LU,*)'*********************************************'
c$$$      IF (LNG.EQ.1) THEN
c$$$         WRITE(LU,*)'  ERREUR A LA LECTURE DU FICHIER DE DONNEES  '
c$$$         WRITE(LU,*)'      OU FIN DE FICHIER PREMATUREE           '
c$$$      ELSE
c$$$         WRITE(LU,*)'  ERROR WHILE READING DATA FILE '
c$$$         WRITE(LU,*)'    OR UNEXPECTED END OF FILE           '
c$$$      ENDIF
c$$$      WRITE(LU,*)'*********************************************'
c$$$      CALL PLANTE(0)
c$$$C
c$$$      RETURN
c$$$      END
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
      INTEGER NJDM(0:12)
      DOUBLE PRECISION TV,DDC,DAT
C
C-----------------------------------------------------------------------
C
      DATA NJDM /0,0,31,59,90,120,151,181,212,243,273,304,334/
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
