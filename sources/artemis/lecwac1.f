!                    ******************
                     SUBROUTINE LECWAC1
!                    ******************
!
!
!***********************************************************************
! ARTEMIS   V7P0                                  07/2014
!***********************************************************************
!
!brief    READ A SPECTRUM IN 1 POINT AND 1 DATE FROM A TOMAWAC SPE FILE
!         THIS SPECTRUM IS APPLIED ON THE WAVE INCIDENT BOURNDARIES
! 
!history  C PEYRARD (LNHE)
!+        07/2014
!+        V7P0
!+        creation
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!      USE INTERFACE_ARTEMIS, ONLY: SPE,SPD
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      CHARACTER*80 TITRE,FILE_SPE,PATH
      CHARACTER*32 TEXTEBID
      INTEGER      NBI1,NSPE,IBID(10),NBCL,IP,ILEN
      INTEGER      IPLAN,IFF,ID,KK,J,IDEC,JD
      REAL         BID, CL1(MAXDIR,MAXFRE,1)
      REAL         XTWC(MAXDIR,MAXFRE),YTWC(MAXDIR,MAXFRE)
      REAL         READTWC(MAXDIR*MAXFRE),STOCKD(MAXDIR)
      REAL         PI,DEGRAD,DF,DTETA,FF,DD,EPS
      PARAMETER(PI = 3.1415926535897932384626433D0 , DEGRAD=PI/180.D0)

!     A AJOUTER DANS LE DICO 
!      NFTWC  = 50 
!      NDTWC  = 48
!      TPSTWC =30000D0
!.....CONTROLE SIZE OF TABLES FROM TOMAWAC
      IF (NDTWC.GT.MAXDIR) THEN 
        WRITE(LU,*) 'TOO MANY DIRECTIONS IN TOMAWAC SPECTRUM  '
        WRITE(LU,*) 'INCREASE MAXDIR IN declarations_artemis.f'
        CALL PLANTE(0)
      ENDIF
      IF (NFTWC.GT.MAXFRE) THEN 
        WRITE(LU,*) 'TOO MANY FREQUENCIES IN TOMAWAC SPECTRUM  '
        WRITE(LU,*) 'INCREASE MAXFRE IN declarations_artemis.f'
        CALL PLANTE(0)
      ENDIF
!-------------------------------------------   
      WRITE(LU,*) '==================================================='
      WRITE(LU,*) '========== READING SPECTRUM FROM TOMAWAC =========='
!=====READING SELAPHIN FILE (.spe)
      NBI1 = ART_FILES(ARTTC1)%LU
!.....READ FIRST LINES
      READ(NBI1) TITRE
!
      READ(NBI1) IBID(1),IBID(2)
!
      NBCL=IBID(1)
      DO IP=1,NBCL
        READ(NBI1) TEXTEBID
      ENDDO
      READ(NBI1) (IBID(IP),IP=1,10)
!
      IF (IBID(10).EQ.1) THEN
        READ(NBI1) (IBID(IP),IP=1,6)
!
      ENDIF
      READ(NBI1) (IBID(IP),IP=1,4)
!
      READ(NBI1) (IBID(IP),IP=1,1)
!
      READ(NBI1) (IBID(IP),IP=1,1)
!
      READ(NBI1)  ((XTWC(IPLAN,IFF),IPLAN=1,NDTWC),IFF=1,NFTWC)
      READ(NBI1)  ((YTWC(IPLAN,IFF),IPLAN=1,NDTWC),IFF=1,NFTWC)
!
!
!.....READ DATA
   50 CONTINUE
      READ(NBI1) BID
!     Looking for the right date
      IF (ABS(BID-TPSTWC).GE.1E-2) THEN
        READ(NBI1) (READTWC(KK),KK=1,NFTWC*NDTWC)        
        GOTO 50
      ENDIF
!
      DO IP=1,NBCL 
        READ(NBI1) (READTWC(KK),KK=1,NFTWC*NDTWC)
        KK=1
!       order DATA into line,column format : CL1
        DO IFF=1,NFTWC
          DO IPLAN=1,NDTWC
            CL1(IPLAN,IFF,IP)=READTWC(KK)
            KK=KK+1
          ENDDO
        ENDDO
      ENDDO
!.....END READING
!==========================================



!.......SPECTRUM CONSTRUCTION 
!=============================
!=====DIRECTION 
      DO ID=1,NDTWC
!       COMPUTE DIRECTION FROM .spe file
        DIRTWC(NDTWC-ID+1)  = ATAN2(YTWC(ID,NFTWC),XTWC(ID,NFTWC))
!       Into degres
        DIRTWC(NDTWC-ID+1)  = DIRTWC(NDTWC-ID+1)/DEGRAD
!       Into [0,360]
        IF (DIRTWC(NDTWC-ID+1).LT.0D0) THEN
          DIRTWC(NDTWC-ID+1)  =360D0+DIRTWC(NDTWC-ID+1)
        ENDIF
      ENDDO
!     FIND THE SMALLEST DIRECTION IN [0;360] => IDEC
      IDEC=0
      EPS=1000D0
      DO ID=1,NDTWC
!       STOCK THE DIRECTION
        STOCKD(ID)=DIRTWC(ID)
        IF (ABS(DIRTWC(ID)).LT.EPS) THEN
          IDEC=ID
          EPS=ABS(DIRTWC(ID))
        ENDIF
      ENDDO
!     ORDER DIRTWC FROM 0 to 360
      DO ID=1,NDTWC
        IF (ID.GE.IDEC) THEN
          J=ID-IDEC+1
        ELSE
          J=NDTWC+ID-IDEC+1
        ENDIF
        DIRTWC(J)=STOCKD(ID)
      ENDDO
      DIRTWC(NDTWC+1)=DIRTWC(1)+360D0
!
!=====FREQUENCY 
      DO IFF=1,NFTWC
        FREQTWC(IFF)  = SQRT(XTWC(1,IFF)**2+YTWC(1,IFF)**2)
      ENDDO
!=====AMPLITUDE 
      DO ID=1,NDTWC
!       JD correspond to ID in the CL1 table
        JD=NDTWC-ID+1
!       J correspond to ID in the DIRTWC table
        IF (ID.GE.IDEC) THEN
          J=ID-IDEC+1
        ELSE
          J=NDTWC+ID-IDEC+1
        ENDIF
        DO IFF=1,NFTWC
!         Compute SPETWC(J,I) <=> (DIRTWC(J) ; FREQTWC(I))
          SPETWC(J,IFF)= CL1(JD,IFF,1)
          IF (J.EQ.1) THEN
            SPETWC(NDTWC+J,IFF)= SPETWC(J,IFF)
          ENDIF 
        ENDDO
      ENDDO
! END BUILDING
!===========================

      WRITE(LU,*) '========END READING SPRECTRUM FROM TOMAWAC========='
      WRITE(LU,*) '                                                   '

! TEST CHAINAGE          
!      DO ID=1,NDTWC+1
!        DD= DIRTWC(ID)
!        DO IFF=1,NFTWC
!          FF=FREQTWC(IFF) 
!          SPETWC(ID,IFF) =SPE(FF)*SPD(DD-270D0)
!        ENDDO
!      ENDDO
!-----------------------------------------------------------------------
!
      RETURN
      END
