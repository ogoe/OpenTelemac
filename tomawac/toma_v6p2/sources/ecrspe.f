!                    *****************
                     SUBROUTINE ECRSPE
!                    *****************
!
     &( F     , TETA  , NPLAN , FREQ  , NF    , NK    ,
     &  NPOIN2, AT    , AUXIL , NOLEO , NLEO  , NSCO  ,
     &  BINSCO, DEBRES, TITCAS, DATE  , TIME  , KNOLG , MESH,
     &  NSPE  , TISPEF)
!
!***********************************************************************
! TOMAWAC   V6P3                                   15/06/2011
!***********************************************************************
!
!brief    WRITES OUT THE DIRECTIONAL VARIANCE SPECTRUM
!+                AT SELECTED NODES.
!+                (SERAPHIN BINARY FORMAT).
!
!history  OPTIMER
!+        28/08/2000
!+        V5P0
!+   CREATED
!
!history
!+        07/06/2001
!+        V5P2
!+
!
!history  M. BENOIT
!+        13/07/2004
!+        V5P5
!+   CORRECTED A BUG IN THE DECLARATION OF IPOBO WHEN PASSED
!
!history
!+
!+        V6P0
!+
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
!history  G.MATTAROLO (EDF - LNHE)
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  A. LAUGEL & J-M HERVOUET (EDF - LNHE)
!+        22/11/2012
!+        V6P3
!+   Parallelism treated with files.
!
!history  E. GAGNAIRE-RENOU (EDF - LNHE)
!+        12/03/2013
!+        V6P3
!+   Print out the 1D frequential spectrum at (same) selected nodes
!+                (Scopgene format)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| AUXIL          |<->| DIRECTIONAL SPECTRUM WORK TABLE
!| BINSCO         |-->| SPECTRUM FILE FORMAT
!| DATE           |-->| START DATE
!| DEBRES         |-->| LOGICAL INDICATING THE FIRST TIME STEP TO PRINT
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| INUTIL         |<->| WORK TABLE
!| ISLEO          |-->| ARRAY OF LOGICAL
!| KNOLG          |-->| ARRAY LINKING LOCAL TO GLOBAL INDEXES IN PARALL
!| NF             |-->| NUMBER OF FREQUENCIES
!| NK             |-->| DUMMY VARIABLE
!| NLEO           |-->| NUMBER OF SPECTRUM PRINTOUT POINTS
!| NOLEO          |-->| INDEX ARRAY OF SPECTRUM PRINTOUT POINTS
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NSCO           |-->| LOGICAL UNIT NUMBER OF THE PUNCTUAL RESULTS FILE
!| TETA           |-->| DISRETIZED DIRECTION
!| TIME           |-->| START TIME
!| TITCAS         |-->| TITLE
!| TISPEF         |-->| NAME OF THE 1D SPECTRA RESULTS FILE
!| NSPE           |-->| LOGICAL UNIT NUMBER FOR THE 1D SPECTRA FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_TOMAWAC, ONLY : DEUPI
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2,NLEO,NSCO,NF,NK,NPLAN
      INTEGER, INTENT(IN)             :: KNOLG(NPOIN2)
      INTEGER, INTENT(IN)             :: NOLEO(NLEO)
      INTEGER, INTENT(IN)             :: DATE(3),TIME(3)
      DOUBLE PRECISION, INTENT(IN)    :: AT
      DOUBLE PRECISION, INTENT(INOUT) :: AUXIL(NPLAN,NK)
      DOUBLE PRECISION, INTENT(IN)    :: F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(IN)    :: TETA(NPLAN),FREQ(NF)
      LOGICAL, INTENT(IN)             :: DEBRES
      CHARACTER(LEN=72), INTENT(IN)   :: TITCAS
      CHARACTER(LEN=*) , INTENT(IN)   :: BINSCO
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH 
      CHARACTER(LEN=144), INTENT(IN)  :: TISPEF  
      INTEGER, INTENT(IN)             :: NSPE         
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER  ISTAT , II    , JF    , K 
      INTEGER  KAMP1 , KAMP2 , KAMP3 , KAMP4 , KAMP5 , KAMP6 , ILEO
      INTEGER  IBID(1), NELEM, NPSPE
      CHARACTER*72 C
      CHARACTER*32 TEXTE(99)
      CHARACTER*6  NUM
      CHARACTER*2  CC
      CHARACTER*1  C1,C2,C3,C4,C5,C6
      TYPE(BIEF_MESH) :: MESHF
      LOGICAL         :: SORLEO(99)    
      DOUBLE PRECISION AAT(1), DTETAR, F_INTF(NLEO,NF)
      REAL W(1)
      CHARACTER(LEN=11) EXTENS
      EXTERNAL          EXTENS
!
      INTEGER  P_IMAX
      EXTERNAL P_IMAX
!
      SAVE
!
!-----------------------------------------------------------------------
!
      DTETAR=DEUPI/DBLE(NPLAN)
      NPSPE=NF*NPLAN
      NELEM=(NF-1)*NPLAN
!     SORLEO = .FALSE.
      DO ILEO=1,NLEO
        KAMP1=NOLEO(ILEO)
        IF(NCSIZE.GT.1) THEN
          IF(KAMP1.GT.0) KAMP1=KNOLG(NOLEO(ILEO))
          KAMP1=P_IMAX(KAMP1)
        ENDIF
        KAMP2=MOD(KAMP1,100000)
        KAMP3=MOD(KAMP2,10000)
        KAMP4=MOD(KAMP3,1000)
        KAMP5=MOD(KAMP4,100)
        KAMP6=MOD(KAMP5,10)
        CC=CHAR(48+ILEO/10)//CHAR(48+MOD(ILEO,10))
        C1=CHAR(48+KAMP1/100000)
        C2=CHAR(48+KAMP2/10000)
        C3=CHAR(48+KAMP3/1000)
        C4=CHAR(48+KAMP4/100)
        C5=CHAR(48+KAMP5/10)
        C6=CHAR(48+KAMP6)
        NUM=C1//C2//C3//C4//C5//C6
        TEXTE(ILEO)='F'//CC//' PT2D'//NUM//'  UNITE SI       '
        SORLEO(ILEO) = .TRUE.
      ENDDO
!
!     FOR THE FIRST PRINTED TIME STEP, WRITES OUT THE HEADER TO THE FILE
!
      IF(DEBRES) THEN
!
!       CREATES MESHF, MESH ASSOCIATED WITH DISCRETISATION
!       IN FREQUENCY AND DIRECTION
!
        ALLOCATE(MESHF%TYPELM)
        ALLOCATE(MESHF%NELEM)
        ALLOCATE(MESHF%NPOIN)
        ALLOCATE(MESHF%IKLE)
        ALLOCATE(MESHF%IKLE%I(4*NELEM))
        ALLOCATE(MESHF%X)
        ALLOCATE(MESHF%Y)
        ALLOCATE(MESHF%NPTFR)
        ALLOCATE(MESHF%NBOR)
        ALLOCATE(MESHF%NBOR%I(NPSPE))
        ALLOCATE(MESHF%DIM)
        ALLOCATE(MESHF%KNOLG)
        ALLOCATE(MESHF%KNOLG%I(NPSPE))
        MESHF%NAME = 'MESH'
        MESHF%TYPELM = 20 !QUADRANGLE 2D MESH
        MESHF%NELEM  = NELEM
        MESHF%NPOIN  = NPSPE
        MESHF%DIM    = 2
        II=0
        DO JF=1,NF-1
          DO K=1,NPLAN
           II=II+1
           MESHF%IKLE%I(II)=MOD(II,NPLAN)+1+(JF-1)*NPLAN
          ENDDO
        ENDDO
        DO II=1,NELEM
          MESHF%IKLE%I(II+NELEM)=II
          MESHF%IKLE%I(II+2*NELEM)=II+NPLAN
          MESHF%IKLE%I(II+3*NELEM)=MESHF%IKLE%I(II)+NPLAN
        ENDDO
!
!       WRITES OUT THE ARRAYS X AND Y
!       
        ALLOCATE(MESHF%X%R(NPLAN*NF))
        ALLOCATE(MESHF%Y%R(NPLAN*NF))
        MESHF%NPTFR = 2*NPLAN
        DO JF=1,NF
          DO II=1,NPLAN
            MESHF%X%R(II+NPLAN*(JF-1))=FREQ(JF)*SIN(TETA(II))
            MESHF%Y%R(II+NPLAN*(JF-1))=FREQ(JF)*COS(TETA(II))
          ENDDO
        ENDDO
        MESHF%NBOR%I=0
        DO II = 1,NPLAN
          MESHF%NBOR%I(II) = II
        ENDDO
        DO II = NPLAN+1,2*NPLAN
          MESHF%NBOR%I(II)=NPLAN+1+NPSPE-II
        ENDDO
        MESHF%KNOLG%I = 0
!
!       IN PARALLEL ONLY PROCESSOR 0 CREATES THE FILE
!
        IF(IPID.EQ.0) THEN
!
!         CREATES DATA FILE USING A GIVEN FILE FORMAT : FORMAT_RES.
!         THE DATA ARE CREATED IN THE FILE: NRES, AND IS
!         CHARACTERISED BY A TITLE AND NAME OF OUTPUT VARIABLES
!         CONTAINED IN THE FILE.
!
          CALL CREATE_DATASET(BINSCO, ! RESULTS FILE FORMAT
     &                        NSCO,   ! LU FOR RESULTS FILE
     &                        TITCAS, ! TITLE
     &                        NLEO,   ! MAX NUMBER OF OUTPUT VARIABLES
     &                        TEXTE,  ! NAMES OF OUTPUT VARIABLES
     &                        SORLEO) ! PRINT TO FILE OR NOT
!
!         WRITES THE MESH IN THE OUTPUT FILE
!
          CALL WRITE_MESH(BINSCO, ! RESULTS FILE FORMAT
     &                    NSCO,   ! LU FOR RESULTS FILE
     &                    MESHF,  ! CHARACTERISES MESH
     &                    1,      ! NUMBER OF PLANES 
     &                    DATE,   ! START DATE
     &                    TIME,   ! START TIME
     &                    0,0)    ! COORDINATES OF THE ORIGIN.  
!   
          IF(TISPEF(1:1).NE.' ') THEN
            WRITE(NSPE,'(A1,A72)') '/', TITCAS
            WRITE(NSPE,'(I3)') NLEO
            DO ILEO=1,NLEO
              WRITE(NSPE,'(A32)') TEXTE(ILEO)
            ENDDO
            WRITE(NSPE,'(A19)') '0 0 0 0 0 0 0 0 0 0'
          ENDIF
        ENDIF
!
      ENDIF
!
!     RECORDS THE CURRENT TIME STEP
!
      IF(IPID.EQ.0) THEN
        AAT(1) = AT
        CALL ECRI2(AAT,IBID,C,1,'R4',NSCO,'STD',ISTAT)
        IF(LNG.EQ.1) WRITE(NSPE,1007) AAT
        IF(LNG.EQ.2) WRITE(NSPE,1008) AAT
      ENDIF   
1007  FORMAT('TEMPS = ',F13.5) 
1008  FORMAT('TIME  = ',F13.5)     
!
      IF(NCSIZE.GT.1) THEN
!
!       1) EVERY PROCESSOR WRITES ITS OWN POINTS
!          MESH%ELTCAR IS USED AS FOR THE CHARACTERISTICS
!
        DO ILEO=1,NLEO
          II=NOLEO(ILEO)
          IF(II.GT.0) THEN
          IF(MESH%ELTCAR%I(II).NE.0) THEN
            DO JF=1,NF
              DO K=1,NPLAN
                AUXIL(K,JF)=F(II,K,JF)
              ENDDO
            ENDDO
            OPEN(99,FILE=EXTENS(NLEO,ILEO),
     &              FORM='UNFORMATTED',STATUS='NEW')
            CALL ECRI2(AUXIL,IBID,C,NPSPE,'R8',99,'STD',ISTAT)
            CLOSE(99)
          ENDIF
          ENDIF
        ENDDO
!
!       WAITING COMPLETION OF THE WORK BY ALL PROCESSORS
!
        CALL P_SYNC
!
!       2) PROCESSOR 0 READS ALL FILES AND MERGES IN THE FINAL FILE
!
        IF(IPID.EQ.0) THEN
          DO ILEO=1,NLEO
            OPEN(99,FILE=EXTENS(NLEO,ILEO),
     &              FORM='UNFORMATTED',STATUS='OLD')
            CALL LIT(AUXIL,W,IBID,C,NPSPE,'R8',99,'STD',ISTAT)
            CALL ECRI2(AUXIL,IBID,C,NPSPE,'R4',NSCO,'STD',ISTAT)
            DO JF=1,NF
              F_INTF(ILEO,JF)=0.0D0
              DO K=1,NPLAN
                F_INTF(ILEO,JF)=F_INTF(ILEO,JF)+AUXIL(K,JF)*DTETAR
              ENDDO
            ENDDO  
            CLOSE(99,STATUS='DELETE')
          ENDDO
          DO JF=1,NF
            WRITE(NSPE,'(100(E10.4,2X))') FREQ(JF),
     &                                (F_INTF(ILEO,JF),ILEO=1,NLEO)
          ENDDO
        ENDIF
!
      ELSE
!
        DO ILEO=1,NLEO
          II=NOLEO(ILEO)
          DO JF=1,NF
            F_INTF(ILEO,JF)=0.D0
            DO K=1,NPLAN
              AUXIL(K,JF)=F(II,K,JF)
              F_INTF(ILEO,JF)=F_INTF(ILEO,JF)+F(II,K,JF)*DTETAR
            ENDDO
            IF(ABS(F_INTF(ILEO,JF)).LT.1.D-90) F_INTF(ILEO,JF)=0.D0
          ENDDO
          CALL ECRI2(AUXIL,IBID,C,NPSPE,'R4',NSCO,'STD',ISTAT)
        ENDDO		  
        DO JF=1,NF
          WRITE(NSPE,'(100(E10.4,2X))') FREQ(JF),
     &                                (F_INTF(ILEO,JF),ILEO=1,NLEO)
        ENDDO	  	  
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
