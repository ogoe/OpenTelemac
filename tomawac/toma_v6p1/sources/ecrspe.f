!                    *****************
                     SUBROUTINE ECRSPE
!                    *****************
!
     &( F     , B     , TETA  , NPLAN , FREQ  , NF    , NK    ,
     &  NPOIN2, AT    , LT    , AUXIL , INUTIL, NOLEO , NLEO  , NSCO  ,
     &  BINSCO, DEBRES, TITCAS, DATE  , TIME ,ISLEO ,KNOLG)
!
!***********************************************************************
! TOMAWAC   V6P1                                   15/06/2011
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| AUXIL          |<->| DIRECTIONAL SPECTRUM WORK TABLE
!| B              |-->| JACOBIAN TO TRANSFORM N(KX,KY) INTO F(FR,TETA)
!| BINSCO         |-->| SPECTRUM FILE FORMAT
!| DATE           |-->| START DATE
!| DEBRES         |-->| LOGICAL INDICATING THE FIRST TIME STEP TO PRINT
!| F              |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| INUTIL         |<->| WORK TABLE
!| ISLEO          |-->| ARRAY OF LOGICAL
!| KNOLG          |-->| ARRAY LINKING LOCAL TO GLOBAL INDEXES IN PARALL
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE TOMAWAC_MPI
      USE TOMAWAC_MPI_TOOLS
!
      IMPLICIT NONE
      COMMON/ECRSPE_MPI/SPE_SEND
      INTEGER ::SPE_SEND
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER  NPOIN2, NLEO  , NSCO  , NF , NK , NPLAN
      INTEGER  NOLEO(NLEO)   , INUTIL(1)
      INTEGER  DATE(3),TIME(3)
      DOUBLE PRECISION AT    , AUXIL(NPLAN,NK), AAT(1)
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF) , TETA(NPLAN), FREQ(NF)
      DOUBLE PRECISION B(NPOIN2,NPLAN)
      INTEGER  LT
!
      LOGICAL DEBRES
      CHARACTER*72 TITCAS
      CHARACTER(LEN=*)   BINSCO
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  ISTAT , II    , JF    , K     , IB(10)
      INTEGER  KAMP1 , KAMP2 , KAMP3 , KAMP4 , KAMP5 , KAMP6 , ILEO
      INTEGER  IBID(1), NELEM, NPSPE
      CHARACTER*72 C
      CHARACTER*32 TEXTE(99)
      CHARACTER*6  NUM
      CHARACTER*2  CC
      CHARACTER*1  C0    , C1    , C2    , C3    , C4    , C5    , C6
!BD_INCKA ADDS A GRID ON THE FREQUENCIES AND PLANES
      TYPE(BIEF_MESH) :: MESHF
      LOGICAL         :: SORLEO(99)
      INTEGER :: I,IER
      INTEGER, ALLOCATABLE :: IKLE(:) ! GLOBAL CONNECTIVITY
      TYPE(BIEF_OBJ)  :: BVARSOR
!BD_INCKA MODIFICATION FOR PARALLEL MODE
      INTEGER, DIMENSION(NLEO) :: NRECV_LEO
      LOGICAL         :: ISLEO(NLEO) !
      INTEGER, DIMENSION(NCSIZE) :: NSPE_RECV
      INTEGER :: KNOLG(NPOIN2)
!BD_INCKA
!BD_INCKA END OF MODIFICATION
!
      NPSPE=NF*NPLAN
      NELEM=(NF-1)*NPLAN
      SORLEO = .FALSE.
      DO ILEO=1,NLEO
          KAMP1=NOLEO(ILEO)
          IF (NCSIZE.GT.1) KAMP1=KNOLG(NOLEO(ILEO))
          KAMP2=MOD(KAMP1,100000)
          KAMP3=MOD(KAMP2,10000)
          KAMP4=MOD(KAMP3,1000)
          KAMP5=MOD(KAMP4,100)
          KAMP6=MOD(KAMP5,10)
!          IF(ILEO.GT.9) THEN
!            C0='1'
!          ELSE
!            C0='0'
!          ENDIF
!          CC=C0//CHAR(48+MOD(ILEO,10))
          CC=CHAR(48+INT(ILEO/10))//CHAR(48+MOD(ILEO,10))
          C1=CHAR(48+INT(KAMP1/100000))
          C2=CHAR(48+INT(KAMP2/10000))
          C3=CHAR(48+INT(KAMP3/1000))
          C4=CHAR(48+INT(KAMP4/100))
          C5=CHAR(48+INT(KAMP5/10))
          C6=CHAR(48+KAMP6)
          NUM=C1//C2//C3//C4//C5//C6
          TEXTE(ILEO)='F'//CC//' PT2D'//NUM//'  UNITE SI       '
          IF (.NOT.ISLEO(ILEO)) TEXTE(ILEO) =
     &     'POINT HORS MAILLAGE             '
          SORLEO(ILEO) = .TRUE.
      ENDDO
!
!=====C
!  1  C FOR THE FIRST PRINTED TIME STEP, WRITES OUT THE HEADER TO THE FILE
!=====C================================================================
      IF (DEBRES) THEN
!
!.......2.1 NAME OF THE VARIABLES
!       """""""""""""""""""""""""""""""""""""
!BD_INCKA CREATES MESHF, MESH ASSOCIATED WITH DISCRETISATION
!         IN FREQUENCY AND DIRECTION
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
        ALLOCATE(IKLE(4*NELEM))
        II=0
        DO JF=1,NF-1
          DO K=1,NPLAN
           II=II+1
           IKLE(II)=MOD(II,NPLAN)+1+(JF-1)*NPLAN
          ENDDO
        ENDDO
        DO II=1,NELEM
          IKLE(II+NELEM)=II
          IKLE(II+2*NELEM)=II+NPLAN
          IKLE(II+3*NELEM)=IKLE(II)+NPLAN
        ENDDO
        MESHF%IKLE%I=IKLE
!
!        DEALLOCATE(IKLE)
!        DEALLOCATE(IPOBO)
!
!.......2.9 WRITES OUT THE ARRAYS X AND Y
!       """"""""""""""""""""""""""""""""
        ALLOCATE(MESHF%X%R(NPLAN*NF))
        ALLOCATE(MESHF%Y%R(NPLAN*NF))
        MESHF%NPTFR = 2*NPLAN!+2*(NF-2)
        DO JF=1,NF
          DO II=1,NPLAN
            MESHF%X%R(II+NPLAN*(JF-1))=FREQ(JF)*SIN(TETA(II))
          ENDDO
        ENDDO
        DO JF=1,NF
          DO II=1,NPLAN
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
!BD_INCKA IN PARALLEL MODE, INITIALISES THE VALUES OF MPI PARAMETERS
      IF (NCSIZE.GT.1)         CALL GET_MPI_PARAMETERS(MPI_INTEGER,
     &                          MPI_REAL8,MPI_UB,
     &                          MPI_COMM_WORLD,MPI_SUCCESS)
!BD_INCKA END OF MODIFICATION
!BD_INCKA IN PARALLEL MODE, ASSOCIATES THE SUB-DOMAIN NODE NUMBERS
! WITH THE POINTS WHERE SPECTRAL OUTPUT IS REQUIRED
        IF (NCSIZE.GT.1) THEN
        CALL SPECTRE_SEND(SPE_SEND,NSPE_RECV,NLEO,ISLEO,
     &                           NRECV_LEO)
        CALL TEXTE_SENDRECV(TEXTE(1:NLEO),NLEO,NPSPE,ISLEO,NRECV_LEO)
        ENDIF
!BD_INCKA END OF MODIFICATION
        IF (((NCSIZE.GT.1).AND.(IPID==0)).OR.NCSIZE.LE.1) THEN
        ! CREATES DATA FILE USING A GIVEN FILE FORMAT : FORMAT_RES.
        ! THE DATA ARE CREATED IN THE FILE: NRES, AND IS
        ! CHARACTERISED BY A TITLE AND NAME OF OUTPUT VARIABLES
        ! CONTAINED IN THE FILE.
        CALL CREATE_DATASET(BINSCO, ! RESULTS FILE FORMAT
     &                      NSCO,   ! LU FOR RESULTS FILE
     &                      TITCAS, ! TITLE
     &                      NLEO,   ! MAX NUMBER OF OUTPUT VARIABLES
     &                      TEXTE,  ! NAMES OF OUTPUT VARIABLES
     &                      SORLEO) ! PRINT TO FILE OR NOT
        ! WRITES THE MESH IN THE OUTPUT FILE :
        ! IN PARALLEL, REQUIRES NCSIZE AND NPTIR.
        ! THE REST OF THE INFORMATION IS IN MESH.
        ! ALSO WRITES : START DATE/TIME AND COORDINATES OF THE
        ! ORIGIN.
        CALL WRITE_MESH(BINSCO, ! RESULTS FILE FORMAT
     &                  NSCO,   ! LU FOR RESULTS FILE
     &                  MESHF,  ! CHARACTERISES MESH
     &                  1,      ! NUMBER OF PLANES /NA/
     &                  DATE,   ! START DATE
     &                  TIME,   ! START TIME
     &                  0,0)    ! COORDINATES OF THE ORIGIN.
        ENDIF
!
      ENDIF
!BD_INCKA IN PARALLEL MODE, ASSOCIATES THE SUB-DOMAIN NODE NUMBERS
! WITH THE POINTS WHERE SPECTRAL OUTPUT IS REQUIRED
        IF (NCSIZE.GT.1) THEN
        CALL SPECTRE_SEND(SPE_SEND,NSPE_RECV,NLEO,ISLEO,
     &                           NRECV_LEO)
        CALL TEXTE_SENDRECV(TEXTE,NLEO,NPSPE,ISLEO,NRECV_LEO)
        ENDIF
!BD_INCKA END OF MODIFICATION
!=====C
!  3  C RECORDS THE CURRENT TIME STEP
!=====C========================================
!
!.....3.1 WRITES OUTPUT AT TIME 'AT'
!     """"""""""""""""""""""""
      AAT(1) = AT
      ALLOCATE(BVARSOR%ADR(NLEO))
      DO II=1,NLEO
        ALLOCATE(BVARSOR%ADR(II)%P)
        ALLOCATE(BVARSOR%ADR(II)%P%R(NPSPE))
        BVARSOR%ADR(II)%P%DIM1 = NPSPE
        BVARSOR%ADR(II)%P%ELM  = 21
      ENDDO
      DO ILEO=1,NLEO
        II=NOLEO(ILEO)
        DO JF=1,NF
          DO K=1,NPLAN
            BVARSOR%ADR(ILEO)%P%R(K+(JF-1)*NPLAN)=F(II,K,JF)
          ENDDO
        ENDDO
      ENDDO
!
!BD_INCKA MODIFICATION FOR PARALLEL MODE
      IF (NCSIZE.GT.1) THEN
        CALL BVARSOR_SENDRECV(BVARSOR,NLEO,NPSPE,ISLEO,NRECV_LEO)
        IF (IPID==0) CALL WRITE_DATA(BINSCO,NSCO,99,AT,LT,SORLEO,TEXTE,
     &                BVARSOR,NPSPE)
      ELSE
        CALL WRITE_DATA(BINSCO,NSCO,99,AT,LT,SORLEO,TEXTE,
     &                BVARSOR,NPSPE)
      ENDIF
!
!
      RETURN
      END
