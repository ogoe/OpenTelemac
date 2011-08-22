!                    ************************
                     SUBROUTINE SUITE_SERAFIN
!                    ************************
!
     &(VARSOR,CLAND,NUMDEB,NPRE,STD,HIST,NHIST,NPOIN,AT,TEXTPR,VARCLA,
     & NVARCL,TROUVE,ALIRE,LISTIN,FIN,MAXVAR,NPLAN,DT)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS THE OUTPUT FROM A SERAFIN RESULT FILE.
!
!note     JMH 15/03/07 : THIS SUBROUTINE IS NOW OPTIMISED TO
!+                        BE CALLED A LARGE NUMBER OF TIMES WITHOUT
!+                        REWINDING AT EVERY CALL. FOR EVERY LOGICAL
!+                        UNIT A NUMBER OF DATA HAVE TO BE SAVED
!+                        BETWEEN EACH CALL.
!
!warning  THIS WILL NOT WORK IF A LOGICAL UNIT IS CLOSED AND RE-OPENED
!
!history
!+        09/12/2008
!+
!+   STD IS NOW A STRING OF ANY SIZE
!
!history  J-M HERVOUET (LNHE)
!+        09/04/2009
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALIRE          |-->| VARIABLES TO BE READ (FOR OTHERS THE RECORD IS
!|                |   | SKIPPED). CLANDESTINE VARIABLES ARE READ
!|                |   | SYSTEMATICALLY.
!| AT             |-->| TIME
!| CLAND          |<--| BLOCK OF CLANDESTINE VARIABLES
!| DT             |<->| TIME STEP BETWEEN 2 RECORDS (OPTIONAL)
!| FIN            |-->| SEE NUMDEB
!| HIST           |-->| ARRAY OF VALUES THAT MAY BE PUT IN THE TIME RECORD
!| LISTIN         |-->| IF YES,  PRINTING INFORMATIONS ON LISTING
!| MAXVAR         |-->| SIZE OF ARRAYS OF VARIABLES : ALIRE, ETC
!| NHIST          |-->| NUMBER OF VALUES IN HIST
!| NPLAN          |-->| NUMBER OF PLANES IN 3D (OPTIONAL)
!| NPOIN          |-->| NUMBER OF POINTS
!| NPRE           |-->| LOGICAL UNIT OF FILE
!| NUMDEB         |<->| FIN = .TRUE. WILL BE THE LAST RECORD NUMBER
!|                |   | FIN = .FALSE. : PRESCRIBED RECORD TO BE READ
!| NVARCL         |-->| NUMBER OF CLANDESTINE VARIABLES.
!| STD            |-->| FILE BINARY: STD, IBM OR I3E
!| TEXTPR         |-->| NAMES AND UNITS OF VARIABLES.
!| TROUVE         |<--| GIVES (TROUVE(K)=1) VARIABLES FOUND IN THE FILE
!| VARCLA         |-->| BLOCK WHERE TO PUT CLANDESTINE VARIABLES
!| VARSOR         |<--| BLOCK CONTAINING VARIABLES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SUITE_SERAFIN => SUITE_SERAFIN
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VARSOR,CLAND
      INTEGER, INTENT(IN), OPTIONAL :: NPLAN
      INTEGER, INTENT(IN)           :: NHIST,NVARCL,MAXVAR
      INTEGER                       :: NUMDEB,NPRE,NPOIN,TROUVE(MAXVAR)
      INTEGER                       :: ALIRE(MAXVAR)
      CHARACTER(LEN=*)              :: STD
      CHARACTER(LEN=32)             :: TEXTPR(MAXVAR),VARCLA(NVARCL)
      DOUBLE PRECISION              :: HIST(*),AT
      DOUBLE PRECISION, INTENT(OUT) , OPTIONAL :: DT
      LOGICAL                       :: FIN,LISTIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     100 IS HERE THE MAXIMUM LOGICAL UNIT IN FORTRAN
      INTEGER, PARAMETER :: NLU  =  100
      INTEGER IBID(1),ISTAT,NVAR,IVAR,KK,ERR
      INTEGER NUMSUI,NPLAN_PREV(NLU),NPOIN_PREV(NLU),NREAD
!
      DOUBLE PRECISION AAT(20),ATPREV
      REAL, ALLOCATABLE :: W(:)
      DOUBLE PRECISION, ALLOCATABLE :: WD(:)
!
      CHARACTER(LEN=1) :: CBID
!
      CHARACTER(LEN=72) :: TITPRE
!     10 : LIMIT OF 10 FILES READ BY SUITE IN A SINGLE RUN
!     100 IS HERE A MAXIMUM FOR MAXVAR
      CHARACTER(LEN=32) :: TEXTLU(100,10)
!
      LOGICAL YAWD
!
      INTEGER SIZE_W,SIZE_WD,ADRA,NUMEN(NLU)
      DATA SIZE_W/0/
      DATA SIZE_WD/0/
      DATA ADRA/0/
!                NLU
      DATA NUMEN/100*0/
!
!     KNO   : NUMBER OF RECORDS PER TIME STEP, INCLUDING TIME
!     ENREG : LAST RECORD READ, STARTING AFTER THE GEOMETRY
!     A     : ADDRESS IN TEXTLU FOR LOGICAL UNIT NPRE
!
      INTEGER KNO(NLU),ENREG(NLU),A(NLU)
!
      SAVE W,SIZE_W,SIZE_WD,NUMEN,KNO,ENREG,TEXTLU,NPOIN_PREV,NPLAN_PREV
      SAVE A,ADRA,WD
!
!-----------------------------------------------------------------------
!
      IF(NUMEN(NPRE).EQ.0) THEN
!       A NEW FILE, ADDRESS IN TEXTLU ATTRIBUTED
        ADRA=ADRA+1
        IF(ADRA.GT.10) STOP 'LIMITATION TO 10 FILES IN SUITE'
        A(NPRE)=ADRA
      ENDIF
!
      YAWD = .FALSE.
!
!-----------------------------------------------------------------------
!
      IF(NUMEN(NPRE).EQ.0) THEN
!
!     READS THE GEOMETRY SECTION FROM THE FILE
!
!     STANDARD SELAFIN
!
      CALL SKIPGEO(NPRE,TITPRE,NPOIN_PREV(NPRE),NVAR,TEXTLU(1,A(NPRE)),
     &             NPLAN_PREV(NPRE))
      ENREG(NPRE)=0
!
      IF(NPOIN_PREV(NPRE).EQ.NPOIN) THEN
!
!       OK, NOTHING TO DO
!
      ELSEIF(.NOT.PRESENT(NPLAN).OR.NPLAN_PREV(NPRE).EQ.0) THEN
!
!       ERROR
!
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,3222)
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,3223)
3222    FORMAT(1X,'SUITE_SERAFIN : MAILLAGE (OU STANDARD FAUX), ARRET')
3223    FORMAT(1X,'SUITE_SERAFIN : DIFFERENT MESH (OR WRONG STANDARD)')
        CALL PLANTE(1)
        STOP
!
      ELSEIF(NPOIN_PREV(NPRE)/NPLAN_PREV(NPRE).EQ.NPOIN/NPLAN) THEN
!
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,3224)
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,3225)
3224    FORMAT(1X,'SUITE_SERAFIN : NOMBRE DE PLANS DIFFERENT',/,1X,
     &            '                LES DONNEES SERONT INTERPOLEES')
3225    FORMAT(1X,'SUITE_SERAFIN: DIFFERENT NUMBER OF PLANES',/,1X,
     &            '               DATA WILL BE INTERPOLATED')
        IF(SIZE_WD.EQ.0) THEN
          ALLOCATE(WD(NPOIN_PREV(NPRE)),STAT=ERR)
        ELSEIF(NPOIN_PREV(NPRE).GT.SIZE_WD) THEN
          DEALLOCATE(WD)
          ALLOCATE(WD(NPOIN_PREV(NPRE)),STAT=ERR)
          SIZE_WD=NPOIN_PREV(NPRE)
        ENDIF
        IF(ERR.NE.0) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'SUITE_SERAFIN : MAUVAISE ALLOCATION DE WD'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'SUITE_SERAFIN: WRONG ALLOCATION OF WD'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        YAWD=.TRUE.
!
      ELSE
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,3222)
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,3223)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      ERR=0
!
      IF(SIZE_W.EQ.0) THEN
        ALLOCATE(W(NPOIN_PREV(NPRE)),STAT=ERR)
        SIZE_W=NPOIN_PREV(NPRE)
      ELSEIF(NPOIN_PREV(NPRE).GT.SIZE_W) THEN
        DEALLOCATE(W)
        ALLOCATE(W(NPOIN_PREV(NPRE)),STAT=ERR)
        SIZE_W=NPOIN_PREV(NPRE)
      ENDIF
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'SUITE_SERAFIN : MAUVAISE ALLOCATION DE W'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'SUITE_SERAFIN: WRONG ALLOCATION OF W'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!  PRINTOUTS :
!
      IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,300) TITPRE
      IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,301) TITPRE
300   FORMAT(1X,//,1X,'TITRE DU CAS PRECEDENT: ',A72,/)
301   FORMAT(1X,//,1X,'TITLE OF PREVIOUS COMPUTATION: ',A72,/)
!
      DO 10 IVAR=1,NVAR
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,11)
     &    TEXTLU(IVAR,A(NPRE))(1:16),TEXTLU(IVAR,A(NPRE))(17:32)
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,111)
     &    TEXTLU(IVAR,A(NPRE))(1:16),TEXTLU(IVAR,A(NPRE))(17:32)
11      FORMAT(1X,'NOM: ' ,A16,'  UNITE: ',A16)
111     FORMAT(1X,'NAME: ',A16,'  UNIT: ' ,A16)
10    CONTINUE
!
!-----------------------------------------------------------------------
!               COUNTS THE NUMBER OF RECORDS
!-----------------------------------------------------------------------
!
!     NUMBER OF VARIABLES PER TIMESTEP (TIME INCLUDED)
!
      KNO(NPRE) = NVAR + 1
!
! READS TILL THE END OF THE FILE WITHOUT RECORDING WHAT'S BEEN READ
! TO COUNT THE NUMBER OF RECORDS
!
20    CONTINUE
!
      DO KK=1,KNO(NPRE)
!       TESTS THE END OF THE FILE
        IF(EOF(NPRE)) GO TO 50
        CALL LIT(AAT,W,IBID,CBID,1,'R4',NPRE,STD,ISTAT)
!       ISTAT IS NEGATIVE : READ ERROR
        IF(ISTAT.LT.0) GO TO 40
      ENDDO
!
      NUMEN(NPRE)  =  NUMEN(NPRE)  + 1
      GO TO 20
!
!     ERROR IN THE PREVIOUS COMPUTATION RESULTS FILE
!
40    IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,1010) NUMEN(NPRE)+1
      IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,1011) NUMEN(NPRE)+1
1010  FORMAT(1X,'SUITE_SERAFIN :',/,1X,
     &          'ERREUR DE LECTURE DANS L''ENREGISTREMENT ',I5)
1011  FORMAT(1X,'SUITE_SERAFIN : READ ERROR IN RECORD NUMBER ',I5)
!
! DETERMINES THE LAST RECORD
!
50    CONTINUE
!
! READS THE FILE AGAIN TO STOP RIGHT BEFORE THE RECORD TO READ
! AND CALL SUBROUTINE LITENR THEN.
!
      CALL SKIPGEO(NPRE,TITPRE,NPOIN_PREV(NPRE),
     &             NVAR,TEXTLU(1,A(NPRE)),NPLAN_PREV(NPRE))
      ENREG(NPRE)=0
!
!     IF(NUMEN(NPRE).EQ.0)
      ENDIF
!
      IF(NUMEN(NPRE).EQ.0) THEN
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,120)
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,121)
120     FORMAT(1X,'SUITE_SERAFIN : AUCUN ENREGISTREMENT TROUVE')
121     FORMAT(1X,'SUITE_SERAFIN : NO RECORD FOUND')
        CALL PLANTE(1)
        STOP
      ELSEIF(FIN) THEN
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,130) NUMEN(NPRE)
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,131) NUMEN(NPRE)
130     FORMAT(/,1X,'SUITE_SERAFIN : LECTURE A L''ENREGISTREMENT ',1I5)
131     FORMAT(/,1X,'SUITE_SERAFIN : READ OF RECORD ',1I5)
      ELSE
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,130) NUMDEB
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,131) NUMDEB
      ENDIF
!
!-----------------------------------------------------------------------
!               READS THE RESULT RECORDS
!-----------------------------------------------------------------------
!
!     SKIPD RECORDS THAT WILL NOT BE READ
!
      IF(FIN) THEN
        NUMSUI=NUMEN(NPRE)
        NUMDEB=NUMEN(NPRE)
      ELSE
        NUMSUI=NUMDEB
      ENDIF
!
135   CONTINUE
      IF(NUMSUI.GE.1) THEN
        NREAD=KNO(NPRE)*(NUMSUI-1)-ENREG(NPRE)
        IF(NREAD.GE.1) THEN
          DO KK = 1 , NREAD
            READ(UNIT=NPRE,END=133,ERR=133)
            ENREG(NPRE)=ENREG(NPRE)+1
          ENDDO
        ELSEIF(NREAD.LT.0) THEN
!         GOING AGAIN AT THE BEGINNING
          CALL SKIPGEO(NPRE,TITPRE,NPOIN_PREV(NPRE),
     &                 NVAR,TEXTLU(1,A(NPRE)),NPLAN_PREV(NPRE))
          ENREG(NPRE)=0
          GO TO 135
        ENDIF
      ENDIF
      GO TO 134
!
133   CONTINUE
      IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,1010) ((KK-1)/KNO(NPRE))+1
      IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,1011) ((KK-1)/KNO(NPRE))+1
      CALL PLANTE(1)
      STOP
!
!     READS THE RECORD:
!
134   CONTINUE
!
!     THE TIMESTEP IN THE FILE IS REQUESTED
!
      IF(PRESENT(DT)) THEN
        IF(NUMEN(NPRE).GT.1) THEN
          DO KK=1,KNO(NPRE)
            BACKSPACE (UNIT=NPRE)
          ENDDO
!         READS TIME OF PREVIOUS RECORD
          CALL LIT(AAT,W,IBID,CBID,1,'R4',NPRE,STD,ISTAT)
          ATPREV = AAT(1)
          DO KK=1,KNO(NPRE)-1
            READ(UNIT=NPRE)
          ENDDO
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'SUITE_SERAFIN : UN SEUL ENREGISTREMENT'
            WRITE(LU,*) '                PAS DE TEMPS DT IMPOSSIBLE'
            WRITE(LU,*) '                A TROUVER SUR CANAL ',NPRE
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'SUITE_SERAFIN: ONLY ONE RECORD, THE TIME STEP'
            WRITE(LU,*) '               IS IMPOSSIBLE TO FIND'
            WRITE(LU,*) '               ON LOGICAL UNIT ',NPRE
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      IF(YAWD) THEN
      CALL LITENR(VARSOR,CLAND,
     &            NPRE,STD,HIST,NHIST,NPOIN,AT,TEXTPR,TEXTLU(1,A(NPRE)),
     &            KNO(NPRE)-1,
     &            VARCLA,NVARCL,TROUVE,ALIRE,W,LISTIN,MAXVAR,
     &            NPOIN_PREV(NPRE),NPLAN_PREV(NPRE),WD)
      ELSE
!     CASE WHERE WD HAS NOT BEEN ALLOCATED (PASSING IT IN ARGUMENT IS A MISTAKE)
      CALL LITENR(VARSOR,CLAND,
     &            NPRE,STD,HIST,NHIST,NPOIN,AT,TEXTPR,TEXTLU(1,A(NPRE)),
     &            KNO(NPRE)-1,
     &            VARCLA,NVARCL,TROUVE,ALIRE,W,LISTIN,MAXVAR,
     &            NPOIN_PREV(NPRE),NPLAN_PREV(NPRE))
      ENDIF
      ENREG(NPRE)=ENREG(NPRE)+KNO(NPRE)
!
      IF(PRESENT(DT)) DT=AT-ATPREV
!
!-----------------------------------------------------------------------
!
      RETURN
      END
