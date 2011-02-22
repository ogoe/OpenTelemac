C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS THE OUTPUT FROM A SERAFIN RESULT FILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  JMH 15/03/07 : THIS SUBROUTINE IS NOW OPTIMISED TO
!>                        BE CALLED A LARGE NUMBER OF TIMES WITHOUT
!>                        REWINDING AT EVERY CALL. FOR EVERY LOGICAL
!>                        UNIT A NUMBER OF DATA HAVE TO BE SAVED
!>                        BETWEEN EACH CALL.

!>  @warning  THIS WILL NOT WORK IF A LOGICAL UNIT IS CLOSED AND RE-OPENED

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALIRE, AT, CLAND, DT, FIN, HIST, LISTIN, MAXVAR, NHIST, NPLAN, NPOIN, NPRE, NUMDEB, NVARCL, STD, TEXTPR, TROUVE, VARCLA, VARSOR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, AAT, ADRA, ATPREV, CBID, ENREG, ERR, IBID, ISTAT, IVAR, KK, KNO, NLU, NPLAN_PREV, NPOIN_PREV, NREAD, NUMEN, NUMSUI, NVAR, SIZE_W, SIZE_WD, TEXTLU, TITPRE, W, WD, YAWD
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SUITE_SERAFIN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> EOF(), LIT(), LITENR(), PLANTE(), SKIPGEO()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BIEF_SUITE()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 6.0                                       </center>
!> </td><td> 09/04/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 09/12/2008
!> </td><td>
!> </td><td> STD IS NOW A STRING OF ANY SIZE
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALIRE
!></td><td>--></td><td>VARIABLES QU'IL FAUT LIRE (POUR LES AUTRES ON
!>                  SAUTE L'ENREGISTREMENT CORRESPONDANT)
!>                  LES VARIABLES CLANDESTI-NES SONT LUES
!>                  SYSTEMATIQUEMENT.
!>    </td></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>CLAND
!></td><td><--</td><td>BLOC DES VARIABLES CLANDESTINES
!>    </td></tr>
!>          <tr><td>DT
!></td><td><-></td><td>TIME STEP BETWEEN 2 RECORDS (OPTIONAL)
!>    </td></tr>
!>          <tr><td>FIN
!></td><td>--></td><td>VOIR LE TROISIEME ARGUMENT NUMDEB
!>    </td></tr>
!>          <tr><td>HIST
!></td><td>--></td><td>TABLEAU DE VALEURS MISES DANS L'ENREGISTREMENT
!>                  DU TEMPS.
!>    </td></tr>
!>          <tr><td>LISTIN
!></td><td>--></td><td>SI OUI, IMPRESSION D'INFORMATIONS SUR LISTING
!>    </td></tr>
!>          <tr><td>MAXVAR
!></td><td>--></td><td>DIMENSION DES TABLEAUX DES VARIABLES : ALIRE, ETC
!>    </td></tr>
!>          <tr><td>NHIST
!></td><td>--></td><td>NOMBRE DE VALEURS DANS LE TABLEAU HIST.
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF PLANES IN 3D (OPTIONAL)
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DANS LE MAILLAGE
!>    </td></tr>
!>          <tr><td>NPRE
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER
!>    </td></tr>
!>          <tr><td>NUMDEB
!></td><td><-></td><td>FIN = .TRUE. NUMERO DU DERNIER ENREGISTREMENT
!>                  FIN = .FALSE. : NUMERO DE L'ENREGISTREMENT
!>                  QUE L'ON VEUT LIRE.
!>    </td></tr>
!>          <tr><td>NVARCL
!></td><td>--></td><td>NOMBRE DE VARIABLES CLANDESTI-NES.
!>    </td></tr>
!>          <tr><td>STD
!></td><td>--></td><td>BINAIRE DU FICHIER : STD, IBM OU I3E
!>    </td></tr>
!>          <tr><td>TEXTPR
!></td><td>--></td><td>NOMS ET UNITES DES VARIABLES.
!>    </td></tr>
!>          <tr><td>TROUVE
!></td><td><--</td><td>INDIQUE (TROUVE(K)=1) LES VARIABLES TROUVEES
!>                  DANS LE FICHIER.
!>    </td></tr>
!>          <tr><td>VARCLA
!></td><td>--></td><td>TABLEAU OU L'ON RANGE LES VARIABLES
!>                  CLANDESTINES.
!>    </td></tr>
!>          <tr><td>VARSOR
!></td><td><--</td><td>BLOC DES TABLEAUX CONTENANT LES VARIABLES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SUITE_SERAFIN
     &(VARSOR,CLAND,NUMDEB,NPRE,STD,HIST,NHIST,NPOIN,AT,TEXTPR,VARCLA,
     & NVARCL,TROUVE,ALIRE,LISTIN,FIN,MAXVAR,NPLAN,DT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALIRE          |-->| VARIABLES QU'IL FAUT LIRE (POUR LES AUTRES ON
C|                |   | SAUTE L'ENREGISTREMENT CORRESPONDANT)
C|                |   | LES VARIABLES CLANDESTI-NES SONT LUES
C|                |   | SYSTEMATIQUEMENT.
C| AT             |-->| TEMPS
C| CLAND          |<--| BLOC DES VARIABLES CLANDESTINES
C| DT             |<->| TIME STEP BETWEEN 2 RECORDS (OPTIONAL)
C| FIN            |-->| VOIR LE TROISIEME ARGUMENT NUMDEB
C| HIST           |-->| TABLEAU DE VALEURS MISES DANS L'ENREGISTREMENT
C|                |   | DU TEMPS.
C| LISTIN         |-->| SI OUI, IMPRESSION D'INFORMATIONS SUR LISTING
C| MAXVAR         |-->| DIMENSION DES TABLEAUX DES VARIABLES : ALIRE, ETC
C| NHIST          |-->| NOMBRE DE VALEURS DANS LE TABLEAU HIST.
C| NPLAN          |-->| NUMBER OF PLANES IN 3D (OPTIONAL)
C| NPOIN          |-->| NOMBRE DE POINTS DANS LE MAILLAGE
C| NPRE           |-->| NUMERO DE CANAL DU FICHIER
C| NUMDEB         |<->| FIN = .TRUE. NUMERO DU DERNIER ENREGISTREMENT
C|                |   | FIN = .FALSE. : NUMERO DE L'ENREGISTREMENT
C|                |   | QUE L'ON VEUT LIRE.
C| NVARCL         |-->| NOMBRE DE VARIABLES CLANDESTI-NES.
C| STD            |-->| BINAIRE DU FICHIER : STD, IBM OU I3E
C| TEXTPR         |-->| NOMS ET UNITES DES VARIABLES.
C| TROUVE         |<--| INDIQUE (TROUVE(K)=1) LES VARIABLES TROUVEES
C|                |   | DANS LE FICHIER.
C| VARCLA         |-->| TABLEAU OU L'ON RANGE LES VARIABLES
C|                |   | CLANDESTINES.
C| VARSOR         |<--| BLOC DES TABLEAUX CONTENANT LES VARIABLES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SUITE_SERAFIN => SUITE_SERAFIN
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
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
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     100 IS HERE THE MAXIMUM LOGICAL UNIT IN FORTRAN
      INTEGER, PARAMETER :: NLU  =  100
      INTEGER IBID(1),ISTAT,NVAR,IVAR,KK,ERR
      INTEGER NUMSUI,NPLAN_PREV(NLU),NPOIN_PREV(NLU),NREAD
C
      DOUBLE PRECISION AAT(20),ATPREV
      REAL, ALLOCATABLE :: W(:)
      DOUBLE PRECISION, ALLOCATABLE :: WD(:)
C
      CHARACTER(LEN=1) :: CBID
C
      CHARACTER(LEN=72) :: TITPRE
C     10 : LIMIT OF 10 FILES READ BY SUITE IN A SINGLE RUN
C     100 IS HERE A MAXIMUM FOR MAXVAR
      CHARACTER(LEN=32) :: TEXTLU(100,10)
C
      LOGICAL YAWD
C
      INTEGER SIZE_W,SIZE_WD,ADRA,NUMEN(NLU)
      DATA SIZE_W/0/
      DATA SIZE_WD/0/
      DATA ADRA/0/
C                NLU
      DATA NUMEN/100*0/
C
C     KNO   : NUMBER OF RECORDS PER TIME STEP, INCLUDING TIME
C     ENREG : LAST RECORD READ, STARTING AFTER THE GEOMETRY
C     A     : ADDRESS IN TEXTLU FOR LOGICAL UNIT NPRE
C
      INTEGER KNO(NLU),ENREG(NLU),A(NLU)
C
      SAVE W,SIZE_W,SIZE_WD,NUMEN,KNO,ENREG,TEXTLU,NPOIN_PREV,NPLAN_PREV
      SAVE A,ADRA,WD
C
C-----------------------------------------------------------------------
C
      IF(NUMEN(NPRE).EQ.0) THEN
C       A NEW FILE, ADDRESS IN TEXTLU ATTRIBUTED
        ADRA=ADRA+1
        IF(ADRA.GT.10) STOP 'LIMITATION TO 10 FILES IN SUITE'
        A(NPRE)=ADRA
      ENDIF
C
      YAWD = .FALSE.
C
C-----------------------------------------------------------------------
C
      IF(NUMEN(NPRE).EQ.0) THEN
C
C     READS THE GEOMETRY SECTION FROM THE FILE
C
C     STANDARD SELAFIN
C
      CALL SKIPGEO(NPRE,TITPRE,NPOIN_PREV(NPRE),NVAR,TEXTLU(1,A(NPRE)),
     &             NPLAN_PREV(NPRE))
      ENREG(NPRE)=0
C
      IF(NPOIN_PREV(NPRE).EQ.NPOIN) THEN
C
C       OK, NOTHING TO DO
C
      ELSEIF(.NOT.PRESENT(NPLAN).OR.NPLAN_PREV(NPRE).EQ.0) THEN
C
C       ERROR
C
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,3222)
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,3223)
3222    FORMAT(1X,'SUITE_SERAFIN : MAILLAGE (OU STANDARD FAUX), ARRET')
3223    FORMAT(1X,'SUITE_SERAFIN : DIFFERENT MESH (OR WRONG STANDARD)')
        CALL PLANTE(1)
        STOP
C
      ELSEIF(NPOIN_PREV(NPRE)/NPLAN_PREV(NPRE).EQ.NPOIN/NPLAN) THEN
C
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
C
      ELSE
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,3222)
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,3223)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      ERR=0
C
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
C
C-----------------------------------------------------------------------
C
C  PRINTOUTS :
C
      IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,300) TITPRE
      IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,301) TITPRE
300   FORMAT(1X,//,1X,'TITRE DU CAS PRECEDENT: ',A72,/)
301   FORMAT(1X,//,1X,'TITLE OF PREVIOUS COMPUTATION: ',A72,/)
C
      DO 10 IVAR=1,NVAR
        IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,11)
     &    TEXTLU(IVAR,A(NPRE))(1:16),TEXTLU(IVAR,A(NPRE))(17:32)
        IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,111)
     &    TEXTLU(IVAR,A(NPRE))(1:16),TEXTLU(IVAR,A(NPRE))(17:32)
11      FORMAT(1X,'NOM: ' ,A16,'  UNITE: ',A16)
111     FORMAT(1X,'NAME: ',A16,'  UNIT: ' ,A16)
10    CONTINUE
C
C-----------------------------------------------------------------------
C               COUNTS THE NUMBER OF RECORDS
C-----------------------------------------------------------------------
C
C     NUMBER OF VARIABLES PER TIMESTEP (TIME INCLUDED)
C
      KNO(NPRE) = NVAR + 1
C
C READS TILL THE END OF THE FILE WITHOUT RECORDING WHAT'S BEEN READ
C TO COUNT THE NUMBER OF RECORDS
C
20    CONTINUE
C
      DO KK=1,KNO(NPRE)
C       TESTS THE END OF THE FILE
        IF(EOF(NPRE)) GO TO 50
        CALL LIT(AAT,W,IBID,CBID,1,'R4',NPRE,STD,ISTAT)
C       ISTAT IS NEGATIVE : READ ERROR
        IF(ISTAT.LT.0) GO TO 40
      ENDDO
C
      NUMEN(NPRE)  =  NUMEN(NPRE)  + 1
      GO TO 20
C
C     ERROR IN THE PREVIOUS COMPUTATION RESULTS FILE
C
40    IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,1010) NUMEN(NPRE)+1
      IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,1011) NUMEN(NPRE)+1
1010  FORMAT(1X,'SUITE_SERAFIN :',/,1X,
     &          'ERREUR DE LECTURE DANS L''ENREGISTREMENT ',I5)
1011  FORMAT(1X,'SUITE_SERAFIN : READ ERROR IN RECORD NUMBER ',I5)
C
C DETERMINES THE LAST RECORD
C
50    CONTINUE
C
C READS THE FILE AGAIN TO STOP RIGHT BEFORE THE RECORD TO READ
C AND CALL SUBROUTINE LITENR THEN.
C
      CALL SKIPGEO(NPRE,TITPRE,NPOIN_PREV(NPRE),
     &             NVAR,TEXTLU(1,A(NPRE)),NPLAN_PREV(NPRE))
      ENREG(NPRE)=0
C
C     IF(NUMEN(NPRE).EQ.0)
      ENDIF
C
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
C
C-----------------------------------------------------------------------
C               READS THE RESULT RECORDS
C-----------------------------------------------------------------------
C
C     SKIPD RECORDS THAT WILL NOT BE READ
C
      IF(FIN) THEN
        NUMSUI=NUMEN(NPRE)
        NUMDEB=NUMEN(NPRE)
      ELSE
        NUMSUI=NUMDEB
      ENDIF
C
135   CONTINUE
      IF(NUMSUI.GE.1) THEN
        NREAD=KNO(NPRE)*(NUMSUI-1)-ENREG(NPRE)
        IF(NREAD.GE.1) THEN
          DO KK = 1 , NREAD
            READ(UNIT=NPRE,END=133,ERR=133)
            ENREG(NPRE)=ENREG(NPRE)+1
          ENDDO
        ELSEIF(NREAD.LT.0) THEN
C         GOING AGAIN AT THE BEGINNING
          CALL SKIPGEO(NPRE,TITPRE,NPOIN_PREV(NPRE),
     &                 NVAR,TEXTLU(1,A(NPRE)),NPLAN_PREV(NPRE))
          ENREG(NPRE)=0
          GO TO 135
        ENDIF
      ENDIF
      GO TO 134
C
133   CONTINUE
      IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,1010) ((KK-1)/KNO(NPRE))+1
      IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,1011) ((KK-1)/KNO(NPRE))+1
      CALL PLANTE(1)
      STOP
C
C     READS THE RECORD:
C
134   CONTINUE
C
C     THE TIMESTEP IN THE FILE IS REQUESTED
C
      IF(PRESENT(DT)) THEN
        IF(NUMEN(NPRE).GT.1) THEN
          DO KK=1,KNO(NPRE)
            BACKSPACE (UNIT=NPRE)
          ENDDO
C         READS TIME OF PREVIOUS RECORD
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
C
      IF(YAWD) THEN
      CALL LITENR(VARSOR,CLAND,
     &            NPRE,STD,HIST,NHIST,NPOIN,AT,TEXTPR,TEXTLU(1,A(NPRE)),
     &            KNO(NPRE)-1,
     &            VARCLA,NVARCL,TROUVE,ALIRE,W,LISTIN,MAXVAR,
     &            NPOIN_PREV(NPRE),NPLAN_PREV(NPRE),WD)
      ELSE
C     CASE WHERE WD HAS NOT BEEN ALLOCATED (PASSING IT IN ARGUMENT IS A MISTAKE)
      CALL LITENR(VARSOR,CLAND,
     &            NPRE,STD,HIST,NHIST,NPOIN,AT,TEXTPR,TEXTLU(1,A(NPRE)),
     &            KNO(NPRE)-1,
     &            VARCLA,NVARCL,TROUVE,ALIRE,W,LISTIN,MAXVAR,
     &            NPOIN_PREV(NPRE),NPLAN_PREV(NPRE))
      ENDIF
      ENREG(NPRE)=ENREG(NPRE)+KNO(NPRE)
C
      IF(PRESENT(DT)) DT=AT-ATPREV
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C