!                    *******************
                     SUBROUTINE READGEO1
!                    *******************
!
     &(NPOIN,NELEM,NPTFR,NDP,IB,NFIC,NELEBD)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS OR COMPUTES THE VALUES OF NPOIN, NELEM, NPTFR,
!+                MXPTVS, MXELVS IN THE GEOMETRY FILE (CHANNEL NGEO).
!
!warning  USER SUBROUTINE (MAY BE REWRITTEN FOR ANOTHER FILE FORMAT)
!
!history  J-M HERVOUET (LNH)     ; REGINA NEBAUER; LAM MINH PHUONG
!+        29/04/04
!+        V5P5
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
!| IB             |<--| 10 INTEGERS, SEE SELAFIN FILE STANDARD
!| NDP            |<--| NUMBER OF NODES PER ELEMENT
!| NELEBD         |<--| NUMBER OF BOUNDARY ELEMENTS
!| NELEM          |<--| NUMBER OF ELEMENTS IN THE MESH
!| NFIC           |-->| LOGICAL UNIT FOR GEOMETRY FILE
!| NPOIN          |<--| NUMBER OF POINTS IN THE MESH
!| NPTFR          |<--| NUMBER OF BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_READGEO1 => READGEO1
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(OUT)         :: NPOIN  ! NUMBER OF MESH NODES
      INTEGER, INTENT(OUT)         :: NELEM  ! NUMBER OF ELEMENTS
      INTEGER, INTENT(OUT)         :: NDP    ! NUMBER OF ELEMENT FACES
      INTEGER, INTENT(OUT)         :: IB(10) ! INTEGER ARRAY
      INTEGER, INTENT(OUT)         :: NPTFR  ! NUMBER OF BOUNDARY NODES
      INTEGER, INTENT(IN)          :: NFIC   ! FILE TO READ
      INTEGER,OPTIONAL,INTENT(OUT) :: NELEBD ! NUMBER OF BOUNDARY ELEMENTS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION  :: XB(2)
      REAL              :: RB(2)
      INTEGER           :: ISTAT
      INTEGER           :: NVAR
      INTEGER           :: I,IB6(6)
      CHARACTER(LEN=2)  :: CB
      CHARACTER(LEN=72) :: TITRE
!
!-----------------------------------------------------------------------
!
!   GOES TO THE BEGINNING OF THE FILE
!
      REWIND NFIC
!
!     1: TITLE
      CALL LIT(XB,RB,IB,TITRE,72,'CH',NFIC,'STD',ISTAT)
!
!     2: NUMBER OF ARRAYS IN THE RESULT FILE
      CALL LIT(XB,RB,IB,CB,2,'I ',NFIC,'STD',ISTAT)
      NVAR =  IB(1)  +  IB(2)
!     3: NAMES AND UNITS OF VARIABLES
      IF(NVAR.GE.1) THEN
        DO I=1,NVAR
          CALL LIT(XB,RB,IB,CB,2,'CH',NFIC,'STD',ISTAT)
        ENDDO
      ENDIF
!
!     4: LIST OF 10 INTEGER PARAMETERS
      CALL LIT(XB,RB,IB,CB,10,'I ',NFIC,'STD',ISTAT)
!
!     CASE WHERE DATE AND TIME ARE IN THE FILE
      IF(IB(10).EQ.1) CALL LIT(XB,RB,IB6,CB,6,'I ',NFIC,'STD',ISTAT)
!
!     READS THE NUMBER OF BOUNDARY ELEMENTS FOR 3D MESH
      IF(IB(7).NE.0.AND.PRESENT(NELEBD)) THEN
        NELEBD = IB(7)
      END IF
!     CASE WHERE KNOLG IS GIVEN INSTEAD OF IPOBO (PARALLEL MODE)
      IF(IB(8).NE.0) THEN
        NPTFR=IB(8)
!       NOTE JMH : NEXT LINE MOVED AFTER ENDIF ON 22/07/02
!                  SUBDOMAINS MAY HAVE NPTFR=0
!       NPTIR=IB(9)
      ENDIF
      NPTIR=IB(9)
!
!     5: 4 INTEGERS
      CALL LIT(XB,RB,IB6,CB,4,'I ',NFIC,'STD',ISTAT)
!
      NELEM = IB6(1)
      NPOIN = IB6(2)
      NDP   = IB6(3)
!
!-----------------------------------------------------------------------
!
!  PRINTOUT FORMATS:
!
      IF(LNG.EQ.1) WRITE(LU,300) TITRE
      IF(LNG.EQ.1) WRITE(LU,500) NELEM,NPOIN
      IF(LNG.EQ.2) WRITE(LU,301) TITRE
      IF(LNG.EQ.2) WRITE(LU,501) NELEM,NPOIN
!
      IF(NPOIN.LT.3) THEN
        IF(LNG.EQ.1) WRITE(LU,23) NPOIN
        IF(LNG.EQ.2) WRITE(LU,24) NPOIN
        CALL PLANTE(1)
        STOP
      ENDIF
!
23    FORMAT(1X,'READGEO1 : NOMBRE DE POINTS DU MAILLAGE : ',1I9,/,1X,
     &          '           NOMBRE DE POINTS DE FRONTIERE: ',1I8,/,1X,
     &          '           DONNEES ERRONEES, ARRET DU PROGRAMME')
24    FORMAT(1X,'READGEO1 : NUMBER OF POINTS IN THE MESH: ',1I9,/,1X,
     &          '           NUMBER OF BOUNDARY POINTS: ',1I8,/,1X,
     &          '           WRONG DATA, PROGRAMME STOPPED')
300   FORMAT(1X,//,1X,'READGEO1 : TITRE= ',A72,/)
301   FORMAT(1X,//,1X,'READGEO1: TITLE= ',A72,/)
500   FORMAT(1X,'NOMBRE D''ELEMENTS:',1I9,/,
     &       1X,'NOMBRE REEL DE POINTS:',1I9)
501   FORMAT(1X,'NUMBER OF ELEMENTS:',1I9,/,
     &       1X,'NUMBER OF POINTS:',1I9)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
