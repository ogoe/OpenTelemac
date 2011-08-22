!                    *****************
                     SUBROUTINE ECRGEO
!                    *****************
!
     &(X,Y,NPOIN,NBOR,NFIC,NVAR,TEXTE,VARCLA,NVARCL,
     & TITRE,SORLEO,NSOR,IKLE,NELEM,NPTFR,NDP,DATE,TIME,
     & NCSIZE,NPTIR,KNOLG,NPLAN,I3,I4)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    WRITES THE GEOMETRY FILE TO SELAFIN FORMAT.
!code
!+    LIST OF THE RECORDS IN THE GEOMETRY FILE:
!+
!+      1    : TITLE
!+      2    : NUMBER OF FUNCTIONS READ FROM GRID 1 AND GRID 2
!+      3    : NAME AND UNIT OF THE VARIABLES
!+      4    : 1,0,0,0,0,0,0,0,0,0
!+      5    : NELEM,NPOIN,NDP,1
!+      6    : IKLE
!+      7    : IPOBO ARRAY OF DIMENSION NPOIN
!+             0 FOR INTERIOR POINTS, A NUMBER OTHERWISE
!+      8    : X
!+      9    : Y
!+
!+    WHAT FOLLOWS IS NOT DONE IN FM3SEL
!+
!+     10    : TIME
!+     11    : VARIABLES DECLARED IN 3 (IN THE ORDER OF THE DECLARATIONS)
!
!note     JMH 01/12/2003 : VARCLA,NVARCL ARE NO LONGER USED.
!
!history  J-M HERVOUET (LNH)
!+        27/12/05
!+        V5P6
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
!| DATE           |-->| DATE (3 INTEGERS) 
!| I3,I4          |-->| INTEGERS, WILL BE PUT IN FILE IN POSITION 3
!|                |   | AND 4 OF THE ARRAY OF 10 INTEGERS
!| IKLE           |<->| CONNECTIVITY TABLE
!| KNOLG          |-->| GLOBAL NUMBERS OF LOCAL POINTS IN PARALLEL
!| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS.
!| NCSIZE         |-->| NUMBER OF PROCESSORS
!| NDP            |<->| NUMBER OF NODES PER ELEMENT
!| NELEM          |<->| NUMBER OF ELEMENTS IN THE MESH
!| NFIC           |-->| LOGICAL UNIT OF FILE TO BE READ
!| NPLAN          |-->| NUMBER OF PLANES (3D MESHES IN PRISMS)
!| NPOIN          |<->| NUMBER OF POINTS IN THE MESH
!| NPTFR          |<->| NUMBER OF BOUNDARY POINTS IN THE MESH
!| NPTIR          |-->| NUMBER OF INTERFACE POINTS IN PARALLEL
!| NSOR           |-->| DIMENSION OF SORLEO AND SORIMP
!| NVAR           |<->| NUMBER OF VARIABLES IN THE MESH
!| NVARCL         |-->| NUMBER OF CLANDESTINE VARIABLES.
!| SORLEO         |-->| SAYS WHICH VARIABLES TO BE WRITTEN IN THE FILE
!|                |   | (ARRAY OF LOGICAL)
!| TEXTE          |<->| NAMES AND UNITS OF VARIABLES.
!| TIME           |-->| TIME (3 INTEGERS)
!| TITRE          |<->| TITLE OF FILE
!| VARCLA         |-->| ARRAY WITH NAMES OF CLANDESTINE VARIABLES
!| X,Y            |<->| MESH COORDINATES.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NFIC,NVARCL,NSOR,NELEM,NPTFR,NDP
      INTEGER, INTENT(OUT) :: NVAR
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*)
!                                    IKLE(NELEM,NDP)
      INTEGER, INTENT(IN) :: NBOR(*),IKLE(*)
      CHARACTER(LEN=32), INTENT(IN) :: TEXTE(*),VARCLA(NVARCL)
!                                            NSOR      NSOR+NVARCL
      CHARACTER(LEN=72), INTENT(IN) :: TITRE
      LOGICAL, INTENT(IN) :: SORLEO(*)
      INTEGER, INTENT(IN) :: NCSIZE,NPTIR
      INTEGER, INTENT(IN) :: TIME(3),DATE(3)
      INTEGER, INTENT(IN) :: KNOLG(NPOIN)
      INTEGER, INTENT(IN), OPTIONAL :: NPLAN,I3,I4
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION XBID(2)
!
      INTEGER IB(10),ISTAT,I,IBID(1),IELEM,ERR
!
      INTEGER, ALLOCATABLE :: IPOBO(:),IKLES(:)
!
      LOGICAL YA_IPOBO,YA_IKLES
!
      CHARACTER*2 CBID
      CHARACTER*80 TITSEL
!
!-----------------------------------------------------------------------
!
      YA_IPOBO = .FALSE.
      YA_IKLES = .FALSE.
!
!   GOES TO THE BEGINNING OF THE FILE
!
      REWIND NFIC
!
!   LEC/ECR 1   : NAME OF GEOMETRY FILE
!
      TITSEL = TITRE // 'SERAPHIN'
      CALL ECRI2(XBID,IBID,TITSEL,80,'CH',NFIC,'STD',ISTAT)
!
!   LEC/ECR 2   : NUMBER OF DISCRETISATION FUNCTIONS 1 AND 2
!
      IB(1)=0
      IB(2)=0
      DO 91 I=1,NSOR
        IF(SORLEO(I)) IB(1) = IB(1) + 1
91    CONTINUE
      CALL ECRI2(XBID,IB,CBID,2,'I ',NFIC,'STD',ISTAT)
      NVAR =  IB(1)  +  IB(2)
!
!   LEC/ECR 3 : NAME AND UNIT OF THE VARIABLES
!
      IF(NVAR.GE.1) THEN
        DO I=1,NSOR
          IF(SORLEO(I)) THEN
           CALL ECRI2(XBID,IBID,TEXTE(I)(1:32),32,'CH',NFIC,'STD',ISTAT)
          ENDIF
        ENDDO
!       IF(NVARCL.NE.0) THEN
!         DO I=1,NVARCL
!         CALL ECRI2(XBID,IBID,VARCLA(I)(1:32),32,'CH',NFIC,'STD',ISTAT)
!         ENDDO
!       ENDIF
      ENDIF
!
!   LEC/ECR 4   : LIST OF 10 INTEGER PARAMETERS
!
        IB(1) = 1
        DO 29 I = 2,10
         IB(I) = 0
29      CONTINUE
!
!       ORIGIN COORDINATES IN METRES
!
        IF(PRESENT(I3)) IB(3)=I3
        IF(PRESENT(I4)) IB(4)=I4
!
!       NUMBER OF PLANES IN 3D
!
        IF(PRESENT(NPLAN)) IB(7)=NPLAN
!
!PARA   MARKING TO INTRODUCE THE READING OF KNOLG
        IF(NCSIZE.GT.1) THEN
          IB(8)=NPTFR
          IB(9)=NPTIR
        ENDIF
!PARA END
!   IS THE DATE PASSED OVER?
        IF(DATE(1)+DATE(2)+DATE(3)+TIME(1)+TIME(2)+TIME(3).NE.0) THEN
         IB(10) = 1
        ENDIF
!   WRITES THE ARRAY OF 10 PARAMETERS
        CALL ECRI2(XBID,IB,CBID,10,'I ',NFIC,'STD',ISTAT)
!   PASSES THE DATE
        IF(IB(10).EQ.1) THEN
          IB(1)=DATE(1)
          IB(2)=DATE(2)
          IB(3)=DATE(3)
          IB(4)=TIME(1)
          IB(5)=TIME(2)
          IB(6)=TIME(3)
          CALL ECRI2(XBID,IB,CBID,6,'I ',NFIC,'STD',ISTAT)
        ENDIF
!
!   LEC/ECR 5 : 4 INTEGERS
!
      IF(NDP.NE.4) THEN
        IB(1) = NELEM
      ELSE
!       TETRAHEDRONS REGROUPED INTO PRISMS
        IB(1)=NELEM/3
      ENDIF
      IB(2) = NPOIN
      IF(NDP.NE.4) THEN
        IB(3) = NDP
      ELSE
!       TETRAHEDRONS REGROUPED INTO PRISMS
        IB(3) = 6
      ENDIF
      IB(4) = 1
      CALL ECRI2(XBID,IB,CBID,4,'I ',NFIC,'STD',ISTAT)
!
!   LEC/ECR 6 : IKLE
!
      IF(NDP.NE.4) THEN
        ALLOCATE(IKLES(NELEM*NDP),STAT=ERR)
      ELSE
!       TETRAHEDRONS REGROUPED INTO PRISMS
        ALLOCATE(IKLES(NELEM*2)  ,STAT=ERR)
      ENDIF
      IF(ERR.NE.0) STOP 'ECRGEO : ALLOCATION DE IKLES'
      YA_IKLES = .TRUE.
!     INVERTS IKLE  IN IKLES FOR SELAFIN
      IF(NDP.NE.4) THEN
        DO I      = 1,NDP
          DO IELEM  = 1,NELEM
            IKLES((IELEM-1)*NDP+I) = IKLE((I-1)*NELEM+IELEM)
          ENDDO
        ENDDO
      ELSE
!     TETRAHEDRONS REGROUPED INTO PRISMS
        DO IELEM  = 1,NELEM/3
          IKLES((IELEM-1)*6+1) = IKLE(      IELEM)
          IKLES((IELEM-1)*6+2) = IKLE(NELEM+IELEM)
          IKLES((IELEM-1)*6+3) = IKLE(NELEM+IELEM)
          IKLES((IELEM-1)*6+4) = IKLE(      IELEM)+NPOIN/NPLAN
          IKLES((IELEM-1)*6+5) = IKLE(NELEM+IELEM)+NPOIN/NPLAN
          IKLES((IELEM-1)*6+6) = IKLE(NELEM+IELEM)+NPOIN/NPLAN
        ENDDO
      ENDIF
!
      IF(NDP.NE.4) THEN
        CALL ECRI2(XBID,IKLES,CBID,NELEM*NDP,'I ',NFIC,'STD',ISTAT)
      ELSE
!       TETRAHEDRONS REGROUPED INTO PRISMS
        CALL ECRI2(XBID,IKLES,CBID,NELEM*2,'I ',NFIC,'STD',ISTAT)
      ENDIF
!
!   LEC/ECR 7 : IPOBO (FILES IN SCALAR MODE)
!
      IF(IB(8).EQ.0.AND.IB(9).EQ.0) THEN
!
        ALLOCATE(IPOBO(NPOIN),STAT=ERR)
        IF(ERR.NE.0) STOP 'ECRGEO : ALLOCATION DE IPOBO'
        YA_IPOBO = .TRUE.
        DO 40 I=1,NPOIN
         IPOBO(I) = 0
40      CONTINUE
!       ONLY LATERAL BOUNDARY POINTS WITH PRISMS
        DO 41 I =1,NPTFR
         IPOBO(NBOR(I)) = I
41      CONTINUE
        CALL ECRI2(XBID,IPOBO,CBID,NPOIN,'I ',NFIC,'STD',ISTAT)
!
      ENDIF
!
      IF(IB(8).NE.0.OR.IB(9).NE.0) THEN
!
!   LEC/ECR  7.1 KNOLG (PARALLEL MODE ONLY)
!
      CALL ECRI2(XBID,KNOLG,CBID,NPOIN,'I ',NFIC,'STD',ISTAT)
!
      ENDIF
!
!   LEC/ECR 8 AND 9: X AND Y COORDINATES OF THE MESH POINTS
!
      CALL ECRI2(X   ,IBID,CBID,NPOIN,'R4',NFIC,'STD',ISTAT)
      CALL ECRI2(Y   ,IBID,CBID,NPOIN,'R4',NFIC,'STD',ISTAT)
!
!-----------------------------------------------------------------------
!
      IF(YA_IPOBO) DEALLOCATE(IPOBO)
      IF(YA_IKLES) DEALLOCATE(IKLES)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
