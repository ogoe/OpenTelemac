!                    *****************
                     SUBROUTINE DESSED
!                    *****************
!
     & (NPF,IVIDE,EPAI,HDEP,CONC,TEMP,ZR,NPOIN2,NPFMAX,NCOUCH,
     &  NIT,GRAPRD,LT,DTC,TASSE,GIBSON,NRSED,TITCAS,BIRSED,GRADEB)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PROVIDES GRAPHICAL OUTPUTS
!+                FOR THE VARIABLES DESCRIBING THE MUDDY BED.
!
!warning  ONLY WORKS WITH THE GIBSON MODEL
!warning  ASSUMES THAT DTC IS IN FACT AT
!
!history  C LE NORMANT (LNH)
!+        12/06/92
!+
!+
!
!history
!+        6/05/93
!+
!+   MODIFIED
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  S.E.BOURBAN AND N.DURAND (NRC-CHC)
!+        27/03/06
!+        V5P7
!+   SELAFIN IMPLEMENTATION
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
!| BIRSED         |-->| BINARY OF FILE OF SEDIMENT TRANSPORT RESULTS
!| CONC           |<--| CONCENTRATION OF MUD BED LAYER
!|                |   | (MULTILAYER MODEL)
!| DTC            |-->| TIME STEP FOR CONSOLIDATION PHENOMENON
!| EPAI           |<--| THICKNESS OF SOLID FRACTION OF THE BED LAYER
!| GIBSON         |-->| GIBSON SETTLING MODEL
!| GRADEB         |-->| FIRST TIME STEP TO WRITE RESULTS
!| GRAPRD         |-->| KEYWORD 'GRAPHIC PRINTOUT PERIOD'
!| HDEP           |<--| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| IVIDE          |<--| VOID INDEX OF MESH POINTS
!| LT             |-->| CURRENT TIME STEP NUMBER
!| NCOUCH         |-->| NUMBER OF LAYERS DISCRETISING THE MUD BED
!|                |   | (MULTILAYER CONSOLIDATION MODEL)
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NIT            |-->| NUMBER OF TIME STEP
!| NPF            |<--| NUMBER OF POINTS WITHIN THE BED ALONG THE VERTICAL
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES DISCRETISING
!|                |   | WITHIN THE MUDDY BED (GIBSON MODEL)
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NRSED          |-->| NUMBER OF LOGICAL UNIT OF RESULT FILE
!| TASSE          |-->| MULTILAYER SETTLING MODEL LOGICAL
!| TEMP           |<--| TIME COUNTER FOR CONSOLIDATION MODEL
!|                |   | (MULTILAYER MODEL)
!| TITCAS         |-->| TITLE OF TEST CASE
!| ZR             |<--| ELEVATION OF RIDIG BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, ONLY: NCSIZE,NPTIR
      USE DECLARATIONS_TELEMAC3D, ONLY: MESH2D,RHOS
!#####< SEB-changes
      IMPLICIT NONE
!#####> SEB-CHANGES
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER ERR, I, IPLAN,JPLAN, IPOIN, IELEM
      INTEGER NPLAN,NPOIN3,NELEM3,NELEM2,NPTFR2,NDP
      CHARACTER*80 TITSEL
      DOUBLE PRECISION UNITCONV, ECOUCH,ZPLAN
!
      INTEGER, ALLOCATABLE :: IPOBO(:),IKLES(:)       ! THESE WILL BE 3D
      DOUBLE PRECISION, ALLOCATABLE :: WSEB(:)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####< SEB-changes
!
      INTEGER, INTENT(IN)          :: NPOIN2, NPFMAX, NRSED
      INTEGER, INTENT(IN)          :: LT, NIT , NCOUCH
      INTEGER, INTENT(IN)          :: GRAPRD, GRADEB
      INTEGER, INTENT(IN)          :: NPF(NPOIN2)
!#####> SEB-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!      DOUBLE PRECISION, INTENT(IN) :: EPAI((NPFMAX-1)*NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: EPAI(NPFMAX-1,NPOIN2)
!      DOUBLE PRECISION, INTENT(IN) :: IVIDE(NPFMAX*NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: HDEP(NPOIN2), ZR(NPOIN2)
!      DOUBLE PRECISION, INTENT(IN) :: CONC(NCOUCH), TEMP(NCOUCH*NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: CONC(NCOUCH), TEMP(NCOUCH,NPOIN2)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####< SEB-changes
      DOUBLE PRECISION, INTENT(IN) :: DTC
      LOGICAL, INTENT(IN)          :: TASSE,GIBSON
      CHARACTER(LEN=72), INTENT(IN):: TITCAS
      CHARACTER(LEN=3), INTENT(IN) :: BIRSED
!
      DOUBLE PRECISION XB(2)
      INTEGER IB(10), ISTAT
      CHARACTER(LEN=2) CB
!
!----------------------------------------------------------------------
!
      IF((LT/GRAPRD)*GRAPRD.NE.LT) RETURN
!
      IF(LT.LT.GRADEB) RETURN
!
      IF(LT.EQ.0) THEN
!
      REWIND NRSED
!
!#####> SEB-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        NELEM2 = MESH2D%NELEM
        NPTFR2 = MESH2D%NPTFR
!       LEC/ECR 1: NAME OF GEOMETRY FILE
        TITSEL = TITCAS // 'SERAPHIN'
        CALL ECRI2(XB,IB,TITSEL,80,'CH',NRSED,BIRSED,ISTAT)
!
!       LEC/ECR 2: NUMBER OF 1 AND 2 DISCRETISATION FUNCTIONS
        IF(TASSE) THEN
          IB(1)=4
          IB(2)=0
        ELSEIF (GIBSON) THEN
          IB(1)=4
          IB(2)=0
        ELSE
          IF(LNG.EQ.1) WRITE(LU,*) "OPTION DE CONSOLIDATION NON PREVUE"
          IF(LNG.EQ.2) WRITE(LU,*) "UNKNOWN CONSOLIDATION OPTION"
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL ECRI2(XB,IB,CB,2,'I ',NRSED,BIRSED,ISTAT)
!
!   LEC/ECR 3: NAMES AND UNITS OF THE VARIABLES
      IF(TASSE) THEN
           TITSEL(1:32) = 'ELEVATION Z     M               '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
           TITSEL(1:32) = 'EPAISSEUR VRAIE M               '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
           TITSEL(1:32) = 'CONC. VASE      KG/M3           '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
           TITSEL(1:32) = 'COMPTEUR TEMPS  S               '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
      ELSEIF(GIBSON) THEN
           TITSEL(1:32) = 'ELEVATION Z     M               '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
           TITSEL(1:32) = 'EPAISSEUR VRAIE M               '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
           TITSEL(1:32) = 'DENSITE VRAIE   KG/M3           '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
           TITSEL(1:32) = 'LAYER IPF                       '
           CALL ECRI2(XB,IB,TITSEL(1:32),32,'CH',NRSED,BIRSED,ISTAT)
      ENDIF
!
!   LEC/ECR 4: LIST OF 10 INTEGER PARAMETERS (AND DATE)
        IB(1) = 1
        DO I = 2,10
          IB(I) = 0
        ENDDO
        IF (TASSE) THEN
           IB(7) = NCOUCH
        ELSEIF (GIBSON) THEN
           IB(7) = NPFMAX
        ENDIF
        NPLAN = IB(7)
        IF(NCSIZE.GT.1) THEN       ! CAN YOU MAKE SURE THESE ARE 3D
           IB(8) = NPTFR2*NPLAN    ! 3D -> TO BE CALCULATED FROM 2D
           IB(9) = NPTIR           ! CAN THIS ONLY BE 2D ?
        ENDIF
!        IF(DATE(1)+DATE(2)+DATE(3)+TIME(1)+TIME(2)+TIME(3).NE.0) THEN
!           IB(10) = 1
!        ENDIF
        CALL ECRI2(XB,IB,CB,10,'I ',NRSED,BIRSED,ISTAT)
!
!   DATE
!       IF(IB(10).EQ.1) THEN
!          IB(1)=DATE(1)
!          IB(2)=DATE(2)
!          IB(3)=DATE(3)
!          IB(4)=TIME(1)
!          IB(5)=TIME(2)
!          IB(6)=TIME(3)
!          CALL ECRI2(XB,IB,CB,6,'I ',NRSED,BIRSED,ISTAT)
!       ENDIF
!
!   LEC/ECR 5: 4 INTEGERS
        IB(1) = NELEM2*(NPLAN-1)    ! 3D -> TO BE CALCULATED FROM 2D
        NELEM3 = IB(1)
        IB(2) = NPOIN2*NPLAN        ! 3D -> TO BE CALCULATED FROM 2D
        NPOIN3 = IB(2)
        IB(3) = 6                   ! PARTICULAR CASE OF PRISMS NDP=6
        NDP = IB(3)
        IB(4) = 1
        CALL ECRI2(XB,IB,CB,4,'I ',NRSED,BIRSED,ISTAT)
!
!   LEC/ECR 6: IKLE
!   BUILDS 3D LAYERED PRISMATIC MESH OUT OF 2D IMPRINT
        ALLOCATE(IKLES(NELEM3*NDP),STAT=ERR)  ! PARTICULAR CASE OF PRISMS
        IF(ERR.NE.0) STOP 'DESSED : ALLOCATION DE IKLES'
        DO IPLAN = 1,NPLAN-1
         DO IELEM = 1,NELEM2
          I = ((IPLAN-1)*NELEM2+IELEM-1)*NDP
          IKLES(I+1)=MESH2D%IKLE%I(IELEM)+(IPLAN-1)*NPOIN2
          IKLES(I+2)=MESH2D%IKLE%I(IELEM+NELEM2)+(IPLAN-1)*NPOIN2
          IKLES(I+3)=MESH2D%IKLE%I(IELEM+2*NELEM2)+(IPLAN-1)*NPOIN2
          IKLES(I+4)=MESH2D%IKLE%I(IELEM)+IPLAN*NPOIN2
          IKLES(I+5)=MESH2D%IKLE%I(IELEM+NELEM2)+IPLAN*NPOIN2
          IKLES(I+6)=MESH2D%IKLE%I(IELEM+2*NELEM2)+IPLAN*NPOIN2
         ENDDO
        ENDDO
        CALL ECRI2(XB,IKLES,CB,NELEM3*NDP,'I ',NRSED,BIRSED,ISTAT)
        DEALLOCATE(IKLES)
!
!   LEC/ECR 7: IPOBO (CASE OF FILES WITHOUT PARALLELISM)
!
        IF( IB(8).EQ.0.AND.IB(9).EQ.0 ) THEN
           ALLOCATE(IPOBO(NPLAN*NPOIN2),STAT=ERR)
           IF(ERR.NE.0) STOP 'DESSED : ALLOCATION DE IPOBO'
           DO IPOIN = 1,NPLAN*NPOIN2            ! THIS IS INDEED 3D
             IPOBO(IPOIN) = 0
           ENDDO
           DO IPLAN = 1,NPLAN
             DO IPOIN = 1,NPTFR2
               IPOBO(MESH2D%NBOR%I(IPOIN)+(IPLAN-1)*NPOIN2) =
     &         IPOIN+(IPLAN-1)*NPTFR2
             ENDDO
           ENDDO
           CALL ECRI2(XB,IPOBO,CB,NPLAN*NPOIN2,'I ',NRSED,BIRSED,ISTAT)
           DEALLOCATE(IPOBO)
        ENDIF
!   LEC/ECR 7.1: KNOLG (ONLY IN THE EVENT OF PARALLEL MODE)
        IF(IB(8).NE.0.OR.IB(9).NE.0) THEN
           ALLOCATE(IPOBO(NPLAN*NPOIN2),STAT=ERR)
           IF(ERR.NE.0) STOP 'DESSED : ALLOCATION DE IPOBO'
           DO IPOIN = 1,NPLAN*NPOIN2            ! THIS IS INDEED 3D
             IPOBO(IPOIN) = 0
           ENDDO
           DO IPLAN = 1,NPLAN
              DO IPOIN = 1,NPOIN2
                 IPOBO(IPOIN+(IPLAN-1)*NPOIN2) =
     &              MESH2D%KNOLG%I(IPOIN)+(IPLAN-1)*NPOIN2
              ENDDO
           ENDDO
           CALL ECRI2(XB,IPOBO,CB,NPOIN3,'I ',NRSED,BIRSED,ISTAT)
        ENDIF
!
!   LEC/ECR 8 AND 9: X AND Y COORDINATES OF THE MESH NODES
!
        ALLOCATE(WSEB(NPLAN*NPOIN2),STAT=ERR)
        IF(ERR.NE.0) THEN
           IF(LNG.EQ.1) WRITE(LU,*) 'FONSTR : MAUVAISE ALLOCATION DE W'
           IF(LNG.EQ.2) WRITE(LU,*) 'FONSTR: WRONG ALLOCATION OF W'
           STOP
        ENDIF
        DO IPOIN = 1, NPOIN2
          DO IPLAN = 1,NPLAN
             WSEB(IPOIN+(IPLAN-1)*NPOIN2) = MESH2D%X%R(IPOIN)
          ENDDO
        ENDDO
        CALL ECRI2(WSEB,IB,CB,NPOIN3,'R4',NRSED,BIRSED,ISTAT)
        DO IPOIN = 1, NPOIN2
          DO IPLAN = 1,NPLAN
            WSEB(IPOIN+(IPLAN-1)*NPOIN2) = MESH2D%Y%R(IPOIN)
          ENDDO
        ENDDO
        CALL ECRI2(WSEB,IB,CB,NPOIN3,'R4',NRSED,BIRSED,ISTAT)
        DEALLOCATE(WSEB)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####< SEB-changes
      ENDIF
!
! A TRICK TO WRITE ONE NUMBER
!
      XB(1) = DTC
      CALL ECRI2(XB,IB,CB,1,'R4',NRSED,BIRSED,ISTAT)
!
      IF (TASSE) THEN
!
!#####> SEB-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!  /!\ THIS PART SHOULD BE ENTIRELY REVISITED ...
         UNITCONV = 1.D0                     ! VARIABLES CAN BE ENLARGED
         ALLOCATE(WSEB(NCOUCH*NPOIN2),STAT=ERR)
         IF(ERR.NE.0) THEN
            IF(LNG.EQ.1) WRITE(LU,*) 'FONSTR : MAUVAISE ALLOCATION DE W'
            IF(LNG.EQ.2) WRITE(LU,*) 'FONSTR: WRONG ALLOCATION OF W'
            STOP
         ENDIF
! THIS IS THE Z FOR THE LAYERING -
         DO IPOIN = 1, NPOIN2
!>            WSEB(IPOIN) = ZR(IPOIN) + EPAI(IPOIN)
         ENDDO
         DO IPLAN = 2,NCOUCH
            DO IPOIN = 1, NPOIN2
!>               WSEB(IPOIN+NPOIN2*(IPLAN-1)) =
!>     & WSEB(IPOIN+NPOIN2*(IPLAN-2)) + EPAI(IPOIN+NPOIN2*(IPLAN-1))
            ENDDO
         ENDDO
         CALL ECRI2(WSEB,IB,CB,NCOUCH*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
!
         DO IPOIN = 1, (NCOUCH-1)*NPOIN2
!>            WSEB(IPOIN) = EPAI(IPOIN+NPOIN2) * UNITCONV
         ENDDO
!        DO IPOIN = 1, NPOIN2
!            WSEB(IPOIN+(NCOUCH-1)*NPOIN2) = HDEP(IPOIN) * UNITCONV
!        ENDDO
         CALL ECRI2(WSEB,IB,CB,NCOUCH*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
!        CALL ECRI2(EPAI,IB,CB,NCOUCH*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
!!
         DO IPLAN = 1,NCOUCH
            DO IPOIN = 1, NPOIN2
!>               WSEB(IPOIN+NPOIN2*(IPLAN-1)) = CONC(IPLAN) * UNITCONV
            ENDDO
         ENDDO
         CALL ECRI2(WSEB,IB,CB,NCOUCH*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
!        CALL ECRI2(CONC,IB,CB,NCOUCH,'R4',NRSED,BIRSED,ISTAT)
!#####< SEB-changes
!
        CALL ECRI2(TEMP,IB,CB,NCOUCH*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
!#####> SEB-CHANGES
         DEALLOCATE(WSEB)
!#####< SEB-changes
!
      ELSEIF (GIBSON) THEN
!
!#####> SEB-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
! ASSUMPTIONS - Z-LEVELS:
!  * B KENUE'S BOTTOM Z-LEVEL IS ZR (1), TOP Z-LEVEL IS ZF (NPFMAX)
!  * SEDI3D'S NON-EMPTY LAYERS ARE B KENUE'S LAYERS UNDER ZF
!  * B KENUE'S PLANES FROM 1 TO NPFMAX-NPF ARE EMPTY (EPAI=0) AND
!      CORRESPOND TO SEDI3D'S PLANES FROM NPF+2 TO NPFMAX
!  * ALL EMPTY PLANES (EXCEPT FOR HDEP) ARE SET TO COINCIDENT WITH ZR
!      (ROCK BOTTOM), WHILE SEDI3D'S 1ST PLANE IS ZR
!  * B KENUE'S NON-EMPTY TOP PLANES FROM NPFMAX-NPF+1 TO NPFMAX-1
!      CORRESPOND TO SEDI3D'S PLANES FROM 2 TO NPF IN THE SAME ORDER
!  * B KENUE'S VERY TOP PLANE AT NPFMAX CORRESPONDS TO
!      SEDI3D'S NPF+1-TH PLANE, WHICH IS ALSO HDEP - EVEN IF EMPTY !
!
! ASSUMPTIONS - VARIABLE THICKNESS:
!  * B KENUE'S THICKNESS BETWEEN TWO PLANES IS STORED ON THE UPPER PLANE
!      WHICH IS CONTRARY TO SEDI3D'S CONVENTION
!  * B KENUE'S NPFMAX-TH THICKNESS STORES HDEP
!  * FOR STORAGE PURPOSES, B KENUE'S 1ST PLANE HOLDS THE NPF
!
         UNITCONV = 1.D0                     ! VARIABLES CAN BE ENLARGED
         ALLOCATE(WSEB(NPFMAX*NPOIN2),STAT=ERR)
         IF(ERR.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,*) 'FONSTR : MAUVAISE ALLOCATION DE W'
          IF(LNG.EQ.2) WRITE(LU,*) 'FONSTR: WRONG ALLOCATION OF W'
          STOP
         ENDIF
!> TRUE LAYERING - ELEVATION Z
         DO IPOIN = 1, NPOIN2
            JPLAN = 0
            ZPLAN = ZR(IPOIN)
            DO IPLAN = 1,NPFMAX-NPF(IPOIN)
               JPLAN = JPLAN + 1
               WSEB(IPOIN+(JPLAN-1)*NPOIN2) = ZPLAN
            ENDDO
            DO IPLAN = 1,NPF(IPOIN)-1
               JPLAN = JPLAN + 1
               ECOUCH=(IVIDE(IPLAN,IPOIN)+IVIDE(IPLAN+1,IPOIN))/2.D0
               ZPLAN = ZPLAN +
     &               ( 1.D0+ECOUCH ) * EPAI(IPLAN,IPOIN)
               WSEB(IPOIN+(JPLAN-1)*NPOIN2) = ZPLAN
            ENDDO
            WSEB(IPOIN+(NPFMAX-1)*NPOIN2) = ZPLAN +
     &               HDEP(IPOIN)
         ENDDO
         CALL ECRI2(WSEB,IB,CB,NPFMAX*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
!> TRUE THICKNESS - THICKNESS DZ
         DO IPOIN = 1, NPOIN2
            JPLAN = 0
            DO IPLAN = 1,NPFMAX-NPF(IPOIN)
               JPLAN = JPLAN + 1
               WSEB(IPOIN+(JPLAN-1)*NPOIN2) = 0.D0
            ENDDO
            DO IPLAN = 1,NPF(IPOIN)-1
              JPLAN = JPLAN + 1
              ECOUCH=(IVIDE(IPLAN,IPOIN)+IVIDE(IPLAN+1,IPOIN))/2.D0
              WSEB(IPOIN+(JPLAN-1)*NPOIN2) =
     &            (1.D0+ECOUCH)*EPAI(IPLAN,IPOIN)*UNITCONV
            ENDDO
            WSEB(IPOIN+(NPFMAX-1)*NPOIN2) = HDEP(IPOIN) *UNITCONV
            WSEB(IPOIN) = 1.D0 * NPF(IPOIN) ! RESET THIS ONE ! OR NOT ?
         ENDDO
         CALL ECRI2(WSEB,IB,CB,NPFMAX*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
!> TRUE DENSITY
         DO IPOIN = 1, NPOIN2
            JPLAN = NPFMAX
            DO IPLAN = NPF(IPOIN),1,-1
          WSEB(IPOIN+(JPLAN-1)*NPOIN2)=RHOS/(1.D0+IVIDE(IPLAN,IPOIN))
              JPLAN = JPLAN - 1
            ENDDO
            DO IPLAN = NPFMAX,NPF(IPOIN)+1,-1
              WSEB(IPOIN+(JPLAN-1)*NPOIN2) = 0.D0
              JPLAN = JPLAN - 1
            ENDDO
         ENDDO
         CALL ECRI2(WSEB,IB,CB,NPFMAX*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
!> LAYERING - LAYER IPF
         DO IPOIN = 1, NPOIN2
            DO IPLAN = 1,NPFMAX-1
               WSEB(IPOIN+(IPLAN-1)*NPOIN2) = IPLAN
            ENDDO
         ENDDO
         CALL ECRI2(WSEB,IB,CB,NPFMAX*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
!        CALL ECRI2(IVIDE,IB,CB,NPFMAX*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
!        CALL ECRI2(XB,NPF,CB,NPOIN2,'I',NRSED,BIRSED,ISTAT)
!        CALL ECRI2(EPAI,IB,CB,(NPFMAX-1)*NPOIN2,'R4',NRSED,BIRSED,ISTAT)
         DEALLOCATE(WSEB)
!#####< SEB-changes
!
      ENDIF
!
!      CALL ECRI2(HDEP,IB,CB,NPOIN2,'R4',NRSED,BIRSED,ISTAT)
!
!      CALL ECRI2(ZR,IB,CB,NPOIN2,'R4',NRSED,BIRSED,ISTAT)
!
!----------------------------------------------------------------------
!
      RETURN
      END
