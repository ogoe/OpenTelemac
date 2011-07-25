!                    *****************
                     SUBROUTINE CONDIS
!                    *****************
!
     &(IVIDE, EPAI  , TREST , CONC , TEMP   , HDEP   ,
     & ZR   , ZF    , X     , Y    , NPOIN2 , NPOIN3 ,
     & NPF  , NPFMAX, NCOUCH, TASSE, GIBSON , PRIVE  , CONSOL )
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE SEDIMENT VARIABLES.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  NOEMIE DURAND (CHC-NRC); C LE NORMANT (LNH)
!+        18/07/06
!+        V5P7
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
!| CONC           |<--| CONCENTRATION OF MUD BED LAYER
!|                |   | (MULTILAYER MODEL)
!| CONSOL         |-->|
!| EPAI           |<--| THICKNESS OF SOLID FRACTION oF THE BED LAYER
!|                |   | (EPAI=DZ/(1+IVIDE), DZ BED LAYER THICKNESS)
!| GIBSON         |-->| GIBSON SETTLING MODEL
!| HDEP           |<--| THICKNESS OF FRESH DEPOSIT(FLUID MUD LAYER)
!| IVIDE          |<--| VOID RATIO
!|                |   | (GIBSON MODEL ONLY)
!| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (GIBSON MODEL)
!| NPF            |<--| NUMBER OF POINTS WITHIN THE BED ALONG THE VERTICAL
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES WITHIN THE BED
!|                |   | (GIBSON MODEL)
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF POINTS IN 3D
!| PRIVE          |-->| BLOCK OF PRIVATE ARRAYS FOR USER
!| TASSE          |-->| MULTILAYER SETTLING MODEL
!| TEMP           |<--| TIME COUNTER FOR CONSOLIDATION MODEL
!|                |   | (MULTILAYER MODEL)
!| TREST          |<->| CONSOLIDATION TIME SCALE
!|                |   | (ONLY FOR MULTILAYER MODEL)
!| X,Y            |-->| COORDINATES OF 2D MESH
!| ZF             |-->| BOTTOM ELEVATION
!| ZR             |<--| ELEVATION OF RIDIG BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!#####> NOE-CHANGES
      USE DECLARATIONS_TELEMAC3D, ONLY: NPRIV,
     &                                  EPAI0,CFDEP,RHOS,GRAV,CFMAX,DT
!#####< NOE-CHANGES
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3,NPFMAX,NCOUCH
      DOUBLE PRECISION, INTENT(OUT)   :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(OUT)   :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(OUT)   :: CONC(NCOUCH)
      DOUBLE PRECISION, INTENT(OUT)   :: TEMP(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(OUT)   :: HDEP(NPOIN2)
      DOUBLE PRECISION, INTENT(OUT)   :: ZR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TREST(NCOUCH)
      INTEGER, INTENT(OUT)            :: NPF(NPOIN2)
      TYPE(BIEF_OBJ)                  :: PRIVE
      LOGICAL, INTENT(IN)             :: TASSE, GIBSON,CONSOL
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION ECOUCH , TCAR
      INTEGER IPOIN, IC, IPF
!
      INTRINSIC LOG10,MAX
!#####> NOE-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      LOGICAL CVGCSL
      INTEGER NITCSL, ITCSL, ERR
      DOUBLE PRECISION DTCSL,RESCSL
      DOUBLE PRECISION, ALLOCATABLE :: TRA01(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: TRA02(:),TRA03(:),ZNOE(:)
!
! ALLOCATES MEMORY AND SETS INITIAL INTERFACE BETWEEN HDEP AND EPAI
      ALLOCATE(TRA01(NPFMAX,6),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'CONDIS : MAUVAISE ALLOCATION DE TRA01'
        IF(LNG.EQ.2) WRITE(LU,*) 'CONDIS: WRONG ALLOCATION OF TRA01'
        STOP
      ENDIF
      ALLOCATE(TRA02(NPFMAX),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'CONDIS : MAUVAISE ALLOCATION DE TRA02'
        IF(LNG.EQ.2) WRITE(LU,*) 'CONDIS: WRONG ALLOCATION OF TRA02'
        STOP
      ENDIF
      ALLOCATE(TRA03(NPFMAX),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'CONDIS : MAUVAISE ALLOCATION DE TRA03'
        IF(LNG.EQ.2) WRITE(LU,*) 'CONDIS: WRONG ALLOCATION OF TRA03'
        STOP
      ENDIF
      ALLOCATE(ZNOE(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'CONDIS : MAUVAISE ALLOCATION DE ZNOE'
        IF(LNG.EQ.2) WRITE(LU,*) 'CONDIS: WRONG ALLOCATION OF ZNOE'
        STOP
      ENDIF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####< NOE-CHANGES
!
!=======================================================================
!
!     -----  INITIALISES HDEP                  -----
!     -----  NOT USED BY THE MULTILAYER MODEL  -----
!
!     HERE, THE DEFAULT ACTION IS TO SET HDEP TO 100., HENCE ENABLE THE
!     EROSION PROCESS. THIS VALUE IS COMPATIBLE WITH SUBROUTINE NOEROD
!     IN SISYPHE.
!
      CALL OV('X=C     ',HDEP,HDEP,HDEP,100.D0,NPOIN2)
!
!     -----  INITIALISES ZR  -----
!
      CALL OV('X=Y-Z   ' ,ZR,ZF,HDEP,0.D0,NPOIN2)
!
!#####> NOE-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!     NOTE THAT THE USE OF HDEP & ZR HERE IS ONLY TEMPORARY, IN THE CASE
!     OF NPRIV.GE.1
!     BEFORE SPLIT AND CONSOLIDATION, HDEP WILL BE SET TO ZF - ZR(USER)
! DELETED BY JMH, AWAITING EXPLANATIONS/COMMENTS
!     IF(NPRIV.GE.1) THEN
!       CALL OV('X=Y     ',ZR,PRIVE%ADR(1)%P%R,ZR,0.D0,NPOIN2)
!       CALL OV( 'X=Y-Z   ' ,HDEP,ZF,ZR,0.D0,NPOIN2)
!     ENDIF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####< NOE-CHANGES
!
!     -------------------------------------------
!     INITIAL CONDITIONS FOR THE MULTILAYER MODEL
!     -------------------------------------------
!
      IF(TASSE) THEN
!
!       -----  INITIALISES EPAI  -----
!
        CALL OV('X=C     ',EPAI,EPAI,EPAI,0.D0,NPOIN2*NCOUCH)
!
!       -----  INITIALISES CONC  -----
!
        CALL OV('X=C     ',CONC,CONC,CONC,0.D0,NCOUCH)
!
!       DEFAULT OPTION:
!             CONCENTRATIONS ARE COMPUTED AS A FUNCTION
!             OF CONSOLIDATION TIME SCALE
!            (EMPIRICAL RELATION, LOIRE ESTUARY , FRISTCH ET AL. 1989)
!
        TCAR = TREST(NCOUCH)/2.D0
        DO IC = NCOUCH , 1 , -1
          IF (IC.LT.NCOUCH) TCAR = TCAR + (TREST(IC+1)+TREST(IC))/2.D0
          IF (TCAR.LT.24.D0) THEN
            CONC(IC) = 136.2D0*LOG10(TCAR+5.424D0)
          ELSE
            CONC(IC) = 200.D0+70.D0*LOG10(TCAR/24.D0)
          ENDIF
        ENDDO
!
!       -----  CHANGES HOURS INTO SECONDS  -----
!
        CALL OV( 'X=CX    ',TREST,TREST,TREST,3600.D0,NCOUCH)
!
!       -----  INITIALISES TEMP  -----
!
        CALL OV( 'X=C     ',TEMP,TEMP,TEMP,0.D0,NPOIN2*NCOUCH)
!
!       -----  MODIFIES ZR  -----
!
        DO IPOIN=1,NPOIN2
          DO IC=1,NCOUCH
            ZR(IPOIN)=ZR(IPOIN)-EPAI(IC,IPOIN)
          ENDDO
        ENDDO
!
!     ---------------------------------------
!     INITIAL CONDITIONS FOR THE GIBSON MODEL
!     ---------------------------------------
!
      ELSEIF (GIBSON) THEN
!
!       -----  INITIALISES NPF  -----
!
        DO IPOIN=1,NPOIN2
          NPF(IPOIN)=0
        ENDDO
!
!       -----  INITIALISES IVIDE  -----
!
        CALL OV( 'X=C     ', IVIDE ,IVIDE , IVIDE, 0.D0, NPOIN2*NPFMAX)
!
!       -----  INITIALISES EPAI  -----
!
        CALL OV( 'X=C     ', EPAI, EPAI, EPAI, 0.D0, NPOIN2*(NPFMAX-1))
!
!       -----  MODIFIES ZR  -----
!
        DO IPOIN=1,NPOIN2
          DO IPF=1,NPF(IPOIN)-1
            ZR(IPOIN)=ZR(IPOIN)-EPAI(IPF,IPOIN)
            ECOUCH=(IVIDE(IPF,IPOIN)+IVIDE(IPF+1,IPOIN))/2.D0
            EPAI(IPF,IPOIN)=EPAI(IPF,IPOIN)/(1.D0+ECOUCH)
          ENDDO
        ENDDO
!
!#####> NOE-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!       HERE IS OUR CHANCE TO SET
!       IVIDE AND (TRUE) EPAI REPRESENTATIVE OF CONSOLIDATED MATTER
!
!       CONSOL SHOULD BE .TRUE. IF THE LAYER DISTRIBUTION IS NOT KNOWN
!       AND THE USER WANTS TO LET THE MODEL CONSOLIDATE THE BED
!
        IF(CONSOL) THEN
!
! TIME FOR CONSOLIDATION, WHICH COULD ALSO BE READ FROM PRIVE%ADR(2)
          DTCSL = DT*10.D0
!          NITCSL = 100
!
! START PAST TIME LOOP
!
!          DO ITCSL = 1,NITCSL
          CVGCSL = .FALSE.
          DO WHILE(.NOT.CVGCSL)
!
!     -----  MANAGES THE DEPOSITED SEDIMENT:   -----
!     -----  ADDS NEW LAYERS TO THE MUDDY BED  -----
!
            CALL GESTDP( IVIDE,EPAI,HDEP,
     &        NPOIN2,NPFMAX,NPF, EPAI0,CFDEP,RHOS )
!
!     -----  CONSOLIDATES THE MUDDY BED  -----
!     -----  USING GIBSON EQUATION       -----
!
            CALL TASSEM( IVIDE,EPAI, NPOIN2,NPFMAX,NPF, GRAV,RHOS,
     &        DTCSL, CFMAX, TRA01,TRA02,TRA03 )
!
!     -----  UPDATES THE BOTTOM LAYER  -----
!     -----  RECOMPUTES THE INTERFACE  -----
!
      RESCSL = 0.D0
      DO IPOIN = 1,NPOIN2
        ZNOE(IPOIN) = ZR(IPOIN)
        DO IPF = 1,NPF(IPOIN)-1
          ECOUCH=(IVIDE(IPF,IPOIN)+IVIDE(IPF+1,IPOIN))/2.D0
          ZNOE(IPOIN) = ZNOE(IPOIN)+(1.D0+ECOUCH)*EPAI(IPF,IPOIN)
        ENDDO
        IF(ZF(IPOIN).LT.ZNOE(IPOIN)) THEN
        RESCSL = RESCSL + ( MAX(
     & ZF(IPOIN)-(ZNOE(IPOIN)-(1.D0+ECOUCH)*EPAI(NPF(IPOIN)-1,IPOIN)),
     &  0.D0) - HDEP(IPOIN) )**2
      ECOUCH =(IVIDE(NPF(IPOIN)-1,IPOIN)+IVIDE(NPF(IPOIN),IPOIN))/2.D0
        HDEP(IPOIN) = MAX(
     &  ZF(IPOIN)-(ZNOE(IPOIN)-(1.D0+ECOUCH)*EPAI(NPF(IPOIN)-1,IPOIN)),
     &        0.D0)
            EPAI(NPF(IPOIN)-1,IPOIN) = 0.D0
            NPF(IPOIN) = NPF(IPOIN)-1
          ELSE
            RESCSL = RESCSL + (ZF(IPOIN)-ZNOE(IPOIN)-HDEP(IPOIN))**2
            HDEP(IPOIN) = ZF(IPOIN) - ZNOE(IPOIN)
          ENDIF
        ENDDO
        CVGCSL = RESCSL.LT.(1.D-8)
      ENDDO
!
! END PAST TIME LOOP
!
        ENDIF
!
! END IF CONSOL
!
      ENDIF
!
! END IF TASSE/GIBSON
!
      DEALLOCATE(TRA01)
      DEALLOCATE(TRA02)
      DEALLOCATE(TRA03)
      DEALLOCATE(ZNOE)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####< NOE-CHANGES
!
!-----------------------------------------------------------------------
!
      RETURN
      END
