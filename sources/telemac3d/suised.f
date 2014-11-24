!                    *****************
                     SUBROUTINE SUISED
!                    *****************
!
     &(EPAI,HDEP,CONC,ZR,ZF,T2,NPOIN2,NCOUCH,ITASS,NSUIS,FFORMAT,
     & PRIVE)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    READS SEDIMENTOLOGICAL DATA
!+                FOR A RESUMING COMPUTATION.
!
!warning  IMPLEMENTATION WITH SELAFIN ONLY WORKS WITH GIBSON
!
!history  C LE NORMANT (LNH)
!+        05/05/93
!+
!+
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
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history T. BENSON (HR-WALLINGFORD)
!+        15/08/2014
!+        V7P0
!+        Major rewrite to use 2D result file (simplified)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| BINARY OF PREVIOUS SEDIMENT COMPUTATION FILE
!| CONC           |<->| CONCENTRATION OF MUD BED LAYER
!|                |   | (MULTILAYER MODEL)
!| EPAI           |<->| THICKNESS OF SOLID BED LAYER
!| HDEP           |<->| TOTAL THICKNESS OF MUD DEPOSIT
!| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE BED
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NSUIS          |-->| LOGICAL UNIT NUMBER OF THE PREVIOUS SEDIMENT
!|                |   | COMPUTATION FILE
!| ZF             |-->| ELEVATION OF BED 
!|                |   | (FROM GEOMETRY OR 3D PREVIOUS COMPUTATION FILE)
!| ZR             |<--| ELEVATION OF RIGID BED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!!!      USE DECLARATIONS_TELEMAC3D, ONLY: RHOS
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!----------------------------------------------------------------------
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T2, PRIVE
      INTEGER, INTENT(IN)             :: NPOIN2,NCOUCH
      INTEGER, INTENT(IN)             :: ITASS, NSUIS
!
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: CONC(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(OUT)   :: ZR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2)
!!! TBE - THESE ARE FOR GIBSON MODEL (COMMENTED FOR NOW)
!!!      DOUBLE PRECISION, INTENT(OUT)   :: IVIDE(NPOIN2,NCOUCH+1)
!!!      DOUBLE PRECISION, INTENT(INOUT) :: TEMP(NPOIN2,NCOUCH)      
!!!      DOUBLE PRECISION, INTENT(OUT)   :: FLUER(NPOIN2), PDEPOT(NPOIN2)
!!!      INTEGER, INTENT(OUT)            :: NPF(NPOIN2)
!
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT 
!
!!!      LOGICAL, INTENT(IN)             :: TASSE 
      
!
!----------------------------------------------------------------------
!
      INTEGER IPOIN,ISTAT
!
      INTEGER           :: IB(10),ERR
      DOUBLE PRECISION, ALLOCATABLE :: XB(:)
      REAL, ALLOCATABLE :: RB(:)
      CHARACTER(LEN=80) :: CB
      INTEGER           :: REC 
      LOGICAL           :: FOUND 
      CHARACTER(LEN=16) :: VARNAME 
!      
      INTEGER           :: NVAR,I
!
! ALLOCATE ARRAY FOR READING FROM FILE
      ALLOCATE(XB(NPOIN2),STAT=ERR)
      ALLOCATE(RB(NPOIN2),STAT=ERR)
!
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'SUISED : ALLOCATION DE RB DEFECTUEUSE'
        IF(LNG.EQ.2) WRITE(LU,*) 'SUISED : WRONG ALLOCATION OF RB'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!    NOTE: GIBSON MODEL NOT YET CODED
      IF (ITASS.EQ.2) THEN
        WRITE(LU,*)
     &    'SUISED: GIBSON MODEL NOT YET IMPLEMENTED'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!   READ THE HEADER AND CHECK QUANTITIES / OPTIONS 
      WRITE(LU,*) ' '
      IF(LNG.EQ.1) WRITE(LU,*)
     &   'LECTURE DU FICHIER DE SEDIMENT PRECEDENT'
      IF(LNG.EQ.2) WRITE(LU,*)
     &   'READING BED SEDIMENT FROM PREVIOUS COMPUTATION FILE'
      WRITE(LU,*) ' '
!
!   1: NAME OF THE GEOMETRY FILE
      CALL LIT(XB,RB,IB,CB,80,'CH',NSUIS,'STD',ISTAT)
      IF(LNG.EQ.1) WRITE(LU,*) 'TITRE : ',CB(1:72)
      IF(LNG.EQ.2) WRITE(LU,*) 'TITTLE: ',CB(1:72)
!
!   2: NUMBER OF 1 AND 2 DISCRETISATION FUNCTIONS
      CALL LIT(XB,RB,IB,CB,2,'I',NSUIS,'STD',ISTAT)
      IF(LNG.EQ.1) WRITE(LU,*) 'NOMBRE DE VARIABLES : ',IB(1)
      IF(LNG.EQ.2) WRITE(LU,*) 'NUMBER OF VARIABLES: ',IB(1)
      IF(LNG.EQ.1) WRITE(LU,*) 'ET DE VARIABLES CLANDESTINES: ',IB(2)
      IF(LNG.EQ.2) WRITE(LU,*) 'AND OF CLANDESTINE VARIABLES: ',IB(2)
!!!      IF( (IB(1).NE.4 .AND.ITASS.EQ.1).OR.(IB(1).NE.4 .AND.ITASS.EQ.2) )
!!!     &   THEN
!!!        IF(LNG.EQ.1) WRITE(LU,*)
!!!     &'SUISED : CONSOLIDATION NON COMPATIBLE AVEC NOMBRE DE VARIABLES'
!!!        IF(LNG.EQ.2) WRITE(LU,*)
!!!     &'SUISED: CONSOLIDATION NOT COMPATIBLE WITH NUMBER OF VARIABLES'
!!!        CALL PLANTE(1)
!!!        STOP
!!!      ENDIF
      NVAR = IB(1)+IB(2)
!
!   3: VARIABLES NAMES AND UNITS
      DO I = 1,NVAR
        CALL LIT(XB,RB,IB,CB,32,'CH',NSUIS,'STD',ISTAT)
        IF(LNG.EQ.1) WRITE(LU,*) I,".- ",CB(1:16)
        IF(LNG.EQ.2) WRITE(LU,*) I,".- ",CB(1:16)
      ENDDO
!
!   4: LIST OF 10 INTEGER PARAMETERS
      CALL LIT(XB,RB,IB,CB,10,'I',NSUIS,'STD',ISTAT)     
!     CASE WHERE DATE AND TIME ARE IN THE FILE
      IF(IB(10).EQ.1) THEN 
          CALL LIT(XB,RB,IB,CB,6,'I ',NSUIS,'STD',ISTAT)
          WRITE(LU,*) IB(1),IB(2),IB(3)
          WRITE(LU,*) IB(4),IB(5),IB(6)
      ENDIF
!!!      IF( (IB(7).NE.NPFMAX .AND.ITASS.EQ.2).OR.
!!!     &                   (IB(7).NE.NCOUCH .AND.ITASS.EQ.1) ) THEN
!!!        IF(LNG.EQ.1) WRITE(LU,*)
!!!     &'SUISED : NOMBRE DE COUCHES NON COMPATIBLE AVEC CONSOLIDATION'
!!!        IF(LNG.EQ.2) WRITE(LU,*)
!!!     &'SUISED: NUMBER OF LAYERS NOT COMPATIBLE WITH CONSOLIDATION'
!!!        CALL PLANTE(1)
!!!        STOP
!!!      ENDIF
!
!   5: 4 INTEGERS
      CALL LIT(XB,RB,IB,CB,4,'I',NSUIS,'STD',ISTAT)
      WRITE(LU,*) IB(1),IB(2),IB(3),IB(4)
      IF( (IB(2).NE.NPOIN2) .OR. (IB(3).NE.3) ) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'SUISED : NOMBRE DE NOEUDS NON COMPATIBLE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'SUISED: NUMBER OF NODES NOT COMPATIBLE'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!   6: IKLE
      READ(NSUIS)
!
!   7: IPOBO (NOT IN PARALLEL MODE)
      READ(NSUIS)
!
!   8 AND 9: X AND Y COORDINATES OF THE GRID NODES
      READ(NSUIS)
      READ(NSUIS)
!
!-----------------------------------------------------------------------
!     QUICKLY LOOP TO FIND THE NUMBER OF RECORDS IN THE FILE
      REC = 0
      DO
        READ(NSUIS,END=1901)          ! AT
        DO I = 1,NVAR
           READ(NSUIS)
        ENDDO
        REC = REC + 1
      ENDDO
 1901 CONTINUE
!      
!     REWIND BACK TO BEGINNING OF FILE
      REWIND NSUIS
!
      IF(ITASS.EQ.1) THEN
! 
!----------------------------------------------------------------------- 
! 
!     LOOK FOR 1. BED THICKNESS PER LAYER 
!     IN THE RESULT FILE: 
!     VARIABLE NAMES ARE BED DZ1, BED DZ2, etc....FOR EACH LAYER 
!     UP TO NCOUCH (N.B. NCOUCH IS THE LOWEST LAYER)
! 
!     ZERO THE TOTAL THICKNESS ARRAY
      DO IPOIN = 1,NPOIN2
          HDEP(IPOIN) = 0.D0
      ENDDO
!
!     FIND LAYER THICKNESS VARIABLES IN THE 2D RESULT FILE
      DO I=1,NCOUCH 
! 
!       MAKE THE NUMBERED NAME STRING 
        IF(I.LT.10) THEN
          WRITE(VARNAME,'(A5,I1,A10)')  'LAYER',I,'  THICKNES'  
        ELSEIF(I.LT.100) THEN
          WRITE(VARNAME,'(A5,I2,A9)')  'LAYER',I,' THICKNES'  
        ELSE
          WRITE (LU,*) 'SUISED: NOT IMPLEMENTED FOR ',NCOUCH,' LAYERS'
          CALL PLANTE(1)
          STOP            
        ENDIF 
!           
!       INITIALISE THE FOUND CHECK
        FOUND = .FALSE. 
!
        WRITE(LU,*)  'CHECKING PREVIOUS SED FILE FOR VARIABLE'
        
!       PUT RESULTS INTO T2 BIEF OBJECT
        CALL FIND_IN_SEL(T2,VARNAME,NSUIS,FFORMAT,RB,FOUND,REC) 
!                       
        IF (FOUND)  THEN
          WRITE(LU,*)
     &     'BED LAYER THICKNESS (LAYER',I,') FOUND IN GEOMETRY FILE' 
!
!         TRANSFER THE DATA FROM BIEF OBJECT TO DOUBLE ARRAY
          DO IPOIN = 1,NPOIN2
            EPAI(IPOIN,I) = T2%R(IPOIN)
          ENDDO  
        ELSE
          WRITE(LU,*)  
     &     'BED LAYER THICKNESS (LAYER',I,') NOT FOUND IN GEOMETRY FILE'
        ENDIF
! 
!       SUM THE TOTAL THICKNESS OF DEPOSIT (HDEP)
        DO IPOIN = 1,NPOIN2
          HDEP(IPOIN) = HDEP(IPOIN)+EPAI(IPOIN,I)
        ENDDO
!
      ENDDO 
!       
!     ELEVATION OF RIGID BED
      DO IPOIN = 1,NPOIN2
          ZR(IPOIN) = ZF(IPOIN)-HDEP(IPOIN)
      ENDDO
!             
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! STILL NEED TO CODE CONCENTRATION AND TIME OF CONSOLIDATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!!! CONCENTRATION
!!!        CALL LIT(XB,RB,IB,CB,NPOIN2,'R4',NSUIS,'STD',ISTAT)
!!!        DO I=1,NPOIN2
!!!          DO IPLAN = 1,NCOUCH
!!!            CONC(IPOIN,IPLAN) = DBLE( RB(1+(IPLAN-1)*NPOIN2))* UNITCONV
!!!          ENDDO
!!!        ENDDO
!!!! TIME OF CONSOLIDATION
!!!        CALL LIT(XB,RB,IB,CB,NCOUCH*NPOIN2,'R4',NSUIS,'STD',ISTAT)
!!!        DO IPOIN = 1,NCOUCH*NPOIN2
!!!!>        TEMP(IPOIN) = DBLE( RB(IPOIN) )
!!!        ENDDO
!!!
        ELSEIF (ITASS.EQ.2) THEN
!###>TBE - COMMENTED GIBSON (NOT WORKING YET)
!!!!> ELEVATION Z (TEMPORARILY STORES TRUE LAYER THICKNESS)
!!!        CALL LIT(XB,RB,IB,CB,NPFMAX*NPOIN2,'R4',NSUIS,'STD',ISTAT)
!!!        DO IPOIN = 1,NPOIN2
!!!          ZR(IPOIN) = DBLE( RB(IPOIN) )
!!!          ZPLAN = ZR(IPOIN)
!!!          DO IPLAN = 1,NPFMAX-1
!!!            EPAI(IPOIN,IPLAN) =
!!!     &         DBLE( RB(IPOIN+(IPLAN-1)*NPOIN2) ) - ZPLAN
!!!            ZPLAN = ZPLAN + EPAI(IPOIN,IPLAN)
!!!          ENDDO
!!!          ZF(IPOIN) = DBLE( RB(IPOIN+(NPFMAX-1)*NPOIN2) )
!!!!         HDEB(IPOIN) = ZF(IPOIN) - ZPLAN
!!!        ENDDO
!!!!> TRUE THICKNESS AND NUMBER OF LAYERS
!!!        CALL LIT(XB,RB,IB,CB,NPFMAX*NPOIN2,'R4',NSUIS,'STD',ISTAT)
!!!        DO IPOIN = 1,NPOIN2
!!!          NPF(IPOIN) = INT( RB(IPOIN) )
!!!          IF( NPF(IPOIN).GT.NPFMAX ) THEN
!!!            IF(LNG.EQ.1) THEN
!!!              WRITE(LU,*) 'SUISED : NOMBRE DE COUCHE MAXIMAL NON VALIDE'
!!!            ENDIF
!!!            IF(LNG.EQ.2) THEN
!!!              WRITE(LU,*) 'SUISED: MAXIMUM NUMBER OF LAYERS NOT VALID'
!!!            ENDIF
!!!            CALL PLANTE(1)
!!!            STOP
!!!          ENDIF
!!!          HDEP(IPOIN)=DBLE(RB(IPOIN+(NPFMAX-1)*NPOIN2))*UNITCONV
!!!        ENDDO
!!!!> TRUE DENSITY AND RENUMBERING
!!!        CALL LIT(XB,RB,IB,CB,NPFMAX*NPOIN2,'R4',NSUIS,'STD',ISTAT)
!!!        DO IPOIN = 1, NPOIN2
!!!          DO IPLAN = NPFMAX,NPF(IPOIN)+1,-1
!!!            IVIDE(IPLAN,IPOIN) = 0.D0
!!!          ENDDO
!!!          JPLAN = NPFMAX
!!!          DO IPLAN = NPF(IPOIN),1,-1
!!!            IVIDE(IPLAN,IPOIN) =
!!!     &         RHOS/DBLE( RB(IPOIN+(JPLAN-1)*NPOIN2) ) - 1.D0
!!!            JPLAN = JPLAN - 1
!!!          ENDDO
!!!        ENDDO
!!!!> VIRTUAL THICKNESS EPAI AND RENUMBERING
!!!        DO IPOIN = 1,NPOIN2
!!!          JPLAN = NPFMAX-NPF(IPOIN)+1
!!!          DO IPLAN = 1,NPF(IPOIN)
!!!            ECOUCH =
!!!     &      ( IVIDE(IPLAN,IPOIN) + IVIDE(IPLAN+1,IPOIN) )/2.D0
!!!            EPAI(IPOIN,IPLAN) = EPAI(IPOIN,IPLAN)/( 1.D0+ECOUCH )
!!!            JPLAN = JPLAN + 1
!!!          ENDDO
!!!          DO IPLAN = NPF(IPOIN),NCOUCH
!!!            EPAI(IPOIN,IPLAN) = 0.D0
!!!          ENDDO
!!!        ENDDO
!!!!> LAYER IPF: ONLY USEFUL FOR PLOTTING PURPOSES
!!!!         CALL LIT(XB,RB,IB,CB,NPFMAX*NPOIN2,'R4',NSUIS,'STD',ISTAT)
!!!        DEALLOCATE(XB)
        ENDIF
!        
!     DEALLOCATE TEMPORARY STORAGE
      DEALLOCATE(XB) 
      DEALLOCATE(RB) 
!      
      RETURN
      END
!
