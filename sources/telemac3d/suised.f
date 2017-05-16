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
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
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
      USE INTERFACE_HERMES
!!!      USE DECLARATIONS_TELEMAC3D, ONLY: RHOS
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
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
      INTEGER           :: IB(10),IERR
      CHARACTER(LEN=80) :: TITLE
      INTEGER           :: RECORD
      LOGICAL           :: FOUND
      CHARACTER(LEN=16) :: VARNAME
      INTEGER :: NPOIN_SUIS,TYP_ELEM,NELEM,NPTFR,NDP_SUIS,NPLAN
      DOUBLE PRECISION :: TIME
!
      INTEGER           :: NVAR,I
!
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
      CALL READ_MESH_INFO(FFORMAT,NSUIS,TITLE,NVAR,NPOIN_SUIS,TYP_ELEM,
     &                    NELEM,NPTFR,NPTIR,NDP_SUIS,NPLAN)
!
      IF( (NPOIN_SUIS.NE.NPOIN2) .OR. (NDP_SUIS.NE.3) ) THEN
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
        WRITE(LU,*)  'CHECKING PREVIOUS SED FILE FOR VARIABLE'

!       PUT RESULTS INTO T2 BIEF OBJECT
        RECORD = -1 ! To get the last time step
        CALL FIND_VARIABLE(FFORMAT,NSUIS,VARNAME,T2%R,NPOIN_SUIS,IERR,
     &                 RECORD=RECORD,TIME_RECORD=TIME)
!
        IF (IERR.EQ.0)  THEN
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
!!!! YA
!!!! USE FIND_VARIABLE INSTEAD OF LIT
!!!!
!!!! CONCENTRATION
!!!        RECORD = -1 ! To get the last time step
!!!        VARNAME = 'xxxxxxxxxxxxxxxx'
!!!        CALL FIND_VARIABLE(FFORMAT,NSUIS,VARNAME,VALUE,NPOIN_SUIS,IERR,
!!!     &                 RECORD=RECORD,TIME_RECORD=TIME)
!!!        DO I=1,NPOIN2
!!!          DO IPLAN = 1,NCOUCH
!!!            CONC(IPOIN,IPLAN) = VALUE(1+(IPLAN-1)*NPOIN2)* UNITCONV
!!!          ENDDO
!!!        ENDDO
!!!! TIME OF CONSOLIDATION
!!!        VARNAME = 'xxxxxxxxxxxxxxxx'
!!!        CALL FIND_VARIABLE(FFORMAT,NSUIS,VARNAME,TEMP,NPOIN_SUIS,IERR,
!!!     &                 RECORD=RECORD,TIME_RECORD=TIME)
!!!
        ELSEIF (ITASS.EQ.2) THEN
!##>TBE - COMMENTED GIBSON (NOT WORKING YET)
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
!
      RETURN
      END
!
