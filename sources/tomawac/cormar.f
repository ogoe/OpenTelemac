!                    *****************
                     SUBROUTINE CORMAR
!                    *****************
!
     &( AT    , LT    , TC1   , TC2   , TM1   , TM2   ,
     &  NVHMA , NVCOU , PART  , U_TEL , V_TEL , H_TEL )
!
!***********************************************************************
! TOMAWAC   V7P0                                   14/06/2011
!***********************************************************************
!
!brief    INITIALISES ARRAYS OF PHYSICAL PARAMETERS.
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
!history  G.MATTAROLO (EDF)
!+        05/2011
!+        V6P1
!+   Modification for direct coupling with TELEMAC
!
!history  G.MATTAROLO (EDF - LNHE)
!+        14/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF - LNHE)
!+        07/12/2012
!+        V6P3
!+   Taking into account tidal flats + various optimisations.
!
!history  J-M HERVOUET (EDF - LNHE)
!+        08/01/2014
!+        V7P0
!+   CALL PARCOM suppressed by using new argument ASSPAR in VECTOR
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| H_TEL          |-->| TELEMAC WATER DEPTH
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!| NVCOU          |<--| NUMBER OF VARIABLES OF THE FORMATTED CURRENT FILE
!| NVHMA          |<--| N.OF VARIABLES OF THE FORMATTED WATER LEVEL FILE
!| PART           |-->| FLAG FOR DIRECT COUPLING WITH TELEMAC
!| TC1            |<--| TIME T1 IN THE CURRENT FILE
!| TC2            |<--| TIME T2 IN THE CURRENT FILE
!| TM1            |<--| TIME T1 IN THE WATER LEVEL FILE
!| TM2            |<--| TIME T2 IN THE WATER LEVEL FILE
!| U_TEL          |-->| X-AXIS TELEMAC CURRENT SPEED
!| V_TEL          |-->| Y-AXIS TELEMAC CURRENT SPEED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC, EX_CORMAR => CORMAR
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             ::LT
      INTEGER, INTENT(INOUT)             :: NVHMA
      INTEGER, INTENT(INOUT)          :: NVCOU
      DOUBLE PRECISION, INTENT(IN)    :: AT
      DOUBLE PRECISION, INTENT(INOUT) :: TC1,TC2,TM1,TM2
      INTEGER, INTENT(IN)        :: PART
      TYPE(BIEF_OBJ), INTENT(IN) :: U_TEL,V_TEL,H_TEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IP,UL
      LOGICAL TROUVE(3)
      CHARACTER(LEN=8) FMTCOU, FMTMAR
!
!-----------------------------------------------------------------------
!
!     UPDATES THE TIDAL CURRENT AND WATER LEVEL ARRAYS
!     ================================================
!
!     UPDATES THE CURRENT AT TIME 'AT'
!
      IF(WAC_FILES(WACCOB)%NAME(1:1).NE.' '.OR.
     &   WAC_FILES(WACCOF)%NAME(1:1).NE.' '    ) THEN
!
        IF(WAC_FILES(WACCOB)%NAME(1:1).NE.' ') THEN
          UL=WAC_FILES(WACCOB)%LU
          FMTCOU=WAC_FILES(WACCOB)%FMT
        ELSE
          UL=WAC_FILES(WACCOF)%LU
          FMTCOU=WAC_FILES(WACCOF)%FMT
        ENDIF
        CALL NOUDON(UC,NAMEU, 'VELOCITY U      M/S             ',2,
     &              VC,NAMEV, 'VELOCITY V      M/S             ',2,
     &              DEPTH,NAMEH, 'WATER DEPTH     M               ',1,
     &              X, Y, NPOIN2,
     &              UL,FMTCOU,NBOR,NPTFR,AT,DDC,TC1,TC2,
     &              UC1, UC2, VC1, VC2, ZM1, ZM2, INDIC,
     &              'COURANT',NVCOU,TEXCOB,TROUVE,UNITCOB,PHASCOB)
        IF(TROUVE(3)) THEN
          CALL OV('X=Y-Z   ',DZHDT, ZM2, ZM1, 0.D0, NPOIN2)
          CALL OV('X=CX    ',DZHDT, DZHDT, DZHDT, 1.D0/(TC2-TC1),NPOIN2)
        ENDIF
!
      ELSEIF (PART.EQ.-1) THEN
!
        CALL ANAMAR(UC, VC, DEPTH, ZM1, ZM2,
     &              DZHDT, X, Y, NPOIN2, AT, DDC, LT)
!
      ENDIF
!
      IF(PART.EQ.1) THEN
        CALL OV('X=Y     ',UC, U_TEL%R, U_TEL%R, 0.D0, NPOIN2)
        CALL OV('X=Y     ',VC, V_TEL%R, V_TEL%R, 0.D0, NPOIN2)
      ENDIF
!
!     UPDATES THE WATER DEPTH AT TIME 'AT' IF NOT FOUND IN CURRENT FILE
!
      IF(.NOT.TROUVE(3)) THEN
      IF(WAC_FILES(WACMAB)%NAME(1:1).NE.' '.OR.
     &   WAC_FILES(WACMAF)%NAME(1:1).NE.' ') THEN
!
        IF(WAC_FILES(WACMAB)%NAME(1:1).NE.' ') THEN
          UL=WAC_FILES(WACMAB)%LU
          FMTMAR=WAC_FILES(WACMAB)%FMT
        ELSE
          UL=WAC_FILES(WACMAF)%LU
          FMTMAR=WAC_FILES(WACMAF)%FMT
        ENDIF
!
        CALL NOUDON(UC,NAMEU, 'VELOCITY U      M/S             ',0,
     &              VC,NAMEV, 'VELOCITY V      M/S             ',0,
     &              DEPTH,NAMEH,  'WATER DEPTH     M               ',2,
     &              X, Y, NPOIN2,
     &              UL,FMTMAR,NBOR,NPTFR,AT,DDC,TM1,TM2,
     &              UC1, UC2, VC1, VC2, ZM1, ZM2, INDIM,
     &              'HAUTEUR',NVHMA,TEXMAB,TROUVE,UNITMAB,PHASMAB)
        CALL OV('X=Y-Z   ',DZHDT, ZM2, ZM1, 0.D0, NPOIN2)
        CALL OV('X=CX    ',DZHDT,DZHDT,DZHDT,1.D0/(TM2-TM1),NPOIN2)
!
      ELSEIF (PART.EQ.-1) THEN
!
        IF(WAC_FILES(WACCOF)%NAME(1:1).NE.' '.OR.
     &     WAC_FILES(WACCOB)%NAME(1:1).NE.' ') THEN
          CALL ANAMAR(UC, VC, DEPTH, ZM1, ZM2,
     &                DZHDT, X, Y, NPOIN2, AT, DDC, LT)
        ENDIF
!
      ENDIF
      ENDIF
!
      IF(PART.EQ.1) THEN
!       water depth time gradient is updated
!       SDEPTH has still water depth values of the previous time step)
        DO IP=1,NPOIN2
          DZHDT(IP)=(H_TEL%R(IP)-DEPTH(IP))/DT
        ENDDO
!       water depth is updated
        CALL OV('X=Y     ',DEPTH,H_TEL%R,H_TEL%R,0.D0,NPOIN2)
      ENDIF
!
!     UPDATES THE CURRENT AND WATER DEPTH GRADIENTS AT TIME 'AT'
!
      IF(.NOT.PROINF) THEN
        CALL VECTOR(SDZX,'=','GRADF          X',IELM2,1.D0,SDEPTH,
     &              ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,ASSPAR=.TRUE.)
        CALL VECTOR(SDZY,'=','GRADF          Y',IELM2,1.D0,SDEPTH,
     &              ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,ASSPAR=.TRUE.)
      ENDIF
!
      CALL VECTOR(SDUX,'=','GRADF          X',IELM2,1.D0,SUC,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,ASSPAR=.TRUE.)
      CALL VECTOR(SDUY,'=','GRADF          Y',IELM2,1.D0,SUC,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,ASSPAR=.TRUE.)
!
      CALL VECTOR(SDVX,'=','GRADF          X',IELM2,1.D0,SVC,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,ASSPAR=.TRUE.)
      CALL VECTOR(SDVY,'=','GRADF          Y',IELM2,1.D0,SVC,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,ASSPAR=.TRUE.)
!
!     INTEGRAL OF TEST FUNCTIONS
!
      CALL VECTOR(ST0,'=','MASBAS          ',IELM2,1.D0,ST0,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0,ASSPAR=.TRUE.)
!
      CALL OV('X=1/Y   ',T0 ,T0, T0, 0.D0, NPOIN2)
!
!     DIVISION BY INTEGRAL OF TEST FUNCTIONS TO GET NODAL VALUES
!
      IF(.NOT.PROINF) THEN
        CALL OV('X=XY    ',DZX, T0, T0, 0.D0, NPOIN2)
        CALL OV('X=XY    ',DZY, T0, T0, 0.D0, NPOIN2)
      ENDIF
      CALL OV('X=XY    ',DUX, T0, T0, 0.D0, NPOIN2)
      CALL OV('X=XY    ',DVX, T0, T0, 0.D0, NPOIN2)
      CALL OV('X=XY    ',DUY, T0, T0, 0.D0, NPOIN2)
      CALL OV('X=XY    ',DVY, T0, T0, 0.D0, NPOIN2)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
