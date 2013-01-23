!                    *****************
                     SUBROUTINE CORMAR
!                    *****************
!
     &( AT    , LT    , TC1   , TC2   , TV1   , TV2   , TM1   , TM2   ,
     &  NVHMA , NVCOU , PART  , U_TEL , V_TEL , H_TEL )
!
!***********************************************************************
! TOMAWAC   V6P3                                   14/06/2011
!***********************************************************************
!
!brief    INITIALISES ARRAYS OF PHYSICAL PARAMETERS.
!
!history
!+        25/08/2000
!+        V5P0
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
!| TV1            |<--| TIME T1 IN THE WIND FILE
!| TV2            |<--| TIME T2 IN THE WIND FILE
!| U_TEL          |-->| X-AXIS TELEMAC CURRENT SPEED
!| V_TEL          |-->| Y-AXIS TELEMAC CURRENT SPEED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC, EX_CORMAR => CORMAR
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER LT
      INTEGER NVHMA, NVCOU
      DOUBLE PRECISION AT, TC1, TC2 , TV1, TV2, TM1 , TM2
      INTEGER, INTENT(IN)        :: PART
      TYPE(BIEF_OBJ), INTENT(IN) :: U_TEL,V_TEL,H_TEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IP
!
!-----------------------------------------------------------------------
!         UPDATES THE TIDAL CURRENT AND WATER LEVEL ARRAYS
!       ==============================================================
!
!            UPDATES THE CURRENT AT TIME 'AT'
!         ---------------------------------------------
!
      IF(WAC_FILES(WACCOB)%NAME(1:1).NE.' ') THEN
        CALL NOUDON
     & ( SUC%R , SVC%R  , MESH%X%R, MESH%Y%R, NPOIN2     ,
     &   WAC_FILES(WACCOB)%LU , BINCOU , NBOR      , NPTFR , AT , DDC ,
     &   TC1      , TC2       , 
     &   SUC1%R, SVC1%R , SUC2%R  , SVC2%R  , INDIC  ,
     &   'COURANT', NVCOU     )
      ELSEIF (WAC_FILES(WACCOF)%NAME(1:1).NE.' ') THEN
        CALL NOUDON
     & ( SUC%R , SVC%R  , MESH%X%R, MESH%Y%R, NPOIN2     ,
     &   WAC_FILES(WACCOF)%LU  , BINCOU , NBOR   , NPTFR , AT , DDC   ,
     &   TC1      , TC2       , 
     &   SUC1%R, SVC1%R , SUC2%R  , SVC2%R  , INDIC  ,
     &   'COURANT', NVCOU )
      ELSE
        CALL ANAMAR
     & ( SUC%R   , SVC%R   , TRA01(1:NPOIN2), ZM1 , ZM2 ,
     &   SDZHDT%R, MESH%X%R, MESH%Y%R    ,
     &   NPOIN2     , AT   , DDC , LT )
      ENDIF
!
      IF(PART.EQ.1) THEN
        CALL OV('X=Y     ',SUC%R,U_TEL%R,U_TEL%R,0.D0,NPOIN2)
        CALL OV('X=Y     ',SVC%R,V_TEL%R,V_TEL%R,0.D0,NPOIN2)
      ENDIF
!
!            UPDATES THE WATER DEPTH AT TIME 'AT'
!         ------------------------------------------------------
!
      IF(WAC_FILES(WACMAB)%NAME(1:1).NE.' ') THEN
        CALL NOUMAR
     & (TRA01(1:NPOIN2) , SDZHDT%R, MESH%X%R , MESH%Y%R ,
     &  NPOIN2,WAC_FILES(WACMAB)%LU,BINMAR,NBOR,NPTFR,AT,DDC,
     &  TM1,TM2,ZM1,ZM2,INDIM,IDHMA,NVHMA)
      ELSEIF (WAC_FILES(WACMAF)%NAME(1:1).NE.' ') THEN
        CALL NOUMAR
     & (TRA01(1:NPOIN2) , SDZHDT%R, MESH%X%R , MESH%Y%R ,
     &  NPOIN2,WAC_FILES(WACMAF)%LU,BINMAR,NBOR,NPTFR,AT,DDC,
     &  TM1,TM2,ZM1,ZM2,INDIM,IDHMA,NVHMA)
      ELSE
        IF(WAC_FILES(WACCOF)%NAME(1:1).NE.' '.OR.
     &     WAC_FILES(WACCOB)%NAME(1:1).NE.' ') THEN
          CALL ANAMAR
     &   ( SUC%R   , SVC%R   , TRA01(1:NPOIN2), ZM1 , ZM2 ,
     &     SDZHDT%R, MESH%X%R, MESH%Y%R    , NPOIN2    ,
     &     AT   , DDC , LT )
       ENDIF
      ENDIF
!
      IF(PART.LT.0) THEN
        CALL OV('X=X+Y   ', SDEPTH%R , TRA01(1:NPOIN2) , ST0%R ,
     &          0.D0 , NPOIN2)
      ELSEIF(PART.EQ.1) THEN
!..... water depth time gradient is updated
!     (SDEPTH has still water depth values of the previous time step)
        DO IP=1,NPOIN2
          DZHDT(IP)=(H_TEL%R(IP)-DEPTH(IP))/DT
        ENDDO
!......water depth is updated
        CALL OV('X=Y     ',SDEPTH%R,H_TEL%R,H_TEL%R,0.D0,NPOIN2)
      ENDIF
!GM Fin
!
!     UPDATES THE CURRENT AND WATER DEPTH GRADIENTS AT TIME 'AT'
!
      CALL VECTOR(SDZX,'=','GRADF          X',IELM2,1.D0,SDEPTH,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL VECTOR(SDZY,'=','GRADF          Y',IELM2,1.D0,SDEPTH,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
!
      CALL VECTOR(SDUX,'=','GRADF          X',IELM2,1.D0,SUC,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL VECTOR(SDUY,'=','GRADF          Y',IELM2,1.D0,SUC,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
!
      CALL VECTOR(SDVX,'=','GRADF          X',IELM2,1.D0,SVC,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
      CALL VECTOR(SDVY,'=','GRADF          Y',IELM2,1.D0,SVC,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
!
!     INTEGRAL OF TEST FUNCTIONS
!
      CALL VECTOR(ST0,'=','MASBAS          ',IELM2,1.D0,ST0,
     &            ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,ST0)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(SDZX,2,MESH)
        CALL PARCOM(SDUX,2,MESH)
        CALL PARCOM(SDVX,2,MESH)
        CALL PARCOM(SDZY,2,MESH)
        CALL PARCOM(SDUY,2,MESH)
        CALL PARCOM(SDVY,2,MESH)
        CALL PARCOM(ST0,2,MESH)
      ENDIF
!
      CALL OV('X=1/Y   ',ST0%R ,ST0%R,ST0%R,0.D0,NPOIN2)
!
!     DIVISION BY INTEGRAL OF TEST FUNCTIONS TO GET NODAL VALUES
!
      CALL OV('X=XY    ',SDZX%R,ST0%R,ST0%R,0.D0,NPOIN2)
      CALL OV('X=XY    ',SDUX%R,ST0%R,ST0%R,0.D0,NPOIN2)
      CALL OV('X=XY    ',SDVX%R,ST0%R,ST0%R,0.D0,NPOIN2)
      CALL OV('X=XY    ',SDZY%R,ST0%R,ST0%R,0.D0,NPOIN2)
      CALL OV('X=XY    ',SDUY%R,ST0%R,ST0%R,0.D0,NPOIN2)
      CALL OV('X=XY    ',SDVY%R,ST0%R,ST0%R,0.D0,NPOIN2)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

