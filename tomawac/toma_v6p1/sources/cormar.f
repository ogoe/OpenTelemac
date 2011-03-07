!                    *****************
                     SUBROUTINE CORMAR
!                    *****************
!
     &( AT    , LT    , TC1   , TC2   , TV1   , TV2   , TM1   , TM2   ,
     &  NPC   , NPM   , NVHMA , NVCOU )
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |---|
!| LT             |---|
!| NPC            |---|
!| NPM            |---|
!| NVCOU          |---|
!| NVHMA          |---|
!| TC1            |---|
!| TC2            |---|
!| TM1            |---|
!| TM2            |---|
!| TV1            |---|
!| TV2            |---|
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
      INTEGER          NPC , NPM, NVHMA, NVCOU
      INTEGER          LT
      DOUBLE PRECISION AT, TC1, TC2 , TV1, TV2, TM1 , TM2
!
!     LOCAL VARIABLES
      INTEGER N1,N2,N3,N4
!
!-----------------------------------------------------------------------
!         UPDATES THE TIDAL CURRENT AND WATER LEVEL ARRAYS
!       ==============================================================
!
!            UPDATES THE CURRENT AT TIME 'AT'
!         ---------------------------------------------
!
      N1=NPOIN3_G+1
      N2=2*NPOIN3_G
      N3=N2+1
      N4=3*NPOIN3_G
!
      IF (WAC_FILES(WACCOB)%NAME(1:1).NE.' ') THEN
        CALL NOUDON
     & ( SUC%R , SVC%R  , MESH%X%R, MESH%Y%R, NPOIN2     ,
     &   WAC_FILES(WACCOB)%LU , BINCOU , NBOR      , NPTFR , AT , DDC ,
     &   TC1      , TC2       , NPC        , SXRELC%R, SYRELC%R,
     &   TRA01(1:NPOIN3_G),TRA01(N1:N2),TRA01(N3:N4) ,
     &   SUC1%R, SVC1%R , SUC2%R  , SVC2%R  , INDIC  ,
     &   'COURANT', NVCOU     )
      ELSEIF (WAC_FILES(WACCOF)%NAME(1:1).NE.' ') THEN
        CALL NOUDON
     & ( SUC%R , SVC%R  , MESH%X%R, MESH%Y%R, NPOIN2     ,
     &   WAC_FILES(WACCOF)%LU  , BINCOU , NBOR   , NPTFR , AT , DDC   ,
     &   TC1      , TC2       , NPC        , SXRELC%R, SYRELC%R,
     &   TRA01(1:NPOIN3_G),TRA01(N1:N2),TRA01(N3:N4) ,
     &   SUC1%R, SVC1%R , SUC2%R  , SVC2%R  , INDIC  ,
     &   'COURANT', NVCOU )
      ELSE
        CALL ANAMAR
     & ( SUC%R   , SVC%R   , TRA01(1:NPOIN2), ZM1 , ZM2 ,
     &   SDZHDT%R, MESH%X%R, MESH%Y%R    ,
     &   NPOIN2     , AT   , DDC , LT )
      ENDIF
!
!            UPDATES THE WATER DEPTH AT TIME 'AT'
!         ------------------------------------------------------
!
      IF (WAC_FILES(WACMAB)%NAME(1:1).NE.' ') THEN
        CALL NOUMAR
     & (TRA01(1:NPOIN2) , SDZHDT%R, MESH%X%R , MESH%Y%R ,
     &  NPOIN2,WAC_FILES(WACMAB)%LU,BINMAR,NBOR,NPTFR,AT    , DDC ,
     &  TM1     , TM2   , NPM        , SXRELM%R , SYRELM%R ,
     &  TRA01(N1:N2), TRA01(N3:N4), ZM1, ZM2 , INDIM , IDHMA , NVHMA)
      ELSEIF (WAC_FILES(WACMAF)%NAME(1:1).NE.' ') THEN
        CALL NOUMAR
     & (TRA01(1:NPOIN2) , SDZHDT%R, MESH%X%R , MESH%Y%R ,
     &  NPOIN2 ,WAC_FILES(WACMAF)%LU,BINMAR,NBOR,NPTFR,AT   , DDC ,
     &  TM1     , TM2   , NPM        , SXRELM%R , SYRELM%R ,
     &  TRA01(N1:N2), TRA01(N3:N4), ZM1, ZM2 , INDIM , IDHMA , NVHMA)
      ELSE
        IF((WAC_FILES(WACCOF)%NAME(1:1).NE.' ').OR.
     &     (WAC_FILES(WACCOB)%NAME(1:1).NE.' ')) THEN
          CALL ANAMAR
     &   ( SUC%R   , SVC%R   , TRA01(1:NPOIN2), ZM1 , ZM2 ,
     &     SDZHDT%R, MESH%X%R, MESH%Y%R    , NPOIN2    ,
     &     AT   , DDC , LT )
       ENDIF
      ENDIF
!
      CALL OV('X=X+Y   ', SDEPTH%R , TRA01(1:NPOIN2) , ST0%R ,
     &         0.D0 , NPOIN2)
!
!
!            UPDATES THE CURRENT AND WATER DEPTH
!                GRADIENTS AT TIME 'AT'
!         ------------------------------------------------------
!
! W1 ( EX MASKEL) SET TO 1 FOR GRADF
!
      CALL OV ( 'X=C     ' , SW1%R , ST0%R , ST1%R ,
     &          1.D0 , NELEM2 )
!
      CALL VECTOR(ST1,'=','GRADF          X',IELM2,1.D0,SDEPTH,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!
      CALL VECTOR(ST2,'=','GRADF          X',IELM2,1.D0,SUC,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!
      CALL VECTOR(ST3,'=','GRADF          X',IELM2,1.D0,SVC,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!
      CALL VECTOR(ST4,'=','GRADF          X',IELM2,1.D0,MESH%X,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!BD_INCKA MODIFICATION FOR PARALLEL MODE
       IF(NCSIZE.GT.1) THEN
          CALL PARCOM(ST1,2,MESH)
          CALL PARCOM(ST2,2,MESH)
          CALL PARCOM(ST3,2,MESH)
          CALL PARCOM(ST4,2,MESH)
       ENDIF
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
!
      CALL OV('X=Y/Z   ',SDZX%R,ST1%R,ST4%R,0.D0,NPOIN2)
      CALL OV('X=Y/Z   ',SDUX%R,ST2%R,ST4%R,0.D0,NPOIN2)
      CALL OV('X=Y/Z   ',SDVX%R,ST3%R,ST4%R,0.D0,NPOIN2)
!
      CALL VECTOR(ST1,'=','GRADF          Y',IELM2,1.D0,SDEPTH,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!
      CALL VECTOR(ST2,'=','GRADF          Y',IELM2,1.D0,SUC,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!
      CALL VECTOR(ST3,'=','GRADF          Y',IELM2,1.D0,SVC,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!
      CALL VECTOR(ST4,'=','GRADF          Y',IELM2,1.D0,MESH%Y,
     & ST0,ST0,ST0,ST0,ST0,MESH,.FALSE.,SW1)
!BD_INCKA MODIFICATION FOR PARALLEL MODE
       IF(NCSIZE.GT.1) THEN
          CALL PARCOM(ST1,2,MESH)
          CALL PARCOM(ST2,2,MESH)
          CALL PARCOM(ST3,2,MESH)
          CALL PARCOM(ST4,2,MESH)
       ENDIF
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
!
      CALL OV('X=Y/Z   ',SDZY%R,ST1%R,ST4%R,0.D0,NPOIN2)
      CALL OV('X=Y/Z   ',SDUY%R,ST2%R,ST4%R,0.D0,NPOIN2)
      CALL OV('X=Y/Z   ',SDVY%R,ST3%R,ST4%R,0.D0,NPOIN2)
!
!-----------------------------------------------------------------------
!
      RETURN
      END