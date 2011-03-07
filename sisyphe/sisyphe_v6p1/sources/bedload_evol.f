!                    *************************
                     SUBROUTINE BEDLOAD_EVOL !
!                    *************************
!
     &(HN,Q,S,ELAY,ACLADM, AVA,COEFPN,CALFA,SALFA,LIMTEC,EBOR,
     & MASKEL,MASK,V2DPAR,UNSV2D,DEBUG,NPOIN,NPTFR,
     & IELMT,KENT,KDIR,KDDL,LOADMETH,
     & DTS,DM,D90,HMIN,LS0,GRAV,XMVS,XMVE,VCE,
     & VF,ENTET,MSK,CONST_ALAYER,LCONDIS,MESH,
     & QS,T1, T2, T3, T4, T5, T6, T7, T8, T9,
     & T10, T11, T12, T13, ELAY0, BREACH, QSX, QSY, ZFCL,SLOPEFF)
!
!***********************************************************************
! SISYPHE   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE EVOLUTION FOR THE BEDLOAD TRANSPORT.
!
!history  F. HUVELIN
!+        14/09/2004
!+        V6P0
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
!| ACLADM         |---| 
!| AVA            |---| 
!| BREACH         |---| 
!| CALFA          |---| 
!| COEFPN         |---| 
!| CONST_ALAYER   |---| 
!| D90            |---| 
!| DEBUG          |---| 
!| DM             |---| 
!| DTS            |---| 
!| EBOR           |---| 
!| ELAY           |---| 
!| ELAY0          |---| 
!| ENTET          |---| 
!| GRAV           |---| 
!| HMIN           |---| 
!| HN             |---| 
!| IELMT          |---| 
!| KDDL           |---| 
!| KDIR           |---| 
!| KENT           |---| 
!| LCONDIS        |---| 
!| LIMTEC         |---| 
!| LOADMETH       |---| 
!| LS0            |---| 
!| MASK           |---| 
!| MASKEL         |---| 
!| MESH           |---| 
!| MSK            |---| 
!| NPOIN          |---| 
!| NPTFR          |---| 
!| Q              |---| 
!| QS             |---| 
!| QSX            |---| 
!| QSY            |---| 
!| S              |---| 
!| SALFA          |---| 
!| SLOPEFF        |---| 
!| T1             |---| 
!| T10            |---| 
!| T11            |---| 
!| T12            |---| 
!| T13            |---| 
!| T2             |---| 
!| T3             |---| 
!| T4             |---| 
!| T5             |---| 
!| T6             |---| 
!| T7             |---| 
!| T8             |---| 
!| T9             |---| 
!| UNSV2D         |---| 
!| V2DPAR         |---| 
!| VCE            |---| 
!| VF             |---| 
!| XMVE           |---| 
!| XMVS           |---| 
!| ZFCL           |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_BEDLOAD_EVOL => BEDLOAD_EVOL
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HN,Q,S,UNSV2D,ELAY,ACLADM
      TYPE(BIEF_OBJ),   INTENT(IN)    :: COEFPN,CALFA,SALFA,LIMTEC
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,MASK,V2DPAR
      INTEGER,          INTENT(IN)    :: DEBUG,SLOPEFF,NPOIN,NPTFR
      INTEGER,          INTENT(IN)    :: IELMT,KENT,KDIR,LOADMETH,KDDL
      DOUBLE PRECISION, INTENT(IN)    :: DTS,DM,D90,HMIN,LS0
      DOUBLE PRECISION, INTENT(IN)    :: GRAV,XMVS,XMVE,VCE,AVA(NPOIN)
      LOGICAL,          INTENT(IN)    :: VF,ENTET,MSK
      LOGICAL,          INTENT(IN)    :: CONST_ALAYER,LCONDIS
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QS,EBOR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1, T2, T3, T4, T5, T6, T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8, T9, T10, T11, T12, T13
      DOUBLE PRECISION, INTENT(INOUT) :: ELAY0
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: BREACH, QSX, QSY, ZFCL

      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: J

!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
      ! **************** !
      ! I - SLOPE EFFECT !
      ! **************** !
        IF (SLOPEFF == 1) THEN
          CALL OS('X=XY    ', X=QS , Y=COEFPN)
        ENDIF
        CALL OS('X=YZ    ', X=QSX, Y=QS, Z=CALFA)
        CALL OS('X=YZ    ', X=QSY, Y=QS, Z=SALFA)
      ! ************************************* !
      ! II - TREATMENT OF NON ERODABLE BOTTOM !
      ! ************************************* !
      IF(DEBUG > 0) WRITE(LU,*) 'BEDLOAD_NERBED_VF'
      IF(VF) THEN
        CALL BEDLOAD_NERBED_VF
     &        (MESH,LIMTEC,KDDL,ELAY%R,V2DPAR%R,QSX,QSY,AVA,NPOIN,
     &         MESH%NSEG, NPTFR, DTS, QS, T1, T2, T3, BREACH)
        CALL OS('X=YZ    ', X=QSX, Y=QS, Z=CALFA)
        CALL OS('X=YZ    ', X=QSY, Y=QS, Z=SALFA)
      ENDIF
      IF(DEBUG > 0) WRITE(LU,*) 'BEDLOAD_NERBED_VF'
      ! ***************************************************** !
      ! IVA - SOLVES THE BED-EVOLUTION EQUATION : F.V.        !
      ! ***************************************************** !
      IF (VF) THEN
         IF(DEBUG > 0) WRITE(LU,*) 'BEDLOAD_SOLVS_VF'
         CALL BEDLOAD_SOLVS_VF
     &        (MESH, QSX, QSY, LIMTEC,UNSV2D, EBOR, BREACH,
     &         MESH%NSEG,NPTFR,NPOIN,KDIR,KDDL,DTS,T10,ZFCL,T11)
         IF(DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_SOLVS_VF'
      ! ****************************************************** !
      ! IVB - SOLVES THE BED-EVOLUTION EQUATION  : F.E.        !
      ! ****************************************************** !
      ELSE
         IF(DEBUG > 0) WRITE(LU,*) 'BEDLOAD_SOLVS_FE'
         DO J=1,NPOIN
           T13%R(J)=AVA(J)*ELAY%R(J)
         ENDDO
         CALL BEDLOAD_SOLVS_FE
     &        (MESH,S,EBOR,MASKEL,MASK,
     &         QSX,QSY,IELMT,NPOIN,NPTFR,KENT,KDIR,
     &         LIMTEC,DTS,MSK,ENTET,T1,T2,T3,T4,T8,
     &         ZFCL,T12,T13,MESH%GLOSEG%I,
     &         MESH%GLOSEG%DIM1,MESH%MSEG%X,
     &         MESH%MSEG%X%R(MESH%NSEG+1:2*MESH%NSEG),
     &         MESH%NSEG,UNSV2D)
         IF(DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_SOLVS_FE'
      ENDIF
      ! ************************************ !
      ! V - VARIABLE ACTIVE LAYER THICKNESS  !
      ! ************************************ !
!     IF(.NOT.CONST_ALAYER.OR.LCONDIS) THEN
!       DO J = 1,NPOIN
!         IF(.NOT.CONST_ALAYER) THEN
!           ELAY0 = MAX(ABS(ZFCL%R(J)),3.D0*ACLADM%R(J))
!         ENDIF
!         IF((QS%R(J)
!    &          (ABS(ZFCL%R(J)/(ELAY0*DTS))
!               ZFCL%R(J) = 0.D0
!         ENDIF
!       ENDDO
!     ENDIF
!======================================================================!
!======================================================================!
      RETURN
      END