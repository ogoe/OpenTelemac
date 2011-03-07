!                    ***********************************
                     SUBROUTINE BEDLOAD_EFFPNT ! (_IMP_)
!                    ***********************************
!
     & (MASKEL,LIQBOR,S,ZF,U2D,V2D,UCMOY,NPOIN,NPTFR,IELMT,KENT,
     &  BETA,PI,MSK,MESH,DZFDX,DZFDY,CTETA,STETA,
     &  COEF,CALFA,SALFA,SLOPEFF,PHISED,DEVIA,BETA2,
     &  TOB,XMVS,XMVE,DM,GRAV,UNSV2D)
!
!***********************************************************************
! SISYPHE   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE PARAMETERS OF THE SLOPE EFFECT.
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+        V5P1
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
!| BETA           |---|
!| BETA2          |---|
!| CALFA          |---|
!| COEF           |---|
!| CTETA          |---|
!| DEVIA          |---|
!| DM             |---|
!| DZFDX          |---|
!| DZFDY          |---|
!| GRAV           |---|
!| IELMT          |---|
!| KENT           |---|
!| LIQBOR         |---|
!| MASKEL         |---|
!| MESH           |---|
!| MSK            |---|
!| NPOIN          |---|
!| NPTFR          |---|
!| PHISED         |---|
!| PI             |---|
!| S              |---|
!| SALFA          |---|
!| SLOPEFF        |---|
!| STETA          |---|
!| TOB            |---|
!| U2D            |---|
!| UCMOY          |---|
!| UNSV2D         |---|
!| V2D            |---|
!| XMVE           |---|
!| XMVS           |---|
!| ZF             |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_BEDLOAD_EFFPNT => BEDLOAD_EFFPNT
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,LIQBOR,S,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF, U2D,V2D, UCMOY, TOB
      INTEGER,          INTENT(IN)    :: NPOIN, NPTFR, IELMT, KENT
      INTEGER,          INTENT(IN)    :: SLOPEFF,DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: BETA, PI, PHISED, BETA2
      DOUBLE PRECISION, INTENT(IN)    :: XMVS, XMVE, GRAV, DM
      LOGICAL,          INTENT(IN)    :: MSK
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: DZFDX, DZFDY
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CTETA,STETA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: COEF, CALFA, SALFA
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: I, K
      DOUBLE PRECISION :: C,ZETA,C1,CALPHA,SALPHA,AA,BB
      DOUBLE PRECISION :: CPSI,SPSI,DZF,TANPHI,CZETA,SZETA,SURBETA2
      DOUBLE PRECISION :: NORM,TT1
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!
! NOTE JMH : IF WE SWAP THE DEVIA AND THE SLOPEFF ACTIONS BELOW
!            IT IS PROBABLY NOT USEFUL TO DO A COPY OF CALFA AND SALFA ?
!
!     COS AND SIN TETA (CALFA AND SALFA HAVE ALREADY BEEN COMPUTED
!                       IN BEDLOAD_SOLIDISCHARGE, SO JUST A COPY HERE)
!     TETA = ANGLE OF THE FLOW WITH OX
!
      CALL OS('X=Y     ',X=CTETA,Y=CALFA)
      CALL OS('X=Y     ',X=STETA,Y=SALFA)
!
!----------------------------------------------------------------------
!
!     COMPUTES THE SLOPE  : D(ZF)/DX ET D(ZF)/DY (AT THE NODES)
!
      CALL VECTOR(DZFDX, '=', 'GRADF          X',IELMT,1.D0,ZF,S,S,
     &            S,S,S,MESH,MSK,MASKEL)
      CALL VECTOR(DZFDY, '=', 'GRADF          Y',IELMT,1.D0,ZF,S,S,
     &            S,S,S,MESH,MSK,MASKEL)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(DZFDX,2,MESH)
        CALL PARCOM(DZFDY,2,MESH)
      ENDIF
!
      CALL OS('X=XY    ',X=DZFDX,Y=UNSV2D)
      CALL OS('X=XY    ',X=DZFDY,Y=UNSV2D)
!
!======================================================================
!
!     COMPUTES THE SOLID TRANSPORT ANGLE ALFA = TETA + DEVIATION
!
!     1 : KOCH AND FLOKSTRA
!
      IF(DEVIA==1) THEN
!
      C = 2.D0*(XMVS-XMVE)*GRAV*DM/3.D0
      DO I=1,NPOIN
        TT1=C/MAX(TOB%R(I),1.D-10)
        AA=STETA%R(I)-TT1*DZFDY%R(I)
        BB=CTETA%R(I)-TT1*DZFDX%R(I)
        NORM=MAX(SQRT(AA**2+BB**2),1.D-10)
        SALFA%R(I)=AA/NORM
        CALFA%R(I)=BB/NORM
      ENDDO
!
!     2 : TALMON ET AL. JHR 1995 33(4)
!
      ELSEIF(DEVIA==2) THEN
!
      SURBETA2=1.D0/BETA2
      C = (XMVS-XMVE)*GRAV*DM*SURBETA2**2
      DO I=1,NPOIN
        TT1=SQRT(C/MAX(TOB%R(I),1.D-10))
        AA=STETA%R(I)-TT1*DZFDY%R(I)
        BB=CTETA%R(I)-TT1*DZFDX%R(I)
        NORM=MAX(SQRT(AA**2+BB**2),1.D-10)
        SALFA%R(I)=AA/NORM
        CALFA%R(I)=BB/NORM
      ENDDO
!
      ENDIF
!
!======================================================================
!
!     COMPUTES THE COEFFICIENT TO TAKE THE SLOPE EFFECT ON THE MAGNITUDE
!     OF THE SOLID TRANSPORT INTO ACCOUNT
!
!     METHOD 1 (EMPIRICAL)
!
      IF(SLOPEFF==1) THEN
!
        DO I=1,NPOIN
          COEF%R(I)=MAX(0.D0,
     &    1.D0-BETA*(DZFDX%R(I)*CTETA%R(I)+DZFDY%R(I)*STETA%R(I)) )
        ENDDO
!
!     METHOD 2 : SOULSBY 1997 DYNAMICS OF MARINE SANDS P107-108
!
      ELSEIF(SLOPEFF.EQ.2) THEN
!
        TANPHI = TAN(PHISED*PI/180.D0)
!
        DO I=1,NPOIN
!
!         COSINE AND SINE OF THE DIRECTION OF THE SLOPE
          DZF=SQRT(DZFDX%R(I)**2+DZFDY%R(I)**2)
          IF(DZF.GT.1.D-12) THEN
            CALPHA=DZFDX%R(I)/DZF
            SALPHA=DZFDY%R(I)/DZF
          ELSE
            CALPHA=1.D0
            SALPHA=0.D0
          ENDIF
!
!         ZETA: ANGLE OF THE SLOPE WITH HORIZONTAL (BETA IN SOULSBY)
          ZETA=ATAN(DZF)
          CZETA=COS(ZETA)
          SZETA=SIN(ZETA)
!
!         PSI: ANGLE OF THE CURRENT WITH THE SLOPE DIRECTION
!         PSI=TETA%R(I)-ALPHA
          CPSI=CTETA%R(I)*CALPHA+STETA%R(I)*SALPHA
          SPSI=STETA%R(I)*CALPHA-CTETA%R(I)*SALPHA
          C1=(CZETA*TANPHI)**2-(SPSI*SZETA)**2
          COEF%R(I)=MAX((CPSI*SZETA+SQRT(MAX(C1,0.D0)))/TANPHI,0.D0)
          COEF%R(I)=MAX(COEF%R(I),0.D0)
!
        ENDDO
!
      ENDIF
!
! ********************************************************************* !
!     V - TREATS THE BOUNDARY POINTS WITH IMPOSED RATE                  !
!         QS IS NOT MODIFIED WHEN SPECIFIED BY THE USER                 !
! ********************************************************************* !
!
      DO K = 1 , NPTFR
         IF (LIQBOR%I(K) == KENT) COEF%R(MESH%NBOR%I(K)) = 1.D0
!                           R.K. MAY 2007
!                           KSORT = 4
         IF (LIQBOR%I(K) == 4) COEF%R(MESH%NBOR%I(K)) = 1.D0
      ENDDO
!
!======================================================================
!======================================================================
!
      RETURN
      END SUBROUTINE BEDLOAD_EFFPNT