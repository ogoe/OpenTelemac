!                       ************************
                        SUBROUTINE CALCUL_Q_WEIR
!                       ************************
!
     &    (IOPTAN)
!
!***********************************************************************
! TELEMAC2D   V7P2                                   22/03/2013
!***********************************************************************
!
!brief    COMPUTE THE DISCHARGE ON THE WEIRS WHEN THE TYPE IS EQUAL 2
!+
!
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        22/03/2013
!+        V6P3
!+   Creation
!
!
!history  C.COULET(ARTELIA)
!+        01/09/2016
!+        V7P2
!+   New management of parallelism
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IOPTAN         |-->| OPTION FOR TANGENTIAL VELOCITIES.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!      USE DECLARATIONS_TELEMAC2D, ONLY: NPSING,NDGA1,NDGA2,NDGB1,NDGB2,
!     &                                  QWA,QWB,QP0,ZDIG,WDIG,
!     &                                  UWEIRA,UWEIRB,VWEIRA,VWEIRB,
!     &                                  TWEIRA,TWEIRB,MAXNPS
!
      USE DECLARATIONS_SPECIAL
!##> JR @ RWTH: ALLOW COMPILERS TO CHECK PARALLEL INTERFACE
      USE INTERFACE_PARALLEL, ONLY : P_DMAX,P_DMIN
!##< JR @ RWTH
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: IOPTAN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          I, N, ITRAC, INDIC, NB16
      INTEGER          I_1A_1, I_1B_1, I_1A_2, I_1B_2
      INTEGER          I_2A_1, I_2B_1, I_2A_2, I_2B_2
!
      DOUBLE PRECISION PHI, RELAX
      DOUBLE PRECISION YS1, YS2
      DOUBLE PRECISION SL_1A_1, SL_1B_1, SL_1A_2, SL_1B_2
      DOUBLE PRECISION SL_2A_1, SL_2B_1, SL_2A_2, SL_2B_2
      DOUBLE PRECISION ZF_1A_1, ZF_1B_1, ZF_1A_2, ZF_1B_2
      DOUBLE PRECISION ZF_2A_1, ZF_2B_1, ZF_2A_2, ZF_2B_2
      DOUBLE PRECISION H_1A_1, H_1B_1, H_1A_2, H_1B_2
      DOUBLE PRECISION H_2A_1, H_2B_1, H_2A_2, H_2B_2
      DOUBLE PRECISION SLA, SLB
      DOUBLE PRECISION ZFA, ZFB
      DOUBLE PRECISION HSA, HSB
      DOUBLE PRECISION H_A, H_B, HMINI
      DOUBLE PRECISION TXA,TYA,DLA,NXA,NYA
      DOUBLE PRECISION TXB,TYB,DLB,NXB,NYB
      DOUBLE PRECISION UTANA, UTANB, UTANC, UTAND
      DOUBLE PRECISION PENTA,PENTB
      DOUBLE PRECISION DENOM
!
      DOUBLE PRECISION XP_1A_1,XP_1B_1,XP_1A_2,XP_1B_2
      DOUBLE PRECISION XP_2A_1,XP_2B_1,XP_2A_2,XP_2B_2
      DOUBLE PRECISION YP_1A_1,YP_1B_1,YP_1A_2,YP_1B_2
      DOUBLE PRECISION YP_2A_1,YP_2B_1,YP_2A_2,YP_2B_2
!
      DOUBLE PRECISION TRAC_1A_1(NTRAC),TRAC_1B_1(NTRAC)
      DOUBLE PRECISION TRAC_1A_2(NTRAC),TRAC_1B_2(NTRAC)
      DOUBLE PRECISION TRAC_2A_1(NTRAC),TRAC_2B_1(NTRAC)
      DOUBLE PRECISION TRAC_2A_2(NTRAC),TRAC_2B_2(NTRAC)
      DOUBLE PRECISION TRAC_A1(NTRAC) ,TRAC_B1(NTRAC)
      DOUBLE PRECISION TRAC_A2(NTRAC) ,TRAC_B2(NTRAC)
!
!##> JR @ RWTH: INTERFACE CHECKED SO NO NEED FOR EXTERNALS
!      DOUBLE PRECISION P_DMAX,P_DMIN
!      EXTERNAL         P_DMAX,P_DMIN
!##< JR @ RWTH
!
      INTEGER, DIMENSION(:), ALLOCATABLE :: LIST_NODES
      INTEGER, DIMENSION(:), ALLOCATABLE :: LIST_PROC
      INTEGER :: ERROR
!
      INTRINSIC MAX,SQRT,ABS
!
!-----------------------------------------------------------------------
!
!
      HMINI = 1.D-2
      PHI = 0.4
      RELAX = 0.5D0 ! TODO: DEFINE AS USER SETTING
!
!      CALL OS('X=0     ',X=UWEIRA)
!      CALL OS('X=0     ',X=UWEIRB)
!      CALL OS('X=0     ',X=VWEIRA)
!      CALL OS('X=0     ',X=VWEIRB)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING COLLECT_VALUES'
      CALL COLLECT_VALUES
      IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM COLLECT_VALUES'
!      WRITE(LU,*) 'Verif ZF', NWEIRS_NODES
!      DO I=1, NWEIRS_NODES
!        WRITE(LU,*) I, WNODES(I)%ZFN
!      ENDDO
!      WRITE(LU,*) 'Verif H', NWEIRS_NODES
!      DO I=1, NWEIRS_NODES
!        WRITE(LU,*) I, WNODES(I)%HN
!        call flush(lu)
!      ENDDO
!
      DO N = 1, NWEIRS   !LOOP ON ELEMENTARY PEACE OF WEIRS
!        IF(AT.GT.11510) WRITE(LU,*) '1-', N
        I_1A_1  = WEIRS(N)%N_1A_1
        I_1A_2  = WEIRS(N)%N_1A_2
        I_2A_1  = WEIRS(N)%N_2A_1
        I_2A_2  = WEIRS(N)%N_2A_2
        I_1B_1  = WEIRS(N)%N_1B_1
        I_1B_2  = WEIRS(N)%N_1B_2
        I_2B_1  = WEIRS(N)%N_2B_1
        I_2B_2  = WEIRS(N)%N_2B_2
!
        ZF_1A_1 = WNODES(I_1A_1)%ZFN
        ZF_1A_2 = WNODES(I_1A_2)%ZFN
        ZF_2A_1 = WNODES(I_2A_1)%ZFN
        ZF_2A_2 = WNODES(I_2A_2)%ZFN
        ZF_1B_1 = WNODES(I_1B_1)%ZFN
        ZF_1B_2 = WNODES(I_1B_2)%ZFN
        ZF_2B_1 = WNODES(I_2B_1)%ZFN
        ZF_2B_2 = WNODES(I_2B_2)%ZFN
!
        H_1A_1  = WNODES(I_1A_1)%HN
        H_1A_2  = WNODES(I_1A_2)%HN
        H_2A_1  = WNODES(I_2A_1)%HN
        H_2A_2  = WNODES(I_2A_2)%HN
        H_1B_1  = WNODES(I_1B_1)%HN
        H_1B_2  = WNODES(I_1B_2)%HN
        H_2B_1  = WNODES(I_2B_1)%HN
        H_2B_2  = WNODES(I_2B_2)%HN
!
        ZFA = 0.25D0 * (ZF_1A_1 + ZF_1A_2 + ZF_2A_1 + ZF_2A_2 )
        ZFB = 0.25D0 * (ZF_1B_1 + ZF_1B_2 + ZF_2B_1 + ZF_2B_2 )
        H_A = 0.25D0 * (H_1A_1 + H_1A_2 + H_2A_1 + H_2A_2 )
        H_B = 0.25D0 * (H_1B_1 + H_1B_2 + H_2B_1 + H_2B_2 )
        SLA = ZFA + H_A
        SLB = ZFB + H_B
!
!       CALCULATES THE NORMAL VECTOR, OUTGOING SIDE A, ENTERING SIDE B
!
!        TXA=XP_2A_2-XP_1A_1
!        TYA=YP_2A_2-YP_1A_1
!        DLA=SQRT(TXA**2+TYA**2)
!        TXA=TXA/DLA
!        TYA=TYA/DLA
!        NXA=-TYA
!        NYA=TXA
!
!        TXB=XP_2B_2-XP_1B_1
!        TYB=YP_2B_2-YP_1B_1
!        DLB=SQRT(TXB**2+TYB**2)
!        TXB=TXB/DLB
!        TYB=TYB/DLB
!        NXB=-TYB
!        NYB=TXB
!
!       COMPUTATION OF THE DISCHARGE
!
!       ADDING A SECURITY ON THE LEVEL OF THE WEIR
        YS1 = MAX(WEIRS(N)%Z1,ZFA+0.01D0,ZFB+0.01D0)
        YS2 = MAX(WEIRS(N)%Z2,ZFA+0.01D0,ZFB+0.01D0)
!
        WEIRS(N)%Q = 0.D0
!       UPSTREAM IS ON SIDE A
        IF (SLA.GT.SLB) THEN
          IF (H_A.GT.HMINI) THEN
            CALL LOI_W_INC(SLA,SLB,YS1,YS2,WEIRS(N)%WIDTH,
     &                     PHI,WEIRS(N)%Q,GRAV)
          ENDIF
!       UPSTREAM IS ON SIDE B
        ELSE
          IF (H_B.GT.HMINI) THEN
            CALL LOI_W_INC(SLB,SLA,YS1,YS2,WEIRS(N)%WIDTH,
     &                     PHI,WEIRS(N)%Q,GRAV)
            WEIRS(N)%Q = -WEIRS(N)%Q
          ENDIF
        ENDIF
!
!        IF(AT.GT.11510) WRITE(LU,*) '4-', WEIRS(N)%Q, WEIRS(N)%Q0
!        WRITE(LU,*) 'Q ', N, SLA, SLB, WEIRS(N)%Q
!        call flush(lu)
        WEIRS(N)%Q  = WEIRS(N)%Q * (1D0-RELAX) + WEIRS(N)%Q0 * RELAX
        WEIRS(N)%Q0 = WEIRS(N)%Q
      ENDDO
!
! TODO: STREALINE DISCHARGES COMPUTED ON IDENTICAL ELEMENTS
!
!       NOW WE DISTRIBUTE THE COMPUTED DISCHARGE OF EACH ELEMENTS OF WEIRS ON NODES
!       QELEM > 0 means the flow is from side A to side B
!
      DO N = 1, NWEIRS_NODES
        WNODES(N)%QN = 0.D0
        DO ITRAC=1, NTRAC
          WNODES(N)%TRAC(ITRAC) = 0.D0
        ENDDO
      ENDDO
!
      DO N = 1, NWEIRS
        I_1A_1 = WEIRS(N)%N_1A_1
        I_1A_2 = WEIRS(N)%N_1A_2
        I_2A_1 = WEIRS(N)%N_2A_1
        I_2A_2 = WEIRS(N)%N_2A_2
        I_1B_1 = WEIRS(N)%N_1B_1
        I_1B_2 = WEIRS(N)%N_1B_2
        I_2B_1 = WEIRS(N)%N_2B_1
        I_2B_2 = WEIRS(N)%N_2B_2
        WNODES(I_1A_1)%QN = WNODES(I_1A_1)%QN - 0.25D0 * WEIRS(N)%Q
        WNODES(I_1A_2)%QN = WNODES(I_1A_2)%QN - 0.25D0 * WEIRS(N)%Q
        WNODES(I_2A_1)%QN = WNODES(I_2A_1)%QN - 0.25D0 * WEIRS(N)%Q
        WNODES(I_2A_2)%QN = WNODES(I_2A_2)%QN - 0.25D0 * WEIRS(N)%Q
        WNODES(I_1B_1)%QN = WNODES(I_1B_1)%QN + 0.25D0 * WEIRS(N)%Q
        WNODES(I_1B_2)%QN = WNODES(I_1B_2)%QN + 0.25D0 * WEIRS(N)%Q
        WNODES(I_2B_1)%QN = WNODES(I_2B_1)%QN + 0.25D0 * WEIRS(N)%Q
        WNODES(I_2B_2)%QN = WNODES(I_2B_2)%QN + 0.25D0 * WEIRS(N)%Q
      ENDDO
!
!     MANAGEMENT OF THE TRACER
!
      DO N = 1, NWEIRS_NODES
        DO ITRAC=1, NTRAC
          WNODES(N)%TRAC(ITRAC) = 0.D0
        ENDDO
      ENDDO
!
      IF(NTRAC.GT.0) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING COLLECT_VALUES_TRAC'
        CALL COLLECT_VALUES_TRAC
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM COLLECT_VALUES_TRAC'
!        WRITE(LU,*) 'Verif Trac 1', NWEIRS_NODES
!        DO I=1, NWEIRS_NODES
!          WRITE(LU,*) I, WNODES(I)%TRAC(1)
!        ENDDO
      ENDIF
!
    !! DO N = 1, NWEIRS
    !!   I_1A_1 = WEIRS(N)%N_1A_1
    !!   I_1A_2 = WEIRS(N)%N_1A_2
    !!   I_2A_1 = WEIRS(N)%N_2A_1
    !!   I_2A_2 = WEIRS(N)%N_2A_2
    !!   I_1B_1 = WEIRS(N)%N_1B_1
    !!   I_1B_2 = WEIRS(N)%N_1B_2
    !!   I_2B_1 = WEIRS(N)%N_2B_1
    !!   I_2B_2 = WEIRS(N)%N_2B_2
!   !!    WRITE(LU,*) 'Ind ', I_1A_1, I_1A_2, I_2A_1, I_2A_2,
!   !! &                      I_1B_1, I_1B_2, I_2B_1, I_2B_2
!   !!
    !!   DO ITRAC = 1, NTRAC
    !!     TRAC_1A_1(ITRAC) = WNODES(I_1A_1)%TRAC(ITRAC)
    !!     TRAC_1A_2(ITRAC) = WNODES(I_1A_2)%TRAC(ITRAC)
    !!     TRAC_2A_1(ITRAC) = WNODES(I_2A_1)%TRAC(ITRAC)
    !!     TRAC_2A_2(ITRAC) = WNODES(I_2A_2)%TRAC(ITRAC)
    !!     TRAC_1B_1(ITRAC) = WNODES(I_1B_1)%TRAC(ITRAC)
    !!     TRAC_1B_2(ITRAC) = WNODES(I_1B_2)%TRAC(ITRAC)
    !!     TRAC_2B_1(ITRAC) = WNODES(I_2B_1)%TRAC(ITRAC)
    !!     TRAC_2B_2(ITRAC) = WNODES(I_2B_2)%TRAC(ITRAC)
    !!     TRAC_A1(ITRAC) = 0.5D0 * (TRAC_1A_1(ITRAC)+TRAC_1A_2(ITRAC))
    !!     TRAC_B1(ITRAC) = 0.5D0 * (TRAC_1B_1(ITRAC)+TRAC_1B_2(ITRAC))
    !!     TRAC_A2(ITRAC) = 0.5D0 * (TRAC_2A_1(ITRAC)+TRAC_2A_2(ITRAC))
    !!     TRAC_B2(ITRAC) = 0.5D0 * (TRAC_2B_1(ITRAC)+TRAC_2B_2(ITRAC))
!   !!      WRITE(LU,*) 'A- ',TRAC_A1(ITRAC), TRAC_A2(ITRAC)
!   !!      WRITE(LU,*) 'B- ',TRAC_B1(ITRAC), TRAC_B2(ITRAC)
!   !!
    !!     IF(WEIRS(N)%Q.GT.0.D0) THEN ! A --> B
!   !!        WRITE(LU,*) 'A1-', I_1B_1, WNODES(I_1B_1)%TRAC(ITRAC),
!   !! &         TRAC_A1(ITRAC), WNODES(I_1B_1)%QN
!   !!        WRITE(LU,*) 'A2-', I_1B_2, WNODES(I_1B_2)%TRAC(ITRAC),
!   !! &         TRAC_A1(ITRAC), WNODES(I_1B_2)%QN
!   !!        WRITE(LU,*) 'A3-', I_2B_1, WNODES(I_2B_1)%TRAC(ITRAC),
!   !! &         TRAC_A2(ITRAC), WNODES(I_2B_1)%QN
!   !!        WRITE(LU,*) 'A4-', I_2B_2, WNODES(I_2B_2)%TRAC(ITRAC),
!   !! &         TRAC_A2(ITRAC), WNODES(I_2B_2)%QN
    !!       WNODES(I_1B_1)%TRAC(ITRAC) = WNODES(I_1B_1)%TRAC(ITRAC) +
    !!&         TRAC_1A_1(ITRAC) * 0.25D0 * WEIRS(N)%Q
!   !! &         TRAC_A1(ITRAC) * WNODES(I_1A_1)%QN
    !!       WNODES(I_1B_2)%TRAC(ITRAC) = WNODES(I_1B_2)%TRAC(ITRAC) +
    !!&         TRAC_1A_2(ITRAC) * 0.25D0 * WEIRS(N)%Q
!   !! &         TRAC_A1(ITRAC) * WNODES(I_1A_2)%QN
    !!       WNODES(I_2B_1)%TRAC(ITRAC) = WNODES(I_2B_1)%TRAC(ITRAC) +
    !!&         TRAC_2A_1(ITRAC) * 0.25D0 * WEIRS(N)%Q
!   !! &         TRAC_A2(ITRAC) * WNODES(I_2A_1)%QN
    !!       WNODES(I_2B_2)%TRAC(ITRAC) = WNODES(I_2B_2)%TRAC(ITRAC) +
    !!&         TRAC_2A_2(ITRAC) * 0.25D0 * WEIRS(N)%Q
!   !! &         TRAC_A2(ITRAC) * WNODES(I_2A_2)%QN
    !!     ELSEIF(WEIRS(N)%Q.LT.0.D0) THEN ! B --> A
!   !!        WRITE(LU,*) 'A1-', I_1A_1, WNODES(I_1A_1)%TRAC(ITRAC),
!   !! &         TRAC_B1(ITRAC), WNODES(I_1A_1)%QN
!   !!        WRITE(LU,*) 'A2-', I_1A_2, WNODES(I_1A_2)%TRAC(ITRAC),
!   !! &         TRAC_B1(ITRAC), WNODES(I_1A_2)%QN
!   !!        WRITE(LU,*) 'A3-', I_2A_1, WNODES(I_2A_1)%TRAC(ITRAC),
!   !! &         TRAC_B2(ITRAC), WNODES(I_2A_1)%QN
!   !!        WRITE(LU,*) 'A4-', I_2A_2, WNODES(I_2A_2)%TRAC(ITRAC),
!   !! &         TRAC_B2(ITRAC), WNODES(I_2A_2)%QN
    !!       WNODES(I_1A_1)%TRAC(ITRAC) = WNODES(I_1A_1)%TRAC(ITRAC) -
    !!&         TRAC_B1(ITRAC) * 0.25D0 * WEIRS(N)%Q
!   !! &         TRAC_B1(ITRAC) * WNODES(I_1B_1)%QN
    !!       WNODES(I_1A_2)%TRAC(ITRAC) = WNODES(I_1A_2)%TRAC(ITRAC) -
    !!&         TRAC_B1(ITRAC) * 0.25D0 * WEIRS(N)%Q
!   !! &         TRAC_B1(ITRAC) * WNODES(I_1B_2)%QN
    !!       WNODES(I_2A_1)%TRAC(ITRAC) = WNODES(I_2A_1)%TRAC(ITRAC) -
    !!&         TRAC_B2(ITRAC) * 0.25D0 * WEIRS(N)%Q
!   !! &         TRAC_B2(ITRAC) * WNODES(I_2B_1)%QN
    !!       WNODES(I_2A_2)%TRAC(ITRAC) = WNODES(I_2A_2)%TRAC(ITRAC) -
    !!&         TRAC_B2(ITRAC) * 0.25D0 * WEIRS(N)%Q
!   !! &         TRAC_B2(ITRAC) * WNODES(I_2B_2)%QN
    !!     ENDIF
    !!   ENDDO
    !! ENDDO
!   !!
!   !! THE DISCHARGE IS NOW COMPUTED ON ALL NODES OF WEIRS
!   !! SO WE COULD COMPUTE AN AVERAGE VALUE OF CONCENTRATION OF TRACERS
!   !!
    !! DO N = 1, NWEIRS_NODES
!   !!    IF(ABS(WNODES(N)%QN).GT.0) THEN
    !!   IF(WNODES(N)%QN.GT.0) THEN
    !!     DENOM = 1.D0 / ABS(WNODES(N)%QN)
!   !!      WRITE(LU,*) '1- ',N, WNODES(N)%QN, WNODES(N)%TRAC(1)
    !!     DO ITRAC=1, NTRAC
    !!       WNODES(N)%TRAC(ITRAC) = WNODES(N)%TRAC(ITRAC) * DENOM
    !!     ENDDO
    !!   ENDIF
    !! ENDDO
!
      RETURN
      END






!!!
!!        DO I = 1 ,NPSING%I(N)-1
!!!         FIND THE NODES OF MESH AROUND THE NODE COMPOSING THE WEIR
!!          I_1A_1 = NDGA1%ADR(N)%P%I(I)
!!          I_1B_1 = NDGB1%ADR(N)%P%I(I)
!!!          I_1A_2 = NDGA2%ADR(N)%P%I(I)
!!!          I_1B_2 = NDGB2%ADR(N)%P%I(I)
!!          I_2A_1 = NDGA1%ADR(N)%P%I(I+1)
!!          I_2B_1 = NDGB1%ADR(N)%P%I(I+1)
!!!          I_2A_2 = NDGA2%ADR(N)%P%I(I+1)
!!!          I_2B_2 = NDGB2%ADR(N)%P%I(I+1)
!!!         FIND THE VALUE OF BOTTOM, WATER LEVEL, TRACER
!!!           AND ALSO POSITION OF NODES
!!          CALL COLLECT_VALUES(MESH%X%R,MESH%Y%R,ZF%R,H%R,NTRAC,T,
!!     &       I_1A_1,XP_1A_1,YP_1A_1,ZF_1A_1,H_1A_1,SL_1A_1,TRAC_1A_1)
!!          CALL COLLECT_VALUES(MESH%X%R,MESH%Y%R,ZF%R,H%R,NTRAC,T,
!!     &       I_1B_1,XP_1B_1,YP_1B_1,ZF_1B_1,H_1B_1,SL_1B_1,TRAC_1B_1)
!!          CALL COLLECT_VALUES(MESH%X%R,MESH%Y%R,ZF%R,H%R,NTRAC,T,
!!     &       I_1A_2,XP_1A_2,YP_1A_2,ZF_1A_2,H_1A_2,SL_1A_2,TRAC_1A_2)
!!          CALL COLLECT_VALUES(MESH%X%R,MESH%Y%R,ZF%R,H%R,NTRAC,T,
!!     &       I_1B_2,XP_1B_2,YP_1B_2,ZF_1B_2,H_1B_2,SL_1B_2,TRAC_1B_2)
!!          CALL COLLECT_VALUES(MESH%X%R,MESH%Y%R,ZF%R,H%R,NTRAC,T,
!!     &       I_2A_1,XP_2A_1,YP_2A_1,ZF_2A_1,H_2A_1,SL_2A_1,TRAC_2A_1)
!!          CALL COLLECT_VALUES(MESH%X%R,MESH%Y%R,ZF%R,H%R,NTRAC,T,
!!     &       I_2B_1,XP_2B_1,YP_2B_1,ZF_2B_1,H_2B_1,SL_2B_1,TRAC_2B_1)
!!          CALL COLLECT_VALUES(MESH%X%R,MESH%Y%R,ZF%R,H%R,NTRAC,T,
!!     &       I_2A_2,XP_2A_2,YP_2A_2,ZF_2A_2,H_2A_2,SL_2A_2,TRAC_2A_2)
!!          CALL COLLECT_VALUES(MESH%X%R,MESH%Y%R,ZF%R,H%R,NTRAC,T,
!!     &       I_2B_2,XP_2B_2,YP_2B_2,ZF_2B_2,H_2B_2,SL_2B_2,TRAC_2B_2)
!!!
!!!         COMPUTE MEAN VALUE
!!!
!!          SLA = 0.25D0 * (SL_1A_1 +SL_1A_2 +SL_2A_1 +SL_2A_2 )
!!          SLB = 0.25D0 * (SL_1B_1 +SL_1B_2 +SL_2B_1 +SL_2B_2 )
!!          ZFA = 0.25D0 * (ZF_1A_1 +ZF_1A_2 +ZF_2A_1 +ZF_2A_2 )
!!          ZFB = 0.25D0 * (ZF_1B_1 +ZF_1B_2 +ZF_2B_1 +ZF_2B_2 )
!!          H_A = 0.25D0 * ( H_1A_1 + H_1A_2 + H_2A_1 + H_2A_2 )
!!          H_B = 0.25D0 * ( H_1B_1 + H_1B_2 + H_2B_1 + H_2B_2 )
!!          DO ITRAC = 1, NTRAC
!!            TRAC_A(ITRAC) = 0.5D0 * (TRAC_1A_1(ITRAC)+TRAC_1A_2(ITRAC))
!!            TRAC_B(ITRAC) = 0.5D0 * (TRAC_1B_1(ITRAC)+TRAC_1B_2(ITRAC))
!!            TRAC_C(ITRAC) = 0.5D0 * (TRAC_2A_1(ITRAC)+TRAC_2A_2(ITRAC))
!!            TRAC_D(ITRAC) = 0.5D0 * (TRAC_2B_1(ITRAC)+TRAC_2B_2(ITRAC))
!!          ENDDO
!!!
!!!         CALCULATES THE NORMAL VECTOR, OUTGOING SIDE A, ENTERING SIDE B
!!!
!!          TXA=XP_2A_2-XP_1A_1
!!          TYA=YP_2A_2-YP_1A_1
!!          DLA=SQRT(TXA**2+TYA**2)
!!          TXA=TXA/DLA
!!          TYA=TYA/DLA
!!          NXA=-TYA
!!          NYA=TXA
!!!
!!          TXB=XP_2B_2-XP_1B_1
!!          TYB=YP_2B_2-YP_1B_1
!!          DLB=SQRT(TXB**2+TYB**2)
!!          TXB=TXB/DLB
!!          TYB=TYB/DLB
!!          NXB=-TYB
!!          NYB=TXB
!!!
!!!         COMPUTATION OF THE DISCHARGE
!!!
!!!         ADDING A SECURITY ON THE LEVEL OF THE WEIR
!!          YS1 = MAX(ZDIG%ADR(N)%P%R(I)  ,ZFA+0.01D0,ZFB+0.01D0)
!!          YS2 = MAX(ZDIG%ADR(N)%P%R(I+1),ZFA+0.01D0,ZFB+0.01D0)
!!!
!!          QELEM = 0.D0
!!!         UPSTREAM IS ON SIDE A
!!          IF (SLA.GT.SLB) THEN
!!            IF (H_A.GT.HMINI) THEN
!!              CALL LOI_W_INC(SLA,SLB,YS1,YS2,WDIG%ADR(N)%P%R(I),
!!     &                       PHI,QELEM,GRAV)
!!            ELSE
!!              QELEM=0.D0
!!            ENDIF
!!!         UPSTREAM IS ON SIDE B
!!          ELSE
!!            IF (H_B.GT.HMINI) THEN
!!              CALL LOI_W_INC(SLB,SLA,YS1,YS2,WDIG%ADR(N)%P%R(I),
!!     &                       PHI,-QELEM,GRAV)
!!            ELSE
!!              QELEM=0.D0
!!            ENDIF
!!          ENDIF
!!!
!!          QELEM = (QELEM*(1D0-RELAX)+QP0%ADR(N)%P%R(I)*RELAX)
!!          QP0%ADR(N)%P%R(I)=QELEM
!!!
!!!       DISTRIBUTE THE COMPUTED DISCHARGE OF EACH ELEMENTS OF WEIRS ON NODES
!!!       QELEM > 0 means the flow is from side A to side B
!!!
!!          QWA%ADR(N)%P%R(I)   = QWA%ADR(N)%P%R(I  ) - 0.5D0 * QELEM
!!          QWA%ADR(N)%P%R(I+1) = QWA%ADR(N)%P%R(I+1) - 0.5D0 * QELEM
!!          QWB%ADR(N)%P%R(I)   = QWB%ADR(N)%P%R(I  ) + 0.5D0 * QELEM
!!          QWB%ADR(N)%P%R(I+1) = QWB%ADR(N)%P%R(I+1) + 0.5D0 * QELEM
!!!
!!! COMPUTATION OF TRACERS ACCORDING THE COMPUTED DISCHARGE
!!!
!!          IF(NTRAC.GT.0) THEN
!!            INDIC = (N-1)*MAXNPS + I
!!            DO ITRAC=1,NTRAC
!!              IF(QELEM.GT.0.D0) THEN ! A --> B
!!                IF(I_1A_1.GT.0) THEN
!!                  TWEIRA%ADR(ITRAC)%P%R(INDIC  ) = TRAC_A(ITRAC) *
!!     &               0.5D0 * QELEM + TWEIRA%ADR(ITRAC)%P%R(INDIC  )
!!                  TWEIRA%ADR(ITRAC)%P%R(INDIC+1) = TRAC_C(ITRAC) *
!!     &               0.5D0 * QELEM + TWEIRA%ADR(ITRAC)%P%R(INDIC+1)
!!                  TWEIRB%ADR(ITRAC)%P%R(INDIC  ) = TRAC_A(ITRAC) *
!!     &               0.5D0 * QELEM + TWEIRB%ADR(ITRAC)%P%R(INDIC  )
!!                  TWEIRB%ADR(ITRAC)%P%R(INDIC+1) = TRAC_C(ITRAC) *
!!     &               0.5D0 * QELEM + TWEIRB%ADR(ITRAC)%P%R(INDIC+1)
!!                ELSE
!!                  TWEIRA%ADR(ITRAC)%P%R(INDIC  ) = 0.D0 +
!!     &               TWEIRA%ADR(ITRAC)%P%R(INDIC  )
!!                  TWEIRA%ADR(ITRAC)%P%R(INDIC+1) = 0.D0 +
!!     &               TWEIRA%ADR(ITRAC)%P%R(INDIC+1)
!!                  TWEIRB%ADR(ITRAC)%P%R(INDIC  ) = 0.D0 +
!!     &               TWEIRB%ADR(ITRAC)%P%R(INDIC  )
!!                  TWEIRB%ADR(ITRAC)%P%R(INDIC+1) = 0.D0 +
!!     &               TWEIRB%ADR(ITRAC)%P%R(INDIC+1)
!!                ENDIF
!!              ELSEIF(QELEM.LT.0.D0) THEN ! B --> A
!!                IF(I_1B_1.GT.0) THEN
!!                  TWEIRA%ADR(ITRAC)%P%R(INDIC  ) = -TRAC_B(ITRAC) *
!!     &               0.5D0 * QELEM + TWEIRA%ADR(ITRAC)%P%R(INDIC  )
!!                  TWEIRA%ADR(ITRAC)%P%R(INDIC+1) = -TRAC_D(ITRAC) *
!!     &               0.5D0 * QELEM + TWEIRA%ADR(ITRAC)%P%R(INDIC+1)
!!                  TWEIRB%ADR(ITRAC)%P%R(INDIC  ) = -TRAC_B(ITRAC) *
!!     &               0.5D0 * QELEM + TWEIRB%ADR(ITRAC)%P%R(INDIC  )
!!                  TWEIRB%ADR(ITRAC)%P%R(INDIC+1) = -TRAC_D(ITRAC) *
!!     &               0.5D0 * QELEM + TWEIRB%ADR(ITRAC)%P%R(INDIC+1)
!!                ELSE
!!                  TWEIRA%ADR(ITRAC)%P%R(INDIC  ) = 0.D0 +
!!     &               TWEIRA%ADR(ITRAC)%P%R(INDIC  )
!!                  TWEIRA%ADR(ITRAC)%P%R(INDIC+1) = 0.D0 +
!!     &               TWEIRA%ADR(ITRAC)%P%R(INDIC+1)
!!                  TWEIRB%ADR(ITRAC)%P%R(INDIC  ) = 0.D0 +
!!     &               TWEIRB%ADR(ITRAC)%P%R(INDIC  )
!!                  TWEIRB%ADR(ITRAC)%P%R(INDIC+1) = 0.D0 +
!!     &               TWEIRB%ADR(ITRAC)%P%R(INDIC+1)
!!                ENDIF
!!              ELSE ! No Flow
!!                  TWEIRA%ADR(ITRAC)%P%R(INDIC  ) = 0.D0 +
!!     &               TWEIRA%ADR(ITRAC)%P%R(INDIC  )
!!                  TWEIRA%ADR(ITRAC)%P%R(INDIC+1) = 0.D0 +
!!     &               TWEIRA%ADR(ITRAC)%P%R(INDIC+1)
!!                  TWEIRB%ADR(ITRAC)%P%R(INDIC  ) = 0.D0 +
!!     &               TWEIRB%ADR(ITRAC)%P%R(INDIC  )
!!                  TWEIRB%ADR(ITRAC)%P%R(INDIC+1) = 0.D0 +
!!     &               TWEIRB%ADR(ITRAC)%P%R(INDIC+1)
!!              ENDIF
!!              IF(NCSIZE.GT.1) THEN
!!                TWEIRA%ADR(ITRAC)%P%R(INDIC)=
!!     &            P_DMAX(MAX( TWEIRA%ADR(ITRAC)%P%R(INDIC)  ,0.D0))
!!     &           -P_DMIN(MAX(-TWEIRA%ADR(ITRAC)%P%R(INDIC)  ,0.D0))
!!                TWEIRA%ADR(ITRAC)%P%R(INDIC+1)=
!!     &            P_DMAX(MAX( TWEIRA%ADR(ITRAC)%P%R(INDIC+1),0.D0))
!!     &           -P_DMIN(MAX(-TWEIRA%ADR(ITRAC)%P%R(INDIC+1),0.D0))
!!                TWEIRB%ADR(ITRAC)%P%R(INDIC)=
!!     &            P_DMAX(MAX( TWEIRB%ADR(ITRAC)%P%R(INDIC)  ,0.D0))
!!     &           -P_DMIN(MAX(-TWEIRB%ADR(ITRAC)%P%R(INDIC)  ,0.D0))
!!                TWEIRB%ADR(ITRAC)%P%R(INDIC+1)=
!!     &            P_DMAX(MAX( TWEIRB%ADR(ITRAC)%P%R(INDIC+1),0.D0))
!!     &           -P_DMIN(MAX(-TWEIRB%ADR(ITRAC)%P%R(INDIC+1),0.D0))
!!              ENDIF
!!            ENDDO
!!          ENDIF
!!!
!!! COMPUTATION OF VELOCITIES
!!!
!!!
!!!         CALCULATES THE TANGENTIAL VELOCITY
!!!
!!          IF(IOPTAN.EQ.0) THEN
!!            UTANA = 0.D0
!!            UTANB = 0.D0
!!          ELSEIF(IOPTAN.EQ.1) THEN
!!            HSA = MAX(SLA - 0.5D0 * (YS1 + YS2), 0.D0)
!!            HSB = MAX(SLB - 0.5D0 * (YS1 + YS2), 0.D0)
!!            PENTA = (SL_2A_2 - SL_1A_1) / DLA
!!            PENTB = (SL_2B_2 - SL_1B_1) / DLB
!!            CALL CALCUL_TANG_W2(I_1A_1,NKFROT,CHESTR,HSA,PENTA,KARMAN,
!!     &         UTANA)
!!            CALL CALCUL_TANG_W2(I_2A_2,NKFROT,CHESTR,HSA,PENTA,KARMAN,
!!     &         UTANC)
!!            CALL CALCUL_TANG_W2(I_1B_1,NKFROT,CHESTR,HSB,PENTB,KARMAN,
!!     &         UTANB)
!!            CALL CALCUL_TANG_W2(I_2B_2,NKFROT,CHESTR,HSB,PENTB,KARMAN,
!!     &         UTAND)
!!          ELSE
!!            IF (LNG.EQ.1) THEN
!!              WRITE(LU,*)'CLHUVT : OPTION INCONNUE :',IOPTAN
!!              WRITE(LU,*)'         POUR LES VITESSES TANGENTIELLES'
!!            ELSEIF(LNG.EQ.2) THEN
!!              WRITE(LU,*)'CLHUVT : UNKNOWN OPTION:',IOPTAN
!!              WRITE(LU,*)'         FOR THE TANGENTIAL VELOCITY'
!!            ENDIF
!!            CALL PLANTE(1)
!!            STOP
!!          ENDIF
!!!
!!!         ONE CALCULATES VELOCITY COMPONENTS U AND V
!!!         IN THE ORDINARY COORDINATE SYSTEM (X,Y).
!!!
!!          IF(ABS(QELEM).GT.0.D0) THEN
!!            UWEIRA%ADR(N)%P%R(I)   = UTANA * TXA + QELEM * NXA / H_A +
!!     &         UWEIRA%ADR(N)%P%R(I)
!!            VWEIRA%ADR(N)%P%R(I)   = UTANA * TYA + QELEM * NYA / H_A +
!!     &         VWEIRA%ADR(N)%P%R(I)
!!            UWEIRA%ADR(N)%P%R(I+1) = UTANC * TXA + QELEM * NXA / H_A +
!!     &         UWEIRA%ADR(N)%P%R(I+1)
!!            VWEIRA%ADR(N)%P%R(I+1) = UTANC * TYA + QELEM * NYA / H_A +
!!     &         VWEIRA%ADR(N)%P%R(I+1)
!!            UWEIRB%ADR(N)%P%R(I)   = UTANB * TXB + QELEM * NXB / H_B +
!!     &         UWEIRB%ADR(N)%P%R(I)
!!            VWEIRB%ADR(N)%P%R(I)   = UTANB * TYB + QELEM * NYB / H_B +
!!     &         VWEIRB%ADR(N)%P%R(I)
!!            UWEIRB%ADR(N)%P%R(I+1) = UTAND * TXB + QELEM * NXB / H_B +
!!     &         UWEIRB%ADR(N)%P%R(I+1)
!!            VWEIRB%ADR(N)%P%R(I+1) = UTAND * TYB + QELEM * NYB / H_B +
!!     &         VWEIRB%ADR(N)%P%R(I+1)
!!          ELSE
!!            UWEIRA%ADR(N)%P%R(I)   = 0.D0 + UWEIRA%ADR(N)%P%R(I)
!!            VWEIRA%ADR(N)%P%R(I)   = 0.D0 + VWEIRA%ADR(N)%P%R(I)
!!            UWEIRA%ADR(N)%P%R(I+1) = 0.D0 + UWEIRA%ADR(N)%P%R(I+1)
!!            VWEIRA%ADR(N)%P%R(I+1) = 0.D0 + VWEIRA%ADR(N)%P%R(I+1)
!!            UWEIRB%ADR(N)%P%R(I)   = 0.D0 + UWEIRB%ADR(N)%P%R(I)
!!            VWEIRB%ADR(N)%P%R(I)   = 0.D0 + VWEIRB%ADR(N)%P%R(I)
!!            UWEIRB%ADR(N)%P%R(I+1) = 0.D0 + UWEIRB%ADR(N)%P%R(I+1)
!!            VWEIRB%ADR(N)%P%R(I+1) = 0.D0 + VWEIRB%ADR(N)%P%R(I+1)
!!          ENDIF
!!        ENDDO
!!      ENDDO
!!!
!!! THE DISCHARGE IS NOW COMPUTED ON ALL NODES OF WEIRS
!!! SO WE COULD COMPUTE AN AVERAGE VALUE OF CONCENTRATION OF TRACERS
!!!
!!      IF(NTRAC.GT.0) THEN
!!        DO N = 1, NWEIRS
!!          DO I = 1 ,NPSING%I(N)
!!            INDIC = (N-1)*MAXNPS + I
!!            QELEM = ABS(QWA%ADR(N)%P%R(I))
!!            IF(QELEM.GT.0.D0) THEN
!!              DENOM = 1.D0 / QELEM
!!              DO ITRAC = 1, NTRAC
!!                TWEIRA%ADR(ITRAC)%P%R(INDIC) = DENOM *
!!     &             TWEIRA%ADR(ITRAC)%P%R(INDIC)
!!                IF(NCSIZE.GT.1) THEN
!!                  TWEIRA%ADR(ITRAC)%P%R(INDIC)=
!!     &              P_DMAX(MAX( TWEIRA%ADR(ITRAC)%P%R(INDIC),0.D0))
!!     &             -P_DMIN(MAX(-TWEIRA%ADR(ITRAC)%P%R(INDIC),0.D0))
!!                ENDIF
!!              ENDDO
!!            ENDIF
!!            QELEM = ABS(QWB%ADR(N)%P%R(I))
!!            IF(QELEM.GT.0.D0) THEN
!!              DENOM = 1.D0 / QELEM
!!              DO ITRAC = 1, NTRAC
!!                TWEIRB%ADR(ITRAC)%P%R(INDIC) = DENOM *
!!     &             TWEIRB%ADR(ITRAC)%P%R(INDIC)
!!                IF(NCSIZE.GT.1) THEN
!!                  TWEIRB%ADR(ITRAC)%P%R(INDIC)=
!!     &              P_DMAX(MAX( TWEIRB%ADR(ITRAC)%P%R(INDIC),0.D0))
!!     &             -P_DMIN(MAX(-TWEIRB%ADR(ITRAC)%P%R(INDIC),0.D0))
!!                ENDIF
!!              ENDDO
!!            ENDIF
!!          ENDDO
!!        ENDDO
!!      ENDIF
!!!
!!! END OF COMPUTATION OF TRACERS
!!!
!!      RETURN
!!      END
