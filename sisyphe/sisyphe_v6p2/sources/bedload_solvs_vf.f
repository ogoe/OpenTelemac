!                    *****************************
                     SUBROUTINE BEDLOAD_SOLVS_VF !
!                    *****************************
!
     &(MESH,QSX,QSY,LIEBOR,UNSV2D,EBOR,BREACH,NSEG,NPTFR,
     & NPOIN,KENT,KSORT,DT,T10,ZFCL,FLUX,CSF_SABLE)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    SOLVES EXNER EQUATION WITH THE FINITE VOLUME METHOD.
!
!history  M. GONZALES DE LINARES
!+        07/05/2002
!+        V5P5
!+
!
!history  F. HUVELIN
!+        14/09/2004
!+        V5P5
!+
!
!history  J-M HERVOUET
!+        30/10/2007
!+        V5P8
!+   UNSV2D +DIRICL DELETED
!
!history  JMH
!+        15/09/2009
!+
!+   KENT KSORT ADDED (WERE HARD-CODED BEFORE !!!)
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables   
!+   
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| BREACH         |<->| INDICATOR FOR NON ERODIBLE BED (FINITE VOLUMES SHEMES)
!| DT             |-->| TIME STEP
!| EBOR           |<->| BOUNDARY CONDITION FOR BED EVOLUTION (DIRICHLET)
!| FLUX           |<->| SEDIMENT FLUX  
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT  
!| LIEBOR         |<->| PHYSICAL BOUNDARY CONDITIONS FOR BED EVOLUTION
!| MESH           |<->| MESH STRUCTURE
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| QSX            |<->| BEDLOAD TRANSPORT RATE X-DIRECTION
!| QSY            |<->| BEDLOAD TRANSPORT RATE Y-DIRECTION
!| T10            |<->| WORK BIEF_OBJ STRUCTURE
!| UNSV2D         |-->| INVERSE OF INTEGRALS OF TEST FUNCTIONS
!| ZFCL           |<->| BEDLOAD EVOLUTION FOR EACH SEDIMENT CLASS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_BEDLOAD_SOLVS_VF => BEDLOAD_SOLVS_VF
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: QSX, QSY
      TYPE(BIEF_OBJ),   INTENT(IN)    :: LIEBOR,UNSV2D, EBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: BREACH
      INTEGER,          INTENT(IN)    :: NSEG,NPTFR,NPOIN,KENT,KSORT
      DOUBLE PRECISION, INTENT(IN)    :: DT,CSF_SABLE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T10
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: ZFCL, FLUX
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: ISEGIN, K
      INTEGER          :: IEL, IEL1, IEL2
      DOUBLE PRECISION :: QSMOY1, QSMOY2
      DOUBLE PRECISION :: QSP
      DOUBLE PRECISION :: VNOIN1, VNOIN2, RNORM
      DOUBLE PRECISION :: XN, YN
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
      ! ***************** !
      ! I - INTIALISES !
      ! ***************** !
      CALL OS('X=0     ', X=FLUX)
      ! ************************************************* !
      ! II - DETERMINES THE OUTGOING FLUX FOR EACH CELL   !
      ! ************************************************* !
      DO ISEGIN = 1, NSEG
         IEL1 = MESH%NUBO%I(2*ISEGIN - 1)
         IEL2 = MESH%NUBO%I(2*ISEGIN    )
         ! II.1 - RNORM : SEGMENT LENGTH
         ! ----------------------------------
         VNOIN1 = MESH%VNOIN%R(3*ISEGIN - 2)
         VNOIN2 = MESH%VNOIN%R(3*ISEGIN - 1)
         RNORM  = MESH%VNOIN%R(3*ISEGIN    )
         ! II.2 - QS FOR THE SEGMENT, BROKEN UP ACCORDING TO X AND Y
         ! ---------------------------------------------
         QSMOY1 = 0.5D0*(QSX%R(IEL1) + QSX%R(IEL2))
         QSMOY2 = 0.5D0*(QSY%R(IEL1) + QSY%R(IEL2))
         ! II.3 - PROJECTS QS FOR THE SEGMENT ONTO THE SEGMENT NORMAL
         ! ------------------------------------------------------------
         QSP = VNOIN1*QSMOY1 + VNOIN2*QSMOY2
         ! II.4 - UPWIND SCHEME ON NODES WITH A "PROBLEM"
         ! ----------------------------------------------
         IF(BREACH%I(IEL1).EQ.1.AND.QSP.GT.0.D0) THEN
           QSMOY1 = QSX%R(IEL1)
           QSMOY2 = QSY%R(IEL1)
         ENDIF
         IF(BREACH%I(IEL2).EQ.1.AND.QSP.LT.0.D0) THEN
           QSMOY1 = QSX%R(IEL2)
           QSMOY2 = QSY%R(IEL2)
         ENDIF
         QSP = VNOIN1*QSMOY1 + VNOIN2*QSMOY2
         ! II.5 - INTEGRATES BY THE SEGMENT LENGTH
         ! ---------------------------------------------
         FLUX%R(IEL1) = FLUX%R(IEL1) + RNORM*QSP
         FLUX%R(IEL2) = FLUX%R(IEL2) - RNORM*QSP
      ENDDO
      ! ******************************* !
      ! III - BOUNDARIES                ! (_IMP_)
      ! ******************************* !
      DO K = 1 , NPTFR
         IEL = MESH%NBOR%I(K)
         ! III.1 - FREE EVOLUTION: SEDIMENTS ARE FREE TO LEAVE
         ! --------------------------------------------------------
         IF (LIEBOR%I(K).EQ.KSORT) THEN
            ! XNEBOR (*+NPTFR) AND YNEBOR (*+NPTFR)
            ! CONTAIN THE VECTOR NORMAL TO A BOUNDARY NODE
            ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            XN = MESH%XNEBOR%R(K+NPTFR)
            YN = MESH%YNEBOR%R(K+NPTFR)
            ! ADDS THE CONTRIBUTION OF THE FLUX ON THE BOUNDARY SEGMENT
            ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            FLUX%R(IEL) = FLUX%R(IEL) + QSX%R(IEL)*XN + QSY%R(IEL)*YN
         ENDIF
         ! III.2 - FOR A SOLID BOUNDARY: NOTHING TO PROGRAM
         !         BECAUSE THE SEDIMENT FLUX IS ZERO HERE
         ! -------------------------------------------------------
      ENDDO
      IF(NCSIZE.GT.1) CALL PARCOM(FLUX, 2, MESH)
      ! ************************** !
      ! IV - SOLVES THE SYSTEM     !
      ! ************************** !
      ! NEGATIVE SIGN BECAUSE OUTGOING FLUX IS POSITIVE
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CALL OS('X=CYZ   ', X=ZFCL, Y=FLUX, Z=UNSV2D, C=-DT)
!
      DO K=1,NPTFR
        IF(LIEBOR%I(K).EQ.KENT) THEN
!         ZFCL WILL BE DIVIDED BY CSF_SABLE AFTER, AND THEN IT WILL
!         BE EBOR...
          ZFCL%R(MESH%NBOR%I(K)) = EBOR%R(K)*CSF_SABLE
        ENDIF
      ENDDO
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
