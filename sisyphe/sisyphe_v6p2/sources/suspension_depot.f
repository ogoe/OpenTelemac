!                    ******************************
                     SUBROUTINE SUSPENSION_DEPOT  !
!                    ******************************
!
     &(TOB,HN, NPOIN, HMIN,XWC,VITCD,
     & ZERO,KARMAN,XMVE, T1,T2,ZREF,FLUDPT,DEBUG,SEDCO)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    COMPUTES THE FLUX OF DEPOSITION AND EROSION.
!
!note     T1: TOB
!note  TO DO:  REPLACE USTAR WITH TOB
!
!history  J-M HERVOUET + C VILLARET
!+        31/07/2008
!+        V5P9
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
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+   Name of variables   
!+   
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEBUG          |-->| FLAG FOR DEBUGGING
!| FLUDPT         |<->| IMPLICIT DEPOSITION FLUX
!| HMIN           |-->| MINIMUM VALUE OF WATER DEPTH
!| HN             |-->| WATER DEPTH
!| KARMAN         |-->| VON KARMAN CONSTANT
!| NPOIN          |-->| NUMBER OF POINTS 
!| SEDCO          |-->| LOGICAL, SEDIMENT COHESIVE OR NOT
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| TOB            |-->| BED SHEAR STRESS (TOTAL FRICTION)
!| VITCD          |-->| CRITICAL SHEAR VELOCITY FOR MUD DEPOSITION
!| XMVE           |-->| FLUID DENSITY 
!| XWC            |-->| SETTLING VELOCITIES 
!| ZERO           |-->| ZERO
!| ZREF           |<->| REFERENCE ELEVATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE,EX_SUSPENSION_DEPOT => SUSPENSION_DEPOT
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE (BIEF_OBJ),  INTENT(IN)    ::  HN,TOB
      INTEGER,          INTENT(IN)    ::  NPOIN,DEBUG
      LOGICAL,          INTENT(IN)    :: SEDCO
      DOUBLE PRECISION, INTENT(IN)    ::  HMIN
      DOUBLE PRECISION, INTENT(IN)    :: XWC
      DOUBLE PRECISION, INTENT(IN)    :: VITCD
      DOUBLE PRECISION, INTENT(IN)    :: ZERO, KARMAN,XMVE
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T1,T2
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZREF
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUDPT
!
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER :: I
      DOUBLE PRECISION:: AUX
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!     ! ****************************************            !
      ! THE TOTAL FRICTION VELOCITY    --> USTAR (T1)       !
      ! HAS BEEN REPLACED BY USTARP (SKIN FRICTION VELOCITY)!
      ! FOR EROSION FLUX IN V6P0                            !
      ! ****************************************            !
!
      CALL OS('X=CY    ', X=T1, Y=TOB, C=1.D0/XMVE)
      CALL OS('X=+(Y,C)', X=T1, Y=T1, C=ZERO)
      CALL OS('X=SQR(Y)', X=T1, Y=T1)
!
      IF(SEDCO) THEN
!
      ! ************************************************ !
      ! IA - FORMULATION FOR COHESIVE SEDIMENTS          !
      !      (WITHOUT BEDLOAD)                           !
      ! ************************************************ !
!
!  COMPUTES THE PROBABILITY FOR DEPOSITION
!
         DO I = 1, NPOIN
           IF(VITCD.GT.1.D-08) THEN
             AUX = MAX(1.D0-(T1%R(I)/VITCD)**2,ZERO)
           ELSE
             AUX=0.D0
           ENDIF
!          COMPUTES THE IMPLICIT PART OF THE DEPOSITION FLUX
           FLUDPT%R(I)= XWC*AUX
         ENDDO
! UNIFORM SEDIMENT ALONG THE VERTICAL
         CALL OS('X=C     ', X=T2, C=1.D0)
!
      ! ******************************************* !
      ! IB - FORMULATION FOR NON-COHESIVE SEDIMENTS !
      !      (WITH BEDLOAD)                         !
      ! ******************************************* !
!
      ELSE
!
            ! ***************************************************** !
            !  COMPUTES THE RATIO BETWEEN NEAR BED CONC. AND MEAN CONC.  !
            !                                  -->  T2    (TO KEEP )     !
            ! ***************************************************** !
        IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_ROUSE'
        CALL SUSPENSION_ROUSE(T1,HN,NPOIN,
     &                        KARMAN,HMIN,ZERO,XWC,ZREF,T2)
        IF (DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_ROUSE'
!
            ! *****************************************************  !
            !  COMPUTES THE DEPOSITION FLUX --> FLUDPT = XWC * T2    !
            ! *****************************************************  !
!
         CALL OS('X=CY    ', X=FLUDPT, Y=T2, C=XWC)
!
      ENDIF
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
