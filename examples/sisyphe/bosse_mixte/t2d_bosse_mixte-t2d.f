!                    *****************
                     SUBROUTINE NOEROD
!                    *****************
!
     & (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    FIXES THE NON-ERODABLE BED ELEVATION ZR.
!
!note     METHODS OF TREATMENT OF NON-ERODABLE BEDS CAN LEAD TO ZF.
!note  CHOOSE TO SMOOTH THE SOLUTION WITH NLISS > 0.
!
!history  C. LENORMANT
!+
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
!| CHOIX          |-->| SELECTED METHOD FOR THE TREATMENT OF RIGID BEDS
!| H              |-->| WATER DEPTH
!| NLISS          |<->| NUMBER OF SMOOTHINGS
!| NPOIN          |-->| NUMBER OF 2D POINTS
!| X,Y            |-->| 2D COORDINATES
!| Z              |-->| FREE SURFACE
!| ZF             |-->| BED LEVEL
!| ZR             |<--| RIGID BED LEVEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS
!
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!--------------------
! RIGID BEDS POSITION
!---------------------
!
!     DEFAULT VALUE:       ZR=ZF-100
!
      CALL OV( 'X=C       ',ZR,ZF,ZF,-1.D0,NPOIN)
!
!------------------
! SMOOTHING OPTION
!------------------
!
!     NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE
!             DEFAULT VALUE : NLISS = 0 (NO SMOOTHING)
!
      NLISS = 0
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!
!                    *************************
                     SUBROUTINE INIT_COMPO_COH
!                    *************************
!
     &(ES,CONC_VASE,CONC,NPOIN,NOMBLAY,NSICLA,AVAIL,AVA0)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/07/2011
!***********************************************************************
!
!brief    INITIAL FRACTION DISTRIBUTION, STRATIFICATION,
!+                VARIATION IN SPACE.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_SISYPHE, EX_INIT_COMPO_COH=> INIT_COMPO_COH
      USE DECLARATIONS_SISYPHE, ONLY : NLAYMAX
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!    
      INTEGER, INTENT(IN)              :: NPOIN,NOMBLAY,NSICLA
      DOUBLE PRECISION, INTENT(INOUT)  :: ES(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(IN)     :: CONC_VASE(NOMBLAY)
      DOUBLE PRECISION,  INTENT(INOUT) :: CONC(NPOIN,NOMBLAY)
      DOUBLE PRECISION, INTENT(INOUT)  :: AVAIL(NPOIN,NOMBLAY,NSICLA)
      DOUBLE PRECISION, INTENT(IN)     :: AVA0(NSICLA)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION  EPAI_VASE(NLAYMAX),EPAI_SABLE(NLAYMAX)

      INTEGER I,J

!-----------------------------------------------------------------------

      EPAI_VASE(1)=0.01D0
      EPAI_VASE(2)=0.02D0
      EPAI_VASE(3)=0.03995D0
      EPAI_VASE(4)=0.0437D0
      EPAI_VASE(5)=0.0517D0
      EPAI_VASE(6)=0.1259D0
      EPAI_VASE(7)=0.4889D0
      EPAI_VASE(8)=1.5071D0
      EPAI_VASE(9)=0.86410D0
!
!-----------------------------------------------------------------------
!
!     INITIALISING THE NUMBER OF LAYERS
!        

      DO J= 1,NOMBLAY
        EPAI_VASE(J) = 0.1D0
        IF(NSICLA.GT.1) THEN
          EPAI_SABLE(J) = AVA0(1)/AVA0(2)*EPAI_VASE(J)
        ENDIF
      ENDDO
!-----------------------------------------------------------------------
!
!     INITIALISING OF LAYER THICKNESS AND CONC 
!        
 
!     BY DEFAULT : UNIFORM BED COMPOSITION (KEY WORDS)
!     V6P3: IT WILL BE POSSIBLE TO HAVE A SPATIAL DISTRIBUTION OF THE BED CONC
!     V6P2: SO FAR THE MUD CONC IS CONSTANT PER LAYER 
!     si mixte: calculer aussi les AVAI!
!
      DO I=1,NPOIN
        DO J= 1,NOMBLAY
!
          CONC(I,J) = CONC_VASE(J)
          ES(I,J)   = EPAI_VASE(J)
!
          IF(NSICLA.GT.1) THEN
            ES(I,J)= ES(I,J) + EPAI_SABLE(J)
            IF(ES(I,J).GE.1.D-6) THEN
! Class 1 is for sand, class 2 is mud
              AVAIL(I,J,1)= EPAI_SABLE(J)/ES(I,J)
              AVAIL(I,J,2)= EPAI_VASE(J)/ES(I,J)
            ELSE
              AVAIL(I,J,1)= AVA0(1)
              AVAIL(I,J,2)= AVA0(2)
            ENDIF
          ENDIF   
!
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

