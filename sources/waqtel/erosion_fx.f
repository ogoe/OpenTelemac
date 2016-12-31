!                    *********************
                     SUBROUTINE EROSION_FX
!                    *********************
!
     &(SEDERO,TAUB,SF,TAUR,ERO,ZZERO,NPOIN)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES EROSION FLUX
!
!
!history  R. ATA (LNHE)
!+        02/09/2015
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ERO            |-->| EROSION RATE
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES
!| SEDERO         |<--| EROSION FLUX
!| SF             |-->| BED SEDIMENTS
!| TAUB           |-->| BED SHEAR STRESS
!| TAUR           |-->| CRITICAL STRESS OF RESUSPENSION
!| ZZERO          |-->| EPS UNDER WHICH VALUE IS CONSIDERED ZERO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_WAQTEL, EX_EROSION_FX => EROSION_FX
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)     :: NPOIN
      DOUBLE PRECISION, INTENT(IN)     :: TAUR,ERO,ZZERO
      TYPE(BIEF_OBJ)   , INTENT(IN   ) :: TAUB,SF
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: SEDERO
      INTRINSIC MAX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     LOCAL VARIABLES
      INTEGER I
      DOUBLE PRECISION  CC
!
      IF (ABS(TAUR).LT.ZZERO)THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'EROSION_FX:CISAILL. CRITIQUE DE SEDIMENTATION'
          WRITE(LU,*) '           TAUR TROP PETIT - VERIFIER VALEUR !!!'
          WRITE(LU,*) '           TAUR = ',TAUR
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'EROSION_FX: CRITICAL STRESS OF SEDIMENTATION  '
          WRITE(LU,*) '          TAUR VERY SMALL OR NIL - VERIFY !!!'
          WRITE(LU,*) '          TAUR = ',TAUR
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!     THIS WAY WORKS WELL FOR 2D AND FOR 3D AS WELL SINCE BED LAYER IS 
!     FOR I=1 TO NPOIN 
      DO I=1,NPOIN
        IF(TAUB%R(I).GT.TAUR)THEN
          CC=ERO*(TAUB%R(I)/TAUR -1.D0)
          SEDERO%R(I)=CC*ANTI_DIRAC(SF%R(I),ZZERO)
        ELSE
          SEDERO%R(I)=0.D0
        ENDIF
      ENDDO
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!
