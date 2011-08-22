!                    ****************
                     SUBROUTINE CFLVF
!                    ****************
!
     &(DTMAX,HSTART,H,FXMAT,FXMATPAR,MAS,DT,FXBOR,SMH,YASMH,TAB1,NSEG,
     & NPOIN,NPTFR,GLOSEG,SIZGLO,MESH,MSK,MASKPT)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE MAXIMUM TIMESTEP THAT ENABLES
!+                MONOTONICITY IN THE ADVECTION STEP.
!
!history  JMH
!+        11/04/2008
!+
!+   ADDED YASMH
!
!history  JMH
!+        10/06/2008
!+
!+   ADDED SIZGLO
!
!history  JMH
!+        02/10/2008
!+
!+   PARALLEL MODE (ADDED FXMATPAR, ETC.)
!
!history  C-T PHAM (LNHE)
!+        30/11/2009
!+        V6P0
!+   REFINED COMPUTATION OF DTMAX (AS IN 3D)
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
!| DT             |-->| TIME STEP
!| DTMAX          |<--| MAXIMUM TIME STEP FOR STABILITY
!| FXBOR          |-->| BOUNDARY FLUXES
!| FXMAT          |-->| FLUXES
!| FXMATPAR       |-->| FLUXES ASSEMBLED IN PARALLEL
!| GLOSEG         |-->| GLOBAL NUMBER OF THE 2 POINTS OF A SEGMENT
!| H              |-->| H AT THE END OF FULL TIME STEP
!| HSTART         |-->| H AT BEGINNING OF SUB TIME STEP
!| MAS            |-->| INTEGRAL OF TEST FUNCTIONS (=AREA AROUND POINTS)
!| MASKPT         |-->| ARRAY FOR MASKING POINTS
!| MESH           |-->| MESH STRUCTURE
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| SIZGLO         |-->| FIRST DIMENSION OF GLOSEG
!| SMH            |-->| RIGHT HAND SIDE OF CONTINUITY EQUATION
!| TAB1           |-->| WORK ARRAY
!| YASMH          |-->| IF YES, TAKE SHM INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CFLVF => CFLVF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG,NPOIN,NPTFR,SIZGLO
      INTEGER, INTENT(IN)             :: GLOSEG(SIZGLO,2)
      DOUBLE PRECISION, INTENT(INOUT) :: DTMAX
      DOUBLE PRECISION, INTENT(IN)    :: DT,HSTART(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: H(NPOIN),MAS(NPOIN),SMH(NPOIN)
!                                              NOT NPTFR, SEE TRACVF
      DOUBLE PRECISION, INTENT(IN)    :: FXBOR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FXMAT(NSEG),FXMATPAR(NSEG)
      LOGICAL, INTENT(IN)             :: YASMH,MSK
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TAB1
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)      :: MASKPT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION DENOM,A,B
!
!-----------------------------------------------------------------------
!
! COMPUTES THE CRITERION FOR COURANT NUMBER
!
      DO I = 1,NPOIN
        TAB1%R(I) = 0.D0
      ENDDO
!
! USES HERE FXMAT ASSEMBLED IN PARALLEL FOR UPWINDING
!
      DO I = 1,NSEG
        IF(FXMATPAR(I).LT.0.D0) THEN
          TAB1%R(GLOSEG(I,1)) = TAB1%R(GLOSEG(I,1)) + FXMAT(I)
        ELSEIF(FXMATPAR(I).GT.0.D0) THEN
          TAB1%R(GLOSEG(I,2)) = TAB1%R(GLOSEG(I,2)) - FXMAT(I)
        ENDIF
      ENDDO
!
      IF(NCSIZE.GT.1) CALL PARCOM(TAB1,2,MESH)
!
!     MASKS TAB1
!
      IF(MSK) THEN
        CALL OS('X=XY    ',X=TAB1,Y=MASKPT)
      ENDIF
!
! STABILITY (AND MONOTONICITY) CRITERION
!
! NOTE THAT TAB1(I)<0 MIN(FXBOR(I),0.D0)<0 AND -MAX(SMH(I),0.D0)<0
!           SO ABS(TAB1(I)+MIN(FXBOR(I),0.D0)-MAX(SMH(I),0.D0))=
!              -(TAB1(I)+MIN(FXBOR(I),0.D0)-MAX(SMH(I),0.D0))
!
!     ANY TIME LARGER THAN THE REMAINING DT
      DTMAX = 2.D0*DT
!
!     SEE RELEASE NOTES 5.7, CRITERION AT THE END OF 4.4 PAGE 33
!     BUT HERE THE FINAL H IS NOT H(N+1) BUT A FUNCTION OF DTMAX ITSELF
!     H FINAL = HSTART + DTMAX/DT *(H-HSTART)
!
      IF(YASMH) THEN
        DO I = 1,NPOIN
          DENOM=TAB1%R(I)+MIN(FXBOR(I),0.D0)-MAX(SMH(I),0.D0)
          A=-MAS(I)/MIN(DENOM,-1.D-12)
          B=DT+A*(HSTART(I)-H(I))
          IF(B.GT.0.D0) THEN
            DTMAX = MIN(DTMAX,A*HSTART(I)*DT/B)
          ENDIF
        ENDDO
      ELSE
        DO I = 1,NPOIN
          DENOM=TAB1%R(I)+MIN(FXBOR(I),0.D0)
          A=-MAS(I)/MIN(DENOM,-1.D-12)
          B=DT+A*(HSTART(I)-H(I))
          IF(B.GT.0.D0) THEN
            DTMAX = MIN(DTMAX,A*HSTART(I)*DT/B)
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
