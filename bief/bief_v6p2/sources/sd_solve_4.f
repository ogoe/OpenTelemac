!                    *********************
                     SUBROUTINE SD_SOLVE_4
!                    *********************
!
     &(NPOIN,NSEGB,GLOSEGB,DAB1,DAB2,DAB3,DAB4,XAB1,XAB2,XAB3,XAB4,
     & XX1,XX2,CVB1,CVB2,INFOGR,TYPEXT)
!
!***********************************************************************
! BIEF   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    DIRECT RESOLUTION OF A SYSTEM 2 X 2 WITH
!+                MINIMUM DEGREE PERMUTATION AND LDLT DECOMPOSITION.
!+
!+            FROM SEGMENT STORAGE TO COMPACT STORAGE (MORSE).
!
!note     IMPORTANT: INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP
!
!history  E. RAZAFINDRAKOTO (LNH)
!+        20/11/06
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CVB1,CVB2      |-->| SECOND MEMBERS OF THE 2 SUB-SYSTEMS
!| DABX           |-->| DIAGONAL TERMS OF SUB-MATRIX X
!| GLOSEGB        |-->| GLOBAL NUMBER OF SEGMENT OF A SUB-MATRIX
!| INFOGR         |-->| IF, YES INFORMATIONS ON LISTING
!| NPOIN          |-->| NOMBRE D'INCONNUES
!| NSEGB          |-->| NOMBRE DE SEGMENTS
!| TYPEXT         |-->| = 'S' : SYMETRIC MATRIX
!| XABX           |-->| OFF-DIAGONAL TERMS OF SUB-MATRIX X
!| XX1,XX2        |<--| SOLUTIONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_SD_SOLVE_4 => SD_SOLVE_4
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NSEGB
      INTEGER, INTENT(IN) :: GLOSEGB(NSEGB*2)
      LOGICAL, INTENT(IN) :: INFOGR
      DOUBLE PRECISION, INTENT(IN)    :: DAB1(NPOIN),DAB2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: DAB3(NPOIN),DAB4(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XAB1(NSEGB),XAB2(NSEGB)
      DOUBLE PRECISION, INTENT(IN)    :: XAB3(NSEGB),XAB4(NSEGB)
      DOUBLE PRECISION, INTENT(INOUT) :: XX1(NPOIN),XX2(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: CVB1(NPOIN),CVB2(NPOIN)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NPBLK,NSEGBLK,I
!
      INTEGER, ALLOCATABLE          :: GLOSEG4(:)
      DOUBLE PRECISION, ALLOCATABLE :: XA(:),DA(:)
      DOUBLE PRECISION, ALLOCATABLE :: RHS(:),XINC(:)
!
      INTEGER SIZE_GLOSEG4,SIZE_DA,SIZE_XA,SIZE_RHS,SIZE_XINC
!
      DATA SIZE_GLOSEG4/0/
      DATA SIZE_DA     /0/
      DATA SIZE_XA     /0/
      DATA SIZE_RHS    /0/
      DATA SIZE_XINC   /0/
!
      SAVE
!
!-----------------------------------------------------------------------
!
      NPBLK=NPOIN*2
      NSEGBLK=4*NSEGB+NPOIN
!
      IF(SIZE_GLOSEG4.EQ.0) THEN
        ALLOCATE(GLOSEG4(2*NSEGBLK))
        SIZE_GLOSEG4=    2*NSEGBLK
      ELSEIF(            2*NSEGBLK.GT.SIZE_GLOSEG4) THEN
        DEALLOCATE(GLOSEG4)
        ALLOCATE(GLOSEG4(2*NSEGBLK))
        SIZE_GLOSEG4=    2*NSEGBLK
      ENDIF
      IF(SIZE_DA.EQ.0) THEN
        ALLOCATE(DA(NPBLK))
        SIZE_DA=    NPBLK
      ELSEIF(       NPBLK.GT.SIZE_DA) THEN
        DEALLOCATE(DA)
        ALLOCATE(DA(NPBLK))
        SIZE_DA=    NPBLK
      ENDIF
      IF(SIZE_XA.EQ.0) THEN
        ALLOCATE(XA(2*NSEGBLK))
        SIZE_XA=    2*NSEGBLK
      ELSEIF(       2*NSEGBLK.GT.SIZE_XA) THEN
        DEALLOCATE(XA)
        ALLOCATE(XA(2*NSEGBLK))
        SIZE_XA=    2*NSEGBLK
      ENDIF
      IF(SIZE_RHS.EQ.0) THEN
        ALLOCATE(RHS(NPBLK))
        SIZE_RHS=    NPBLK
      ELSEIF(        NPBLK.GT.SIZE_RHS) THEN
        DEALLOCATE(RHS)
        ALLOCATE(RHS(NPBLK))
        SIZE_RHS=    NPBLK
      ENDIF
      IF(SIZE_XINC.EQ.0) THEN
        ALLOCATE(XINC(NPBLK))
        SIZE_XINC=    NPBLK
      ELSEIF(         NPBLK.GT.SIZE_XINC) THEN
        DEALLOCATE(XINC)
        ALLOCATE(XINC(NPBLK))
        SIZE_XINC=    NPBLK
      ENDIF
!
!-----------------------------------------------------------------------
!
!     1. SECOND MEMBER OF THE SYSTEM
!     ===========================
!
      DO I=1,NPOIN
        RHS(I)      = CVB1(I)
        RHS(I+NPOIN)= CVB2(I)
      ENDDO
!
!     2. BUILDS SEGMENT STORAGE MATRIX BLOCK (OF 4)
!     =====================================================
!
      CALL SD_STRSG4(NPOIN,NSEGB,GLOSEGB,NPBLK,NSEGBLK,GLOSEG4)
!
      CALL SD_FABSG4(NPOIN,NSEGB,DAB1,DAB2,DAB3,DAB4,
     &               XAB1,XAB2,XAB3,XAB4,NPBLK,NSEGBLK,DA,XA)
!
!     3. SOLVES LIKE A STANDARD SYMMETRICAL MATRIX
!     ==================================================
!
      CALL SD_SOLVE_1(NPBLK,NSEGBLK,GLOSEG4,NSEGBLK,DA,XA,
     &                XINC,RHS,INFOGR,TYPEXT)
!
!     4. RECOVERS THE UNKNOWNS
!     =============================
!
      DO I=1,NPOIN
        XX1(I)= XINC(I)
        XX2(I)= XINC(I+NPOIN)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
