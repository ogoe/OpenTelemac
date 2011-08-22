!                    **********************
                     SUBROUTINE BIEF_ALLVEC
!                    **********************
!
     &( NAT , VEC , NOM , IELM , DIM2 , STATUT , MESH )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    ALLOCATES MEMORY FOR A VECTOR STRUCTURE.
!
!history  J-M HERVOUET (LNHE)
!+        09/01/06
!+        V5P6
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
!| DIM2           |-->| SECOND DIMENSION OF VECTOR
!| IELM           |-->| TYPE OF ELEMENT, OR DIMENSION
!|                |   | (DEPENDING ON 'STATUT')
!| NAT            |<--| 1: REAL VECTOR   2:VECTOR OF INTEGERS
!| NOM            |-->| FORTRAN NAME
!| STATUT         |-->| VECTOR STATUS:
!|                |   | 0 : FREE VECTOR, IELM IS ITS DIMENSION
!|                |   | 1 : VECTOR DEFINED ON A MESH
!|                |   | IELM IS THEN THE ELEMENT TYPE
!|                |   | CHANGING DISCRETISATION FORBIDDEN
!|                |   | 2 : LIKE 1 BUT CHANGING DISCRETISATION ALLOWED
!| VEC            |<--| VECTOR TO BE ALLOCATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_BIEF_ALLVEC => BIEF_ALLVEC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: VEC
      INTEGER         , INTENT(IN)    :: NAT,IELM,DIM2,STATUT
      CHARACTER(LEN=6), INTENT(IN)    :: NOM
      TYPE(BIEF_MESH) , INTENT(IN)    :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ERR
      INTRINSIC MAX
!
      INTEGER IMAX,I
      DOUBLE PRECISION XMAX
!
!-----------------------------------------------------------------------
!  HEADER COMMON TO ALL OBJECTS
!-----------------------------------------------------------------------
!
!     KEY OF THE OBJECT - TO CHECK MEMORY CRASHES
!
      VEC%KEY = 123456
!
!     TYPE OF THE OBJECT (HERE VECTOR)
!
      VEC%TYPE = 2
!
!     NAME OF THE OBJECT
!
      VEC%NAME = NOM
!
!-----------------------------------------------------------------------
!  PART SPECIFIC TO VECTORS
!-----------------------------------------------------------------------
!
!     NATURE
!
      VEC%NAT = NAT
!
!     MAXIMUM SIZE PER DIMENSION
!
      IF(STATUT.EQ.1.OR.STATUT.EQ.2) THEN
        VEC%MAXDIM1 = BIEF_NBMPTS(IELM,MESH)
      ELSE
        VEC%MAXDIM1 = IELM
      ENDIF
!
!     VEC%MAXDIM1 MUST BE AT LEAST 1
!     TO AVOID BOUND CHECKING ERRORS ON SOME COMPILERS
!
      VEC%MAXDIM1=MAX(VEC%MAXDIM1,1)
!
!     DISCRETISES
!
      IF(STATUT.EQ.1.OR.STATUT.EQ.2) THEN
        VEC%ELM = IELM
      ELSE
        VEC%ELM = -1000
      ENDIF
!
!     FIRST DIMENSION OF VECTOR
!
      IF(STATUT.EQ.1.OR.STATUT.EQ.2) THEN
        VEC%DIM1 = BIEF_NBPTS(IELM,MESH)
      ELSE
        VEC%DIM1 = IELM
      ENDIF
!
!     SECOND DIMENSION OF VECTOR (VEC%DIM2 MAY BE CHANGED)
!
      VEC%DIM2    = DIM2
      VEC%MAXDIM2 = DIM2
!
!     CASE OF DISCONTINUITY BETWEEN ELEMENTS
!     (SEE CORRSL, VC13AA, VC13BB)
!
      VEC%DIMDISC = 0
!
!     STATUS
!
      VEC%STATUS = STATUT
!
!     INFORMATION ON CONTENT
!
      VEC%TYPR = '?'
      VEC%TYPI = '?'
!
!     DYNAMICALLY ALLOCATES MEMORY (REAL OR INTEGER, DEPENDING OF NAT)
!
      IF(NAT.EQ.1) THEN
!
        ALLOCATE(VEC%R(VEC%MAXDIM1*VEC%DIM2),STAT=ERR)
!       JAJ NULLIFY THE INTEGER PART
        NULLIFY(VEC%I)
!
!       FILLS ARRAY WITH BIG NUMBERS
!       TO RAISE QUESTIONS IF NOT INITIALISED
!
        XMAX = HUGE(100.D0)
        CALL OV('X=C     ',VEC%R,VEC%R,VEC%R,XMAX,
     &          VEC%MAXDIM1*VEC%DIM2)
!
      ELSEIF(NAT.EQ.2) THEN
!
        ALLOCATE(VEC%I(VEC%MAXDIM1*VEC%DIM2),STAT=ERR)
!       JAJ NULLIFY THE REAL PART
        NULLIFY(VEC%R)
!
!       FILLS ARRAY WITH BIG NUMBERS
!       TO RAISE QUESTIONS IF NOT INITIALISED
!
        IMAX = HUGE(100)
          DO I=1,VEC%MAXDIM1*VEC%DIM2
            VEC%I(I) = IMAX
          ENDDO
!
      ELSE
        STOP 'UNKNOWN NAT IN ALLVEC'
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(ERR.EQ.0) THEN
!       IF(LNG.EQ.1) WRITE(LU,*) 'VECTEUR : ',NOM,' ALLOUE'
!       IF(LNG.EQ.2) WRITE(LU,*) 'VECTOR: ',NOM,' ALLOCATED'
      ELSE
        IF(LNG.EQ.1) WRITE(LU,10) NOM,ERR
        IF(LNG.EQ.2) WRITE(LU,20) NOM,ERR
10      FORMAT(1X,'ERREUR A L''ALLOCATION DU VECTEUR : ',A6,/,1X,
     &            'CODE D''ERREUR : ',1I6)
20      FORMAT(1X,'ERROR DURING ALLOCATION OF VECTOR: ',A6,/,1X,
     &            'ERROR CODE: ',1I6)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
