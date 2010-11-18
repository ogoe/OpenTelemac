C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DIRECT RESOLUTION OF A SYSTEM 2 X 2 WITH
!>                MINIMUM DEGREE PERMUTATION AND LDLT DECOMPOSITION.
!><br>            FROM SEGMENT STORAGE TO COMPACT STORAGE (MORSE).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT: INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CVB1, CVB2, DAB1, DAB2, DAB3, DAB4, GLOSEGB, INFOGR, NPOIN, NSEGB, TYPEXT, XAB1, XAB2, XAB3, XAB4, XX1, XX2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DA, GLOSEG4, I, NPBLK, NSEGBLK, RHS, SIZE_DA, SIZE_GLOSEG4, SIZE_RHS, SIZE_XA, SIZE_XINC, XA, XINC
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_SOLVE_4
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> SD_FABSG4(), SD_SOLVE_1(), SD_STRSG4()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SOLVE()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 5.9                                       </center>
!> </td><td> 20/11/06
!> </td><td> E. RAZAFINDRAKOTO (LNH) 01 30 87 74 03
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CVB1,CVB2
!></td><td>--></td><td>SECONDS MEMBRES
!>    </td></tr>
!>          <tr><td>DA,XA
!></td><td>--></td><td>DIAGONALES ET TERMES EXTRA-DIAGONAUX DES
!>                  MATRICES
!>    </td></tr>
!>          <tr><td>DAB1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DAB2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DAB3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DAB4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DES SEGMENTS
!>    </td></tr>
!>          <tr><td>GLOSEGB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INFOGR
!></td><td>--></td><td>IF, YES INFORMATIONS ON LISTING
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE D'INCONNUES
!>    </td></tr>
!>          <tr><td>NSEGB
!></td><td>--></td><td>NOMBRE DE SEGMENTS
!>    </td></tr>
!>          <tr><td>TYPEXT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XAB1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XAB2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XAB3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XAB4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XX1,XX2
!></td><td><--</td><td>SOLUTIONS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                       SUBROUTINE SD_SOLVE_4
     &(NPOIN,NSEGB,GLOSEGB,DAB1,DAB2,DAB3,DAB4,XAB1,XAB2,XAB3,XAB4,
     & XX1,XX2,CVB1,CVB2,INFOGR,TYPEXT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CVB1,CVB2      |-->| SECONDS MEMBRES
C| DA,XA          |-->| DIAGONALES ET TERMES EXTRA-DIAGONAUX DES
C|                |   | MATRICES
C| DAB1           |---| 
C| DAB2           |---| 
C| DAB3           |---| 
C| DAB4           |---| 
C| GLOSEG         |-->| NUMEROS GLOBAUX DES POINTS DES SEGMENTS
C| GLOSEGB        |---| 
C| INFOGR         |-->| IF, YES INFORMATIONS ON LISTING
C| NPOIN          |-->| NOMBRE D'INCONNUES
C| NSEGB          |-->| NOMBRE DE SEGMENTS
C| TYPEXT         |---| 
C| XAB1           |---| 
C| XAB2           |---| 
C| XAB3           |---| 
C| XAB4           |---| 
C| XX1,XX2        |<--| SOLUTIONS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_SOLVE_4 => SD_SOLVE_4
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
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
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NPBLK,NSEGBLK,I
C
      INTEGER, ALLOCATABLE          :: GLOSEG4(:)
      DOUBLE PRECISION, ALLOCATABLE :: XA(:),DA(:)
      DOUBLE PRECISION, ALLOCATABLE :: RHS(:),XINC(:)
C
      INTEGER SIZE_GLOSEG4,SIZE_DA,SIZE_XA,SIZE_RHS,SIZE_XINC
C
      DATA SIZE_GLOSEG4/0/
      DATA SIZE_DA     /0/
      DATA SIZE_XA     /0/
      DATA SIZE_RHS    /0/
      DATA SIZE_XINC   /0/
C
      SAVE
C
C-----------------------------------------------------------------------
C
      NPBLK=NPOIN*2
      NSEGBLK=4*NSEGB+NPOIN
C
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
C
C-----------------------------------------------------------------------
C
C     1. SECOND MEMBER OF THE SYSTEM
C     ===========================
C
      DO I=1,NPOIN
        RHS(I)      = CVB1(I)
        RHS(I+NPOIN)= CVB2(I)
      ENDDO
C
C     2. BUILDS SEGMENT STORAGE MATRIX BLOCK (OF 4)
C     =====================================================
C
      CALL SD_STRSG4(NPOIN,NSEGB,GLOSEGB,NPBLK,NSEGBLK,GLOSEG4)
C
      CALL SD_FABSG4(NPOIN,NSEGB,DAB1,DAB2,DAB3,DAB4,
     &               XAB1,XAB2,XAB3,XAB4,NPBLK,NSEGBLK,DA,XA)
C
C     3. SOLVES LIKE A STANDARD SYMMETRICAL MATRIX
C     ==================================================
C
      CALL SD_SOLVE_1(NPBLK,NSEGBLK,GLOSEG4,NSEGBLK,DA,XA,
     &                XINC,RHS,INFOGR,TYPEXT)
C
C     4. RECOVERS THE UNKNOWNS
C     =============================
C
      DO I=1,NPOIN
        XX1(I)= XINC(I)
        XX2(I)= XINC(I+NPOIN)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C