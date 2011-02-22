C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS THE DIAGONAL OF THE MURD MATRIX.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DIAG, IKLE, NELEM, NELMAX, NPOIN3, XM
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I1, I2, I3, I4, I5, I6, IELEM
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PRECON()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 23/04/2010
!> </td><td> J-M HERVOUET (LNHE)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DIAG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T
!></td><td><--</td><td>DIAGONAL
!>    </td></tr>
!>          <tr><td>XM
!></td><td>--></td><td>OFF-DIAGONAL TERMS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DIAG_MURD
     &(DIAG,XM,NELEM,NELMAX,NPOIN3,IKLE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DIAG           |---| 
C| IKLE           |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C| NPOIN3         |---| 
C| T             |<--| DIAGONAL
C| XM             |-->| OFF-DIAGONAL TERMS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN3
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
C
      DOUBLE PRECISION, INTENT(INOUT) :: DIAG(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: XM(30,NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,I1,I2,I3,I4,I5,I6
C
C-----------------------------------------------------------------------
C
C     INITIALISES DIAG
C
      DO I1=1,NPOIN3
        DIAG(I1)=0.D0
      ENDDO
C
C     COMPUTES THE SUM OF EACH LINE (WITH A - SIGN)
C     THE DIAGONAL TERMS ARE 0
C
      DO IELEM = 1,NELEM
        I1=IKLE(IELEM,1)
        I2=IKLE(IELEM,2)
        I3=IKLE(IELEM,3)
        I4=IKLE(IELEM,4)
        I5=IKLE(IELEM,5)
        I6=IKLE(IELEM,6)
        DIAG(I1)=DIAG(I1) - XM(01,IELEM)-XM(02,IELEM)
     &                    - XM(03,IELEM)-XM(04,IELEM)-XM(05,IELEM)
        DIAG(I2)=DIAG(I2) - XM(16,IELEM)-XM(06,IELEM)
     &                    - XM(07,IELEM)-XM(08,IELEM)-XM(09,IELEM)
        DIAG(I3)=DIAG(I3) - XM(17,IELEM)-XM(21,IELEM)
     &                    - XM(10,IELEM)-XM(11,IELEM)-XM(12,IELEM)
        DIAG(I4)=DIAG(I4) - XM(18,IELEM)-XM(22,IELEM)
     &                    - XM(25,IELEM)-XM(13,IELEM)-XM(14,IELEM)
        DIAG(I5)=DIAG(I5) - XM(19,IELEM)-XM(23,IELEM)
     &                    - XM(26,IELEM)-XM(28,IELEM)-XM(15,IELEM)
        DIAG(I6)=DIAG(I6) - XM(20,IELEM)-XM(24,IELEM)
     &                    - XM(27,IELEM)-XM(29,IELEM)-XM(30,IELEM)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C