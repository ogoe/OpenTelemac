C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES A FUNCTION TO A CONSTANT VALUE
!>                INSIDE OF A POLYGON.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, F, IKLE, NELEM, NELMAX, NSOM, X, XSOM, Y, YSOM
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I1, I2, I3, IELEM
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FILP10
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> INPOLY()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FILPOL()

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
!>      <td><center> 5.1                                       </center>
!> </td><td> 06/12/94
!> </td><td> C MOULIN (LNH) 30 87 83 81
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSOM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XSOM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Y
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YSOM
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FILP10
     &( F , C , XSOM , YSOM , NSOM , X , Y , NELEM , NELMAX , IKLE )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |---| 
C| F             |---| 
C| IKLE           |---| 
C| NELEM          |---| 
C| NELMAX         |---| 
C| NSOM           |---| 
C| X             |---| 
C| XSOM           |---| 
C| Y             |---| 
C| YSOM           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_FILP10 => FILP10
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NSOM , NELEM , NELMAX
      DOUBLE PRECISION, INTENT(INOUT) :: F(*)
      DOUBLE PRECISION, INTENT(IN) :: X(*) , Y(*)
      DOUBLE PRECISION, INTENT(IN) :: XSOM(NSOM) , YSOM(NSOM) , C
      INTEGER, INTENT(IN) :: IKLE(NELMAX,3)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM , I1 , I2 , I3
C
C-----------------------------------------------------------------------
C
      DO 10 IELEM = 1 , NELEM
C
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        IF( INPOLY(X(I1),Y(I1),XSOM,YSOM,NSOM) .AND.
     &      INPOLY(X(I2),Y(I2),XSOM,YSOM,NSOM) .AND.
     &      INPOLY(X(I3),Y(I3),XSOM,YSOM,NSOM) ) F(IELEM) = C
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C