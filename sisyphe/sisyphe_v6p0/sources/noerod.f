C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FIXES THE NON-ERODABLE BED ELEVATION ZR.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  METHODS OF TREATMENT OF NON-ERODABLE BEDS CAN LEAD TO ZF.

!>  @note  CHOOSE TO SMOOTH THE SOLUTION WITH NLISS > 0.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CHOIX, H, NLISS, NPOIN, X, Y, Z, ZF, ZR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE()

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
!> </td><td>
!> </td><td> C. LENORMANT
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CHOIX
!></td><td>--></td><td>SELECTED METHOD FOR THE TREATMENT OF RIGID BEDS
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>WATER DEPTH
!>    </td></tr>
!>          <tr><td>NLISS
!></td><td><-></td><td>NUMBER OF SMOOTHINGS
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF 2D POINTS
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>2D COORDINATES
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>FREE SURFACE
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>BED LEVEL
!>    </td></tr>
!>          <tr><td>ZR
!></td><td><--</td><td>RIGID BED LEVEL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE NOEROD
     & (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHOIX          |-->| SELECTED METHOD FOR THE TREATMENT OF RIGID BEDS
C| H             |-->| WATER DEPTH
C| NLISS          |<->| NUMBER OF SMOOTHINGS
C| NPOIN          |-->| NUMBER OF 2D POINTS
C| X,Y            |-->| 2D COORDINATES
C| Z             |-->| FREE SURFACE
C| ZF             |-->| BED LEVEL
C| ZR             |<--| RIGID BED LEVEL
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
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS
C
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
C
C--------------------
C RIGID BEDS POSITION
C---------------------
C
C     DEFAULT VALUE:       ZR=ZF-100
C
      CALL OV( 'X=C       ',ZR,ZF,ZF,-100.D0,NPOIN)
C
C------------------
C SMOOTHING OPTION
C------------------
C
C     NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE
C             DEFAULT VALUE : NLISS = 0 (NO SMOOTHING)
C
      NLISS = 0
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C