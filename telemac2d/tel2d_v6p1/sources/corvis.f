C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CORRECTS THE DIFFUSION COEFFICIENT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THIS SUBROUTINE IS MERELY AN EXAMPLE; MUST BE CODED BY THE USER
!>  @code
!>   EXAMPLE : SETS THE VALUE TO 0.1 INSIDE A SQUARE.
!>
!>   NSOM = 4
!>   XSOM(1) =     0.D0
!>   YSOM(1) =     0.D0
!>   XSOM(2) =  1000.D0
!>   YSOM(2) =     0.D0
!>   XSOM(3) =  1000.D0
!>   YSOM(3) =  1000.D0
!>   XSOM(4) =     0.D0
!>   YSOM(4) =  1000.D0
!>
!>   CALL FILPOL( VISC , 0.1D0 , XSOM , YSOM , NSOM , MESH )
!>  @endcode

!>  @warning  THE SUBROUTINE IS CALLED AT EVERY TIME STEP

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 17/08/1994
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CORVIS
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
CEX   USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C-----------------------------------------------------------------------
C
C     EXAMPLE : SETS THE VALUE TO 0.1 INSIDE A SQUARE.
C               REMOVE THE CEX COMMENTS TO IMPLEMENT IT.
C
C
CEX   INTEGER NSOM
CEX   DOUBLE PRECISION XSOM(10),YSOM(10)
C
C-----------------------------------------------------------------------
C
C     DESCRIBES THE SQUARE AS A POLYGON TO CALL ROUTINE FILPOL
C
CEX   NSOM = 4
CEX   XSOM(1) =     0.D0
CEX   YSOM(1) =     0.D0
CEX   XSOM(2) =  1000.D0
CEX   YSOM(2) =     0.D0
CEX   XSOM(3) =  1000.D0
CEX   YSOM(3) =  1000.D0
CEX   XSOM(4) =     0.D0
CEX   YSOM(4) =  1000.D0
C
CEX   CALL FILPOL( VISC , 0.1D0 , XSOM , YSOM , NSOM , MESH )
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C