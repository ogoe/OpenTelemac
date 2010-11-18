C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODIFIES THE POROSITY OF ELEMENTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!>  @code
!>     EXAMPLE : POROSITY IS SET TO 0.5 IN A QUADRILATERAL
!>
!>     NSOM = 4
!>     XSOM(1) = 8020.88D0
!>     XSOM(2) = 7761.81D0
!>     XSOM(3) = 8679.17D0
!>     XSOM(4) = 8988.75D0
!>     YSOM(1) =-1547.11D0
!>     YSOM(2) =-2415.26D0
!>     YSOM(3) =-2604.16D0
!>     YSOM(4) =-1543.75D0
!>
!>-----------------------------------------------------------------------
!>
!>     CALL OS( 'X=C     ' , POROS , POROS , POROS , 1.D0 )
!>
!>--------------------------------------------------------------
!>
!>     DO 4 IELEM = 1 , NELEM
!>
!>       XX1 = (  X(IKLE%I(IELEM)          )+
!>    *           X(IKLE%I(IELEM+NELMAX)   )+
!>    *           X(IKLE%I(IELEM+2*NELMAX) ))/3.D0
!>       YY1 = (  Y(IKLE%I(IELEM)          )+
!>    *           Y(IKLE%I(IELEM+NELMAX)   )+
!>    *           Y(IKLE%I(IELEM+2*NELMAX) ))/3.D0
!>
!>       IF(INPOLY(XX1,YY1,XSOM,YSOM,NSOM)) THEN
!>         POROS%R(IELEM) = 0.5D0
!>       ENDIF
!>
!>4     CONTINUE
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> POROS
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>POROS()

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
!> </td><td> 01/03/1990
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>POROS
!></td><td><-></td><td>POROSITY TO BE MODIFIED.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CORPOR
     &(POROS)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| POROS          |<->| POROSITY TO BE MODIFIED.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: POROS
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DOUBLE PRECISION XSOM(4),YSOM(4),XX1,YY1
C     INTEGER NSOM,IELEM
C
C-----------------------------------------------------------------------
C
C     EXAMPLE : POROSITY IS SET TO 0.5 IN A QUADRILATERAL
C
C     NSOM = 4
C     XSOM(1) = 8020.88D0
C     XSOM(2) = 7761.81D0
C     XSOM(3) = 8679.17D0
C     XSOM(4) = 8988.75D0
C     YSOM(1) =-1547.11D0
C     YSOM(2) =-2415.26D0
C     YSOM(3) =-2604.16D0
C     YSOM(4) =-1543.75D0
C
C-----------------------------------------------------------------------
C
C     CALL OS( 'X=C     ' , POROS , POROS , POROS , 1.D0 )
C
C--------------------------------------------------------------
C
C     DO 4 IELEM = 1 , NELEM
C
C       XX1 = (  X(IKLE%I(IELEM)          )+
C    *           X(IKLE%I(IELEM+NELMAX)   )+
C    *           X(IKLE%I(IELEM+2*NELMAX) ))/3.D0
C       YY1 = (  Y(IKLE%I(IELEM)          )+
C    *           Y(IKLE%I(IELEM+NELMAX)   )+
C    *           Y(IKLE%I(IELEM+2*NELMAX) ))/3.D0
C
C       IF(INPOLY(XX1,YY1,XSOM,YSOM,NSOM)) THEN
C         POROS%R(IELEM) = 0.5D0
C       ENDIF
C
C4     CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C