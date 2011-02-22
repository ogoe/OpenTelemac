C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CORRECTS THE FREE SURFACE COMPUTATION BY ELEMENTS
!>                TO TAKE ACCOUNT OF THE TIDAL FLATS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE, NELEM, NELMAX, NEWSL, OLDSL, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, IK, J
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CORRSL(), FSGRAD()

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
!>      <td><center> 5.5                                       </center>
!> </td><td> 27/11/92
!> </td><td> J-M JANIN    (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>TABLES DE CONNECTIVITE
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NEWSL
!></td><td>---</td><td>SURFACE LIBRE MODIFIEE, PAR ELEMENTS
!>    </td></tr>
!>          <tr><td>OLDSL
!></td><td>--></td><td>SURFACE LIBRE REELLE.
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTE DU FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CRSL11
     &(NEWSL,OLDSL,ZF,IKLE,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |-->| TABLES DE CONNECTIVITE
C| NELEM          |-->| NOMBRE D'ELEMENTS
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS
C| NEWSL          |---| SURFACE LIBRE MODIFIEE, PAR ELEMENTS
C| OLDSL          |-->| SURFACE LIBRE REELLE.
C| ZF             |-->| COTE DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      DOUBLE PRECISION, INTENT(INOUT) :: NEWSL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN)    :: OLDSL(*),ZF(*)
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,3)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IK(3),J(3)
C
C-----------------------------------------------------------------------
C
      INTRINSIC MAX
C
C-----------------------------------------------------------------------
C
C  1) SORTS (ASCENDING ORDER) THE BOTTOM ELEVATIONS AND POTENTIALLY
C     CORRECTS THE FREE SURFACE ELEVATION FOR DRYING ELEMENTS
C
C-----------------------------------------------------------------------
C
         DO 4 IELEM = 1 , NELEM
C
           IK(1) = IKLE(IELEM,1)
           IK(2) = IKLE(IELEM,2)
           IK(3) = IKLE(IELEM,3)
           J(1) = 1
           J(2) = 2
           J(3) = 3
C
C          SORTS THE 3 POINTS
C
           IF(ZF(IK(2)).LT.ZF(IK(1)))  THEN
              J(2)=1
              J(1)=2
           ENDIF
           IF(ZF(IK(3)).LT.ZF(IK(J(2)))) THEN
              J(3)=J(2)
              J(2)=3
              IF(ZF(IK(3)).LT.ZF(IK(J(1)))) THEN
                 J(2)=J(1)
                 J(1)=3
              ENDIF
           ENDIF
C
C          CORRECTS
C
           NEWSL(IELEM,J(1))=MAX(ZF(IK(J(2))),OLDSL(IK(J(1))))
           NEWSL(IELEM,J(2))=OLDSL(IK(J(2)))
           NEWSL(IELEM,J(3))=
     &      OLDSL(IK(J(3)))-MAX(0.D0,ZF(IK(J(3)))-OLDSL(IK(J(2))))
C
4        CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C