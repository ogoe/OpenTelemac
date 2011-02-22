C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CAPS THE VALUES IN ARRAY F.
!><br>            MINIMUM OF F: XMIN (IF CLPMIN=.TRUE.).
!><br>            MAXIMUM OF F: XMAX (IF CLPMAX=.TRUE.).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : IF NPOIN IS NEGATIVE, WILL TREAT -NPOIN VALUES.
!><br>                 IF NPOIN IS POSITIVE, WILL TAKE THE SIZE OF F.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CLPMAX, CLPMIN, F, NPOIN, XMAX, XMIN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> NP
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CLIP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CONDIH(), CORRECTION_DEPTH_2D(), CORRECTION_DEPTH_3D(), DIFF3D(), KEPSIL(), TELEMAC2D(), TELEMAC3D(), THOMPS()

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
!> </td><td> 12/01/95
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CLPMAX
!></td><td>--></td><td>LOGIQUE QUI DECIDE DU CLIPPING
!>    </td></tr>
!>          <tr><td>CLPMIN
!></td><td>--></td><td>LOGIQUE QUI DECIDE DU CLIPPING
!>    </td></tr>
!>          <tr><td>F
!></td><td><-></td><td>TABLEAU DES VALEURS
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE TOTAL DE POINTS DU DOMAINE
!>    </td></tr>
!>          <tr><td>XMAX
!></td><td>--></td><td>VALEUR MAX
!>    </td></tr>
!>          <tr><td>XMIN
!></td><td>--></td><td>VALEUR MIN
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CLIP
     &(F,XMIN,CLPMIN,XMAX,CLPMAX,NPOIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CLPMAX         |-->| LOGIQUE QUI DECIDE DU CLIPPING
C| CLPMIN         |-->| LOGIQUE QUI DECIDE DU CLIPPING
C| F             |<->| TABLEAU DES VALEURS
C| NPOIN          |-->| NOMBRE TOTAL DE POINTS DU DOMAINE
C| XMAX           |-->| VALEUR MAX
C| XMIN           |-->| VALEUR MIN
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CLIP => CLIP
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: F
      DOUBLE PRECISION, INTENT(IN)    :: XMIN,XMAX
      LOGICAL         , INTENT(IN)    :: CLPMIN,CLPMAX
      INTEGER         , INTENT(IN)    :: NPOIN
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NP
C
C-----------------------------------------------------------------------
C
      IF(F%TYPE.EQ.2) THEN
C       F IS A VECTOR STRUCTURE
        IF(NPOIN.LT.0) THEN
          NP = - NPOIN
        ELSE
          NP = F%DIM1
        ENDIF
        IF(CLPMIN) CALL OV('X=+(Y,C)',F%R,F%R, F%R , XMIN , NP )
        IF(CLPMAX) CALL OV('X=-(Y,C)',F%R,F%R, F%R , XMAX , NP )
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) F%NAME,' N''EST PAS UN VECTEUR'
        IF(LNG.EQ.2) WRITE(LU,*) F%NAME,' IS NOT A VECTOR'
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C