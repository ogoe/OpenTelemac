C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FRICTION VECTOR FOR THE QUADRATIC ELEMENT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CHESTR, IKLE, LINDDP, LINDNER, LINDSP, NDEFMA, NELEM, NELMAX, NKFROT, NPOIN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FRICTION_CHOICE()

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
!> </td><td>
!> </td><td> JACEK JANKOWSKI (BAW)
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 22/12/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CHESTR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LINDDP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LINDNER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LINDSP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NDEFMA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NKFROT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FRICTION_QUAD
     & (IKLE, NPOIN, NELEM, NELMAX, LINDNER, NKFROT, CHESTR, NDEFMA,
     &  LINDDP, LINDSP)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHESTR         |---| 
C| IKLE           |---| 
C| LINDDP         |---| 
C| LINDNER        |---| 
C| LINDSP         |---| 
C| NDEFMA         |---| 
C| NELEM          |---| 
C| NELMAX         |---| 
C| NKFROT         |---| 
C| NPOIN          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,        INTENT(IN)    :: NPOIN,NELEM,NELMAX
      INTEGER,        INTENT(IN)    :: IKLE(NELMAX,6)
      LOGICAL,        INTENT(IN)    :: LINDNER
      TYPE(BIEF_OBJ), INTENT(INOUT) :: NKFROT,CHESTR,NDEFMA
      TYPE(BIEF_OBJ), INTENT(INOUT) :: LINDDP,LINDSP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER :: IELEM
!
!=======================================================================!
!=======================================================================!
C                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
C IF THE 11 NODES HAVE THE SAME FRICTION LAW, INTERPOLATION A LA CG1113
C FOR THE VALUES AT 13 ADDITIONAL NODES IN THE MIDDLE OF THE EDGES
!
C        X(IKLE(IELEM,4)) = 0.5D0 * ( X(IKLE(IELEM,1))
C     &                             + X(IKLE(IELEM,2)) )
C        X(IKLE(IELEM,5)) = 0.5D0 * ( X(IKLE(IELEM,2))
C     &                             + X(IKLE(IELEM,3)) )
C        X(IKLE(IELEM,6)) = 0.5D0 * ( X(IKLE(IELEM,3))
C     &                             + X(IKLE(IELEM,1)) )
!
C WELL, IF THE THE FRICTION LAWS DIFFER, TAKE THE VALUE ON THE PREVIOUS
C NODE BY CIRCUMVENTING THE ELEMENT...
!
C        X(IKLE(IELEM,4)) = X(IKLE(IELEM,1))
C        X(IKLE(IELEM,5)) = X(IKLE(IELEM,2))
C        X(IKLE(IELEM,6)) = X(IKLE(IELEM,3))
!
C ASSUMED THE TRIVIAL CASE OF -ONE- ZONE ONLY IS NATURALLY EXCLUDED
!
      DO IELEM = 1,NELEM

        IF(NKFROT%I(IKLE(IELEM,1)).EQ.NKFROT%I(IKLE(IELEM,2))) THEN
          NKFROT%I(IKLE(IELEM,4)) = NKFROT%I(IKLE(IELEM,1))
          CHESTR%R(IKLE(IELEM,4)) =
     &      0.5D0*(CHESTR%R(IKLE(IELEM,1))+CHESTR%R(IKLE(IELEM,2)))
          NDEFMA%R(IKLE(IELEM,4)) =
     &      0.5D0*(NDEFMA%R(IKLE(IELEM,1))+NDEFMA%R(IKLE(IELEM,2)))
        ELSE
          NKFROT%I(IKLE(IELEM,4)) = NKFROT%I(IKLE(IELEM,1))
          CHESTR%R(IKLE(IELEM,4)) = CHESTR%R(IKLE(IELEM,1))
          NDEFMA%R(IKLE(IELEM,4)) = NDEFMA%R(IKLE(IELEM,1))
        ENDIF

        IF(NKFROT%I(IKLE(IELEM,2)).EQ.NKFROT%I(IKLE(IELEM,3))) THEN
          NKFROT%I(IKLE(IELEM,5)) = NKFROT%I(IKLE(IELEM,2))
          CHESTR%R(IKLE(IELEM,5)) =
     &      0.5D0*(CHESTR%R(IKLE(IELEM,2))+CHESTR%R(IKLE(IELEM,3)))
          NDEFMA%R(IKLE(IELEM,5)) =
     &      0.5D0*(NDEFMA%R(IKLE(IELEM,2))+NDEFMA%R(IKLE(IELEM,3)))
        ELSE
          NKFROT%I(IKLE(IELEM,5)) = NKFROT%I(IKLE(IELEM,2))
          CHESTR%R(IKLE(IELEM,5)) = CHESTR%R(IKLE(IELEM,2))
          NDEFMA%R(IKLE(IELEM,5)) = NDEFMA%R(IKLE(IELEM,2))
        ENDIF

        IF(NKFROT%I(IKLE(IELEM,3)).EQ.NKFROT%I(IKLE(IELEM,1))) THEN
          NKFROT%I(IKLE(IELEM,6)) = NKFROT%I(IKLE(IELEM,3))
          CHESTR%R(IKLE(IELEM,6)) =
     &      0.5D0*(CHESTR%R(IKLE(IELEM,3))+CHESTR%R(IKLE(IELEM,1)))
          NDEFMA%R(IKLE(IELEM,6)) =
     &      0.5D0*(NDEFMA%R(IKLE(IELEM,3))+NDEFMA%R(IKLE(IELEM,1)))
        ELSE
          NKFROT%I(IKLE(IELEM,6)) = NKFROT%I(IKLE(IELEM,3))
          CHESTR%R(IKLE(IELEM,6)) = CHESTR%R(IKLE(IELEM,3))
          NDEFMA%R(IKLE(IELEM,6)) = NDEFMA%R(IKLE(IELEM,3))
        ENDIF

      ENDDO

      ! THIS RARE CASE SEPARATELY

      IF (LINDNER) THEN

        DO IELEM = 1,NELEM

          IF (NKFROT%I(IKLE(IELEM,1)).EQ.NKFROT%I(IKLE(IELEM,2))) THEN
            LINDDP%R(IKLE(IELEM,4)) =
     &        0.5D0*(LINDDP%R(IKLE(IELEM,1))+LINDDP%R(IKLE(IELEM,2)))
            LINDSP%R(IKLE(IELEM,4)) =
     &        0.5D0*(LINDSP%I(IKLE(IELEM,1))+LINDSP%R(IKLE(IELEM,2)))
          ELSE
            LINDDP%R(IKLE(IELEM,4)) = LINDDP%R(IKLE(IELEM,1))
            LINDSP%R(IKLE(IELEM,4)) = LINDSP%R(IKLE(IELEM,1))
          ENDIF

          IF (NKFROT%I(IKLE(IELEM,2)).EQ.NKFROT%I(IKLE(IELEM,3))) THEN
            LINDDP%R(IKLE(IELEM,5)) =
     &        0.5D0*(LINDDP%R(IKLE(IELEM,2))+LINDDP%R(IKLE(IELEM,3)))
            LINDSP%R(IKLE(IELEM,5)) =
     &        0.5D0*(LINDSP%I(IKLE(IELEM,2))+LINDSP%R(IKLE(IELEM,3)))
          ELSE
            LINDDP%R(IKLE(IELEM,5)) = LINDDP%R(IKLE(IELEM,2))
            LINDSP%R(IKLE(IELEM,5)) = LINDSP%R(IKLE(IELEM,2))
          ENDIF

          IF (NKFROT%I(IKLE(IELEM,3)).EQ.NKFROT%I(IKLE(IELEM,1))) THEN
            LINDDP%R(IKLE(IELEM,6)) =
     &        0.5D0*(LINDDP%R(IKLE(IELEM,3))+LINDDP%R(IKLE(IELEM,1)))
            LINDSP%R(IKLE(IELEM,6)) =
     &        0.5D0*(LINDSP%I(IKLE(IELEM,3))+LINDSP%R(IKLE(IELEM,1)))
          ELSE
            LINDDP%R(IKLE(IELEM,6)) = LINDDP%R(IKLE(IELEM,3))
            LINDSP%R(IKLE(IELEM,6)) = LINDSP%R(IKLE(IELEM,3))
          ENDIF

        ENDDO

      ENDIF
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END SUBROUTINE FRICTION_QUAD
C
C#######################################################################
C