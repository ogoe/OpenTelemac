C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DETERMINES THE TYPE OF TRANSFORMATION.
!><br>            BUILDS THE REDUCED COORDINATES FOR
!>                THE METHOD OF CHARACTERISTICS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NPLAN, TRANSF, TRANSF_PLANE, ZCHAR, ZSTAR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IPLAN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!> </td><td> 11/03/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF HORIZONTAL PLANES
!>    </td></tr>
!>          <tr><td>TRANSF
!></td><td><--</td><td>TYPE OF TRANSFORMATION
!>    </td></tr>
!>          <tr><td>TRANSF_PLANE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZCHAR
!></td><td><--</td><td>COORDINATES IN THE TRANSFORMED MESH
!>                  FOR THE METHOD OF CHARACTERISTICS
!>    </td></tr>
!>          <tr><td>ZSTAR
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TRANSF_ZCHAR
     &(TRANSF,ZCHAR,ZSTAR,TRANSF_PLANE,NPLAN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NPLAN          |-->| NUMBER OF HORIZONTAL PLANES
C| TRANSF         |<--| TYPE OF TRANSFORMATION
C| TRANSF_PLANE   |---| 
C| ZCHAR          |<--| COORDINATES IN THE TRANSFORMED MESH
C|                |   | FOR THE METHOD OF CHARACTERISTICS
C| ZSTAR          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN)           :: NPLAN
      INTEGER, INTENT(INOUT)        :: TRANSF
      TYPE(BIEF_OBJ), INTENT(INOUT) :: ZCHAR,TRANSF_PLANE
      TYPE(BIEF_OBJ), INTENT(IN)    :: ZSTAR
!
!-----------------------------------------------------------------------
!
      INTEGER IPLAN
!
!***********************************************************************
!
C     DETERMINES THE TYPE OF TRANSFORMATION
!
C     1: CLASSICAL SIGMA TRANSFORMATION
C     2: SIGMA TRANSFORMATION WITH GIVEN PROPORTIONS
C     3: GENERALISED SIGMA TRANSFORMATION
!
      IF(TRANSF.NE.0) THEN
        TRANSF=1
        DO IPLAN=2,NPLAN
          IF(TRANSF_PLANE%I(IPLAN).EQ.2) THEN
            TRANSF=2
          ENDIF
        ENDDO
        DO IPLAN=2,NPLAN
          IF(TRANSF_PLANE%I(IPLAN).EQ.3) TRANSF=3
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
C     BUILDS ZCHAR (USED ONLY BY CHARACTERISTICS)
C     ZCHAR IS THE VERTICAL COORDINATE OF THE TRANSFORMED MESH
!
      IF(TRANSF.EQ.0.OR.TRANSF.EQ.1.OR.TRANSF.EQ.3) THEN
        DO IPLAN = 1,NPLAN
          ZCHAR%R(IPLAN) = DBLE(IPLAN-1)/DBLE(NPLAN-1)
        ENDDO
      ELSEIF(TRANSF.EQ.2) THEN
        DO IPLAN = 1,NPLAN
          IF(TRANSF_PLANE%I(IPLAN).EQ.1) THEN
            ZCHAR%R(IPLAN) = DBLE(IPLAN-1)/DBLE(NPLAN-1)
          ELSE
            ZCHAR%R(IPLAN) = ZSTAR%R(IPLAN)
          ENDIF
        ENDDO
      ELSE
        WRITE(LU,*) 'WRONG TRANSFORMATION IN TRANSF_ZCHAR'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C