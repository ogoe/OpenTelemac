C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FRICTION VECTOR FOR THE QUASI-BUBBLE ELEMENT.

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
!>    </th><td> I, I1, I2, I3
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
!>      <td><center> 5.5                                       </center>
!> </td><td>
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
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
                        SUBROUTINE FRICTION_BUBBLE
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
      TYPE(BIEF_OBJ), INTENT(IN)    :: IKLE
      INTEGER,        INTENT(IN)    :: NPOIN, NELEM, NELMAX
      LOGICAL,        INTENT(IN)    :: LINDNER
      TYPE(BIEF_OBJ), INTENT(INOUT) :: NKFROT, CHESTR, NDEFMA
      TYPE(BIEF_OBJ), INTENT(INOUT) :: LINDDP, LINDSP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER :: I, I1, I2, I3
!
!=======================================================================!
!=======================================================================!
C                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
      DO I = NPOIN + 1, NPOIN + NELEM
!
         I1 = IKLE%I(I - NPOIN           )
         I2 = IKLE%I(I - NPOIN +   NELMAX)
         I3 = IKLE%I(I - NPOIN + 2*NELMAX)
!
         ! COMPUTING THE VALUES OF THE MIDDLE-NODE
         ! ---------------------------------------
         IF (NKFROT%I(I1).EQ.NKFROT%I(I2)) THEN

            ! THE 3 NODES HAVE THE SAME LAW !
            ! ***************************** !
            IF (NKFROT%I(I1).EQ.NKFROT%I(I3))THEN
!
               NKFROT%I(I) = NKFROT%I(I1)
               CHESTR%R(I) = (CHESTR%R(I3) + CHESTR%R(I2) +CHESTR%R(I1))
     &                     / 3.D0
               NDEFMA%R(I) = (NDEFMA%R(I3) + NDEFMA%R(I2) +NDEFMA%R(I1))
     &                     / 3.D0
!
               IF (LINDNER) THEN
                  LINDDP%R(I) = ( LINDDP%R(I3) + LINDDP%R(I2)
     &                           +LINDDP%R(I1) )/3.D0
!
                  LINDSP%R(I) = ( LINDSP%R(I3) + LINDSP%R(I2)
     &                           +LINDSP%R(I1) )/3.D0
               ENDIF
!
            ! THE NODES "1" AND "2" HAVE THE SAME LAW !
            ! *************************************** !
            ELSE
               NKFROT%I(I) = NKFROT%I(I1)
               CHESTR%R(I) = (CHESTR%R(I2) + CHESTR%R(I1))/2.D0
               NDEFMA%R(I) = (NDEFMA%R(I2) + NDEFMA%R(I1))/2.D0
!
               IF (LINDNER) THEN
                  LINDDP%R(I) = (LINDDP%R(I2) + LINDDP%R(I1))/2.D0
                  LINDSP%R(I) = (LINDSP%R(I2) + LINDSP%R(I1))/2.D0
               ENDIF
            ENDIF
!
         ! THE NODES "2" AND "3" HAVE THE SAME LAW !
         ! *************************************** !
         ELSE IF (NKFROT%I(I2).EQ.NKFROT%I(I3)) THEN
!
            NKFROT%I(I) = NKFROT%I(I2)
            CHESTR%R(I) = (CHESTR%R(I3) + CHESTR%R(I2))/2.D0
            NDEFMA%R(I) = (NDEFMA%R(I3) + NDEFMA%R(I2))/2.D0
!
            IF (LINDNER) THEN
               LINDDP%R(I) = (LINDDP%R(I3) + LINDDP%R(I2))/2.D0
               LINDSP%R(I) = (LINDSP%R(I3) + LINDSP%R(I2))/2.D0
            ENDIF
!
         ! THE 3 NODES HAVE DIFFERENT LAWS : VALUE OF THE NODE "1" KEPT !
         ! ************************************************************ !
         ELSE
            NKFROT%I(I) = NKFROT%I(I1)
            CHESTR%R(I) = CHESTR%R(I1)
            NDEFMA%R(I) = NDEFMA%R(I1)
!
            IF (LINDNER) THEN
               LINDDP%R(I) = LINDDP%R(I1)
               LINDSP%R(I) = LINDSP%R(I1)
            ENDIF
         ENDIF
      ENDDO
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END
C
C#######################################################################
C