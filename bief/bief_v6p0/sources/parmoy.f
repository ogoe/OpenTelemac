C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       AVERAGE OF A VECTOR AT THE INTERFACES BETWEEN
!>                SUB-DOMAINS.
!><br>            X CAN BE A BLOCK OF VECTORS. IN THIS CASE, ALL THE
!>                VECTORS IN THE BLOCK ARE TREATED.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  IF THE VECTORS HAVE A SECOND DIMENSION, IT IS
!>            IGNORED FOR THE TIME BEING

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> MESH, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, TYPX
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PARMOY
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PARCOM(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CGSTAB(), GRACJG(), RESCJG()

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
!> </td><td> 24/04/97
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td> AFTER REINHARD HINKELMANN (HANNOVER UNI.)
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MAILLAGE.
!>    </td></tr>
!>          <tr><td>X
!></td><td><-></td><td>VECTEUR OU BLOC DE VECTEURS.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PARMOY
     &( X , MESH )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| MESH           |-->| MAILLAGE.
C| X             |<->| VECTEUR OU BLOC DE VECTEURS.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PARMOY => PARMOY
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     STRUCTURES: MESH, VECTORS OR BLOCKS
C
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: X
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,TYPX
C
C  COMPLEMENTS THE INTERFACES:
C
      CALL PARCOM( X , 2 , MESH )
C
C-----------------------------------------------------------------------
C
      TYPX = X%TYPE
C
C-----------------------------------------------------------------------
C
C  CASE WHERE THE STRUCTURES ARE BLOCKS
C
      IF(TYPX.EQ.4) THEN
C
        DO 10 I=1,X%N
          CALL OS('X=XY    ',X=X%ADR(I)%P,Y=MESH%FAC)
10      CONTINUE
C
C-----------------------------------------------------------------------
C
C  CASE WHERE THE STRUCTURE IS A VECTOR
C
      ELSEIF(TYPX.EQ.2) THEN
C
        CALL OS('X=XY    ',X=X,Y=MESH%FAC)
C
C-----------------------------------------------------------------------
C
C  ERROR ON THE STRUCTURE
C
      ELSE
C
         IF (LNG.EQ.1) WRITE(LU,50) X%NAME,X%TYPE
         IF (LNG.EQ.1) WRITE(LU,53)
50       FORMAT(1X,'PARMOY (BIEF) : NOM DE X : ',A6,'  TYPE : ',1I6)
53       FORMAT(1X,'                CAS NON PREVU')
         IF (LNG.EQ.2) WRITE(LU,51) X%NAME,X%TYPE
         IF (LNG.EQ.2) WRITE(LU,54)
51       FORMAT(1X,'PARMOY (BIEF) : NAME OF X: ',A6,'  TYPE : ',1I6)
54       FORMAT(1X,'                UNEXPECTED CASE')
         CALL PLANTE(1)
         STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C