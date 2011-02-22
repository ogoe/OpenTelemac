C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       IN PARALLEL MODE, CHECKS THAT PROCESSORS SHARING AN
!>                INTERFACE POINT HAVE EXACTLY THE SAME VALUE FOR ARRAY F.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, MESH, T1
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, ISTOP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PARCOM(), PLANTE(), P_IMAX()
!>   </td></tr>
!>     </table>

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 02/06/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F
!></td><td>--></td><td>TABLEAU A VERIFIER
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>STRUCTURE DE MAILLAGE
!>    </td></tr>
!>          <tr><td>T1
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CHECK_DIGITS
     &(F,T1,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F             |-->| TABLEAU A VERIFIER
C| MESH           |-->| STRUCTURE DE MAILLAGE
C| T1             |<->| TABLEAU DE TRAVAIL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ),  INTENT(IN   ) :: F
      TYPE(BIEF_OBJ),  INTENT(INOUT) :: T1
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,ISTOP
C
      INTEGER  P_IMAX
      EXTERNAL P_IMAX
C
C-----------------------------------------------------------------------
C
      CALL OS('X=Y     ',X=T1,Y=F)
      CALL PARCOM(T1,3,MESH)
      ISTOP=0
      DO I=1,T1%DIM1
        IF(T1%R(I).NE.F%R(I)) THEN
          IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'CHECK_DIGITS : DIFFERENCE DANS ',F%NAME
          WRITE(LU,*) '               AU POINT LOCAL ',I
          WRITE(LU,*) '               =  POINT GLOBAL ',MESH%KNOLG%I(I)
          WRITE(LU,*) '               VALEUR ',F%R(I)
          WRITE(LU,*) '               MINIMUM ',T1%R(I)
          WRITE(LU,*) '            DIFFERENCE ',F%R(I)-T1%R(I)
          ENDIF
          IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'CHECK_DIGITS : DIFFERENCE IN ',F%NAME
          WRITE(LU,*) '               AT LOCAL POINT ',I
          WRITE(LU,*) '               =  GLOBAL POINT ',MESH%KNOLG%I(I)
          WRITE(LU,*) '               VALUE ',F%R(I)
          WRITE(LU,*) '               MINIMUM ',T1%R(I)
          WRITE(LU,*) '            DIFFERENCE ',F%R(I)-T1%R(I)
          ENDIF
          ISTOP=I
        ENDIF
      ENDDO
!
      IF(NCSIZE.GT.1) ISTOP=P_IMAX(ISTOP)
      IF(ISTOP.GT.0) THEN
        WRITE(LU,*) 'CHECK_DIGITS : ERREUR SUR VECTEUR ',F%NAME
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