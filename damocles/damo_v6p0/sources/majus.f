C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    CONVERTS A CHARACTER STRING FROM LOWER TO UPPER CASE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CHAINE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IPOS, STMAJ, STMIN
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CONDIH(), FRICTION_READ(), FRICTION_SCAN(), INFLU(), LECDON_ARTEMIS(), LECDON_SISYPHE(), LECDON_TELEMAC2D(), LECDON_TELEMAC3D(), LECDON_TOMAWAC(), LOGLU()

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
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 30/01/1992                                              </td>
!>    <td> A. DESITTER (BRISTOL)                                   </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 30/01/1992                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CHAINE
!></td><td><-></td><td>CHAINE DE CARACTERES A MODIFIER
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MAJUS
     &(CHAINE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHAINE         |<->| CHAINE DE CARACTERES A MODIFIER
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      CHARACTER*26 STMAJ,STMIN
      CHARACTER*(*) CHAINE
C
      INTEGER I,IPOS
C
      INTRINSIC LEN,INDEX
C
      DATA STMAJ /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA STMIN /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C
C----------------------------------------------------------------------
C
      DO 10 I=1,LEN(CHAINE)
C
      IPOS=INDEX(STMIN,CHAINE(I:I))
      IF(IPOS.NE.0) CHAINE(I:I)=STMAJ(IPOS:IPOS)
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C