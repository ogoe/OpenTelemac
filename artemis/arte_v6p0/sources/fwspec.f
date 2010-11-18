C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SPECIFIES THE BOTTOM FRICTION COEFFICIENT
!>                IF IT IS VARIABLE IN SPACE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THIS SUBROUTINE IS MERELY AN EXAMPLE;
!>            MUST BE CODED BY THE USER
!>  @code
!>  IN THIS EXAMPLE THE FRICTION COEFFICIENT IS CONSTANT
!>
!>     CALL OV( 'X=C     ' , FW , X , X , FWCOEF , NPOIN )
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_ARTEMIS
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FW, FWCOEF, NPOIN, PRIVE, X, Y, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FWSPEC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BERKHO()

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
!>    <td> 02/06/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td>                                                         </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> LINKED TO BIEF 5.0                                      </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>FW
!></td><td><--</td><td>COEFFICIENT DE FROTTEMENT
!>    </td></tr>
!>          <tr><td>FWCOEF
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT CONSTANT IMPOSE
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAU DE TRAVAIL DEFINI DANS PRINCI
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEE DU MAILLAGE .
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>COTE DU FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FWSPEC
     &(FW,FWCOEF,X,Y,NPOIN,PRIVE,ZF)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FW             |<--| COEFFICIENT DE FROTTEMENT
C| FWCOEF         |-->| COEFFICIENT DE FROTTEMENT CONSTANT IMPOSE
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| PRIVE          |-->| TABLEAU DE TRAVAIL DEFINI DANS PRINCI
C| X,Y            |-->| COORDONNEE DU MAILLAGE .
C| ZF             |-->| COTE DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_ARTEMIS, EX_FWSPEC => FWSPEC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPOIN
C
      DOUBLE PRECISION FW(NPOIN),X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION ZF(NPOIN),FWCOEF
C
      TYPE(BIEF_OBJ) :: PRIVE
C
C-----------------------------------------------------------------------
C
C  IN THIS EXAMPLE THE FRICTION COEFFICIENT IS CONSTANT
C
      CALL OV( 'X=C     ' , FW , X , X , FWCOEF , NPOIN )
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C