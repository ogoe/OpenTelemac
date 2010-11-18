C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CORRECTS THE FRICTION COEFFICIENT ON THE BOTTOM
!>                WHEN IT IS VARIABLE IN TIME.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!>  @code
!>2D   DO I = 1 , NPOIN
!>2D     IF(AT.GT.1200.D0) THEN
!>2D       CHESTR%R(I) = 40.D0
!>2D     ELSE
!>2D       CHESTR%R(I) = 60.D0
!>2D     ENDIF
!>2D   ENDDO
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D(), TELEMAC3D()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 17/08/1994
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TIME.
!>    </td></tr>
!>          <tr><td>CHESTR
!></td><td><--</td><td>COEFFICIENT DE FROTTEMENT
!>    </td></tr>
!>          <tr><td>FFON
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT ASSOCIE A LA LOI
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU.
!>    </td></tr>
!>          <tr><td>KFROT
!></td><td>--></td><td>LOI DE FROTTEMENT (LINEAIRE,CHEZY,STRICKLER)
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
                        SUBROUTINE CORSTR
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TIME.
C| CHESTR         |<--| COEFFICIENT DE FROTTEMENT
C| FFON           |-->| COEFFICIENT DE FROTTEMENT ASSOCIE A LA LOI
C| H             |-->| HAUTEUR D'EAU.
C| KFROT          |-->| LOI DE FROTTEMENT (LINEAIRE,CHEZY,STRICKLER)
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| PRIVE          |-->| TABLEAU DE TRAVAIL DEFINI DANS PRINCI
C| X,Y            |-->| COORDONNEE DU MAILLAGE .
C| ZF             |-->| COTE DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
C     C2D: EXAMPLE FOR TELEMAC-2D
C     C3D: EXAMPLE FOR TELEMAC-3D
C
C2D   USE DECLARATIONS_TELEMAC2D
C3D   USE DECLARATIONS_TELEMAC3D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C2D   INTEGER I
C3D   INTEGER I
C
C-----------------------------------------------------------------------
C
C2D   DO I = 1 , NPOIN
C2D     IF(AT.GT.1200.D0) THEN
C2D       CHESTR%R(I) = 40.D0
C2D     ELSE
C2D       CHESTR%R(I) = 60.D0
C2D     ENDIF
C2D   ENDDO
C
C-----------------------------------------------------------------------
C
C3D   DO I = 1 , NPOIN2
C3D     IF(AT.GT.1200.D0) THEN
C3D       RUGOF%R(I) = 40.D0
C3D     ELSE
C3D       RUGOF%R(I) = 60.D0
C3D     ENDIF
C3D   ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C