C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE BOTTOM FRICTION COEFFICIENT
!>                IF VARIABLE IN SPACE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IN PARAMETER ESTIMATION WITH A LIST OF TESTS,
!>         THESE VALUES ARE DISCARDED.

!>  @warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!>  @code
!>  COMMENTS CEX MUST BE REMOVED TO IMPLEMENT THE EXAMPLE.
!>  HERE A CONSTANT FRICTION VALUE IS GIVEN:
!>
!>CEX   DO I=1,NPOIN
!>CEX     CHESTR%R(I) = 60.D0
!>CEX   ENDDO
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
!><br>FONSTR(), FRICTION_CHOICE(), HOMERE_ADJ_T2D()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 01/10/96
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CHESTR
!></td><td><--</td><td>COEFFICIENT DE FROTTEMENT
!>    </td></tr>
!>          <tr><td>FFON
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT ASSOCIE A LA LOI
!>    </td></tr>
!>          <tr><td>KFROT
!></td><td>--></td><td>LOI DE FROTTEMENT (LINEAIRE,CHEZY,STRICKLER)
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES ENTIERS DU MAILLAGE.
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
                        SUBROUTINE STRCHE
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHESTR         |<--| COEFFICIENT DE FROTTEMENT
C| FFON           |-->| COEFFICIENT DE FROTTEMENT ASSOCIE A LA LOI
C| KFROT          |-->| LOI DE FROTTEMENT (LINEAIRE,CHEZY,STRICKLER)
C| MESH           |-->| BLOC DES ENTIERS DU MAILLAGE.
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| PRIVE          |-->| TABLEAU DE TRAVAIL DEFINI DANS PRINCI
C| X,Y            |-->| COORDONNEE DU MAILLAGE .
C| ZF             |-->| COTE DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
C     DECLARATIONS MUST BE ADAPTED TO EVERY CODE
C     THIS EXAMPLE APPLIES TO TELEMAC2D
C
CEX   USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
CEX   INTEGER I
C
C-----------------------------------------------------------------------
C
C     HERE A CONSTANT FRICTION VALUE IS GIVEN
C
CEX   DO I=1,NPOIN
CEX     CHESTR%R(I) = 60.D0
CEX   ENDDO
C
C-----------------------------------------------------------------------
C
C     COMMENTS HERE MAY BE CHANGED
C
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'STRCHE (BIEF) : PAS DE MODIFICATION DU FROTTEMENT'
        WRITE(LU,*)
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'STRCHE (BIEF): NO MODIFICATION OF FRICTION'
        WRITE(LU,*)
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C