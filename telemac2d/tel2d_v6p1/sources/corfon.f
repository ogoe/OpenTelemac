C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODIFIES THE BOTTOM TOPOGRAPHY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::AM1 AM1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LISFON LISFON@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MESH MESH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MSK MSK@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T1 T1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2 T2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ZF ZF@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> MAS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FILTER()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS(), TELEMAC2D(), TELEMAC3D(), WAC()

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
!> </td><td> 01/03/1990
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A
!></td><td><--</td><td>MATRICE
!>    </td></tr>
!>          <tr><td>LISFON
!></td><td>--></td><td>NOMBRE DE LISSAGES DU FOND.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>TABLEAU PRIVE POUR L'UTILISATEUR.
!>    </td></tr>
!>          <tr><td>T1,2
!></td><td>--></td><td>TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
!>    </td></tr>
!>          <tr><td>W1
!></td><td>--></td><td>TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
!>    </td></tr>
!>          <tr><td>X,Y,(Z)
!></td><td>--></td><td>COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
!>    </td></tr>
!>          <tr><td>ZF
!></td><td><-></td><td>FOND A MODIFIER.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CORFON
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |<--| MATRICE
C| LISFON         |-->| NOMBRE DE LISSAGES DU FOND.
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| PRIVE          |-->| TABLEAU PRIVE POUR L'UTILISATEUR.
C| T1,2           |-->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
C| W1             |-->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
C| X,Y,(Z)        |-->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
C| ZF             |<->| FOND A MODIFIER.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL MAS
C
C-----------------------------------------------------------------------
C
C  SMOOTHING(S) OF THE BOTTOM (OPTIONAL)
C
      IF(LISFON.GT.0) THEN
C
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.1) THEN
        IF(LISFON.EQ.0) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D) : PAS DE MODIFICATION DU FOND'
          WRITE(LU,*)
        ELSE
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D) : ',LISFON,' LISSAGES DU FOND'
          WRITE(LU,*)
        ENDIF
      ENDIF
      IF(LNG.EQ.2) THEN
        IF(LISFON.EQ.0) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D): NO MODIFICATION OF BOTTOM'
          WRITE(LU,*)
        ELSE
          WRITE(LU,*)
          WRITE(LU,*) 'CORFON (TELEMAC2D): ',LISFON,' BOTTOM SMOOTHINGS'
          WRITE(LU,*)
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C