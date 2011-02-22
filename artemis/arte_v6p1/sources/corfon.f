C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODIFIES THE BOTTOM TOPOGRAPHY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!>  @code
!>  EXAMPLE :
!>
!>      DO 10 I = 1,NPOIN
!>        ZF%R(I) = -1.D0 -0.02D0*Y(I)
!>        IF (Y(I).GE.700.D0) THEN
!>           ZF%R(I) = -15.D0
!>        ENDIF
!>10    CONTINUE
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_ARTEMIS
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_ARTEMIS :<br>
!> @link DECLARATIONS_ARTEMIS::AM1 AM1@endlink, 
!> @link DECLARATIONS_ARTEMIS::LISFON LISFON@endlink, 
!> @link DECLARATIONS_ARTEMIS::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_ARTEMIS::MESH MESH@endlink, 
!> @link DECLARATIONS_ARTEMIS::MSK MSK@endlink, 
!> @link DECLARATIONS_ARTEMIS::T1 T1@endlink, 
!> @link DECLARATIONS_ARTEMIS::T2 T2@endlink, 
!> @link DECLARATIONS_ARTEMIS::ZF ZF@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, MAS
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
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 01/03/1990                                              </td>
!>    <td> J-M HERVOUET                                            </td>
!>    <td>                                                         </td>
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
!></td><td>---</td><td>FOND A MODIFIER.
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
C| ZF             |---| FOND A MODIFIER.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_ARTEMIS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER I
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
C  EXAMPLE :
C
C      DO 10 I = 1,NPOIN
C        ZF%R(I) = -1.D0 -0.02D0*Y(I)
C        IF (Y(I).GE.700.D0) THEN
C           ZF%R(I) = -15.D0
C        ENDIF
C10    CONTINUE
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C