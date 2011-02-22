C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       EXTRUDES THE 2D MASK ON THE VERTICAL FOR LATERAL
!>                BOUNDARIES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> MASK, MASKBR, NETAGE, NPTFR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IETAGE, K
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FLUX3D(), PREDIV(), WAVE_EQUATION()

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
!> </td><td> 23/12/2003
!> </td><td> J.M. HERVOUET  (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>MASK
!></td><td>--></td><td>MASQUE 2D
!>    </td></tr>
!>          <tr><td>MASKBR
!></td><td><--</td><td>MASQUE 3D SUR LES BORDS LATERAUX
!>    </td></tr>
!>          <tr><td>NETAGE
!></td><td>--></td><td>NOMBRE D'ETAGES
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS DE BORD EN 2D
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE EXTMSK
     &(MASKBR,MASK,NPTFR,NETAGE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| MASK           |-->| MASQUE 2D
C| MASKBR         |<--| MASQUE 3D SUR LES BORDS LATERAUX
C| NETAGE         |-->| NOMBRE D'ETAGES
C| NPTFR          |-->| NOMBRE DE POINTS DE BORD EN 2D
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)           :: NPTFR,NETAGE
      DOUBLE PRECISION, INTENT(IN)  :: MASK(*)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASKBR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,IETAGE
!
!=======================================================================
!
!=======================================================================
!
      IF(MASKBR%ELM.EQ.70) THEN
!
C         QUADRILATERAL ON THE LATERAL BOUNDARIES
!
          DO K = 1,NPTFR
            DO IETAGE = 1,NETAGE
              MASKBR%R((IETAGE-1)*NPTFR+K)=MASK(K)
            ENDDO
          ENDDO
!
      ELSEIF(MASKBR%ELM.EQ.60) THEN
!
C         TRIANGLES ON THE LATERAL BOUNDARIES
!
          DO K = 1,NPTFR
            DO IETAGE = 1,NETAGE
              MASKBR%R((IETAGE-1)*2*NPTFR+K      )=MASK(K)
              MASKBR%R((IETAGE-1)*2*NPTFR+K+NPTFR)=MASK(K)
            ENDDO
          ENDDO
!
      ELSE
!
        WRITE(LU,*) 'UNKNOWN ELEMENT FOR MASKBR IN EXTMSK'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C