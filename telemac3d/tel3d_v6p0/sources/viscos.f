C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES VISCOSITIES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DNUTAH, DNUTAV, DNUVIH, DNUVIV, ITURBH, ITURBV, NTRAC, VISCTA, VISCVI
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ITRAC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!> </td><td> 14/12/00
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DNUTAH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DNUTAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DNUVIH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DNUVIV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ITURBH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ITURBV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VISCTA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VISCVI
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VISCOS
     &(VISCVI,VISCTA,DNUTAV,DNUTAH,DNUVIV,DNUVIH,
     & NTRAC,ITURBH,ITURBV)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DNUTAH         |---| 
C| DNUTAV         |---| 
C| DNUVIH         |---| 
C| DNUVIV         |---| 
C| ITURBH         |---| 
C| ITURBV         |---| 
C| NTRAC          |---| 
C| VISCTA         |---| 
C| VISCVI         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: NTRAC
      INTEGER, INTENT(IN)            :: ITURBH,ITURBV
      TYPE (BIEF_OBJ), INTENT(INOUT) :: VISCVI, VISCTA
      DOUBLE PRECISION, INTENT(IN)   :: DNUVIH, DNUVIV
      DOUBLE PRECISION, INTENT(IN)   :: DNUTAH, DNUTAV
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ITRAC
!
!***********************************************************************
!
!
C VISCVI%ADR(1)%P IS THE X HORIZONTAL VISCOSITY
C VISCVI%ADR(2)%P IS THE Y HORIZONTAL VISCOSITY
C VISCVI%ADR(3)%P IS THE Z (VERTICAL) VISCOSITY
!
C FOR THE TRACERS:
!
C VISCTA%ADR(ITRAC)%P%ADR(1)%P IS THE X HORIZONTAL DIFFUSIVITY FOR THE
C    ACTIVE TRACER NUMBER ITRAC, ETC...
!
      IF(ITURBH.EQ.1) THEN
!
        CALL OS( 'X=C     ',X=VISCVI%ADR(1)%P,C=DNUVIH)
        CALL OS( 'X=C     ',X=VISCVI%ADR(2)%P,C=DNUVIH)
!
        IF(NTRAC.NE.0) THEN
!
          DO ITRAC=1,NTRAC
!
            CALL OS('X=C     ',X=VISCTA%ADR(ITRAC)%P%ADR(1)%P,
     &                         C=DNUTAH)
            CALL OS('X=C     ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                         C=DNUTAH)
!
          ENDDO
!
        ENDIF
!
      ENDIF
!
      IF(ITURBV.EQ.1) THEN
!
        CALL OS( 'X=C     ',X=VISCVI%ADR(3)%P,C=DNUVIV)
!
        IF(NTRAC.NE.0) THEN
!
          DO ITRAC=1,NTRAC
!
            CALL OS ('X=C     ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                          C=DNUTAV)
!
          ENDDO
!
        ENDIF
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