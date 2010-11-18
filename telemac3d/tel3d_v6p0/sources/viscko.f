C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE TURBULENT VISCOSITY
!>                AND TURBULENT THERMAL DIFFUSIVITY
!>                ACCORDING TO K AND EPSILON.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AK, CMU, DNUTAH, DNUTAV, DNUVIH, DNUVIV, EP, NTRAC, ROTAT, VISCTA, VISCVI
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
!>      <td><center>                                           </center>
!> </td><td> **/02/01
!> </td><td> HOLGER WEILBEER   ISEB/UHA
!> </td><td> K-OMEGA
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 5.3                                       </center>
!> </td><td> 25/11/97
!> </td><td> HOLGER WEILBEER   ISEB/UHA
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AK
!></td><td>--></td><td>ENERGIE TURBULENTE
!>    </td></tr>
!>          <tr><td>CMU
!></td><td>--></td><td>CONSTANTE DU MODELE K-EPSILON
!>    </td></tr>
!>          <tr><td>DNUTAH
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION HORIZONTALE POUR TA
!>    </td></tr>
!>          <tr><td>DNUTAV
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION VERTICALE POUR TA
!>    </td></tr>
!>          <tr><td>DNUVIH
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION HORIZONTALE POUR U
!>    </td></tr>
!>          <tr><td>DNUVIV
!></td><td>--></td><td>COEFFICIENT DE DIFFUSION VERTICALE POUR U
!>    </td></tr>
!>          <tr><td>EP
!></td><td>--></td><td>DISSIPATION TURBULENTE
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NOMBRE DE TRACEURS
!>    </td></tr>
!>          <tr><td>ROTAT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VISCTA
!></td><td><--</td><td>DIFFUSION TURBULENTE POUR LES TRACEURS ACTIFS
!>    </td></tr>
!>          <tr><td>VISCVI
!></td><td><--</td><td>DIFFUSION TURBULENTE POUR LES VITESSES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VISCKO
     &(VISCVI,VISCTA,ROTAT,AK,EP,NTRAC,CMU,DNUVIH,DNUVIV,DNUTAH,DNUTAV)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AK             |-->| ENERGIE TURBULENTE
C| CMU            |-->| CONSTANTE DU MODELE K-EPSILON
C| DNUTAH         |-->| COEFFICIENT DE DIFFUSION HORIZONTALE POUR TA
C| DNUTAV         |-->| COEFFICIENT DE DIFFUSION VERTICALE POUR TA
C| DNUVIH         |-->| COEFFICIENT DE DIFFUSION HORIZONTALE POUR U
C| DNUVIV         |-->| COEFFICIENT DE DIFFUSION VERTICALE POUR U
C| EP             |-->| DISSIPATION TURBULENTE
C| NPOIN3         |-->| NOMBRE DE POINTS DU MAILLAGE 3D
C| NTRAC          |-->| NOMBRE DE TRACEURS
C| ROTAT          |---| 
C| VISCTA         |<--| DIFFUSION TURBULENTE POUR LES TRACEURS ACTIFS
C| VISCVI         |<--| DIFFUSION TURBULENTE POUR LES VITESSES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)          :: NTRAC
      DOUBLE PRECISION, INTENT(IN) :: CMU
      DOUBLE PRECISION, INTENT(IN) :: DNUVIH, DNUVIV
      DOUBLE PRECISION, INTENT(IN) :: DNUTAH, DNUTAV
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: VISCVI, VISCTA
      TYPE(BIEF_OBJ), INTENT(IN)   :: ROTAT, AK, EP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ITRAC
!
!***********************************************************************
!
C VISCVI%ADR(1) AND VISCVI%ADR(2) ARE USED AS TEMPORARY WORK FIELDS
!
      CALL OS('X=CY    ',X=VISCVI%ADR(1)%P,Y=AK,C=0.3D0)
      CALL OS('X=CY    ',X=VISCVI%ADR(2)%P,Y=EP,C=0.3D0)
      CALL OS('X=+(Y,Z)',X=VISCVI%ADR(2)%P,Y=VISCVI%ADR(2)%P,Z=ROTAT)
!
      CALL OS('X=CY/Z  ',X=VISCVI%ADR(3)%P,
     &                   Y=VISCVI%ADR(1)%P,Z=VISCVI%ADR(2)%P,C=1.D0)
!
!-----------------------------------------------------------------------
!
      CALL OS('X=Y+C   ',X=VISCVI%ADR(1)%P,Y=VISCVI%ADR(3)%P,C=DNUVIH)
      CALL OS('X=Y     ',X=VISCVI%ADR(2)%P,Y=VISCVI%ADR(1)%P)
      CALL OS('X=X+C   ',X=VISCVI%ADR(3)%P,C=DNUVIV)
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC = 1,NTRAC
!
C OLIVIER: TURBULENT PRANDTL CLOSURE = 1.0
!
          CALL OS('X=CY    ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                       Y=VISCVI%ADR(3)%P,C=1.D0)
          CALL OS('X=Y+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(1)%P,
     &                       Y=VISCTA%ADR(ITRAC)%P%ADR(3)%P,C=DNUTAH)
          CALL OS('X=Y     ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                       Y=VISCTA%ADR(ITRAC)%P%ADR(1)%P)
          CALL OS('X=X+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,C=DNUTAV)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE VISCKO
C
C#######################################################################
C