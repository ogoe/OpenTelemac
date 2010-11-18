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
!>    </th><td> AK, CMU, DNUTAH, DNUTAV, DNUVIH, DNUVIV, EMIN, EP, ITURBH, ITURBV, KMIN, NTRAC, PRANDTL, VISCTA, VISCVI
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, ITRAC, NPOIN3
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
!>      <td><center> 6.0                                       </center>
!> </td><td>
!> </td><td>
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
!>          <tr><td>EMIN
!></td><td>--></td><td>MINIMUM VALUE OF EPSILON FOR CLIPPING
!>    </td></tr>
!>          <tr><td>EP
!></td><td>--></td><td>DISSIPATION TURBULENTE
!>    </td></tr>
!>          <tr><td>ITURBH
!></td><td>--></td><td>HORIZONTAL TURBULENCE MODEL (3= K-EPSILON)
!>    </td></tr>
!>          <tr><td>ITURBV
!></td><td>--></td><td>VERTICAL TURBULENCE MODEL (3= K-EPSILON)
!>    </td></tr>
!>          <tr><td>KMIN
!></td><td>--></td><td>MINIMUM VALUE OF K FOR CLIPPING
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NUMBER OF TRACERS
!>    </td></tr>
!>          <tr><td>PRANDTL
!></td><td>--></td><td>PRANDTL CONSTANT
!>    </td></tr>
!>          <tr><td>VISCTA
!></td><td><--</td><td>DIFFUSION TURBULENTE POUR LES TRACEURS
!>    </td></tr>
!>          <tr><td>VISCVI
!></td><td><--</td><td>DIFFUSION TURBULENTE POUR LES VITESSES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VISCKE
     &(VISCVI,VISCTA,AK,EP,NTRAC,CMU,
     & DNUVIH,DNUVIV,DNUTAH,DNUTAV,KMIN,EMIN,ITURBH,ITURBV,PRANDTL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AK             |-->| ENERGIE TURBULENTE
C| CMU            |-->| CONSTANTE DU MODELE K-EPSILON
C| DNUTAH         |-->| COEFFICIENT DE DIFFUSION HORIZONTALE POUR TA
C| DNUTAV         |-->| COEFFICIENT DE DIFFUSION VERTICALE POUR TA
C| DNUVIH         |-->| COEFFICIENT DE DIFFUSION HORIZONTALE POUR U
C| DNUVIV         |-->| COEFFICIENT DE DIFFUSION VERTICALE POUR U
C| EMIN           |-->| MINIMUM VALUE OF EPSILON FOR CLIPPING
C| EP             |-->| DISSIPATION TURBULENTE
C| ITURBH         |-->| HORIZONTAL TURBULENCE MODEL (3= K-EPSILON)
C| ITURBV         |-->| VERTICAL TURBULENCE MODEL (3= K-EPSILON)
C| KMIN           |-->| MINIMUM VALUE OF K FOR CLIPPING
C| NTRAC          |-->| NUMBER OF TRACERS
C| PRANDTL        |-->| PRANDTL CONSTANT
C| VISCTA         |<--| DIFFUSION TURBULENTE POUR LES TRACEURS
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
      INTEGER, INTENT(IN)          :: NTRAC,ITURBH,ITURBV
      DOUBLE PRECISION, INTENT(IN) :: CMU,PRANDTL
      DOUBLE PRECISION, INTENT(IN) :: DNUVIH, DNUVIV
      DOUBLE PRECISION, INTENT(IN) :: DNUTAH, DNUTAV,KMIN,EMIN
      TYPE(BIEF_OBJ), INTENT(INOUT):: VISCVI, VISCTA
      TYPE(BIEF_OBJ), INTENT(IN)   :: AK,EP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ITRAC,I,NPOIN3
!
!***********************************************************************
!
      NPOIN3 = AK%DIM1
!
      DO I=1,NPOIN3
!
        IF(EP%R(I).GT.1.1D0*EMIN) THEN
          VISCVI%ADR(3)%P%R(I)=CMU*AK%R(I)**2/EP%R(I)
        ELSE
C         IF EPSILON IS NEAR TO CLIP VALUE, NO TURBULENCE, WHATEVER K
          VISCVI%ADR(3)%P%R(I)=0.D0
        ENDIF
!
      ENDDO
!
C     HORIZONTAL DIFFUSION OF VELOCITIES
!
      IF(ITURBH.EQ.3) THEN
        CALL OS('X=Y+C   ',X=VISCVI%ADR(1)%P,Y=VISCVI%ADR(3)%P,C=DNUVIH)
        CALL OS('X=Y     ',X=VISCVI%ADR(2)%P,Y=VISCVI%ADR(1)%P)
      ENDIF
!
!-----------------------------------------------------------------------
!
C     TRACERS
!
      IF(NTRAC.GT.0) THEN
        IF(ABS(PRANDTL-1.D0).LT.1.D-4) THEN
C         HERE PRANDTL TURBULENT = 1.0
          DO ITRAC = 1,NTRAC
            CALL OS('X=Y+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                         Y=VISCVI%ADR(3)%P,C=DNUTAV)
          ENDDO
        ELSE
          DO ITRAC = 1,NTRAC
            DO I=1,NPOIN3
              VISCTA%ADR(ITRAC)%P%ADR(3)%P%R(I)=
     &        VISCVI%ADR(3)%P%R(I)/PRANDTL + DNUTAV
            ENDDO
          ENDDO
        ENDIF
        IF(ITURBH.EQ.3) THEN
          IF(ABS(PRANDTL-1.D0).LT.1.D-4) THEN
C           HERE PRANDTL TURBULENT = 1.0
            DO ITRAC = 1,NTRAC
              CALL OS('X=Y+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(1)%P,
     &                           Y=VISCVI%ADR(3)%P,C=DNUTAH)
              CALL OS('X=Y     ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                           Y=VISCTA%ADR(ITRAC)%P%ADR(1)%P)
            ENDDO
          ELSE
            DO ITRAC = 1,NTRAC
              DO I=1,NPOIN3
                VISCTA%ADR(ITRAC)%P%ADR(1)%P%R(I)=
     &          VISCVI%ADR(3)%P%R(I)/PRANDTL + DNUTAH
              ENDDO
              CALL OS('X=Y     ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                           Y=VISCTA%ADR(ITRAC)%P%ADR(1)%P)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
C     FINAL VALUE OF VERTICAL DIFFUSION FOR VELOCITIES
!
      CALL OS('X=X+C   ',X=VISCVI%ADR(3)%P,C=DNUVIV)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C