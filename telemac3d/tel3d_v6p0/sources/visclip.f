C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       LIMITS VISCOSITY ON TIDAL FLATS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> H, NPLAN, NPOIN2, NPOIN3, NTRAC, VISCTA, VISCVI
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> COR, HLIM, I, IPLAN, ITRAC
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
!>      <td><center> 5.5                                       </center>
!> </td><td> 23/03/2004
!> </td><td> J.-M. HERVOUET 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>H
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NOMBRE DE TRACEURS ACTIFS
!>    </td></tr>
!>          <tr><td>VISCTA
!></td><td><--</td><td>VISCOSITE DYNAMIQUE DES TRACEURS
!>    </td></tr>
!>          <tr><td>VISCVI
!></td><td><--</td><td>VISCOSITE DYNAMIQUE DE LA VITESSE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VISCLIP
     & (VISCVI,VISCTA,H,NPLAN,NPOIN3,NPOIN2,NTRAC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| H             |---| 
C| HN             |-->| HAUTEUR D'EAU
C| NPLAN          |-->| NOMBRE DE PLANS DU MAILLAGE 3D
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| NPOIN3         |-->| NOMBRE DE POINTS DU MAILLAGE 3D
C| NTRAC          |-->| NOMBRE DE TRACEURS ACTIFS
C| VISCTA         |<--| VISCOSITE DYNAMIQUE DES TRACEURS
C| VISCVI         |<--| VISCOSITE DYNAMIQUE DE LA VITESSE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: NPOIN3, NPOIN2, NPLAN
      INTEGER, INTENT(IN)            :: NTRAC
      TYPE (BIEF_OBJ), INTENT(INOUT) :: VISCVI, VISCTA
      TYPE (BIEF_OBJ), INTENT(IN)    :: H
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPLAN,I,ITRAC
      DOUBLE PRECISION HLIM,COR
      DATA HLIM /0.2D0/
!
!***********************************************************************
!
C     LIMITS TURBULENT VISCOSITY ON TIDAL FLATS
!
      DO I=1,NPOIN2
        IF(H%R(I).LT.HLIM) THEN
          COR=(MAX(H%R(I),0.D0)/HLIM)**2
          DO IPLAN=1,NPLAN
            VISCVI%ADR(1)%P%R(I+(IPLAN-1)*NPOIN2)=
     &      VISCVI%ADR(1)%P%R(I+(IPLAN-1)*NPOIN2)*COR
            VISCVI%ADR(2)%P%R(I+(IPLAN-1)*NPOIN2)=
     &      VISCVI%ADR(2)%P%R(I+(IPLAN-1)*NPOIN2)*COR
            VISCVI%ADR(3)%P%R(I+(IPLAN-1)*NPOIN2)=
     &      VISCVI%ADR(3)%P%R(I+(IPLAN-1)*NPOIN2)*COR
          ENDDO
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
!
        DO ITRAC=1,NTRAC
!
          DO I=1,NPOIN2
            IF(H%R(I).LT.HLIM) THEN
              COR=(MAX(H%R(I),0.D0)/HLIM)**2
              DO IPLAN=1,NPLAN
                VISCTA%ADR(ITRAC)%P%ADR(1)%P%R(I+(IPLAN-1)*NPOIN2)=
     &          VISCTA%ADR(ITRAC)%P%ADR(1)%P%R(I+(IPLAN-1)*NPOIN2)*COR
                VISCTA%ADR(ITRAC)%P%ADR(2)%P%R(I+(IPLAN-1)*NPOIN2)=
     &          VISCTA%ADR(ITRAC)%P%ADR(2)%P%R(I+(IPLAN-1)*NPOIN2)*COR
                VISCTA%ADR(ITRAC)%P%ADR(3)%P%R(I+(IPLAN-1)*NPOIN2)=
     &          VISCTA%ADR(ITRAC)%P%ADR(3)%P%R(I+(IPLAN-1)*NPOIN2)*COR
              ENDDO
            ENDIF
          ENDDO
!
        ENDDO
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