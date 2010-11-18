C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES VISCOSITIES
!>                FOR THE SMAGORINSKI MODEL.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, INTERFACE_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DNUTAH, DNUTAV, DNUVIH, DNUVIV, IELM3, ITURBV, MASKEL, MESH3, MSK, NTRAC, SVIDE, TRAV1, TRAV2, TRAV3, TRAV4, TRAV5, TRAV6, U, V, VISCTA, VISCVI, W
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ITRAC
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_VISSMA
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), SMAGO(), SMAGO3D()
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
!>      <td><center> 5.7                                       </center>
!> </td><td> **/02/04
!> </td><td> OLIVER GOETHEL    UNI-HAN
!> </td><td> SMAGORINSKY 3D
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
!>          <tr><td>IELM3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ITURBV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SVIDE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VISCTA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VISCVI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VISSMA
     &(VISCVI,VISCTA,DNUTAH,DNUVIH,DNUVIV,DNUTAV,
     & U,V,W,TRAV1,TRAV2,TRAV3,TRAV4,TRAV5,TRAV6,
     & SVIDE,MESH3,IELM3,NTRAC,MSK,MASKEL,ITURBV)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DNUTAH         |---| 
C| DNUTAV         |---| 
C| DNUVIH         |---| 
C| DNUVIV         |---| 
C| IELM3          |---| 
C| ITURBV         |---| 
C| MASKEL         |---| 
C| MESH3          |---| 
C| MSK            |---| 
C| NTRAC          |---| 
C| SVIDE          |---| 
C| TRAV1          |---| 
C| TRAV2          |---| 
C| TRAV3          |---| 
C| TRAV4          |---| 
C| TRAV5          |---| 
C| TRAV6          |---| 
C| U             |---| 
C| V             |---| 
C| VISCTA         |---| 
C| VISCVI         |---| 
C| W             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_VISSMA => VISSMA
      USE DECLARATIONS_TELEMAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: NTRAC,ITURBV
      INTEGER, INTENT(IN)            :: IELM3
      LOGICAL, INTENT(IN)            :: MSK
      TYPE (BIEF_OBJ), INTENT(IN)    :: U, V, W
      TYPE (BIEF_OBJ), INTENT(INOUT) :: VISCVI, VISCTA
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TRAV1, TRAV2, TRAV3, TRAV4
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TRAV5      !  NUSMAG
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TRAV6
      TYPE (BIEF_OBJ), INTENT(IN)    :: MASKEL
      TYPE (BIEF_OBJ), INTENT(INOUT) :: SVIDE
      TYPE (BIEF_MESH)               :: MESH3
      DOUBLE PRECISION, INTENT(IN)   :: DNUVIH,DNUTAH,DNUVIV,DNUTAV
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ITRAC
!
!***********************************************************************
!
C INITIALISES VISCOSITIES
!
C VISCVI%ADR(1)%P IS THE X HORIZONTAL VISCOSITY
C VISCVI%ADR(2)%P IS THE Y HORIZONTAL VISCOSITY
C VISCVI%ADR(3)%P IS THE Z (VERTICAL) VISCOSITY
!
C FOR THE TRACERS:
!
C VISCTA%ADR(ITRAC)%P%ADR(1)%P IS THE X HORIZONTAL DIFFUSIVITY FOR THE
C     TRACER NUMBER ITRAC, ETC...
!
!***********************************************************************
!
      IF(ITURBV.NE.4) THEN
!
      CALL SMAGO(U,V,TRAV1,TRAV2,TRAV3,TRAV4,TRAV5,MESH3,IELM3,
     &           MSK,MASKEL)
!
C VISCOSITY COMPUTED BY SMAGORINSKI : IN TRAV5
!
      CALL OS('X=Y+C   ',X=VISCVI%ADR(1)%P,Y=TRAV5,C=DNUVIH)
      CALL OS('X=Y+C   ',X=VISCVI%ADR(2)%P,Y=TRAV5,C=DNUVIH)
!
      IF (NTRAC.NE.0) THEN
!
        DO ITRAC=1,NTRAC
          CALL OS('X=Y+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(1)%P,
     &                       Y=TRAV5,C=DNUTAH)
          CALL OS('X=Y+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                       Y=TRAV5,C=DNUTAH)
        ENDDO
!
      ENDIF
!
      ELSE !ITURBV=4 -> 3D SMAGORINSKI
!
      CALL SMAGO3D(U,V,W,TRAV1,TRAV2,TRAV3,TRAV4,TRAV5,TRAV6,
     &             SVIDE,MESH3,IELM3,MSK,MASKEL)
!
      CALL OS('X=Y+C   ',X=VISCVI%ADR(1)%P,Y=TRAV5,C=DNUVIH)
      CALL OS('X=Y+C   ',X=VISCVI%ADR(2)%P,Y=TRAV5,C=DNUVIH)
      CALL OS('X=Y+C   ',X=VISCVI%ADR(3)%P,Y=TRAV5,C=DNUVIV)
!
      IF(NTRAC.NE.0) THEN
!
        DO ITRAC=1,NTRAC
          CALL OS ('X=Y+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(1)%P,
     &                        Y=TRAV5,C=DNUTAH)
          CALL OS ('X=Y+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(2)%P,
     &                        Y=TRAV5,C=DNUTAH)
          CALL OS ('X=Y+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                        Y=TRAV5,C=DNUTAV)
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