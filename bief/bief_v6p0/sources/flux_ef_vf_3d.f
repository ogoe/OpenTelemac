C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SUMS UP 3D NON-ASSEMBLED FLUXES ON THE VERTICAL
!>                THEN COMPUTES THE 2D SEGMENT FLUXES.
!><br>           (LEO POSTMA'S METHOD).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FLOW, INIFLO, IOPT, MESH2D, MESH3D, NELEM2, NELEM3, NSEG2D, NSEG3D, SENS, W2D, W3D
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, IPLAN, ISEG, N1, N2, NPLAN
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FLUX_EF_VF_3D
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FLUX_EF_VF(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CORRECTION_DEPTH_3D(), FLUX3D(), TEL4DEL()

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
!> </td><td> 15/05/2009
!> </td><td>
!> </td><td> INSPIRED FROM LEO POSTMA (DELTARES)
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>FLOW
!></td><td><--</td><td>FLUX
!>    </td></tr>
!>          <tr><td>INIFLO
!></td><td>--></td><td>IF(YES) FLOW WILL BE INITIALISED AT 0.
!>    </td></tr>
!>          <tr><td>IOPT
!></td><td>--></td><td>CHOICE OF THE CONSTANT IN FLUX_EF_VF
!>    </td></tr>
!>          <tr><td>MESH2D
!></td><td>--></td><td>2D MESH
!>    </td></tr>
!>          <tr><td>MESH3D
!></td><td>--></td><td>3D MESH
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NUMBER OF ELEMENTS IN 2D
!>    </td></tr>
!>          <tr><td>NELEM3
!></td><td>--></td><td>NUMBER OF ELEMENTS IN 3D
!>    </td></tr>
!>          <tr><td>NSEG2D
!></td><td>--></td><td>NUMBER OF SEGMENTS IN 2D
!>    </td></tr>
!>          <tr><td>NSEG3D
!></td><td>--></td><td>NUMBER OF SEGMENTS IN 3D
!>    </td></tr>
!>          <tr><td>SENS
!></td><td>--></td><td>IF 1: HORIZONTAL FLUXES FROM BOTTOM TO TOP
!>                  IF 2: HORIZONTAL FLUXES FROM TOP TO BOTTOM
!>    </td></tr>
!>          <tr><td>W2D
!></td><td><--</td><td>NON ASSEMBLED FLUXES LEAVING POINTS,PER TRIANGLE
!>    </td></tr>
!>          <tr><td>W3D
!></td><td>--></td><td>NON ASSEMBLED FLUXES LEAVING POINTS,PER PRISM
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLUX_EF_VF_3D
     &(FLOW,W2D,W3D,NSEG2D,NSEG3D,NELEM2,NELEM3,MESH2D,MESH3D,INIFLO,
     & IOPT,SENS)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FLOW           |<--| FLUX
C| INIFLO         |-->| IF(YES) FLOW WILL BE INITIALISED AT 0.
C| IOPT           |-->| CHOICE OF THE CONSTANT IN FLUX_EF_VF
C| MESH2D         |-->| 2D MESH
C| MESH3D         |-->| 3D MESH
C| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
C| NELEM3         |-->| NUMBER OF ELEMENTS IN 3D
C| NSEG2D         |-->| NUMBER OF SEGMENTS IN 2D
C| NSEG3D         |-->| NUMBER OF SEGMENTS IN 3D
C| SENS           |-->| IF 1: HORIZONTAL FLUXES FROM BOTTOM TO TOP
C|                |   | IF 2: HORIZONTAL FLUXES FROM TOP TO BOTTOM
C| W2D            |<--| NON ASSEMBLED FLUXES LEAVING POINTS,PER TRIANGLE
C| W3D            |-->| NON ASSEMBLED FLUXES LEAVING POINTS,PER PRISM
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_FLUX_EF_VF_3D => FLUX_EF_VF_3D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      INTEGER, INTENT(IN)             :: NSEG2D,NSEG3D,NELEM2,NELEM3
C                                             *=NSEG2D*NPLAN+NPOIN2*NETAGE
      INTEGER, INTENT(IN)             :: IOPT,SENS
      DOUBLE PRECISION, INTENT(INOUT) :: FLOW(NSEG3D)
      DOUBLE PRECISION, INTENT(IN)    :: W3D(NELEM3,6)
      DOUBLE PRECISION, INTENT(INOUT) :: W2D(NELEM2,3)
      LOGICAL, INTENT(IN)             :: INIFLO
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2D,MESH3D
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      INTEGER IPLAN,NPLAN,N1,N2,IELEM,ISEG
      NPLAN=NELEM3/NELEM2 + 1
C
C-----------------------------------------------------------------------
C
C     INITIALISES FLOW TO 0.D0
C
      IF(INIFLO) THEN
        DO ISEG = 1,NSEG2D*NPLAN
          FLOW(ISEG) = 0.D0
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(SENS.NE.1.AND.SENS.NE.2) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'FLUX_EF_VF_3D : SENS INCONNU = ',SENS
        IF(LNG.EQ.2) WRITE(LU,*) 'FLUX_EF_VF_3D: UNKNOWN SENS = ',SENS
        CALL PLANTE(1)
        STOP
      ENDIF
C
      IF(SENS.EQ.2.AND..NOT.INIFLO) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'FLUX_EF_VF_3D : SENS = 2 ET INIFLO = .FALSE.'
          WRITE(LU,*) '                OPTIONS INCOMPATIBLES'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'FLUX_EF_VF_3D: SENS = 2 AND INIFLO = .FALSE.'
          WRITE(LU,*) '               INCOMPATIBLE OPTIONS'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C     ADDS FLUXES ON HORIZONTAL SEGMENTS (FOR SEGMENT NUMBERING IN 3D
C                                         SEE STOSEG41 IN BIEF)
C
      DO IPLAN=1,NPLAN
C
C       POINTS 1, 2 AND 3 OF UPPER LEVEL
        IF(IPLAN.EQ.1) THEN
C         FIRST PLANE: ONLY POINTS 1, 2 AND 3 OF UPPER LEVEL
          DO IELEM=1,NELEM2
            W2D(IELEM,1)=W3D(IELEM,1)
            W2D(IELEM,2)=W3D(IELEM,2)
            W2D(IELEM,3)=W3D(IELEM,3)
          ENDDO
        ELSEIF(IPLAN.EQ.NPLAN) THEN
C         LAST PLANE: ONLY POINTS 4, 5 AND 6 OF LOWER LEVEL
          N1=NELEM2*(IPLAN-2)
          DO IELEM=1,NELEM2
            W2D(IELEM,1)=W3D(N1+IELEM,4)
            W2D(IELEM,2)=W3D(N1+IELEM,5)
            W2D(IELEM,3)=W3D(N1+IELEM,6)
          ENDDO
        ELSE
C       INTERMEDIATE PLANE
C         POINTS 4, 5 AND 6 OF LOWER LEVEL + 1, 2, 3 OF UPPER LEVEL
          N1=NELEM2*(IPLAN-2)
          N2=N1+NELEM2
          DO IELEM=1,NELEM2
            W2D(IELEM,1)=W3D(N1+IELEM,4)+W3D(N2+IELEM,1)
            W2D(IELEM,2)=W3D(N1+IELEM,5)+W3D(N2+IELEM,2)
            W2D(IELEM,3)=W3D(N1+IELEM,6)+W3D(N2+IELEM,3)
          ENDDO
        ENDIF
        IF(SENS.EQ.1) THEN
          N1=(IPLAN-1    )*NSEG2D+1
          N2= IPLAN         *NSEG2D
        ELSE
          N1=(NPLAN-IPLAN)*NSEG2D+1
          N2=(1+NPLAN-IPLAN)*NSEG2D
        ENDIF
        CALL FLUX_EF_VF(FLOW(N1:N2),W2D,NSEG2D,NELEM2,
     &                  MESH2D%ELTSEG%I,MESH2D%ORISEG%I,
     &                  MESH2D%IKLE%I,.FALSE.,IOPT)
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C