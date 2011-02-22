C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SHARES ASSEMBLED FLUXES BETWEEN SUB-DOMAINS.
!><br>            THIS IS IN SOME SENSE THE CONTRARY OF PARCOM2_SEG, BUT
!>                THE FLUXES ON THE HORIZONTAL SEGMENTS WILL BE DIVIDED
!>                BY XMUL*2 AND THE FLUXES OF VERTICAL INTERFACE SEGMENTS
!>                WILL BE DIVIDED BY XMUL*MESH%FAC%R.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  MUST NOT BE CALLED WHEN NCSIZE = 0

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FLUX, MESH2, MESH3, NPLAN, OPT, XMUL
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NBMAXNSHARE NBMAXNSHARE@endlink, 
!> @link BIEF_DEF::NPTIR NPTIR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, I2D, I3D, IAD, IPLAN, NPOIN2, NSEG, NSEGH, NSEGV
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> MULT_INTERFACE_SEG()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MURD3D_POS()

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
!> </td><td> 14/04/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>FLUX
!></td><td><-></td><td>FLUXES TO BE SHARED
!>    </td></tr>
!>          <tr><td>MESH2
!></td><td>--></td><td>2D MESH
!>    </td></tr>
!>          <tr><td>MESH3
!></td><td>--></td><td>3D MESH
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF PLANES
!>    </td></tr>
!>          <tr><td>OPT
!></td><td>--></td><td>1 : HORIZONTAL AND VERTICAL SEGMENTS ONLY
!>                  2 : ALL SEGMENTS
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>MULTIPLICATING FACTOR
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SHARE_3D_FLUXES
     &(FLUX,XMUL,NPLAN,MESH2,MESH3,OPT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FLUX           |<->| FLUXES TO BE SHARED
C| MESH2          |-->| 2D MESH
C| MESH3          |-->| 3D MESH
C| NPLAN          |-->| NUMBER OF PLANES
C| OPT            |-->| 1 : HORIZONTAL AND VERTICAL SEGMENTS ONLY
C|                |   | 2 : ALL SEGMENTS
C| XMUL           |-->| MULTIPLICATING FACTOR
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
      INTEGER, INTENT(IN) :: NPLAN,OPT
C
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH2,MESH3
C
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(*)
      DOUBLE PRECISION, INTENT(INOUT) :: XMUL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPLAN,NSEG,NSEGH,NSEGV,NPOIN2,I,I3D,I2D,IAD
C
C-----------------------------------------------------------------------
C
      NSEG=MESH2%NSEG
      NPOIN2=MESH2%NPOIN
      NSEGH=NSEG*NPLAN
      NSEGV=NPOIN2*(NPLAN-1)
C
C     HORIZONTAL FLUXES
C
      DO IPLAN=1,NPLAN
        CALL MULT_INTERFACE_SEG(FLUX(1+(IPLAN-1)*NSEG:IPLAN*NSEG),
     &                          MESH2%NH_COM_SEG%I,
     &                          MESH2%NH_COM_SEG%DIM1,
     &                          MESH2%NB_NEIGHB_SEG,
     &                          MESH2%NB_NEIGHB_PT_SEG%I,
     &                          0.5D0*XMUL,NSEG)
      ENDDO
C
C     VERTICAL FLUXES (SAME NUMBERING AS POINTS, SO FAC%R(I))
C
      IAD=1
      DO I=1,NPTIR
C       I2D=NACHB(1,I) WITH NACHB OF SIZE NACHB(NBMAXNSHARE,NPTIR)
C       IAD IS (I-1)*NBMAXNSHARE+1
        I2D=MESH2%NACHB%I(IAD)
        DO IPLAN=1,NPLAN-1
          I3D=(IPLAN-1)*NPOIN2+I2D
          FLUX(NSEGH+I3D)=FLUX(NSEGH+I3D)*MESH3%FAC%R(I3D)*XMUL
        ENDDO
        IAD=IAD+NBMAXNSHARE
      ENDDO
C
C     ALTERNATIVE: SIMPLER BUT LESS EFFICIENT FOR VERTICAL FLUXES:
C
C     DO I3D=1,NSEGV
C       FLUX(NSEGH+I3D)=FLUX(NSEGH+I3D)*MESH3%FAC%R(I3D)*XMUL
C     ENDDO
C
C     CROSSED FLUXES (SEE STOSEG41 FOR STORAGE). THERE ARE 2*NESG
C     PER LAYER AND NPLAN-1 LAYER. HERE ORISEG=1 AND ORISEG=2 SEGMENTS
C     ARE MULTIPLIED BY THE SAME NUMBER, SO GIVEN HOW THE NUMBERING
C     IS BUILT IT IS AS IF WE HAVE 2*NPLAN-2 LAYERS OF HORIZONTAL SEGMENTS
C
      IF(OPT.EQ.2) THEN
        DO IPLAN=1,2*(NPLAN-1)
          CALL MULT_INTERFACE_SEG(FLUX(1+NSEGH+NSEGV+(IPLAN-1)*NSEG:
     &                                   NSEGH+NSEGV+ IPLAN   *NSEG),
     &                            MESH2%NH_COM_SEG%I,
     &                            MESH2%NH_COM_SEG%DIM1,
     &                            MESH2%NB_NEIGHB_SEG,
     &                            MESH2%NB_NEIGHB_PT_SEG%I,
     &                            0.5D0*XMUL,NSEG)
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C