C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BYPASSES FLUXES TO POINTS THAT WILL REMAIN WITH
!>                A ZERO VOLUME.
!><br>            FLUX IS CONVEYED TO UPPER LAYER THROUGH A VERTICAL.
!><br>            THIS AVOIDS USELESS ITERATIONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  HERE FLUXES ARE FROM POINT 2 TO POINT 1.
!>            SEE FLUX3D (HORIZONTAL FLUXES BASED ON FLUINT)
!>            AND PRECON (VERTICAL FLUXES BASED ON WSCONV)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FLUX, IKLE, MESH2, MESH3, NELEM2, NELEM3, NPLAN, NPOIN3, SVOLU, SVOLUN, TRA01, VOLU, VOLUN
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> EPS_VOLUME, I, I1, I2, I3, I4, I5, I6, IELEM2, IELEM3, IPLAN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PARCOM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PRECON()

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
!> </td><td> 20/04/2010
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
!></td><td><-></td><td>FLUXES TO BE CHANGED
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH2,3
!></td><td>--></td><td>2D MESH, 3D MESH
!>    </td></tr>
!>          <tr><td>MESH3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF PLANES
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NUMBER OF 3D POINTS
!>    </td></tr>
!>          <tr><td>SVOLU
!></td><td>--></td><td>BIEF_OBJ STRUCTURE, WITH SVOLU%R=VOLU
!>    </td></tr>
!>          <tr><td>SVOLUN
!></td><td>--></td><td>BIEF_OBJ STRUCTURE, WITH SVOLUN%R=VOLUN
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td><-></td><td>WORK BIEF_OBJ STRUCTURE
!>    </td></tr>
!>          <tr><td>VOLU
!></td><td>--></td><td>VOLUME AROUND POINTS AT TIME N+1
!>    </td></tr>
!>          <tr><td>VOLUN
!></td><td>--></td><td>VOLUME AROUND POINTS AT TIME N+1
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE BYPASS_CRUSHED_POINTS_EBE
     &(VOLU,SVOLU,VOLUN,SVOLUN,FLUX,TRA01,MESH2,MESH3,
     & NPOIN3,NELEM2,NELEM3,NPLAN,IKLE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FLUX           |<->| FLUXES TO BE CHANGED
C| IKLE           |---| 
C| MESH2,3        |-->| 2D MESH, 3D MESH
C| MESH3          |---| 
C| NELEM2         |---| 
C| NELEM3         |---| 
C| NPLAN          |-->| NUMBER OF PLANES
C| NPOIN3         |-->| NUMBER OF 3D POINTS
C| SVOLU          |-->| BIEF_OBJ STRUCTURE, WITH SVOLU%R=VOLU
C| SVOLUN         |-->| BIEF_OBJ STRUCTURE, WITH SVOLUN%R=VOLUN
C| TRA01          |<->| WORK BIEF_OBJ STRUCTURE
C| VOLU           |-->| VOLUME AROUND POINTS AT TIME N+1
C| VOLUN          |-->| VOLUME AROUND POINTS AT TIME N+1
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN3,NELEM3,NELEM2,NPLAN
      INTEGER, INTENT(IN)             :: IKLE(NELEM3,6)
!
      DOUBLE PRECISION, INTENT(IN)    :: VOLUN(NPOIN3),VOLU(NPOIN3)
!
      TYPE(BIEF_OBJ), INTENT(IN)      :: SVOLU,SVOLUN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TRA01
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2,MESH3
!
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(30,NELEM3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM2,IELEM3,I,I1,I2,I3,I4,I5,I6,IPLAN
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION EPS_VOLUME
      DATA EPS_VOLUME /1.D-14/
!
!-----------------------------------------------------------------------
!
C     TRA01=VOLU+VOLUN=0 MEANS THAT BOTH VOLU AND VOLUN ARE EQUAL TO 0
!
      CALL OS('X=Y+Z   ',X=TRA01,Y=SVOLU,Z=SVOLUN)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(TRA01,2,MESH3)
      ENDIF
!
C     GROUPS FLUXES
!
      DO IELEM3=1,NELEM3
        DO I=1,15
          FLUX(I,IELEM3)=FLUX(I,IELEM3)-FLUX(I+15,IELEM3)
        ENDDO
      ENDDO
!
C     BYPASSES FLUXES
!
      DO IELEM2=1,NELEM2
        DO IPLAN=1,NPLAN-1
          IELEM3=IELEM2+(IPLAN-1)*NELEM2
          I1=IKLE(IELEM3,1)
          I2=IKLE(IELEM3,2)
          I3=IKLE(IELEM3,3)
!
          IF(TRA01%R(I1).LT.EPS_VOLUME.OR.
     &       TRA01%R(I2).LT.EPS_VOLUME.OR.
     &       TRA01%R(I3).LT.EPS_VOLUME    ) THEN
!
          I4=IKLE(IELEM3,4)
          I5=IKLE(IELEM3,5)
          I6=IKLE(IELEM3,6)
!
C         CROSSED SEGMENTS
!
C         ISSUED FROM 1
          IF(TRA01%R(I1).LT.EPS_VOLUME.OR.
     &       TRA01%R(I5).LT.EPS_VOLUME) THEN
            FLUX(13,IELEM3)=FLUX(13,IELEM3)+FLUX(04,IELEM3)
            FLUX(03,IELEM3)=FLUX(03,IELEM3)+FLUX(04,IELEM3)
            FLUX(04,IELEM3)=0.D0
          ENDIF
          IF(TRA01%R(I1).LT.EPS_VOLUME.OR.
     &       TRA01%R(I6).LT.EPS_VOLUME) THEN
            FLUX(14,IELEM3)=FLUX(14,IELEM3)+FLUX(05,IELEM3)
            FLUX(03,IELEM3)=FLUX(03,IELEM3)+FLUX(05,IELEM3)
            FLUX(05,IELEM3)=0.D0
          ENDIF
C         ISSUED FROM 2
          IF(TRA01%R(I2).LT.EPS_VOLUME.OR.
     &       TRA01%R(I4).LT.EPS_VOLUME) THEN
            FLUX(13,IELEM3)=FLUX(13,IELEM3)-FLUX(07,IELEM3)
            FLUX(08,IELEM3)=FLUX(08,IELEM3)+FLUX(07,IELEM3)
            FLUX(07,IELEM3)=0.D0
          ENDIF
          IF(TRA01%R(I2).LT.EPS_VOLUME.OR.
     &       TRA01%R(I6).LT.EPS_VOLUME) THEN
            FLUX(15,IELEM3)=FLUX(15,IELEM3)+FLUX(09,IELEM3)
            FLUX(08,IELEM3)=FLUX(08,IELEM3)+FLUX(09,IELEM3)
            FLUX(09,IELEM3)=0.D0
          ENDIF
C         ISSUED FROM 3
          IF(TRA01%R(I3).LT.EPS_VOLUME.OR.
     &       TRA01%R(I4).LT.EPS_VOLUME) THEN
            FLUX(14,IELEM3)=FLUX(14,IELEM3)-FLUX(10,IELEM3)
            FLUX(12,IELEM3)=FLUX(12,IELEM3)+FLUX(10,IELEM3)
            FLUX(10,IELEM3)=0.D0
          ENDIF
          IF(TRA01%R(I3).LT.EPS_VOLUME.OR.
     &       TRA01%R(I5).LT.EPS_VOLUME) THEN
            FLUX(15,IELEM3)=FLUX(15,IELEM3)-FLUX(11,IELEM3)
            FLUX(12,IELEM3)=FLUX(12,IELEM3)+FLUX(11,IELEM3)
            FLUX(11,IELEM3)=0.D0
          ENDIF
!
C         LOWER HORIZONTAL SEGMENTS
!
          IF(TRA01%R(I1).LT.EPS_VOLUME) THEN
C           ISSUED FROM 1
            FLUX(13,IELEM3)=FLUX(13,IELEM3)+FLUX(01,IELEM3)
            FLUX(14,IELEM3)=FLUX(14,IELEM3)+FLUX(02,IELEM3)
            FLUX(03,IELEM3)=FLUX(03,IELEM3)+FLUX(01,IELEM3)
     &                                     +FLUX(02,IELEM3)
            FLUX(08,IELEM3)=FLUX(08,IELEM3)-FLUX(01,IELEM3)
            FLUX(12,IELEM3)=FLUX(12,IELEM3)-FLUX(02,IELEM3)
            FLUX(01,IELEM3)=0.D0
            FLUX(02,IELEM3)=0.D0
          ENDIF
          IF(TRA01%R(I2).LT.EPS_VOLUME) THEN
C           ISSUED FROM 2
            FLUX(13,IELEM3)=FLUX(13,IELEM3)+FLUX(01,IELEM3)
            FLUX(15,IELEM3)=FLUX(15,IELEM3)+FLUX(06,IELEM3)
            FLUX(08,IELEM3)=FLUX(08,IELEM3)-FLUX(01,IELEM3)
     &                                     +FLUX(06,IELEM3)
            FLUX(03,IELEM3)=FLUX(03,IELEM3)+FLUX(01,IELEM3)
            FLUX(12,IELEM3)=FLUX(12,IELEM3)-FLUX(06,IELEM3)
            FLUX(01,IELEM3)=0.D0
            FLUX(06,IELEM3)=0.D0
          ENDIF
          IF(TRA01%R(I3).LT.EPS_VOLUME) THEN
C           ISSUED FROM 3
            FLUX(15,IELEM3)=FLUX(15,IELEM3)+FLUX(06,IELEM3)
            FLUX(14,IELEM3)=FLUX(14,IELEM3)+FLUX(02,IELEM3)
            FLUX(12,IELEM3)=FLUX(12,IELEM3)-FLUX(06,IELEM3)
     &                                     -FLUX(02,IELEM3)
            FLUX(03,IELEM3)=FLUX(03,IELEM3)+FLUX(02,IELEM3)
            FLUX(08,IELEM3)=FLUX(08,IELEM3)+FLUX(06,IELEM3)
            FLUX(06,IELEM3)=0.D0
            FLUX(02,IELEM3)=0.D0
          ENDIF
!
C         THESE PROPERTIES SHOULD BE ENSURED AFTER ASSEMBLING
C         SMALL MASS ERROR IF 3 FOLLOWING LINES ARE DELETED (WHY ?)
!
          IF(TRA01%R(I1).LT.EPS_VOLUME) FLUX(03,IELEM3)=0.D0
          IF(TRA01%R(I2).LT.EPS_VOLUME) FLUX(08,IELEM3)=0.D0
          IF(TRA01%R(I3).LT.EPS_VOLUME) FLUX(12,IELEM3)=0.D0
!
C         UPPER HORIZONTAL SEGMENTS CANNOT BE TREATED
C         NO DEGREE OF FREEDOM LEFT, THEY ARE TRANSFERRED
C         TO UPPER LEVEL
!
          IF(IPLAN.NE.NPLAN-1) THEN
            FLUX(01,IELEM3+NELEM2)=FLUX(01,IELEM3+NELEM2)
     &                            +FLUX(13,IELEM3)
            FLUX(02,IELEM3+NELEM2)=FLUX(02,IELEM3+NELEM2)
     &                            +FLUX(14,IELEM3)
            FLUX(06,IELEM3+NELEM2)=FLUX(06,IELEM3+NELEM2)
     &                            +FLUX(15,IELEM3)
            FLUX(13,IELEM3)=0.D0
            FLUX(14,IELEM3)=0.D0
            FLUX(15,IELEM3)=0.D0
          ENDIF
!
        ELSE
C         NO MORE CRUSHED POINTS ABOVE
          EXIT
        ENDIF
!
        ENDDO
      ENDDO
!
C     UNGROUPS FLUXES
!
      DO IELEM3=1,NELEM3
        DO I=1,15
          IF(FLUX(I,IELEM3).GT.0.D0) THEN
            FLUX(I+15,IELEM3)=0.D0
          ELSE
            FLUX(I+15,IELEM3)=-FLUX(I,IELEM3)
            FLUX(I,IELEM3)=0.D0
          ENDIF
        ENDDO
      ENDDO
C
C=======================================================================
C
      RETURN
      END
C
C#######################################################################
C