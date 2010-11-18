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

!>  @warning  HERE EDGE-BASED STORAGE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DIMGLO, FLUX, GLOSEG, MESH2, MESH3, NPLAN, NPOIN2, NPOIN3, NSEG, SCHCF, SVOLU, SVOLUN, TRA01, VOLU, VOLUN
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::ADV_LPO_TF ADV_LPO_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC_TF ADV_NSC_TF@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> EPS_VOLUME, I1, I2, I2D, I3, I3D, I4, ICR1, ICR2, IHOR, IPLAN, ISEG2D, ISEG3D, NSEGH, NSEGV
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PARCOM(), PLANTE()
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
!>          <tr><td>DIMGLO
!></td><td>--></td><td>FIRST DIMENSION OF GLOSEG
!>    </td></tr>
!>          <tr><td>FLUX
!></td><td><-></td><td>FLUXES TO BE CHANGED
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td>--></td><td>3D LIST OF SEGMENTS POINTS
!>    </td></tr>
!>          <tr><td>MESH2,3
!></td><td>--></td><td>2D MESH, 3D MESH
!>    </td></tr>
!>          <tr><td>MESH3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF PLANES
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF POINTS IN 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NUMBER OF 3D POINTS
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NUMBER OF SEGMENTS IN 2D
!>    </td></tr>
!>          <tr><td>SCHCF
!></td><td>--></td><td>ADVECTION SCHEME
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
                        SUBROUTINE BYPASS_CRUSHED_POINTS_SEG
     &(VOLU,SVOLU,VOLUN,SVOLUN,FLUX,TRA01,MESH2,MESH3,
     & NPOIN3,SCHCF,NPOIN2,GLOSEG,DIMGLO,NSEG,NPLAN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
C| FLUX           |<->| FLUXES TO BE CHANGED
C| GLOSEG         |-->| 3D LIST OF SEGMENTS POINTS
C| MESH2,3        |-->| 2D MESH, 3D MESH
C| MESH3          |---| 
C| NPLAN          |-->| NUMBER OF PLANES
C| NPOIN2         |-->| NUMBER OF POINTS IN 2D
C| NPOIN3         |-->| NUMBER OF 3D POINTS
C| NSEG           |-->| NUMBER OF SEGMENTS IN 2D
C| SCHCF          |-->| ADVECTION SCHEME
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
      INTEGER, INTENT(IN)             :: SCHCF,NPOIN3,NPOIN2
      INTEGER, INTENT(IN)             :: NSEG,NPLAN,DIMGLO
      INTEGER, INTENT(IN)             :: GLOSEG(DIMGLO,2)
!
      DOUBLE PRECISION, INTENT(IN)    :: VOLUN(NPOIN3),VOLU(NPOIN3)
!
      TYPE(BIEF_OBJ), INTENT(IN)      :: SVOLU,SVOLUN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TRA01
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2,MESH3
!
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NSEGH,NSEGV,IHOR
      INTEGER I1,I2,I3,I4,IPLAN,ISEG2D,ISEG3D,I2D,I3D,ICR1,ICR2
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION EPS_VOLUME
      DATA EPS_VOLUME /1.D-14/
!
!-----------------------------------------------------------------------
!
      NSEGH=NSEG*NPLAN
      NSEGV=NPOIN2*(NPLAN-1)
!
!***********************************************************************
!
C     TRA01=VOLU+VOLUN=0 MEANS THAT BOTH VOLU AND VOLUN ARE EQUAL TO 0
!
      CALL OS('X=Y+Z   ',X=TRA01,Y=SVOLU,Z=SVOLUN)
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(TRA01,2,MESH3)
      ENDIF
!
C     SEE STOSEG41.F FOR NUMBERING OF CROSSED SEGMENTS
!
      IF(SCHCF.EQ.ADV_NSC_TF) THEN
!
C       CASE WITH BOTH HORIZONTAL AND CROSSED FLUXES (CASE OF N-SCHEME)
!
        DO ISEG2D=1,NSEG
          DO IPLAN=1,NPLAN-1
C           CROSSED SEGMENT EXITING FROM POINT 1 OF HORIZONTAL SEGMENT
            ICR1=NSEGH+NSEGV+2*(IPLAN-1)*NSEG+ISEG2D
C           CROSSED SEGMENT EXITING FROM POINT 2 OF HORIZONTAL SEGMENT
            ICR2=ICR1+NSEG
C           HORIZONTAL SEGMENT
            ISEG3D=ISEG2D+(IPLAN-1)*NSEG
            I1=GLOSEG(ISEG3D,1)
            I2=GLOSEG(ISEG3D,2)
            I3=GLOSEG(ICR1,2)
            I4=GLOSEG(ICR2,2)
            IF(TRA01%R(I1).LT.EPS_VOLUME.OR.
     &         TRA01%R(I3).LT.EPS_VOLUME) THEN
C             FLUX ADDED TO UPPER LAYER WITH SAME ORIENTATION
              IHOR=ISEG2D+IPLAN*NSEG
              FLUX(IHOR)=FLUX(IHOR)+FLUX(ICR1)
C             FLUX ADDED TO VERTICAL OF I1
              FLUX(NSEGH+I1)=FLUX(NSEGH+I1)+FLUX(ICR1)
C             FLUX CANCELLED
              FLUX(ICR1)=0.D0
            ENDIF
            IF(TRA01%R(I2).LT.EPS_VOLUME.OR.
     &         TRA01%R(I4).LT.EPS_VOLUME) THEN
C             FLUX ADDED TO UPPER LAYER WITH OPPOSITE ORIENTATION
              IHOR=ISEG2D+IPLAN*NSEG
              FLUX(IHOR)=FLUX(IHOR)-FLUX(ICR2)
C             FLUX ADDED TO VERTICAL OF I2
              FLUX(NSEGH+I2)=FLUX(NSEGH+I2)+FLUX(ICR2)
C             FLUX CANCELLED
              FLUX(ICR2)=0.D0
            ENDIF
            IF(TRA01%R(I1).LT.EPS_VOLUME) THEN
C             FLUX (FROM 2 TO 1) ADDED TO UPPER LAYER
              FLUX(ISEG3D+NSEG)=FLUX(ISEG3D+NSEG)+FLUX(ISEG3D)
C             FLUX ADDED TO VERTICAL OF I2 (WITH - SIGN)
              FLUX(NSEGH+I2)=FLUX(NSEGH+I2)-FLUX(ISEG3D)
C             FLUX REMOVED FROM VERTICAL OF I1 (WITH + SIGN)
              FLUX(NSEGH+I1)=FLUX(NSEGH+I1)+FLUX(ISEG3D)
C             FLUX CANCELLED
              FLUX(ISEG3D)=0.D0
            ELSEIF(TRA01%R(I2).LT.EPS_VOLUME) THEN
C             FLUX (FROM 2 TO 1) ADDED TO UPPER LAYER
              FLUX(ISEG3D+NSEG)=FLUX(ISEG3D+NSEG)+FLUX(ISEG3D)
C             FLUX REMOVED FROM VERTICAL OF I2
              FLUX(NSEGH+I2)=FLUX(NSEGH+I2)-FLUX(ISEG3D)
C             FLUX ADDED TO VERTICAL OF I1 (BUT - SIGN)
              FLUX(NSEGH+I1)=FLUX(NSEGH+I1)+FLUX(ISEG3D)
C             FLUX CANCELLED
              FLUX(ISEG3D)=0.D0
            ELSE
              EXIT
            ENDIF
          ENDDO
        ENDDO
C
      ELSEIF(SCHCF.EQ.ADV_LPO_TF) THEN
C
C       CASE WITH ONLY HORIZONTAL FLUXES
C
        DO ISEG2D=1,NSEG
          DO IPLAN=1,NPLAN-1
            ISEG3D=ISEG2D+(IPLAN-1)*NSEG
            I1=GLOSEG(ISEG3D,1)
            I2=GLOSEG(ISEG3D,2)
            IF(TRA01%R(I1).LT.EPS_VOLUME) THEN
C             FLUX (FROM 2 TO 1) ADDED TO UPPER LAYER
              FLUX(ISEG3D+NSEG)=FLUX(ISEG3D+NSEG)+FLUX(ISEG3D)
C             FLUX ADDED TO VERTICAL OF I2 (WITH - SIGN)
              FLUX(NSEGH+I2)=FLUX(NSEGH+I2)-FLUX(ISEG3D)
C             FLUX REMOVED FROM VERTICAL OF I1 (WITH + SIGN)
              FLUX(NSEGH+I1)=FLUX(NSEGH+I1)+FLUX(ISEG3D)
C             FLUX CANCELLED
              FLUX(ISEG3D)=0.D0
            ELSEIF(TRA01%R(I2).LT.EPS_VOLUME) THEN
C             FLUX (FROM 2 TO 1) ADDED TO UPPER LAYER
              FLUX(ISEG3D+NSEG)=FLUX(ISEG3D+NSEG)+FLUX(ISEG3D)
C             FLUX REMOVED FROM VERTICAL OF I2
              FLUX(NSEGH+I2)=FLUX(NSEGH+I2)-FLUX(ISEG3D)
C             FLUX ADDED TO VERTICAL OF I1
              FLUX(NSEGH+I1)=FLUX(NSEGH+I1)+FLUX(ISEG3D)
C             FLUX CANCELLED
              FLUX(ISEG3D)=0.D0
            ELSE
              EXIT
            ENDIF
          ENDDO
        ENDDO
C
      ELSE
        WRITE(LU,*) 'UNKNOWN SCHEME IN BYPASS_CRUSHED_POINTS_SEG:',SCHCF
        CALL PLANTE(1)
        STOP
      ENDIF
C
C=======================================================================
C
      RETURN
      END
C
C#######################################################################
C