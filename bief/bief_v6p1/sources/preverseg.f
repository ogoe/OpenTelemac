C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       BUILDS TRIDIAGONAL SYSTEMS FOR EVERY VERTICAL,
!>                BY LUMPING A MATRIX DEFINED ON PRISMS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AD, AX, MESH, NPOIN, NSEG3D, TYPDIA, TYPEXT, XAUX
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
!>    </th><td> I2, I3, IAN, ICOM, IPLAN, NPLAN, NPOIN2, NSEG2D, NSEGH, SEGDOWN, SEGUP
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PREVERSEG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> NBPTS(), NBSEG(), OV(), PARCOM2(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SOLVE()

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
!> </td><td> 11/08/09
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 11/08/09
!> </td><td> JMH
!> </td><td> CROSSED AND VERTICAL SEGMENTS SWAPPED (SEE STOSEG41)
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG3D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TYPDIA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TYPEXT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XAUX
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PREVERSEG
     &(XAUX,AD,AX,TYPDIA,TYPEXT,NPOIN,MESH,NSEG3D)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AD             |---| 
C| AX             |---| 
C| MESH           |---| 
C| NPOIN          |---| 
C| NSEG3D         |---| 
C| TYPDIA         |---| 
C| TYPEXT         |---| 
C| XAUX           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PREVERSEG => PREVERSEG
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN,NSEG3D
C
      DOUBLE PRECISION, INTENT(IN)    :: AD(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: XAUX(NPOIN,*),AX(NSEG3D,2)
C
      CHARACTER(LEN=1), INTENT(IN) :: TYPDIA,TYPEXT
C
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I2,I3,NPLAN,IAN,ICOM,NPOIN2,SEGUP,SEGDOWN,NSEG2D
      INTEGER IPLAN,NSEGH
C
C-----------------------------------------------------------------------
C
C     HERE WE CONSIDER THAT NPOIN < NELMAX TO USE XAUX AS XAUX(NPOIN,3)
C
C     XAUX(I,1) IS COEFFICIENT OF POINT BELOW I IN EQUATION OF POINT I
C     XAUX(I,2) IS THE DIAGONAL
C     XAUX(I,3) IS COEFFICIENT OF POINT ABOVE I IN EQUATION OF POINT I
C
C-----------------------------------------------------------------------
C     INITIALISES THE DIAGONAL
C-----------------------------------------------------------------------
C
      IF(TYPDIA(1:1).EQ.'0') THEN
        CALL OV('X=C     ',XAUX(1,2),AD,AD,0.D0,NPOIN)
      ELSEIF(TYPDIA(1:1).EQ.'I') THEN
        CALL OV('X=C     ',XAUX(1,2),AD,AD,1.D0,NPOIN)
      ELSEIF(TYPDIA(1:1).EQ.'Q') THEN
        CALL OV('X=Y     ',XAUX(1,2),AD,AD,0.D0,NPOIN)
      ELSE
        WRITE(LU,*) TYPDIA
        IF(LNG.EQ.1) WRITE(LU,*) 'DIAGONALE INCONNUE DANS PREVERSEG'
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOWN TYPE OF DIAGONAL IN PREVERSEG'
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C     LUMPS THE OFF-DIAGONAL TERMS CORRESPONDING TO VERTICAL SEGMENTS
C-----------------------------------------------------------------------
C
      NPOIN2 = BIEF_NBPTS(11,MESH)
      NPLAN  = NPOIN/NPOIN2
      NSEG2D = BIEF_NBSEG(11,MESH)
      NSEGH  = NSEG2D*NPLAN
C
      IF(TYPEXT.EQ.'Q') THEN
C       PLANE ON THE BOTTOM
        DO I2=1,NPOIN2
          SEGUP=NSEGH+I2
          XAUX(I2,1)=0.D0
          XAUX(I2,3)=AX(SEGUP,1)
        ENDDO
C       PLANE AT THE FREE SURFACE
        DO I2=1,NPOIN2
          I3=I2+(NPLAN-1)*NPOIN2
          SEGDOWN=NSEGH+NPOIN2*(NPLAN-2)+I2
          XAUX(I3,1)=AX(SEGDOWN,2)
          XAUX(I3,3)=0.D0
        ENDDO
C       OTHER PLANES
        IF(NPLAN.GT.2) THEN
        DO IPLAN=2,NPLAN-1
          DO I2=1,NPOIN2
            I3=I2+(IPLAN-1)*NPOIN2
            SEGDOWN=NSEGH+NPOIN2*(IPLAN-2)+I2
            SEGUP  =SEGDOWN+NPOIN2
            XAUX(I3,1)=AX(SEGDOWN,2)
            XAUX(I3,3)=AX(SEGUP,1)
          ENDDO
        ENDDO
        ENDIF
      ELSEIF(TYPEXT.EQ.'S') THEN
C       PLANE ON THE BOTTOM
        DO I2=1,NPOIN2
          SEGUP=NSEGH+I2
          XAUX(I2,1)=0.D0
          XAUX(I2,3)=AX(SEGUP,1)
        ENDDO
C       PLANE AT THE FREE SURFACE
        DO I2=1,NPOIN2
          I3=I2+(NPLAN-1)*NPOIN2
          SEGDOWN=NSEGH+NPOIN2*(NPLAN-2)+I2
          XAUX(I3,1)=AX(SEGDOWN,1)
          XAUX(I3,3)=0.D0
        ENDDO
C       OTHER PLANES
        IF(NPLAN.GT.2) THEN
        DO IPLAN=2,NPLAN-1
          DO I2=1,NPOIN2
            I3=I2+(IPLAN-1)*NPOIN2
            SEGDOWN=NSEGH+NPOIN2*(IPLAN-2)+I2
            SEGUP  =SEGDOWN+NPOIN2
            XAUX(I3,1)=AX(SEGDOWN,1)
            XAUX(I3,3)=AX(SEGUP,1)
          ENDDO
        ENDDO
        ENDIF
      ELSEIF(TYPEXT.EQ.'0') THEN
C       NOTHING TO DO (BUT WHAT'S THE USE OF AN ITERATIVE SOLVER ?)
      ELSE
        WRITE(LU,*) TYPEXT
        IF(LNG.EQ.1) WRITE(LU,*) 'TYPE DE TERMES EXTRA-DIAGONAUX'
        IF(LNG.EQ.1) WRITE(LU,*) 'INCONNUS DANS PREVERSEG'
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOWN TYPE OF OFF-DIAGONAL TERMS'
        IF(LNG.EQ.2) WRITE(LU,*) 'IN PREVERSEG'
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C     PARALLEL MODE
C
      IF(NCSIZE.GT.1) THEN
        IAN    = 3
        ICOM   = 2
        CALL PARCOM2(XAUX(1,1),XAUX(1,2),XAUX(1,3),
     &               NPOIN2,NPLAN,ICOM,IAN,MESH)
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
