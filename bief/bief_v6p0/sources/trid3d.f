C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES TRIDIAGONAL SYSTEMS FOR EVERY VERTICAL
!>                IN A MESH OF PRISMS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> B, NPOIN, NPOIN2, X, XAUX
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> EPS, I, I3D, IPLAN, NPLAN
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_TRID3D
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>GRACJG(), RESCJG(), WAVE_EQUATION()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 30/09/05
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>B
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XAUX
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TRID3D
     &(XAUX,X,B,NPOIN,NPOIN2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| B             |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C| NPOIN          |---| 
C| NPOIN2         |---| 
C| X             |---| 
C| XAUX           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_TRID3D => TRID3D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN,NPOIN2
C
      DOUBLE PRECISION, INTENT(IN)    :: B(NPOIN2,*)
      DOUBLE PRECISION, INTENT(INOUT) :: XAUX(NPOIN,*),X(NPOIN2,*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,IPLAN,I3D,NPLAN
      DOUBLE PRECISION EPS
C
      INTRINSIC ABS
C
C-----------------------------------------------------------------------
C
C     XAUX(I,1) IS COEFFICIENT OF POINT BELOW I IN EQUATION OF POINT I
C     XAUX(I,2) IS THE DIAGONAL
C     XAUX(I,3) IS COEFFICIENT OF POINT ABOVE I IN EQUATION OF POINT I
C
C     XAUX(I,4) HERE USED AS WORKING ARRAY
C     XAUX(I,5) HERE USED AS WORKING ARRAY
C
C-----------------------------------------------------------------------
C
      NPLAN=NPOIN/NPOIN2
      EPS=1.D-8
C
C-----------------------------------------------------------------------
C
C     BASIC ALGORITHM TAKEN IN "NUMERICAL RECIPES" PAGE 40 AND ADAPTED
C     TO STORAGE
C
C     XAUX(*,4) : WORKING ARRAY (SIZE NPOIN2)
C     XAUX(*,5) : WORKING ARRAY (SIZE NPOIN)
C
      DO I=1,NPOIN2
        XAUX(I,4)=XAUX(I,2)
        IF(ABS(XAUX(I,4)).LT.EPS) THEN
          IF(LNG.EQ.1) WRITE(LU,*) 'TRID3D : SYSTEME NON DEFINI'
          IF(LNG.EQ.2) WRITE(LU,*) 'TRID3D: SYSTEM ILL-DEFINED'
          CALL PLANTE(1)
          STOP
        ENDIF
        X(I,1)=B(I,1)/XAUX(I,4)
      ENDDO
C
      DO IPLAN=2,NPLAN
      DO I=1,NPOIN2
        I3D=I+NPOIN2*(IPLAN-1)
        XAUX(I3D,5)=XAUX(I3D-NPOIN2,3)/XAUX(I,4)
        XAUX(I,4)=XAUX(I3D,2)-XAUX(I3D,1)*XAUX(I3D,5)
        IF(ABS(XAUX(I,4)).LT.EPS) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'TRID3D : SYSTEME NON DEFINI'
            WRITE(LU,*) '         PRECONDITIONNEMENT 17 IMPOSSIBLE'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'TRID3D: SYSTEM ILL-DEFINED'
            WRITE(LU,*) '        PRECONDITIONING 17 IMPOSSIBLE'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        X(I,IPLAN)=(B(I,IPLAN)-XAUX(I3D,1)*X(I,IPLAN-1))/XAUX(I,4)
      ENDDO
      ENDDO
C
      DO IPLAN=NPLAN-1,1,-1
      DO I=1,NPOIN2
        I3D=I+NPOIN2*IPLAN   ! PLAN TO THE TOP
        X(I,IPLAN)=X(I,IPLAN)-XAUX(I3D,5)*X(I,IPLAN+1)
      ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C