C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CHECKS IF A STEADY STATE IS REACHED.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  ARRET IS NOT INITIALISED

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ARRET, CRIPER, H1, H2, NPH, NPT, NPU, NPV, NTRAC, T1, T2, U1, U2, V1, V2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, ITRAC
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!>      <td><center> 5.8                                       </center>
!> </td><td> 05/09/2007
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ARRET
!></td><td><--</td><td>LOGIQUE MIS A TRUE SI PERMANENT ATTEINT
!>    </td></tr>
!>          <tr><td>CRIPER
!></td><td>--></td><td>CRITERES D'ARRET
!>                  DANS L'ORDRE SUIVANT : H , U , V , T
!>    </td></tr>
!>          <tr><td>H1,H2,NPH
!></td><td>--></td><td>HAUTEURS A COMPARER ET NOMBRE DE POINTS
!>    </td></tr>
!>          <tr><td>NPV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1,T2,NPT
!></td><td>--></td><td>TRACEURS A COMPARER ET NOMBRE DE POINTS
!>    </td></tr>
!>          <tr><td>TRAC
!></td><td>--></td><td>LOGIQUE INDIQUANT S'IL Y A UN TRACEUR.
!>    </td></tr>
!>          <tr><td>U1,U2,NPU
!></td><td>--></td><td>VITESSES A COMPARER ET NOMBRE DE POINTS
!>    </td></tr>
!>          <tr><td>V1,V2,NPU
!></td><td>--></td><td>VITESSES A COMPARER ET NOMBRE DE POINTS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE STEADY
     &(H1,H2,NPH,U1,U2,NPU,V1,V2,NPV,NTRAC,T1,T2,NPT,CRIPER,ARRET)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ARRET          |<--| LOGIQUE MIS A TRUE SI PERMANENT ATTEINT
C| CRIPER         |-->| CRITERES D'ARRET
C|                |   | DANS L'ORDRE SUIVANT : H , U , V , T
C| H1,H2,NPH      |-->| HAUTEURS A COMPARER ET NOMBRE DE POINTS
C| NPV            |---| 
C| NTRAC          |---| 
C| T1,T2,NPT      |-->| TRACEURS A COMPARER ET NOMBRE DE POINTS
C| TRAC           |-->| LOGIQUE INDIQUANT S'IL Y A UN TRACEUR.
C| U1,U2,NPU      |-->| VITESSES A COMPARER ET NOMBRE DE POINTS
C| V1,V2,NPU      |-->| VITESSES A COMPARER ET NOMBRE DE POINTS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)          :: NPH,NPU,NPV,NPT,NTRAC
      LOGICAL, INTENT(INOUT)       :: ARRET
      DOUBLE PRECISION, INTENT(IN) :: H1(NPH),H2(NPH),U1(NPU),U2(NPU)
      DOUBLE PRECISION, INTENT(IN) :: V1(NPV),V2(NPV)
      DOUBLE PRECISION, INTENT(IN) :: CRIPER(3)
      TYPE(BIEF_OBJ)  , INTENT(IN) :: T1,T2
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,ITRAC
C
C-----------------------------------------------------------------------
C
C  CHECKS THE WATER DEPTH
C
      DO 10 I = 1 , NPH
        IF(ABS(H1(I)-H2(I)).GT.CRIPER(1)) GO TO 1000
10    CONTINUE
C
C-----------------------------------------------------------------------
C
C  CHECKS U
C
      DO 20 I = 1 , NPU
        IF(ABS(U1(I)-U2(I)).GT.CRIPER(2)) GO TO 1000
20    CONTINUE
C
C-----------------------------------------------------------------------
C
C  CHECKS V
C
      DO 30 I = 1 , NPV
        IF(ABS(V1(I)-V2(I)).GT.CRIPER(2)) GO TO 1000
30    CONTINUE
C
C-----------------------------------------------------------------------
C
C  CHECKS THE TRACER
C
      IF(NTRAC.GT.0) THEN
C
      DO ITRAC=1,NTRAC
        DO I = 1 , NPT
          IF(ABS(T1%ADR(ITRAC)%P%R(I)
     &          -T2%ADR(ITRAC)%P%R(I)).GT.CRIPER(3)) GO TO 1000
        ENDDO
      ENDDO
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      ARRET=.TRUE.
      IF(LNG.EQ.1) WRITE(LU,100)
      IF(LNG.EQ.2) WRITE(LU,200)
100   FORMAT(/,1X,'ETAT PERMANENT ATTEINT')
200   FORMAT(/,1X,'THE STEADY STATE HAS BEEN REACHED')
C
C-----------------------------------------------------------------------
C
1000  CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C