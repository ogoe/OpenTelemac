C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ONE STEP OF GRADIENT METHOD.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DESC, ESTIME, GRADJ, GRADJN, JCOUT1, NPARAM, OPTID, R02, R03, RO, RSTART
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DENOM, GRAD_JN, I, R1
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ADJ_T2D()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 04/10/2000
!> </td><td> A. LEOPARDI (UNINA)
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 02/08/1993
!> </td><td> E. BARROS
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DESC
!></td><td><--</td><td>VECTOR USED TO CHANGE THE SET OF STRICKLERS'
!>    </td></tr>
!>          <tr><td>ESTIME
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRADJ
!></td><td>--></td><td>GRADIENT OF COST FUNCTION (ITERATION K)
!>    </td></tr>
!>          <tr><td>GRADJN
!></td><td>--></td><td>GRADIENT OF COST FUNCTION (ITERATION K-1)
!>    </td></tr>
!>          <tr><td>JCOUT1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPARAM
!></td><td>--></td><td>TOTAL NUMBER OF PARAMETERS TO ESTIMATE
!>    </td></tr>
!>          <tr><td>OPTID
!></td><td>--></td><td>METHOD 1=GRADIENT, 2=GRADIENT CONJUGUE, 3=LAGRANGE)
!>    </td></tr>
!>          <tr><td>R02
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>R03
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RO
!></td><td><-></td><td>COEFFICIENT OF THE GRADIENT
!>    </td></tr>
!>          <tr><td>RSTART
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE METGRA
     &(RO,ESTIME,GRADJ,GRADJN,JCOUT1,DESC,NPARAM,OPTID,RSTART,R02,R03)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DESC           |<--| VECTOR USED TO CHANGE THE SET OF STRICKLERS'
C| ESTIME         |---| 
C| GRADJ          |-->| GRADIENT OF COST FUNCTION (ITERATION K)
C| GRADJN         |-->| GRADIENT OF COST FUNCTION (ITERATION K-1)
C| JCOUT1         |---| 
C| NPARAM         |-->| TOTAL NUMBER OF PARAMETERS TO ESTIMATE
C| OPTID          |-->| METHOD 1=GRADIENT, 2=GRADIENT CONJUGUE, 3=LAGRANGE)
C| R02            |---| 
C| R03            |---| 
C| RO             |<->| COEFFICIENT OF THE GRADIENT
C| RSTART         |---| 
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
      INTEGER , INTENT(IN)             :: NPARAM,OPTID
      CHARACTER(LEN=72)                :: ESTIME
      DOUBLE PRECISION , INTENT(IN)    :: JCOUT1
      LOGICAL , INTENT(IN)             :: RSTART
      TYPE(BIEF_OBJ) , INTENT(IN)      :: GRADJ,GRADJN
      TYPE(BIEF_OBJ) , INTENT(INOUT)   :: DESC
      DOUBLE PRECISION , INTENT(INOUT) :: R02,R03,RO
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
C
      DOUBLE PRECISION R1,DENOM,GRAD_JN
C
C     COMPUTES THE TRUE GRADIENT (WHICH TAKES THE TRUE NUMBER OF
C                                           PARAMETERS INTO ACCOUNT)
      DENOM=0.D0
      GRAD_JN=0.D0
      DO I = 1,NPARAM
        DENOM  = DENOM + GRADJ%R(I)**2
        GRAD_JN=GRAD_JN+GRADJN%R(I)**2
      ENDDO
C
      IF(DENOM.LT.1.D-12) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'METGRA : GRADIENT TROP PETIT, ARRET'
        IF(LNG.EQ.2) WRITE(LU,*) 'METGRA: GRADIENT TOO SMALL, STOP'
        WRITE(LU,*) 'DENOM = ',DENOM
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C     RO = - JCOUT / GRADJ*GRADJ
C-----------------------------------------------------------------------
C
      IF(OPTID.EQ.1.OR.OPTID.EQ.3.OR.RSTART) THEN
C
            R02 = - JCOUT1 / DENOM
            RO = R02
            R03=0.5D0*R02
C
C           COMPUTES THE DIRECTION OF INITIAL DESCENT
C
            CALL OV('X=Y     ',DESC%R,GRADJ%R,GRADJ%R,0.D0,NPARAM)
C
C-----------------------------------------------------------------------
C
      ELSEIF(OPTID.EQ.2) THEN
C
            R02 = - JCOUT1 / DENOM
C
            R1 = GRAD_JN/DENOM
C
C           COMPUTES THE DIRECTION OF DESCENT
C
            CALL OV('X=Y+CZ  ',DESC%R,GRADJ%R,DESC%R,R1,NPARAM)
C
            DENOM=0.D0
            DO I=1,NPARAM
               DENOM=DENOM+GRADJ%R(I)*DESC%R(I)
            ENDDO
            R03 = - JCOUT1/DENOM
            RO =R03
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C