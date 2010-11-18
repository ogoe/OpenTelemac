C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INTEGRATES IN TIME.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note     DDMIN NOT USED (JMH)
!>  @note     PORTABILITY: CRAY

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AIRS, CF, DDMIN, DT, EPS, FLUX, KFROT, NPOIN, SMH, W, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ALPHAF, DELTA, I, KF, ST2D
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>RESOLU()

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
!>      <td><center>                                           </center>
!> </td><td> 24/11/1997
!> </td><td> N. GOUTAL
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AIRS
!></td><td>--></td><td>TABLEAU DES AIRES DES CELLULES
!>    </td></tr>
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DDMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS.
!>    </td></tr>
!>          <tr><td>EPS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUX
!></td><td>--></td><td>FLUX DE ROE
!>    </td></tr>
!>          <tr><td>H
!></td><td><-></td><td>ENTHALPIE
!>    </td></tr>
!>          <tr><td>KFROT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SMH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INTEMP
     &(W,FLUX,AIRS,DT,NPOIN,ZF,CF,EPS,DDMIN,KFROT,SMH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AIRS           |-->| TABLEAU DES AIRES DES CELLULES
C| CF             |---| 
C| DDMIN          |---| 
C| DT             |-->| PAS DE TEMPS.
C| EPS            |---| 
C| FLUX           |-->| FLUX DE ROE
C| H             |<->| ENTHALPIE
C| KFROT          |---| 
C| NPOIN          |---| 
C| SMH            |---| 
C| W             |---| 
C| ZF             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPOIN,KFROT
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FLUX(NPOIN,3),DT,EPS,DDMIN
      DOUBLE PRECISION, INTENT(IN)    :: AIRS(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN),SMH(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
C
      DOUBLE PRECISION DELTA,KF,ST2D,ALPHAF
C
C-----------------------------------------------------------------------
C
C------
C 1. COMPUTES W AT TIME N+1
C------
C
      DO 10 I = 1 , NPOIN
         W(1,I) = W(1,I) - DT *( FLUX (I,1)-SMH(I)) / AIRS(I)
         W(2,I) = W(2,I) - DT * FLUX (I,2) / AIRS(I)
         W(3,I) = W(3,I) - DT * FLUX (I,3) / AIRS(I)
10    CONTINUE
C
C     TAKES FRICTION INTO ACCOUNT (NO FRICTION RITTER)
C     *****************************
      IF (KFROT.NE.0) THEN
C
         DO 20 I = 1,NPOIN
C
C FH-FRDATA
C            IF (W(1,I).GT.EPS/10.D0) THEN
            IF ((W(1,I).GT.EPS/10.D0).AND.(CF(I).GT.1.D-12)) THEN
C FH-FRDATA
               ST2D = CF(I)
               KF = 9.81D0*DT*DSQRT(W(2,I)**2+W(3,I)**2)/
     &              (ST2D*ST2D*W(1,I)**(7.D0/3.D0))
               IF (KF.GT.1.D-6) THEN
                  DELTA = (1.D0+4.D0*KF)
                  ALPHAF = (-1.D0+SQRT(DELTA))/(2*KF)
               ELSE
                  ALPHAF = 1.D0 - KF
               ENDIF
                  W(2,I) = ALPHAF * W(2,I)
                  W(3,I) = ALPHAF * W(3,I)
            ENDIF
C
20       CONTINUE
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