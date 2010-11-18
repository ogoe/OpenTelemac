C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CORRECTS U2D, V2D VELOCITIES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALPHA, HMIN, HN, KARMAN, KSR, NPOIN, T1, TOB, U2D, UCONV, V2D, VCONV, XMVE, XWC, ZERO, ZREF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, AUX, B, I, I1, I2, LAUX, LL, R1, ROUSE, USTAR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INIT_TRANSPORT(), SUSPENSION_COMPUTATION()

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
!> </td><td> 02/05/2008
!> </td><td>
!> </td><td> ADDED ALPHA IN ARGUMENT, TO KEEP A RECORD OF THE
!>           CORRECTION COEFFICIENT
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 01/08/2006
!> </td><td> C. VILLARET (LNHE) 01 30 87 83 28
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALPHA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CF
!></td><td>--></td><td>FRICTION COEFFICIENT
!>    </td></tr>
!>          <tr><td>HMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF POINTS
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>--></td><td>SHEAR STRESS
!>    </td></tr>
!>          <tr><td>U2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UCONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VCONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>--></td><td>WATER DENSITY
!>    </td></tr>
!>          <tr><td>XWC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZERO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZREF
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                       SUBROUTINE SUSPENSION_CONV
     &(TOB, XMVE, KSR, NPOIN, ZREF, U2D, V2D, HN, HMIN,
     & UCONV, VCONV, KARMAN, ZERO, XWC,T1,ALPHA)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALPHA          |---| 
C| CF             |-->| FRICTION COEFFICIENT
C| HMIN           |---| 
C| HN             |---| 
C| KARMAN         |---| 
C| KSR            |---| 
C| NPOIN          |-->| NUMBER OF POINTS
C| T1             |---| 
C| TOB            |-->| SHEAR STRESS
C| U2D            |---| 
C| UCONV          |---| 
C| V2D            |---| 
C| VCONV          |---| 
C| XMVE           |-->| WATER DENSITY
C| XWC            |---| 
C| ZERO           |---| 
C| ZREF           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE (BIEF_OBJ),  INTENT(IN)    :: HN,U2D,V2D,ZREF,KSR
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: UCONV,VCONV,T1,ALPHA
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TOB
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,XWC,HMIN
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN,XMVE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION :: R1,I1,I2,A,B,AUX,LAUX,LL,USTAR,ROUSE
!
      INTEGER :: I
!
      CALL OS('X=N(Y,Z)', X=T1, Y=U2D, Z=V2D)
!
      LL=LOG(30.D0)
!
      DO I = 1, NPOIN
!
        IF(TOB%R(I).GT.ZERO) THEN
!
          USTAR = SQRT(TOB%R(I)/XMVE)
!
C         B --> KS/H
!
C         AUX = 1.D0 + KARMAN*SQRT(2.D0/MAX(CF%R(I),ZERO))
C         B = 30.D0*EXP(-AUX)
!
          B = KSR%R(I) /MAX(HN%R(I),1.1D0*KSR%R(I))
          A = ZREF%R(I)/MAX(HN%R(I),1.1D0*ZREF%R(I))
!
C TAKES MAX VALUE OF A = ZREF/H AND B=KSR/H
          A=MAX(A,B)
!
C SIMPLIFIED VERSION
          ROUSE=MIN(XWC/MAX(USTAR,ZERO),1.D0)/KARMAN
          R1=  1.D0-ROUSE
          LAUX=LOG(A)
!
          IF(ABS(R1).LT.1.D-8) THEN
            I1= -LAUX
            I2= -LAUX**2/2.D0
          ELSE
            AUX=A**R1
            I1=(1.D0-AUX)/R1
            I2=-(I1+LAUX*AUX)/R1
          ENDIF
!
C         AUX=LOG(A/30.D0)
          AUX=LAUX - LL
          ALPHA%R(I)=-(I2-AUX*I1)/(I1*(AUX+1.D0))
!
        ELSE
!
          ALPHA%R(I)=1.D0
!
        ENDIF
!
C       CHECKS 0
!
        ALPHA%R(I)=MIN(ALPHA%R(I),1.D0)
        ALPHA%R(I)=MAX(ALPHA%R(I),0.D0)
!
        UCONV%R(I) = ALPHA%R(I)*U2D%R(I)
        VCONV%R(I) = ALPHA%R(I)*V2D%R(I)
!
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C