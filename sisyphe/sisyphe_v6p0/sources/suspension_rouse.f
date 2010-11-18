C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE DEPOSITION FLUX AND
!>                CONCENTRATION ACCORDING TO ROUSE PROFILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> HMIN, HN, KARMAN, NPOIN, T2, USTAR, XWC, ZERO, ZREF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> B, EXP, I, ROUSE
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SUSPENSION_ROUSE
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INIT_TRANSPORT(), SUSPENSION_DEPOT()

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
!>      <td><center> 5.7                                       </center>
!> </td><td> 13/07/07
!> </td><td> J-M HERVOUET 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.6                                       </center>
!> </td><td> 04/01/05
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.5                                       </center>
!> </td><td> 14/04/04
!> </td><td> C. VILLARET 01 30 87 83 28; J-M HERVOUET 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CF
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>CS
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>FLUDP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HCHAR
!></td><td>---</td><td>(WORK ARRAY)
!>    </td></tr>
!>          <tr><td>HMIN
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>IEIN
!></td><td>---</td><td>(WORK ARRAY)
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>Q
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>USTAR
!></td><td>---</td><td>(WORK ARRAY)
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>XWC
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>ZERO
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>ZREF
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE SUSPENSION_ROUSE
     &(USTAR,HN,NPOIN,KARMAN,HMIN,ZERO,XWC,ZREF,T2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CF             |-->| 
C| CS             |-->| 
C| FLUDP          |---| 
C| HCHAR          |---| (WORK ARRAY)
C| HMIN           |-->| 
C| HN             |-->| 
C| IEIN           |---| (WORK ARRAY)
C| KARMAN         |-->| 
C| NPOIN          |-->| 
C| Q             |-->| 
C| T2             |---| 
C| TOB            |-->| 
C| USTAR          |---| (WORK ARRAY)
C| XMVE           |-->| 
C| XWC            |-->| 
C| ZERO           |-->| 
C| ZREF           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,EX_SUSPENSION_ROUSE => SUSPENSION_ROUSE
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: USTAR,HN,ZREF
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN,XWC,HMIN,ZERO
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T2

      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: I
      DOUBLE PRECISION :: B,EXP,ROUSE
!
      INTRINSIC MAX,MIN,LOG
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
C     ROUSE NUMBER AND MINIMUM BOUND OF THE EINSTEIN INTEGRAL
!
      DO I=1,NPOIN
!
C        ROUSE NUMBER
!
         ROUSE=  XWC / (KARMAN*MAX(USTAR%R(I),ZERO))
!
C        MINIMUM BOUND OF THE EINSTEIN INTEGRAL -->  B = KS/H
!
C        JMH MODIFICATION 16/12/2009    B ALWAYS
C        B = ZREF%R(I)/MAX(HN%R(I),HMIN)
         B = ZREF%R(I)/MAX(HN%R(I),ZREF%R(I))
!
C        RATIO BETWEEN REFERENCE CONC. ON BOTTOM AND MEAN CONC.
C        ASSUMING EXPONENTIAL PROFILE WITH EXPONENT ROUSE NUMBER --> T2
!
         EXP=ROUSE-1.D0
         IF(ABS(EXP).GT.1.D-4) THEN
C          ADDED BY JMH 12/07/2007
           EXP=MIN(EXP,3.D0)
           T2%R(I)=B*(1.D0-B**EXP)/EXP
         ELSE
           T2%R(I)=-B*LOG(B)
         ENDIF
         T2%R(I)=MAX(1.D0/MAX(T2%R(I),ZERO),1.D0)
      ENDDO
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
C
C#######################################################################
C