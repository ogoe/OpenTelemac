C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES OUT MIN/MAX VALUES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CST, DT, IELMT, MASKEL, MESH, MSK, T1, UCONV, VCONV, ZFCL_S
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
!>    </th><td> IMA, IMAX, XMA, XMAX
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SUSPENSION_LISTING
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CFLPSI(), MAXI(), MINI(), P_DMAX(), P_DMIN(), P_IMAX()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SUSPENSION_COMPUTATION()

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
!> </td><td> 22/12/04
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CST
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELMT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UCONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VCONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZFCL_S
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE SUSPENSION_LISTING !
     &(MESH,CST,ZFCL_S,UCONV,VCONV,MASKEL,IELMT,DT,MSK,T1)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CST            |---| 
C| DT             |---| 
C| IELMT          |---| 
C| MASKEL         |---| 
C| MESH           |---| 
C| MSK            |---| 
C| T1             |---| 
C| UCONV          |---| 
C| VCONV          |---| 
C| ZFCL_S         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,
     &    EX_SUSPENSION_LISTING => SUSPENSION_LISTING
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CST,ZFCL_S
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UCONV,VCONV,MASKEL
      INTEGER,          INTENT(IN)    :: IELMT
      DOUBLE PRECISION, INTENT(IN)    :: DT
      LOGICAL,          INTENT(IN)    :: MSK
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1


      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: IMAX,IMA
      DOUBLE PRECISION :: XMAX,XMA

      INTEGER                        P_IMAX
      DOUBLE PRECISION P_DMAX,P_DMIN
      EXTERNAL         P_DMAX,P_DMIN,P_IMAX

!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      CALL MAXI(XMAX,IMAX,CST%R,MESH%NPOIN)
      IF(NCSIZE.GT.1) THEN
        XMA=P_DMAX(XMAX)
        IF(XMAX.EQ.XMA) THEN
          IMA=MESH%KNOLG%I(IMAX)
        ELSE
          IMA=0
        ENDIF
        IMA=P_IMAX(IMA)
      ELSE
        IMA=IMAX
        XMA=XMAX
      ENDIF
      IF(LNG.EQ.1) WRITE(LU,500) XMA, IMA
      IF(LNG.EQ.2) WRITE(LU,510) XMA, IMA
      CALL MINI(XMAX, IMAX, CST%R, MESH%NPOIN)
      IF(NCSIZE.GT.1) THEN
        XMA=P_DMIN(XMAX)
        IF(XMAX.EQ.XMA) THEN
          IMA=MESH%KNOLG%I(IMAX)
        ELSE
          IMA=0
        ENDIF
        IMA=P_IMAX(IMA)
      ELSE
        IMA=IMAX
        XMA=XMAX
      ENDIF
      IF(LNG.EQ.1) WRITE(LU,501) XMA, IMA
      IF(LNG.EQ.2) WRITE(LU,511) XMA, IMA
      CALL MAXI(XMAX, IMAX, ZFCL_S%R, MESH%NPOIN)
      IF(NCSIZE.GT.1) THEN
        XMA=P_DMAX(XMAX)
        IF(XMAX.EQ.XMA) THEN
          IMA=MESH%KNOLG%I(IMAX)
        ELSE
          IMA=0
        ENDIF
        IMA=P_IMAX(IMA)
      ELSE
        IMA=IMAX
        XMA=XMAX
      ENDIF
      IF(LNG.EQ.1) WRITE(LU,502) XMA, IMA
      IF(LNG.EQ.2) WRITE(LU,512) XMA, IMA
      CALL MINI(XMAX, IMAX, ZFCL_S%R, MESH%NPOIN)
      IF(NCSIZE.GT.1) THEN
        XMA=P_DMIN(XMAX)
        IF(XMAX.EQ.XMA) THEN
          IMA=MESH%KNOLG%I(IMAX)
        ELSE
          IMA=0
        ENDIF
        IMA=P_IMAX(IMA)
      ELSE
        IMA=IMAX
        XMA=XMAX
      ENDIF
      IF(LNG.EQ.1) WRITE(LU,503) XMA, IMA
      IF(LNG.EQ.2) WRITE(LU,513) XMA, IMA
!
      CALL CFLPSI(T1, UCONV, VCONV, DT, IELMT, MESH, MSK, MASKEL)
      CALL MAXI(XMAX, IMAX, T1%R, MESH%NPOIN)
      IF(NCSIZE.GT.1) THEN
        XMA=P_DMAX(XMAX)
        IF(XMAX.EQ.XMA) THEN
          IMA=MESH%KNOLG%I(IMAX)
        ELSE
          IMA=0
        ENDIF
        IMA=P_IMAX(IMA)
      ELSE
        IMA=IMAX
        XMA=XMAX
      ENDIF
      IF(LNG.EQ.1) WRITE(LU,507) XMA,IMA
      IF(LNG.EQ.2) WRITE(LU,517) XMA,IMA

      !----------------------------------------------------------------!
500   FORMAT(' CONCENTRATION MAXIMALE     : ',G16.7,' %, NOEUD = ',1I8)
501   FORMAT(' CONCENTRATION MINIMALE     : ',G16.7,' %, NOEUD = ',1I8)
502   FORMAT(' EVOLUTION MAXIMALE         : ',G16.7,'  , NOEUD = ',1I8)
503   FORMAT(' EVOLUTION MINIMALE         : ',G16.7,'  , NOEUD = ',1I8)
507   FORMAT(' CFL MAX POUR LA SUSPENSION : ',G16.7,'  , NOEUD = ',1I8)
      !----------------------------------------------------------------!
510   FORMAT(' MAXIMAL CONCENTRATION    : ',G16.7,' %, NODE = ',1I8)
511   FORMAT(' MINIMAL CONCENTRATION    : ',G16.7,' %, NODE = ',1I8)
512   FORMAT(' MAXIMAL EVOLUTION        : ',G16.7,'  , NODE = ',1I8)
513   FORMAT(' MINIMAL EVOLUTION        : ',G16.7,'  , NODE = ',1I8)
517   FORMAT(' MAX. CFL FOR SUSPENSION  : ',G16.7,'  , NODE = ',1I8)
      !----------------------------------------------------------------!

!======================================================================!
!======================================================================!

      RETURN
      END SUBROUTINE SUSPENSION_LISTING
C
C#######################################################################
C