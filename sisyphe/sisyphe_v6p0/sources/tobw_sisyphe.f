C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE WAVE FRICTION STRESS. THE FRICTION
!>                COEFFICIENT IS COMPUTED USING SWART FORMULATION (1976).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CF, FW, HN, NPOIN, TOBW, TW, UW, XMVE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, AW, I, KARMAN, KS, PI
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TOB_SISYPHE()

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
!>      <td><center> 5.4                                       </center>
!> </td><td> 01/10/2003
!> </td><td> C. VILLARET (LNHE)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CF
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT QUADRATIQUE (COURAN
!>    </td></tr>
!>          <tr><td>FW
!></td><td><--</td><td>COEFFICIENT DE FROTTEMENT quadratique (houle)
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU AU TEMPS N
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>TOBW
!></td><td><--</td><td>CONTRAINTE TOTALE AU FOND
!>    </td></tr>
!>          <tr><td>TW
!></td><td>--></td><td>PERIODEE DE LA HOULE
!>    </td></tr>
!>          <tr><td>UW
!></td><td>--></td><td>VITESSE ORBITALE DE LA HOULE
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>--></td><td>MASSE VOLUMIQUE DE L'EAU
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TOBW_SISYPHE
     &(TOBW ,CF, FW, UW,TW,HN,NPOIN,XMVE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CF             |-->| COEFFICIENT DE FROTTEMENT QUADRATIQUE (COURAN
C| FW             |<--| COEFFICIENT DE FROTTEMENT quadratique (houle)
C| HN             |-->| HAUTEUR D'EAU AU TEMPS N
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| TOBW           |<--| CONTRAINTE TOTALE AU FOND
C| TW             |-->| PERIODEE DE LA HOULE
C| UW             |-->| VITESSE ORBITALE DE LA HOULE
C| XMVE           |-->| MASSE VOLUMIQUE DE L'EAU
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN
C
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: UW(NPOIN),TW(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XMVE
      DOUBLE PRECISION, INTENT(INOUT) :: TOBW(NPOIN),FW(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
      DOUBLE PRECISION KS,AUX
      DOUBLE PRECISION PI,AW,KARMAN
      PARAMETER (PI=3.141592653589793D0)
      PARAMETER (KARMAN=0.4D0)
C
C-----------------------------------------------------------------------
C
      DO  I=1,NPOIN
C       KS : NIKURADSE COEFFICIENT (TOTAL FRICTION)
        AUX=1.D0+KARMAN*SQRT(2.D0/MAX(CF(I),1.D-10))
        KS=30.D0*MAX(HN(I),1.D-8)*EXP(-AUX)
        AW= UW(I)*TW(I) / (2.D0*PI)
        IF(AW/KS.GT.1.59D0) THEN
          FW(I)=EXP( -6.D0 + 5.2D0 * (AW/KS)**(-0.19D0) )
        ELSE
          FW(I)=0.3D0
        ENDIF
        TOBW(I)=0.5D0 * XMVE * FW(I) * UW(I)*UW(I)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END SUBROUTINE TOBW_SISYPHE
C
C#######################################################################
C