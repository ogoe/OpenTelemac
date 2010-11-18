C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE PARAMETERS OF THE SLOPE EFFECT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BETA, BETA2, CALFA, COEF, CTETA, DEVIA, DM, DZFDX, DZFDY, GRAV, IELMT, KENT, LIQBOR, MASKEL, MESH, MSK, NPOIN, NPTFR, PHISED, PI, S, SALFA, SLOPEFF, STETA, TOB, U2D, UCMOY, UNSV2D, V2D, XMVE, XMVS, ZF
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
!>    </th><td> AA, BB, C, C1, CALPHA, CPSI, CZETA, DZF, I, K, NORM, SALPHA, SPSI, SURBETA2, SZETA, TANPHI, TT1, ZETA
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_EFFPNT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PARCOM(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_SOLIDISCHARGE()

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
!>      <td><center> 5.1                                       </center>
!> </td><td> 11/09/1995
!> </td><td> E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BETA2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CALFA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COEF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CTETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEVIA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DZFDX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DZFDY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELMT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIQBOR
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
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PHISED
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SALFA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SLOPEFF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>STETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UCMOY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_EFFPNT ! (_IMP_)
     & (MASKEL,LIQBOR,S,ZF,U2D,V2D,UCMOY,NPOIN,NPTFR,IELMT,KENT,
     &  BETA,PI,MSK,MESH,DZFDX,DZFDY,CTETA,STETA,
     &  COEF,CALFA,SALFA,SLOPEFF,PHISED,DEVIA,BETA2,
     &  TOB,XMVS,XMVE,DM,GRAV,UNSV2D)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BETA           |---| 
C| BETA2          |---| 
C| CALFA          |---| 
C| COEF           |---| 
C| CTETA          |---| 
C| DEVIA          |---| 
C| DM             |---| 
C| DZFDX          |---| 
C| DZFDY          |---| 
C| GRAV           |---| 
C| IELMT          |---| 
C| KENT           |---| 
C| LIQBOR         |---| 
C| MASKEL         |---| 
C| MESH           |---| 
C| MSK            |---| 
C| NPOIN          |---| 
C| NPTFR          |---| 
C| PHISED         |---| 
C| PI             |---| 
C| S             |---| 
C| SALFA          |---| 
C| SLOPEFF        |---| 
C| STETA          |---| 
C| TOB            |---| 
C| U2D            |---| 
C| UCMOY          |---| 
C| UNSV2D         |---| 
C| V2D            |---| 
C| XMVE           |---| 
C| XMVS           |---| 
C| ZF             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,EX_BEDLOAD_EFFPNT => BEDLOAD_EFFPNT
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,LIQBOR,S,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF, U2D,V2D, UCMOY, TOB
      INTEGER,          INTENT(IN)    :: NPOIN, NPTFR, IELMT, KENT
      INTEGER,          INTENT(IN)    :: SLOPEFF,DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: BETA, PI, PHISED, BETA2
      DOUBLE PRECISION, INTENT(IN)    :: XMVS, XMVE, GRAV, DM
      LOGICAL,          INTENT(IN)    :: MSK
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: DZFDX, DZFDY
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CTETA,STETA
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: COEF, CALFA, SALFA


      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: I, K
      DOUBLE PRECISION :: C,ZETA,C1,CALPHA,SALPHA,AA,BB
      DOUBLE PRECISION :: CPSI,SPSI,DZF,TANPHI,CZETA,SZETA,SURBETA2
      DOUBLE PRECISION :: NORM ,TT1
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
C     DETERMINES COS AND SIN TETA
C     TETA = ANGLE OF THE FLOW WITH OX
!
      DO I=1,NPOIN
        IF(UCMOY%R(I).GE.1.D-12) THEN
          CTETA%R(I)=U2D%R(I)/UCMOY%R(I)
          STETA%R(I)=V2D%R(I)/UCMOY%R(I)
        ELSE
          CTETA%R(I)=1.D0
          STETA%R(I)=0.D0
        ENDIF
      ENDDO
!
!----------------------------------------------------------------------
!
C     COMPUTES THE SLOPE  : D(ZF)/DX ET D(ZF)/DY (AT THE NODES)
!
      CALL VECTOR(DZFDX, '=', 'GRADF          X',IELMT,1.D0,ZF,S,S,
     &            S,S,S,MESH,MSK,MASKEL)
      CALL VECTOR(DZFDY, '=', 'GRADF          Y',IELMT,1.D0,ZF,S,S,
     &            S,S,S,MESH,MSK,MASKEL)
C
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(DZFDX,2,MESH)
        CALL PARCOM(DZFDY,2,MESH)
      ENDIF
C
      CALL OS('X=XY    ',X=DZFDX,Y=UNSV2D)
      CALL OS('X=XY    ',X=DZFDY,Y=UNSV2D)
!
!======================================================================
!
C     COMPUTES THE SOLID TRANSPORT ANGLE ALFA = TETA + DEVIATION
!
C     1 : KOCH AND FLOKSTRA
!
      IF(DEVIA==1) THEN
!
      C = 2.D0*(XMVS-XMVE)*GRAV*DM/3.D0
      DO I=1,NPOIN
        TT1=C/MAX(TOB%R(I),1.D-10)
        AA=STETA%R(I)-TT1*DZFDY%R(I)
        BB=CTETA%R(I)-TT1*DZFDX%R(I)
        NORM=MAX(SQRT(AA**2+BB**2),1.D-10)
        SALFA%R(I)=AA/NORM
        CALFA%R(I)=BB/NORM
      ENDDO
!
C     2 : TALMON ET AL. JHR 1995 33(4)
!
      ELSEIF(DEVIA==2) THEN
!
      SURBETA2=1.D0/BETA2
      C = (XMVS-XMVE)*GRAV*DM*SURBETA2**2
      DO I=1,NPOIN
        TT1=SQRT(C/MAX(TOB%R(I),1.D-10))
        AA=STETA%R(I)-TT1*DZFDY%R(I)
        BB=CTETA%R(I)-TT1*DZFDX%R(I)
        NORM=MAX(SQRT(AA**2+BB**2),1.D-10)
        SALFA%R(I)=AA/NORM
        CALFA%R(I)=BB/NORM
      ENDDO
!
      ENDIF
!
!======================================================================
!
C     COMPUTES THE COEFFICIENT TO TAKE THE SLOPE EFFECT ON THE MAGNITUDE
C     OF THE SOLID TRANSPORT INTO ACCOUNT
!
C     METHOD 1 (EMPIRICAL)
!
      IF(SLOPEFF==1) THEN
!
        DO I=1,NPOIN
          COEF%R(I)=MAX(0.D0,
     &    1.D0-BETA*(DZFDX%R(I)*CTETA%R(I)+DZFDY%R(I)*STETA%R(I)) )
        ENDDO
!
C     METHOD 2 : SOULSBY 1997 DYNAMICS OF MARINE SANDS P107-108
!
      ELSEIF(SLOPEFF.EQ.2) THEN
C
        TANPHI = TAN(PHISED*PI/180.D0)
C
        DO I=1,NPOIN
C
C         COSINE AND SINE OF THE DIRECTION OF THE SLOPE
          DZF=SQRT(DZFDX%R(I)**2+DZFDY%R(I)**2)
          IF(DZF.GT.1.D-12) THEN
            CALPHA=DZFDX%R(I)/DZF
            SALPHA=DZFDY%R(I)/DZF
          ELSE
            CALPHA=1.D0
            SALPHA=0.D0
          ENDIF
C
C         ZETA: ANGLE OF THE SLOPE WITH HORIZONTAL (BETA IN SOULSBY)
          ZETA=ATAN(DZF)
          CZETA=COS(ZETA)
          SZETA=SIN(ZETA)
C
C         PSI: ANGLE OF THE CURRENT WITH THE SLOPE DIRECTION
C         PSI=TETA%R(I)-ALPHA
          CPSI=CTETA%R(I)*CALPHA+STETA%R(I)*SALPHA
          SPSI=STETA%R(I)*CALPHA-CTETA%R(I)*SALPHA
          C1=(CZETA*TANPHI)**2-(SPSI*SZETA)**2
          COEF%R(I)=MAX((CPSI*SZETA+SQRT(MAX(C1,0.D0)))/TANPHI,0.D0)
          COEF%R(I)=MAX(COEF%R(I),0.D0)
C
        ENDDO
!
      ENDIF
!
! ********************************************************************* !
C     V - TREATS THE BOUNDARY POINTS WITH IMPOSED RATE                  !
C         QS IS NOT MODIFIED WHEN SPECIFIED BY THE USER                 !
! ********************************************************************* !
!
      DO K = 1 , NPTFR
         IF (LIQBOR%I(K) == KENT) COEF%R(MESH%NBOR%I(K)) = 1.D0
C                           R.K. MAY 2007
C                           KSORT = 4
         IF (LIQBOR%I(K) == 4) COEF%R(MESH%NBOR%I(K)) = 1.D0
      ENDDO
!
!======================================================================
!======================================================================
!
      RETURN
      END SUBROUTINE BEDLOAD_EFFPNT
C
C#######################################################################
C