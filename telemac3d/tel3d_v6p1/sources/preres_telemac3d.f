C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!>                THE RESULTS FILE OR TO THE LISTING.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC3D, INTERFACE_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> LT
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::DELTAR DELTAR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::EPAI EPAI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLUER FLUER@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GRADEB GRADEB@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GRAPRD GRAPRD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GRAV GRAV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::H H@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NIT NIT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NONHYD NONHYD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN3 NPOIN3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NTRAC NTRAC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPTBAN OPTBAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PDEPO PDEPO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PH PH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PRIVE PRIVE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RHO0 RHO0@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RHOS RHOS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SORG2D SORG2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SORG3D SORG3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_01 T2_01@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_02 T2_02@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_03 T2_03@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_04 T2_04@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_05 T2_05@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_06 T2_06@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_07 T2_07@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_10 T2_10@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_11 T2_11@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_12 T2_12@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_13 T2_13@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_01 T3_01@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_02 T3_02@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_03 T3_03@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TA TA@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TRAV2 TRAV2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::U U@endlink, 
!> @link DECLARATIONS_TELEMAC3D::U2D U2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UETCAR UETCAR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::V V@endlink, 
!> @link DECLARATIONS_TELEMAC3D::V2D V2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::Z Z@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ZF ZF@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C_0, C_1, DELTAZ, I, I3, IPLAN, LEO, LTT, U_0, U_1, V_0, V_1
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PRERES_TELEMAC3D
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), OS(), PHSTAT(), VERMOY()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!> </td><td> 30/03/04
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>LT
!></td><td>--></td><td>ITERATION NUMBER
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PRERES_TELEMAC3D
     &(LT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| LT             |-->| ITERATION NUMBER
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_PRERES_TELEMAC3D => PRERES_TELEMAC3D
      USE DECLARATIONS_TELEMAC3D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: LT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL LEO
C
      INTEGER LTT,I,IPLAN,I3
      DOUBLE PRECISION DELTAZ,U_0,U_1,V_0,V_1,C_0,C_1
C
      INTRINSIC SQRT,MAX
C
C-----------------------------------------------------------------------
C
C     IS THERE AN OUTPUT OR NOT AT THIS TIMESTEP ?
C
      LEO=.FALSE.
      LTT=(LT/GRAPRD)*GRAPRD
      IF((LT.EQ.LTT.OR.LT.EQ.NIT).AND.LT.GE.GRADEB) LEO=.TRUE.
C
C     NO PRINT OUT, NO OUTPUT TO FILE: EXITS
      IF(.NOT.LEO) GO TO 1000
C
C
C-----------------------------------------------------------------------
C
C VARIABLES FOR 2D OUTPUTS
C
C-----------------------------------------------------------------------
C
C
C=======================================================================
C CELERITY OF WAVES = SQRT(GH) : PUT INTO T2_10
C=======================================================================
C
      IF(LEO.AND.SORG2D(3)) THEN
        DO I=1,NPOIN2
          T2_10%R(I)=SQRT(GRAV*MAX(H%R(I),0.D0))
        ENDDO
      ENDIF
C
C=======================================================================
C FREE SURFACE (= H + ZF) : PUT INTO T2_01
C=======================================================================
C
      IF(LEO.AND.SORG2D(5)) THEN
         CALL OS( 'X=Y+Z   ' , T2_01 , H  , ZF , 0.D0 )
      ENDIF
C
C=======================================================================
C FROUDE NUMBER = U/SQRT(GH) : PUT INTO T2_02
C=======================================================================
C
      IF(LEO.AND.SORG2D(7)) THEN
        DO I=1,NPOIN2
          T2_02%R(I)=SQRT(  (U2D%R(I)**2+V2D%R(I)**2)
     &                     /(GRAV*MAX(H%R(I),1.D-3))   )
        ENDDO
      ENDIF
C
C=======================================================================
C SCALAR DISCHARGE : PUT INTO T2_03
C=======================================================================
C
      IF(LEO.AND.SORG2D(8)) THEN
         CALL OS( 'X=N(Y,Z)' , X=T2_03 , Y=U2D, Z=V2D )
         CALL OS( 'X=XY    ' , X=T2_03 , Y=H )
      ENDIF
C
C=======================================================================
C DISCHARGE ALONG X : PUT INTO T2_04
C=======================================================================
C
      IF(LEO.AND.SORG2D(13)) THEN
         CALL OS( 'X=YZ    ' , X=T2_04 , Y=H , Z=U2D )
      ENDIF
C
C=======================================================================
C DISCHARGE ALONG Y : PUT INTO T2_05
C=======================================================================
C
      IF(LEO.AND.SORG2D(14)) THEN
         CALL OS( 'X=YZ    ' , X=T2_05 , Y=H , Z=V2D )
      ENDIF
C
C=======================================================================
C NORM OF VELOCITY : PUT INTO T2_06
C=======================================================================
C
      IF(LEO.AND.SORG2D(15)) THEN
         CALL OS( 'X=N(Y,Z)' , X=T2_06 , Y=U2D , Z=V2D )
      ENDIF
C
C=======================================================================
C SEDIMENT RELATED VARIABLES THAT ARE ONLY KNOWN AT THE END OF THE
C FIRST TIMESTEP : SETS HERE TO 0 IF LT=0
C=======================================================================
C
      IF(LEO.AND.SORG2D(24).AND.LT.EQ.0) CALL OS( 'X=0     ' , X=EPAI)
      IF(LEO.AND.SORG2D(25).AND.LT.EQ.0) CALL OS( 'X=0     ' , X=FLUER)
      IF(LEO.AND.SORG2D(26).AND.LT.EQ.0) CALL OS( 'X=0     ' , X=PDEPO)
C
C=======================================================================
C FRICTION VELOCITY
C=======================================================================
C
      IF(LEO.AND.SORG2D(31)) THEN
         CALL OS( 'X=SQR(Y)' , X=T2_07 , Y=UETCAR )
      ENDIF
C
C=======================================================================
C SOLID DISCHARGE (ALL 3 DONE IF ONE ASKED)
C=======================================================================
C
      IF(LEO.AND.(SORG2D(32).OR.SORG2D(33).OR.SORG2D(34))) THEN
C        QS IN T2_11, QSX IN T2_12, QSY IN T2_13
C        GIVING A STRUCTURE OF LINEAR 2D VECTOR, LIKE H
         CALL CPSTVC(H,T2_11)
         CALL CPSTVC(H,T2_12)
         CALL CPSTVC(H,T2_13)
C        INITIALISES QSX AND QSY
         CALL OS('X=0     ',X=T2_12)
         CALL OS('X=0     ',X=T2_13)
         DO IPLAN=1,NPLAN-1
           DO I=1,NPOIN2
             I3=I+NPOIN2*(IPLAN-1)
             DELTAZ=Z(I3+NPOIN2)-Z(I3)
C            INTEGRATES U*C ON THE VERTICAL
             U_0=U%R(I3)
             U_1=U%R(I3+NPOIN2)
             V_0=V%R(I3)
             V_1=V%R(I3+NPOIN2)
             C_0=TA%ADR(NTRAC)%P%R(I3)
             C_1=TA%ADR(NTRAC)%P%R(I3+NPOIN2)
             T2_12%R(I)=T2_12%R(I)+DELTAZ*((U_0*C_1+U_1*C_0)/2.D0
     &                                    +(U_1-U_0)*(C_1-C_0)/3.D0)
             T2_13%R(I)=T2_13%R(I)+DELTAZ*((V_0*C_1+V_1*C_0)/2.D0
     &                                    +(V_1-V_0)*(C_1-C_0)/3.D0)
           ENDDO
         ENDDO
C        SOLID DISCHARGE IN M2/S (AS IN SISYPHE, FOR COMPARISON)
         CALL OS('X=CX    ',X=T2_12,C=1.D0/RHOS)
         CALL OS('X=CX    ',X=T2_13,C=1.D0/RHOS)
C        QS AS NORM OF QSX AND QSY
         IF(SORG2D(32)) THEN
           CALL OS( 'X=N(Y,Z)' , X=T2_11 , Y=T2_12 , Z=T2_13 )
         ENDIF
      ENDIF
C
C=======================================================================
C DEPTH-AVERAGED TRACERS (VARIABLES 34 TO 34+NTRAC)
C=======================================================================
C
      IF(NTRAC.GT.0) THEN
      DO I=1,NTRAC
      IF(LEO.AND.SORG2D(34+I)) THEN
        CALL VERMOY(TRAV2%ADR(13+I)%P%R,TRAV2%ADR(13+I)%P%R,
     &              TA%ADR(I)%P%R,TA%ADR(I)%P%R,1,Z,
     &              T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,NPLAN,OPTBAN)
      ENDIF
      ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
C VARIABLES FOR 3D OUTPUTS
C
C-----------------------------------------------------------------------
C
C
C=======================================================================
C HYDROSTATIC PRESSURE
C=======================================================================
C
      IF(NONHYD.AND.LEO.AND.SORG3D(12)) THEN
         CALL PHSTAT(PH%R,DELTAR%R,Z,T3_01%R,T3_02%R,RHO0,GRAV,
     &               NPOIN3,NPOIN2,NPLAN,PRIVE)
      ENDIF
C
C=======================================================================
C
1000  CONTINUE
      RETURN
      END
C
C#######################################################################
C