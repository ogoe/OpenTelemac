C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODELS CONSOLIDATION OF THE MUDDDY BED.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  THE EXPERIMENTAL RELATIONSHIPS K(E) AND SIGMA(E)
!>         ARE BASED ON THE WORK OF BEEN AND SILLS.
!>         THE USER CAN CHOOSE DIFFERENT RELATIONSHIPS.

!>  @reference BEEN AND SILLS. GEOTECHNIQUE, 31, N4. 1981.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>INTERFACE_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CFMAX, CHARGE, DTC, EPAI, GRAV, IVIDE, NPF, NPFMAX, NPOIN2, RHOS, SIGMA, TRA01
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DPE, DSIG1, DSIGMA, IPF, IPOIN, NDEB, PE
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_TASSEM
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BISSEL(), COEF()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CONDIS(), FONVAS()

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
!> </td><td> 13/05/92
!> </td><td> C LE NORMANT (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CFMAX
!></td><td>--></td><td>CONCENTRATION(G/L) OF CONSOLIDATED MUD
!>    </td></tr>
!>          <tr><td>CHARGE
!></td><td><--</td><td>WEIGHT SUPPORTED BY SOLID SKELETON
!>    </td></tr>
!>          <tr><td>DTC
!></td><td>--></td><td>TIME STEP OF CONSOLIDATION  MODEL
!>    </td></tr>
!>          <tr><td>EPAI
!></td><td>--></td><td>THICKNESS OF SOLID FRACTION OF THE BED LAYER
!>                  (EPAI=DZ/(1+IVIDE), DZ BED LAYER THICKNESS)
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>ACCELERATION OF GRAVITY
!>    </td></tr>
!>          <tr><td>IVIDE
!></td><td><-></td><td>VOID RATIO
!>    </td></tr>
!>          <tr><td>NPF
!></td><td>--></td><td>NUMBER OF POINTS WITHIN THE BED
!>    </td></tr>
!>          <tr><td>NPFMAX
!></td><td>--></td><td>MAXIMUM NUMBER OF HORIZONTAL PLANES
!>                  WITHIN THE BED
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF POINTS OF 2D MESH
!>    </td></tr>
!>          <tr><td>RHOS
!></td><td>--></td><td>SEDIMENT DENSITY
!>    </td></tr>
!>          <tr><td>SIGMA
!></td><td><--</td><td>EFFECTIVE STRESS
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td><--</td><td>WORK ARRAY
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                      SUBROUTINE TASSEM
     & ( IVIDE , EPAI ,
     &   NPOIN2, NPFMAX, NPF  ,
     &   GRAV  , RHOS  , DTC  , CFMAX ,
     &   TRA01 , SIGMA , CHARGE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CFMAX          |-->| CONCENTRATION(G/L) OF CONSOLIDATED MUD
C| CHARGE         |<--| WEIGHT SUPPORTED BY SOLID SKELETON
C| DTC            |-->| TIME STEP OF CONSOLIDATION  MODEL
C| EPAI           |-->| THICKNESS OF SOLID FRACTION OF THE BED LAYER
C|                |   | (EPAI=DZ/(1+IVIDE), DZ BED LAYER THICKNESS)
C| GRAV           |-->| ACCELERATION OF GRAVITY
C| IVIDE          |<->| VOID RATIO
C| NPF            |-->| NUMBER OF POINTS WITHIN THE BED
C| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES
C|                |   | WITHIN THE BED
C| NPOIN2         |-->| NUMBER OF POINTS OF 2D MESH
C| RHOS           |-->| SEDIMENT DENSITY
C| SIGMA          |<--| EFFECTIVE STRESS
C| TRA01          |<--| WORK ARRAY
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
       USE INTERFACE_TELEMAC3D, EX_TASSEM => TASSEM
       IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
       INTEGER, INTENT(IN)             :: NPOIN2,NPFMAX
       INTEGER, INTENT(IN)             :: NPF(NPOIN2)
       DOUBLE PRECISION, INTENT(IN)    :: EPAI(NPFMAX-1,NPOIN2)
       DOUBLE PRECISION, INTENT(INOUT) :: IVIDE(NPFMAX,NPOIN2)
       DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPFMAX,6)
       DOUBLE PRECISION, INTENT(INOUT) :: SIGMA(NPFMAX)
       DOUBLE PRECISION, INTENT(INOUT) :: CHARGE(NPFMAX)
       DOUBLE PRECISION, INTENT(IN)    :: RHOS,GRAV,DTC,CFMAX
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
       DOUBLE PRECISION DSIGMA,DSIG1,PE,DPE
       INTEGER  IPOIN,IPF,NDEB
!
       INTRINSIC DLOG
!
!=======================================================================
!
C   ----- LOOP ON 2D MESH POINTS-----
!
       DO IPOIN=1,NPOIN2
         IF (NPF(IPOIN).GE.5) THEN
!
           CHARGE(NPF(IPOIN))=0.D0
!
C   ----- COMPUTES EQUATION COEFFICIENTS -----
!
          DO IPF=1,NPF(IPOIN)
!
C   ----- THE FOLLOWING LINES CAN BE MODIFIED BY USER -----
!
C     BED PERMEABILITY: LOG K=0.24*E-7.86
C                       PE=K/(1+IVIDE)) :
            PE=10.D0**(0.24D0*IVIDE(IPF,IPOIN)-7.86D0)/
     &                 (1.D0+IVIDE(IPF,IPOIN))
!
C     DERIVATIVE OF PE RELATIVE TO IVIDE :
!
            DPE=PE*(0.24D0*DLOG(10.D0)-1.D0/(1.D0+IVIDE(IPF,IPOIN)))
!
C     EFFECTIVE STRESS: SIGMA=900/(E-2.5)
!
            SIGMA(IPF)=900.D0/(IVIDE(IPF,IPOIN)-2.5D0)
!
C     DERIVATIVE OF SIGMA RELATIVE TO IVIDE :
!
            DSIGMA=-900.D0/((IVIDE(IPF,IPOIN)-2.5D0)**2)
!
C   **** END OF USER'S MODIFICATIONS
!
!
            IF (IPF.EQ.1) DSIG1=DSIGMA
            TRA01(IPF,1)=((RHOS/1000.D0)-1.D0)*DPE
            TRA01(IPF,2)=-1.D0/(1000.D0*GRAV)*DSIGMA*PE
!
          END DO
!
C   ------ NON LINEAR CHARGE INCREASE -----
!
           DO IPF=NPF(IPOIN)-1,1,-1
!
C     WEIGHT SUPPORTED BY SOLID SKELETON
!
           CHARGE(IPF)=(2650.D0-1000.D0)*9.81D0
     &     * EPAI(IPF,IPOIN)+CHARGE(IPF+1)
           END DO
!
           NDEB=1
           DO IPF=2,NPF(IPOIN)
           IF ((SIGMA(IPF)-CHARGE(IPF)).GT.1.D-3) THEN
           NDEB=IPF
           ELSE
!
C ARGH, OUT OF A LOOP
!
          GOTO 50
            ENDIF
           END DO
50         CONTINUE
!
          IF (NDEB.LT.(NPF(IPOIN)-1)) THEN
!
C   ------COMPUTES THE BISSEL COEFFICIENTS -----
!

          CALL COEF(IVIDE(1,IPOIN), EPAI(1,IPOIN), TRA01,
     &              NPFMAX        , NPF(IPOIN)   , NDEB ,
     &              RHOS          , GRAV         , DTC  , DSIG1)
!
C   ----- SOLVES GIBSON EQUATION -----
!
          CALL BISSEL(IVIDE(1,IPOIN), TRA01, NPFMAX, NPF(IPOIN),NDEB)
!
          ENDIF
!
         ENDIF
       END DO
!
      RETURN
      END SUBROUTINE TASSEM

C
C#######################################################################
C