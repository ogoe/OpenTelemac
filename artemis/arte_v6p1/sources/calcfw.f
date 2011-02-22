C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE BOTTOM FRICTION COEFFICIENT FW
!>                FOR SANDY BOTTOMS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_ARTEMIS
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, CG, DIAM50, DIAM90, ENTREG, ENTRUG, FFW, FORMFR, GRAV, H, HMU, I, K, MVEAU, MVSED, NPOIN, OMEGA, REGIDO, RICOEF, VISCO
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AEX, DIAMAD, KS1, KS2, KSRUGO, LL1, PSI, RAP1, RAP2, REGIME, RH, RMAX1, RMAX2, RMIN1, RMIN2, RS, SP, TAUCR, TETACR, UEX
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CALCFW
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BERKHO()

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
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 02/06/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12; D. PAUGAM (1996 PLACEMENT) </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td>                                                         </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> LINKED TO BIEF 5.0                                      </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DIAM50
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DIAM90
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ENTREG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ENTRUG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FFW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FORMFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>H
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HMU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>I
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>K
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MVEAU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MVSED
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OMEGA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>REGIDO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RICOEF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VISCO
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CALCFW
     &(I,H,C,CG,K,HMU,
     & NPOIN,OMEGA,GRAV,
     & VISCO,DIAM90,DIAM50,MVSED,MVEAU,
     & FORMFR,REGIDO,RICOEF,
     & ENTREG,ENTRUG,FFW)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |---| 
C| CG             |---| 
C| DIAM50         |---| 
C| DIAM90         |---| 
C| ENTREG         |---| 
C| ENTRUG         |---| 
C| FFW            |---| 
C| FORMFR         |---| 
C| GRAV           |---| 
C| H             |---| 
C| HMU            |---| 
C| I             |---| 
C| K             |---| 
C| MVEAU          |---| 
C| MVSED          |---| 
C| NPOIN          |---| 
C| OMEGA          |---| 
C| REGIDO         |---| 
C| RICOEF         |---| 
C| VISCO          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_ARTEMIS, EX_CALCFW => CALCFW
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPOIN,I
      INTEGER REGIME, REGIDO
      INTEGER FORMFR
C
      DOUBLE PRECISION C(NPOIN),CG(NPOIN)
      DOUBLE PRECISION K(NPOIN)
      DOUBLE PRECISION HMU(NPOIN)
C
      DOUBLE PRECISION H(NPOIN)
      DOUBLE PRECISION GRAV,OMEGA
C
      DOUBLE PRECISION VISCO, DIAM90, DIAM50
      DOUBLE PRECISION MVSED, MVEAU, RICOEF
      DOUBLE PRECISION KSRUGO, AEX, UEX, FFW
      DOUBLE PRECISION RAP1, RAP2
      DOUBLE PRECISION LL1
      DOUBLE PRECISION RMAX1, RMIN1, RMAX2, RMIN2
      DOUBLE PRECISION SP, PSI, KS1, RH, RS, KS2
      DOUBLE PRECISION DIAMAD, TETACR, TAUCR
C
      LOGICAL ENTREG,ENTRUG
C
C
      INTRINSIC ABS,EXP
C
C
C  COMPUTES FW OVER THE WHOLE DOMAIN
C--------------------------------------------------------
      RMAX1 = 0.D0
      RMIN1 = 1.D7
      RMAX2 = 0.D0
      RMIN2 = 1.D7
C
      KSRUGO = 3.D0 * DIAM90
C
C COMPUTES AEX AND UEX AT EACH GRID NODE
C THUS THE HYDRAULIC REGIME WILL ALSO BE GIVEN AT EACH
C GRID NODE
C
C
        AEX = HMU(I)/(2.D0*SINH(K(I)*H(I)))
        UEX = OMEGA * AEX
        RAP1 = (UEX*AEX)/VISCO
        RAP2 = AEX/KSRUGO
C
C   COMPARES AGAINST THE MAXIMUM REYNOLDS NUMBER
C   AND THE EXCURSION RATIO ON ROUGHNESS
C
        IF (RAP1 .GT. RMAX1) THEN
            RMAX1 = RAP1
          ENDIF
C
        IF (RAP1 .LT. RMIN1) THEN
             RMIN1 = RAP1
          ENDIF
C
        IF (RAP2 .GT. RMAX2) THEN
              RMAX2 = RAP2
           ENDIF
C
        IF (RAP2 .LT. RMIN2) THEN
              RMIN2 = RAP2
           ENDIF
C
C-----------------------------------------------------------------
C DETERMINES THE HYDRAULIC REGIME
C-----------------------------------------------------------------
C
      IF (ENTREG) THEN
         REGIME = REGIDO
      ELSE
C
C     INITIALIZES THE REGIME (SETS TO 0) BEFORE EACH NEW ITERATION
C
         REGIME = 0
         LL1 = 0.D0
C
         IF (RAP1 .LE. 10250.D0) THEN
            LL1 = 0.0322D0*RAP1+3.33D0
            IF (RAP2 .GE. LL1) THEN
              REGIME = 1
C             WRITE(*,*) 'LE REGIME HYDRAULIQUE EST LAMINAIRE'
C             WRITE(*,*) 'THE HYDRAULIC REGIME IS LAMINAR'
            ENDIF
         ENDIF
C
         IF (RAP1 .GE. 3.D4) THEN
            LL1 = 0.009792D0*RAP1+208.33D0
            IF (RAP2 .GE. LL1) THEN
              REGIME = 2
C             WRITE(*,*) 'LE REGIME HYDRAULIQUE EST TURBULENT LISSE'
C             WRITE(*,*) 'THE HYDRAULIC REGIME IS SMOOTH TURBULENT'
            ENDIF
         ENDIF
C
         IF (RAP1 .GE. 5.D3 .AND. RAP1 .LE. 2.D4) THEN
            LL1 = 0.026D0*RAP1-12.D0
            IF (RAP2 .LE. LL1) THEN
              REGIME = 3
C             WRITE(*,*) 'LE REGIME HYDRAULIQUE EST TURBULENT RUGUEUX'
C             WRITE(*,*) 'THE HYDRAULIC REGIME IS ROUGH TURBULENT'
            ENDIF
         ENDIF
C
         IF (RAP1 .GT. 2.D4) THEN
            LL1 = 0.00099D0*RAP1+30.30D0
            IF (RAP2 .LE. LL1) THEN
              REGIME = 3
C             WRITE(*,*) 'LE REGIME HYDRAULIQUE EST TURBULENT RUGUEUX'
C             WRITE(*,*) 'THE HYDRAULIC REGIME IS ROUGH TURBULENT'
            ENDIF
         ENDIF
C
         IF (REGIME .EQ. 0) THEN
            REGIME = 3
C           WRITE(*,*) 'LE REGIME EST TURBULENT RUGEUX'
C             WRITE(*,*) 'THE HYDRAULIC REGIME IS ROUGH TURBULENT'
         ENDIF
C
      ENDIF
C
C
C--------------------------------------------------------------
C COMPUTES FW: THE COEFFICIENT OF FRICTION
C--------------------------------------------------------------
C
      IF (REGIME .EQ. 1) THEN
         FFW = 2.D0*(((AEX*UEX)/VISCO)**(-0.5D0))
      ENDIF
C
      IF (REGIME .EQ. 2) THEN
         FFW = 0.09*(((AEX*UEX)/VISCO)**(-0.2D0))
      ENDIF
C
      IF (REGIME .EQ. 3) THEN
C
         IF (ENTRUG) THEN
C
             KSRUGO = 3.D0 * DIAM90
C
         ELSE
C
            SP = MVSED/MVEAU
            PSI = (UEX**2.D0)/((SP-1.D0)*GRAV*DIAM50)
C
            IF (PSI .LT. 250.D0) THEN
                 KS1 = 3.D0*DIAM90
C
                 IF (PSI .LT. 10.D0) THEN
                      RH = 0.22D0*AEX
                      RS = 0.18D0
                 ELSE
                      RH = AEX*(2.4D-13)*((250.D0-PSI)**5.D0)
                      RS = (2.D-7)*((250.D0-PSI)**2.5D0)
                 ENDIF
C
                 KS2 = 20.D0*RICOEF*RH*RS
C
C  RICOEF = 1     RIPPLES ONLY
C  RICOEF = 0.7D0 RIPPLES AND SAND WAVES
C
            ELSE
                 KS1 = 3.D0*(0.04D0*PSI-9.D0)*DIAM90
                 KS2 = 0.D0
                 IF (KS1 .LT. 0.01D0) THEN
                      KS1 = 3.D0*DIAM90
                 ENDIF
            ENDIF
C
            KSRUGO = KS1+KS2
            KS1 = (AEX/KSRUGO)
C
         ENDIF
C
         FFW = EXP(-6.D0+5.2D0*((AEX/KSRUGO)**(-0.19D0)))
         IF (FFW .GT. 0.3D0) THEN
             FFW = 0.3D0
         ENDIF
      ENDIF
C
      IF (REGIME .EQ. 4) THEN
C
C     TRANSIENT STATE NOT TAKEN INTO ACCOUNT
C
         IF (ENTRUG) THEN
C
             KSRUGO = 3.D0 * DIAM90
C
         ELSE
C
            DIAMAD = (((MVSED/MVEAU)-1.D0)*GRAV)/(VISCO**2.D0)
            DIAMAD = DIAMAD**(1/3)
            DIAMAD = DIAMAD * DIAM50
C
            TETACR = 0.14D0*(DIAMAD**(-0.64D0))
C
            TAUCR = TETACR*(MVSED-MVEAU)*GRAV*DIAM50
C
            KSRUGO = (3.D0*DIAM90)+((3.3D0*VISCO)/
     &               ((TAUCR/MVEAU)**0.5D0))
C
         ENDIF
C
         FFW = EXP(-6.D0+5.2D0*((AEX/KSRUGO)**(-0.19D0)))
         IF (FFW .GT. 0.3D0) THEN
              FFW = 0.3D0
         ENDIF
C
      ENDIF
C
      RETURN
      END
C
C#######################################################################
C