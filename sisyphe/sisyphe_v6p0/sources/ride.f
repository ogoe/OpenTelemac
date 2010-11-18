C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE DIMENSIONS OF EQUILIBRIUM RIPPLES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @reference "RIPPLE GEOMETRY IN WAVE-DOMINATED ENVIRONMENTS",
!>              WIBERG, P.L. & C.K. HARRIS. 1994. JOURNAL OF
!>              GEOPHYSICAL RESEARCH, 99 (C1): 775-789.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ACLADM, GRAV, KS, KSPRATIO, NPOIN, TW, UNORM, UW, VCE, XMVE, XMVS
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A0, AA, AI, ALPHA, BB, CC, DD, DHRA, ETA, HRA, I, KSP, LAMBDA, LRA, LRO, M, PI, S, UC, VAR1, WH1, WH2, WH3, ZERO
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
!>      <td><center> 6.0                                       </center>
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
!>          <tr><td>ACLADM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DM
!></td><td>--></td><td>DIAMTRE MOYEN DU SEDIMENT
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>KS
!></td><td><--</td><td>COEFFICIENT DE RUGOSITE
!>    </td></tr>
!>          <tr><td>KSPRATIO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS
!>    </td></tr>
!>          <tr><td>Q
!></td><td>--></td><td>DEBIT MOYEN
!>    </td></tr>
!>          <tr><td>TW
!></td><td>--></td><td>PERIODE DE HOULE
!>    </td></tr>
!>          <tr><td>UNORM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UW
!></td><td>--></td><td>COURANT ORBITAL
!>    </td></tr>
!>          <tr><td>VCE
!></td><td>--></td><td>VISCOSITE DE L'EAU
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>--></td><td>MASSE VOLUMIQUE DE L'EAU
!>    </td></tr>
!>          <tr><td>XMVS
!></td><td>--></td><td>MASSE VOLUMIQUE DU SEDIMENT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE RIDE
     & (KS,TW,UW,UNORM,GRAV,XMVE,XMVS,VCE,NPOIN,KSPRATIO,ACLADM)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ACLADM         |---| 
C| DM             |-->| DIAMTRE MOYEN DU SEDIMENT
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR
C| HN             |-->| HAUTEUR D'EAU
C| KS             |<--| COEFFICIENT DE RUGOSITE
C| KSPRATIO       |---| 
C| NPOIN          |-->| NOMBRE DE POINTS
C| Q             |-->| DEBIT MOYEN
C| TW             |-->| PERIODE DE HOULE
C| UNORM          |---| 
C| UW             |-->| COURANT ORBITAL
C| VCE            |-->| VISCOSITE DE L'EAU
C| XMVE           |-->| MASSE VOLUMIQUE DE L'EAU
C| XMVS           |-->| MASSE VOLUMIQUE DU SEDIMENT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER I,NPOIN
      DOUBLE PRECISION KS(NPOIN)
C     IF EXTENDED GRANULOMETRY DM(NPOIN)
      DOUBLE PRECISION  GRAV,XMVE,XMVS, VCE
      DOUBLE PRECISION UNORM(NPOIN), UW(NPOIN), TW(NPOIN)
C
      DOUBLE PRECISION PI, ZERO,AI
C
      DOUBLE PRECISION ETA, LAMBDA
C
      DOUBLE PRECISION AA,BB,CC,DD
      DOUBLE PRECISION ALPHA,S,M,A0
      DOUBLE PRECISION WH1,WH2,WH3
      DOUBLE PRECISION VAR1,DHRA,LRA,HRA,LRO
      DOUBLE PRECISION UC,KSP
      DOUBLE PRECISION, INTENT(IN) :: KSPRATIO
      DOUBLE PRECISION, INTENT(IN) :: ACLADM(NPOIN)
C
C---------------------------------------------------------------------
C
      PI=4.D0*DATAN(1.D0)
      ZERO=1.D-6
C
C     COEFFICIENTS
C
      WH1=0.095D0
      WH2=0.442D0
      WH3=2.28D0
      AA=(WH2+1.D0)/2.D0/WH1
      BB=AA*AA-WH3/WH1
      CC=1.D0/WH1
C
C     LOOP ON THE NODES
C
      DO I=1,NPOIN
C
C       SKIN FRICTION
C
        KSP = KSPRATIO * ACLADM(I)
        AI  = ACLADM(I)*GRAV*(XMVS-XMVE)/XMVE
C
C       MOBILITY NUMBER
C
        M=UW(I)**2/AI
C
        IF(M.LT.1.69D0) THEN
C
          KS(I)=KSP
C
        ELSE
C
C         WIBERG AND HARRIS
C
          A0=UW(I)*TW(I)/(2.D0*PI)
          S=ACLADM(I)*SQRT(AI)/4.D0/VCE
          LRA=535.D0*ACLADM(I)
CJMB***************************************************
CJMB LINE OF CODE MOVED SO ALPHA COMPUTED BEFORE VAR1
CJMB TANNAKA AND DANG (1996)
          UC=UNORM(I)
          IF(UW(I).GT.ZERO) THEN
            ALPHA=(TANH(0.3D0*S**(2.D0/3.D0)))**2.5D0
            ALPHA=1.D0+0.81D0*ALPHA*(UC/UW(I))**1.9D0
          ELSE
            ALPHA=1.D0
          ENDIF
CJMB*******************************************************

          VAR1=LOG(ALPHA*2.D0*A0/LRA)
          DD=MAX((BB-CC*VAR1),0.D0)
          DHRA=EXP(AA-SQRT(DD))
          HRA=ALPHA*2.D0*A0/DHRA

C
          IF(DHRA.LE.20.D0) THEN
C           ORBITAL RIPPLES DHRA
            LRO=0.62D0*2.D0*A0*ALPHA
            LAMBDA=LRO
            ETA=0.17D0*LAMBDA
          ELSEIF(DHRA.LE.100.D0) THEN
C           SUB ORBITAL RIPPLES 20
            LRO=0.62D0*2.D0*A0*ALPHA
            VAR1=(LOG(DHRA)-LOG(100.D0))/(LOG(20.D0)-LOG(100.D0))
            VAR1=LOG(LRA)+VAR1*(LOG(LRO)-LOG(LRA))
            LAMBDA=EXP(VAR1)
            VAR1=LOG(ALPHA*2.D0*A0/LAMBDA)
CCV 25/05               ETA=ALPHA*2.D0*A0/EXP(AA-SQRT(BB-CC*VAR1))
            DD=MAX((BB-CC*VAR1),0.D0)
            ETA=ALPHA*2.D0*A0/EXP(AA-SQRT(DD))
          ELSE
C           ANORBITAL RIPPLES DHRA>100
C           LAMBDA NOT USED HERE BUT KEPT FOR OTHER FORMULATIONS
C           LAMBDA=LRA
            ETA=HRA
          ENDIF
C
          KS(I)=MAX(ETA,KSP)
C
        ENDIF
C
      ENDDO
C
C---------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C