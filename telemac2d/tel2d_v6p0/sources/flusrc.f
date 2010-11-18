C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES FLUXES DUE TO NON CENTERED SOURCES TERMS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AIRS, EPS, FLUSCE, G, IEL1, IEL2, ISEGIN, NPOIN, NSEG, VNOIN, W, X, Y, ZF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A1, A2, ALPHA, CI, CI2, CJ, CJ2, CT, CT2, D1, DIJ, FE, GE, HI, HJ, INDIC, PROD, PSA, PSA1, PSA2, RI, RJ, RLAMB0, RLAMBI, RLAMBJ, RLAMBM, RLAMBP, RNORM, T11, T12, T21, T22, T31, T32, TS11, TS12, TS21, TS22, TS31, TS32, UI, UJ, UM, UN, UT, VI, VJ, VM, VT, XGI, XGJ, XN, YGI, YGJ, YN, ZF1, ZF2
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FLUROE()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 19/08/1994
!> </td><td> N.GOUTAL
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>3
!></td><td>--></td><td>NOMBRE DE FONCTIONS INCONNUES DU PROBLEME
!>    </td></tr>
!>          <tr><td>AIRS
!></td><td>--></td><td>AIRES DES CELLULES DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>EPS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUX
!></td><td><--</td><td>TABLEAU DES FLUX DE TYPE ROE
!>    </td></tr>
!>          <tr><td>G
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IEL1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IEL2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ISEGIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NDIM
!></td><td>--></td><td>DIMENSION DE L'ESPACE.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NOMBRE TOTAL DE SEGMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>VNOIN
!></td><td>--></td><td>NORMALE DU SEGMENT INTERNE
!>                  (2 PREMIERES COMPOSANTES) ET
!>                  LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
!>    </td></tr>
!>          <tr><td>W
!></td><td>--></td><td>VARIABLES CONSERVATIVES DU PB A L'INSTANT N
!>    </td></tr>
!>          <tr><td>WINF
!></td><td>--></td><td>FLUX FRONTIERE ENTREE-SORTIE SI STAT.
!>                  WINF INITIAL SI INSTAT.
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Y
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>---</td><td>FOND.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLUSRC
     &(IEL1,IEL2,ISEGIN,VNOIN,W,FLUSCE,X,Y,AIRS,NPOIN,NSEG,ZF,EPS,G)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| 3             |-->| NOMBRE DE FONCTIONS INCONNUES DU PROBLEME
C| AIRS           |-->| AIRES DES CELLULES DU MAILLAGE.
C| EPS            |---| 
C| FLUSCE         |---| 
C| FLUX           |<--| TABLEAU DES FLUX DE TYPE ROE
C| G             |---| 
C| IEL1           |---| 
C| IEL2           |---| 
C| ISEGIN         |---| 
C| NDIM           |-->| DIMENSION DE L'ESPACE.
C| NPOIN          |---| 
C| NSEG           |-->| NOMBRE TOTAL DE SEGMENTS DU MAILLAGE
C| VNOIN          |-->| NORMALE DU SEGMENT INTERNE
C|                |   | (2 PREMIERES COMPOSANTES) ET
C|                |   | LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
C| W             |-->| VARIABLES CONSERVATIVES DU PB A L'INSTANT N
C| WINF           |-->| FLUX FRONTIERE ENTREE-SORTIE SI STAT.
C|                |   | WINF INITIAL SI INSTAT.
C| X             |---| 
C| Y             |---| 
C| ZF             |---| FOND.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPOIN,NSEG,ISEGIN,IEL1,IEL2
      DOUBLE PRECISION, INTENT(IN)    :: G,EPS,VNOIN(3,NSEG),ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: AIRS(NPOIN),X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUSCE(3,NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER INDIC(2)
C
      DOUBLE PRECISION XGI, YGI, XGJ, YGJ, DIJ, A1, A2
      DOUBLE PRECISION D1,HI,UI,VI,HJ,VJ,UJ,XN,YN,RNORM
      DOUBLE PRECISION CT2,CT,RLAMB0,RLAMBM,ALPHA,CI2
      DOUBLE PRECISION CJ,CJ2,RLAMBJ,PROD,RLAMBI,RLAMBP
      DOUBLE PRECISION RI,RJ,UT,VT,CI,UN
      DOUBLE PRECISION T11(3),T21(3),T31(3),TS11(3),TS21(3),TS31(3)
      DOUBLE PRECISION T12(3),T22(3),T32(3),TS12(3),TS22(3),TS32(3)
      DOUBLE PRECISION GE(3),FE(3),ZF1,ZF2,PSA1,PSA2,PSA,UM,VM
C
      INTRINSIC MIN,MAX
C
C-----------------------------------------------------------------------
C
C------
C 1. INITIALISATIONS
C------
C
      D1 = 0.3D0
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C------
C 1. COMPUTES SOURCES TERMS AT THE INTERFACE IEL1 , IEL2
C------
C
C
C
C   --->    SOME INTERMEDIATE CALCULATIONS
C           ------------------------------
C
       HI = W(1,IEL1)
       IF (HI.GT.EPS) THEN
         UI = W(2,IEL1) / HI
         VI = W(3,IEL1) / HI
         INDIC(1) = 0
       ELSE
         UI = 0.D0
         VI = 0.D0
         INDIC(1) = 1
       ENDIF
C
       HJ = W(1,IEL2)
       IF ( HJ.GT.EPS) THEN
         UJ = W(2,IEL2) / HJ
         VJ = W(3,IEL2) / HJ
         INDIC(2) = 0
       ELSE
         UJ = 0.D0
         VJ = 0.D0
         INDIC(2) = 1
       ENDIF
C
       XN = VNOIN (1,ISEGIN)
       YN = VNOIN (2,ISEGIN)
       RNORM = VNOIN (3,ISEGIN)
C
C    BOTTOM MODIFICATION: AT REST WITH A DRY ELEMENT
C
       UN = UJ*XN+VJ*YN
       ZF1 = ZF(IEL1)
       IF(INDIC(1).EQ.1) THEN
         IF((ZF1+EPS.GT.ZF(IEL2)+HJ).AND.(UN.GE.-EPS)) THEN
           ZF1 = ZF(IEL2)+HJ-EPS
         ENDIF
       ENDIF
C
       UN = UI*XN+VI*YN
       ZF2 = ZF(IEL2)
       IF(INDIC(2).EQ.1) THEN
         IF((ZF2+EPS.GT.ZF(IEL1)+HI).AND.(UN.LE.EPS)) THEN
           ZF2 = ZF(IEL1)+HI-EPS
         ENDIF
       ENDIF
C
C   --->    COMPUTES THE AVERAGES OF ROE OF U,V,H,C**2 AND C
C           ---------------------------------------------
C
       IF(HI.LE.0.D0) THEN
         HI = 0.D0
C FOLLOWING LINE INSERTED BY JMH
         RI = 0.D0
       ELSE
         RI = SQRT ( HI )
       ENDIF
       IF(HJ.LE.0.D0) THEN
         HJ = 0.D0
C FOLLOWING LINE INSERTED BY JMH
         RJ = 0.D0
       ELSE
         RJ = SQRT ( HJ )
       ENDIF
C      MAX OF THE TWO FOLLOWING LINES INSERTED BY JMH
       UT = ( RI * UI + RJ * UJ ) / MAX(RI+RJ,1.D-8)
       VT = ( RI * VI + RJ * VJ ) / MAX(RI+RJ,1.D-8)
       UM = (UI+UJ)/2.D0
       VM = (VI+VJ)/2.D0
       CT2 = G*(HI+HJ)/2.D0
       CT = SQRT ( CT2 )
C
C   --->  TEST ON THE SIGN OF THE EIGENVALUE LAMB0 =
C           ----------------------------------------------------------
C
       RLAMB0 = UT * XN + VT * YN
C
CTBTB BEGINNING: MODIFICATION OF RLAMB0 IF RLAMB0 < D1
CC   IT IS NECESSARY TO ADD FLUXES FOR THE DUPLICATED EIGENVALUES
CC   TO BE COMPLETED BY WHOEVER WISHES TO
CC
CTBTB END
C
C     COMPUTES EIGENVALUES MATRICES
C--------------------------------------------
         T11(1) = 1.D0
         T11(2) = UT - CT * XN
         T11(3) = VT - CT * YN
         T21(1) = 0.D0
         T21(2) = CT * YN
         T21(3) = -CT * XN
         T31(1) = 1.D0
         T31(2) = UT + CT * XN
         T31(3) = VT + CT * YN
C
         T12(1) = 1.D0
         T12(2) = UT + CT * XN
         T12(3) = VT + CT * YN
         T22(1) = 0.D0
         T22(2) = -CT * YN
         T22(3) = +CT * XN
         T32(1) = 1.D0
         T32(2) = UT - CT * XN
         T32(3) = VT - CT * YN
C
         TS11(1) = (UT * XN + VT * YN) * CT + CT2
         TS21(1) = (2.D0 * VT * XN - 2.D0 * UT * YN) * CT
         TS31(1) = -(UT * XN + VT * YN) * CT + CT2
         TS11(2) = -XN * CT
         TS21(2) = 2.D0 * YN * CT
         TS31(2) = XN * CT
         TS11(3) = -YN * CT
         TS21(3) = -2.D0 * XN * CT
         TS31(3) = YN * CT
C
         TS12(1) = -(UT * XN + VT * YN) * CT + CT2
         TS22(1) = -(2.D0 * VT * XN - 2.D0 * UT * YN) * CT
         TS32(1) = +(UT * XN + VT * YN) * CT + CT2
         TS12(2) = +XN * CT
         TS22(2) = -2.D0 * YN * CT
         TS32(2) = -XN * CT
         TS12(3) = +YN * CT
         TS22(3) = +2.D0* XN * CT
         TS32(3) = -YN * CT
C
C----------CALCULS POUR LES TERMES SOURCES--------------------
C
C
          XGI = X(IEL1)
          YGI = Y(IEL1)
          XGJ = X(IEL2)
          YGJ = Y(IEL2)
C
          DIJ = SQRT ((XGJ -XGI)**2 + (YGJ - YGI)**2)
          A1  = VNOIN(3,ISEGIN)*DIJ/2.D0
          A2  = VNOIN(3,ISEGIN)*DIJ/2.D0
C
C  BOTTOM GRADIENTS
C
        GE(1)=0.D0
        GE(2)=G*((HI+HJ)/2.D0)*(ZF2-ZF1)*XN/DIJ
        GE(3)=G*((HI+HJ)/2.D0)*(ZF2-ZF1)*YN/DIJ
C
C  FRICTION TERMS
C
C       CH = 900.D0
C       H = (CT2/G)**(1./3.)
        FE(1)= 0.D0
C       FE(2)= G*UT*SQRT(UT**2 + VT**2)/((CH**2)*H)
C       FE(3)= G*VT*SQRT(UT**2 + VT**2)/((CH**2)*H)
        FE(2) = 0.D0
        FE(3) = 0.D0
C
        GE(1)=GE(1)+FE(1)
        GE(2)=GE(2)+FE(2)
        GE(3)=GE(3)+FE(3)
C
C---------------------------------------------------------------------
         IF(RLAMB0.GE.-.000001D0) THEN
C        ---- EXIT SEGMENT ---------
C
C   --->   SMALL CALCULATIONS
C
           RLAMBM = RLAMB0 - CT
C
C
           ALPHA = UI * XN + VI * YN
C
CTBTB BEGINNING: MODIFICATION OF RLAMBM IF RLAMBM
C
           CI2 = G*HI
           CI = SQRT (CI2)
           CJ2 =  G*HJ
           CJ = SQRT (CJ2)
           RLAMBI = ALPHA - CI
           RLAMBJ = UJ * XN + VJ * YN - CJ
           PROD = RLAMBI * RLAMBJ
C
           IF ( RLAMBI .LT. 0.D0 .AND. RLAMBJ .GT. 0.D0
     &                                                   ) THEN
            RLAMBM = MIN(0.D0,RLAMBM) - ABS(RLAMBI - RLAMBJ) / 4.D0
           ENDIF
C
C------------CALCUL DES TERMES SOURCES ------------------------
C
            FLUSCE (1,IEL1) = 0.D0
            FLUSCE (2,IEL1) = 0.D0
            FLUSCE (3,IEL1) = 0.D0
C
            FLUSCE (1,IEL2) = 0.D0
            FLUSCE (2,IEL2) = 0.D0
            FLUSCE (3,IEL2) = 0.D0
C
C
C
C   --->    TEST ON THE SIGN OF LAMBDAM
C           ----------------------------
C
            IF ( RLAMBM . LT . 0.D0 ) THEN
C           - - - - - - - - - - - - - -
C
C----------CALCUL DES TERMES SOURCES --------------------------
C
              PSA = TS11(1)*GE(1)+TS11(2)*GE(2)+TS11(3)*GE(3)
C
         FLUSCE(1,IEL1) = PSA*T11(1)
         FLUSCE(2,IEL1) = PSA*T11(2)
         FLUSCE(3,IEL1) = PSA*T11(3)
C
C
              PSA1= TS12(1)*GE(1)+TS12(2)*GE(2)+TS12(3)*GE(3)
              PSA2= TS22(1)*GE(1)+TS22(2)*GE(2)+TS22(3)*GE(3)
C
C
         FLUSCE(1,IEL2) = (PSA1*T12(1)+PSA2*T22(1))
         FLUSCE(2,IEL2) = (PSA1*T12(2)+PSA2*T22(2))
         FLUSCE(3,IEL2) = (PSA1*T12(3)+PSA2*T22(3))
C
            ELSE
C           -----
C
C
         FLUSCE(1,IEL1) = 0.D0
         FLUSCE(2,IEL1) = 0.D0
         FLUSCE(3,IEL1) = 0.D0
C
         FLUSCE(1,IEL2) = GE(1)*CT2*2.D0
         FLUSCE(2,IEL2) = GE(2)*CT2*2.D0
         FLUSCE(3,IEL2) = GE(3)*CT2*2.D0
C
            ENDIF
C          -----
C      TESTEST
         ELSE
C      TESTEST
C
C   --->   SMALL CALCULATIONS
C          --------------
C
           RLAMBP = RLAMB0 + CT
C
C          PROD = RLAMBI * RLAMBJ
C
C
           ALPHA = UI * XN + VI * YN
C
CTBTB BEGINNING: MODIFICATION OF RLAMBP IF RLAMBM
C
           CI2 = G*HI
           CI = SQRT (CI2)
           CJ2 =  G*HJ
           CJ = SQRT (CJ2)
           RLAMBI = ALPHA - CI
           RLAMBJ = UJ * XN + VJ * YN - CJ
           PROD = RLAMBI * RLAMBJ
C
CTBTB: MODIFIES ONLY IN THE RELAXATION:
            IF ( RLAMBI .LT. 0.D0 .AND. RLAMBJ .GT. 0.D0) THEN
C
CTBTB : MODIFIES IN THE RELAXATION OR IN THE SHOCK:
C           IF ( PROD . LT . 0.D0 .AND. ABS(RLAMBP).LT.D1 ) THEN
C
               RLAMBP = MAX(0.D0,RLAMBP) + ABS(RLAMBI - RLAMBJ)/4.D0
            ENDIF
CTBTB END
C
C
C-----------CALCUL DES TERMES SOURCE --------------------------
C
         FLUSCE(1,IEL1) = GE(1)*CT2*2.D0
         FLUSCE(2,IEL1) = GE(2)*CT2*2.D0
         FLUSCE(3,IEL1) = GE(3)*CT2*2.D0
C
         FLUSCE(1,IEL2) = 0.D0
         FLUSCE(2,IEL2) = 0.D0
         FLUSCE(3,IEL2) = 0.D0
C
C
C   --->    TEST ON THE SIGN OF LAMBDAP
C           ----------------------------
C
            IF ( RLAMBP . GT . 0.D0 ) THEN
C           - - - - - - - - - - - - - -
C
C-----------CALCUL DES TERMES SOURCE ------------------
C
C
         FLUSCE(1,IEL1) = 0.D0
         FLUSCE(2,IEL1) = 0.D0
         FLUSCE(3,IEL1) = 0.D0
C
         FLUSCE(1,IEL2) = 0.D0
         FLUSCE(2,IEL2) = 0.D0
         FLUSCE(3,IEL2) = 0.D0
              PSA1= TS11(1)*GE(1)+TS11(2)*GE(2)+TS11(3)*GE(3)
              PSA2= TS21(1)*GE(1)+TS21(2)*GE(2)+TS21(3)*GE(3)
C
C
         FLUSCE(1,IEL1) = (PSA1*T11(1)+PSA2*T21(1))
         FLUSCE(2,IEL1) = (PSA1*T11(2)+PSA2*T21(2))
         FLUSCE(3,IEL1) = (PSA1*T11(3)+PSA2*T21(3))
C

              PSA = TS12(1)*GE(1)+TS12(2)*GE(2)+TS12(3)*GE(3)
C
         FLUSCE(1,IEL2) = PSA*T12(1)
         FLUSCE(2,IEL2) = PSA*T12(2)
         FLUSCE(3,IEL2) = PSA*T12(3)
C
            ENDIF
C           -----
C
C       TESTEST
         ENDIF
C       TESTEST
      FLUSCE(1,IEL1)=FLUSCE(1,IEL1)*A1/CT2
      FLUSCE(2,IEL1)=FLUSCE(2,IEL1)*A1/CT2
      FLUSCE(3,IEL1)=FLUSCE(3,IEL1)*A1/CT2
      FLUSCE(1,IEL2)=FLUSCE(1,IEL2)*A2/CT2
      FLUSCE(2,IEL2)=FLUSCE(2,IEL2)*A2/CT2
      FLUSCE(3,IEL2)=FLUSCE(3,IEL2)*A2/CT2
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
       RETURN
       END
C
C#######################################################################
C