C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE CONTRIBUTION OF THE NON-LINEAR
!>                INTERACTIONS SOURCE TERM (FREQUENCY TRIADS).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALFLTA, DEPTH, F, FMOY, FREQ, FTOT, GRAVIT, NF, NPLAN, NPOIN2, RAISF, RFMLTA, TSDER, TSTOT, XK
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BIF, CGR, CGR2P, COEF, CP, CPH, CPH2P, CPHS2, D, DD, DEUKD, DEUPI, DMOIN, DPLUS, E2P, EPS2, F2P, FMAX, FP, FPS2, IFF, IFMA, IIND, IPL, IPO, K2P, KP, O2P, OM2P, OMP, RIND, RP, RPP, RPS2, SMOIN, SPLUS, URS, XK2P, XKP, XKPS2
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> WNSCOU()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SEMIMP()

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
!>      <td><center> 1.1                                       </center>
!> </td><td> 26/12/96
!> </td><td> EDF/DER/LNH
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALFLTA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEPTH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEPTH(
!></td><td>--></td><td>TABLEAU DES PROFONDEURS (METRES)
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F(
!></td><td>---</td><td>SPECTRE DIRECTIONNEL DE VARIANCE
!>    </td></tr>
!>          <tr><td>FMOY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ(
!></td><td>--></td><td>TABLEAU DES FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>FTOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAVIT
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE SPATIAL
!>    </td></tr>
!>          <tr><td>RAISF
!></td><td>--></td><td>RAISON FREQUENTIELLE POUR DISCRETISATION
!>    </td></tr>
!>          <tr><td>RFMLTA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUX1(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>TAUX2(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>TSDER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TSDER(
!></td><td><--</td><td>CONTRIBUTION TERME SOURCE - PARTIE DERIVEE
!>    </td></tr>
!>          <tr><td>TSTOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TSTOT(
!></td><td><--</td><td>CONTRIBUTION TERME SOURCE - PARTIE TOTALE
!>    </td></tr>
!>          <tr><td>XK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XK(
!></td><td>--></td><td>TABLEAU DES NOMBRES D'ONDE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE QTRIA1
     &( F     , XK    , FREQ  , DEPTH , RAISF , GRAVIT, ALFLTA, RFMLTA,
     &  NF    , NPLAN , NPOIN2, TSTOT , TSDER , FTOT  , FMOY  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALFLTA         |---| 
C| DEPTH          |---| 
C| DEPTH(         |-->| TABLEAU DES PROFONDEURS (METRES)
C| F             |---| 
C| F(             |---| SPECTRE DIRECTIONNEL DE VARIANCE
C| FMOY           |---| 
C| FREQ           |---| 
C| FREQ(          |-->| TABLEAU DES FREQUENCES DE DISCRETISATION
C| FTOT           |---| 
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
C| RAISF          |-->| RAISON FREQUENTIELLE POUR DISCRETISATION
C| RFMLTA         |---| 
C| TAUX1(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| TAUX2(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| TSDER          |---| 
C| TSDER(         |<--| CONTRIBUTION TERME SOURCE - PARTIE DERIVEE
C| TSTOT          |---| 
C| TSTOT(         |<--| CONTRIBUTION TERME SOURCE - PARTIE TOTALE
C| XK             |---| 
C| XK(            |-->| TABLEAU DES NOMBRES D'ONDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NF, NPOIN2, NPLAN
      DOUBLE PRECISION  F(NPOIN2,NPLAN,NF), XK(NPOIN2,NF)
      DOUBLE PRECISION  RAISF , FREQ(NF), DEPTH(NPOIN2)
      DOUBLE PRECISION  TSTOT(NPOIN2,NPLAN,NF), TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION  GRAVIT, ALFLTA, RFMLTA
      DOUBLE PRECISION  FTOT(NPOIN2) , FMOY(NPOIN2)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER           IIND  , IFMA  , IFF   , IPL   , IPO
      DOUBLE PRECISION  CPH   , CGR   , BIF   , RPS2  , RP    , DEUPI ,
     &                  FPS2  , CPHS2 , XKPS2 , URS   , RIND  , FMAX  ,
     &                  F2P   , CPH2P , XK2P  , CGR2P , E2P   , EPS2  ,
     &                  OMP   , OM2P  , COEF  , D     , DEUKD ,
     &                  SPLUS , SMOIN , DMOIN , DPLUS , FP    , XKP
      PARAMETER(DEUPI=2.D0*3.141592654D0)
C
C.....FUNCTION / FORMULATION
C     """"""""""""""""
      DOUBLE PRECISION  RPP   , DD    , O2P   , CP    , KP    , K2P
      RPP(KP,CP,K2P,O2P,DD) = KP**2*(GRAVIT*DD+2.D0*CP**2)/(K2P*DD)/
     &   (GRAVIT*DD+(2.D0/15.D0)*GRAVIT*DD**3*K2P**2-0.4D0*(O2P*DD)**2)
C
C
      COEF  = GRAVIT*DSQRT(2.D0)
C
      DO IPO=1,NPOIN2
C
        D=DEPTH(IPO)
C
C.......COMPUTES THE URSELL NUMBER AT THE CONSIDERED POINT IN SPACE
C       """""""""""""""""""""""""""""""""""""""""""""""""""""
        URS = COEF*DSQRT(FTOT(IPO))/(DEUPI*D*FMOY(IPO))**2
C
C.......COMPUTES THE CONTRIBUTION OF TRIADS ONLY IF URSELL > 0.1
C       """""""""""""""""""""""""""""""""""""""""
        IF (URS.GT.0.1D0) THEN
C
C.........COMPUTES THE SINE OF THE BIPHASE
C         """""""""""""""""""""""""""""
          IF (URS.GT.10.D0) THEN
            BIF=1.D0
          ELSE
            BIF=DSIN(DABS(DEUPI/4.D0*(-1.D0+DTANH(0.2D0/URS))))
          ENDIF
C
C.........COMPUTES THE MAXIMUM FREQUENTIAL INDEX
C         """"""""""""""""""""""""""""""""""""""
          FMAX = RFMLTA*MAX(FMOY(IPO),FREQ(1))
          RIND = 1.D0 + DLOG(FMAX/FREQ(1))/DLOG(RAISF)
          IFMA = MIN(IDINT(RIND),NF)
C
          DO IFF=1,IFMA
            FP  = FREQ(IFF)
            OMP = DEUPI*FP
            XKP = XK(IPO,IFF)
            CPH = OMP/XKP
C
C
C...........COMPUTES THE CONTIBUTION S+
C           """""""""""""""""""""""""""
            FPS2 = FP/2.D0
            RIND = 1.D0 + DLOG(FPS2/FREQ(1))/DLOG(RAISF)
            IIND = IDINT(RIND)
            RIND = RIND-DBLE(IIND)
C
            IF (IIND.GT.0) THEN
              DEUKD=2.D0*XKP*D
              IF(DEUKD.LE.7.D2) THEN
                CGR = CPH*(0.5D0+XKP*D/DSINH(2.D0*XKP*D))
              ELSE
                CGR = 0.5D0*CPH
              ENDIF
              CALL WNSCOU(XKPS2,FPS2,D)
              CPHS2 = DEUPI*FPS2/XKPS2
              RPS2 = CPH*CGR*RPP(XKPS2,CPHS2,XKP,OMP,D)**2
C
              DO IPL=1,NPLAN
                EPS2=(1.D0-RIND)*F(IPO,IPL,IIND)+RIND*F(IPO,IPL,IIND+1)
                SPLUS = ALFLTA*RPS2*BIF*(EPS2-2.D0*F(IPO,IPL,IFF))*EPS2
                DPLUS = ALFLTA*RPS2*BIF*(-2.D0*EPS2)
                IF (SPLUS.LT.0.D0) THEN
                  SPLUS = 0.D0
                  DPLUS = 0.D0
                ENDIF
                TSTOT(IPO,IPL,IFF) = TSTOT(IPO,IPL,IFF) + SPLUS
C                TSDER(IPO,IPL,IFF) = TSDER(IPO,IPL,IFF) + DPLUS
              ENDDO
            ENDIF
C
C
C...........COMPUTES THE CONTIBUTION S-
C           """""""""""""""""""""""""""
            F2P = 2.D0*FP
            RIND = 1.D0 + DLOG(F2P/FREQ(1))/DLOG(RAISF)
            IIND = IDINT(RIND)
            RIND = RIND-DBLE(IIND)
            IF (IIND.LT.IFMA) THEN
              OM2P  = DEUPI*F2P
              CALL WNSCOU(XK2P,F2P,D)
              CPH2P = OM2P/XK2P
              DEUKD=2.D0*XK2P*D
              IF(DEUKD.LE.700D2) THEN
                CGR2P = CPH2P*(0.5D0+XK2P*D/DSINH(2.D0*XK2P*D))
              ELSE
                CGR2P = CPH2P*0.5D0
              ENDIF
              RP    = CPH2P*CGR2P*RPP(XKP,CPH,XK2P,OM2P,D)**2
C
              DO IPL=1,NPLAN
                E2P = (1.D0-RIND)*F(IPO,IPL,IIND)+RIND*F(IPO,IPL,IIND+1)
                SMOIN = 2.D0*ALFLTA*RP*BIF*F(IPO,IPL,IFF)
     &                *(F(IPO,IPL,IFF)-2.D0*E2P)
                DMOIN = 4.D0*ALFLTA*RP*BIF*(F(IPO,IPL,IFF)-E2P)
                IF (SMOIN.LT.0.D0) THEN
                  SMOIN = 0.D0
                  DMOIN = 0.D0
                ENDIF
                TSTOT(IPO,IPL,IFF) = TSTOT(IPO,IPL,IFF) - SMOIN
C                TSDER(IPO,IPL,IFF) = TSDER(IPO,IPL,IFF) - DMOIN
              ENDDO
            ENDIF
C
          ENDDO
        ENDIF
      ENDDO
C
      RETURN
      END
C
C#######################################################################
C