C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE CONTRIBUTION OF THE NON-LINEAR
!>                INTERACTIONS SOURCE TERM (FREQUENCY TRIADS).
!><BR>           (INSPIRED FROM THE BOUSSINESQ EQUATIONS)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BDISPB, BDSSPB, COSTET, DEPTH, DFREQ, F, FREQ, GRAVIT, INDI, KSPB, NBD, NF, NPLAN, NPOIN2, RAISF, SINTET, TETA, TSDER, TSTOT, XK
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AP2, BISP, BK1, BK2, BK3, BMS, BMSP, D12, D21, DEP, DEP2, DEUPI, DEUPI2, DTETA, FILT, FR1, FREQ0, FREQ1, FREQ2, FREQ3, IFF, IFR, IP1, IP3, IPL, IPM, IPO, IPP, JFF, JPL, K2NL, LRAISF, NRJ2, PI, RAISM1, TETA2, TK1, TK2, TK3, VAR1, VR1, VR2, VR3, XC1, XC2, XC3, XK1, XK2, XK3
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> KERBOU()
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
!>      <td><center> 5.0                                       </center>
!> </td><td> 11/06/98
!> </td><td> EDF/DER/LNH
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BDISPB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BDSSPB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COSTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COSTET(
!></td><td>--></td><td>VECTEUR DES COSINUS DES DIRECTIONS
!>    </td></tr>
!>          <tr><td>DEPTH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEPTH(
!></td><td>--></td><td>TABLEAU DES PROFONDEURS (METRES)
!>    </td></tr>
!>          <tr><td>DFREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DFREQ(
!></td><td>--></td><td>TABLEAU DES PAS DE FREQUENCE
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F(
!></td><td>---</td><td>SPECTRE DIRECTIONNEL DE VARIANCE
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ(
!></td><td>--></td><td>TABLEAU DES FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>GRAVIT
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>INDI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSPB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBD
!></td><td>---</td><td>
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
!>          <tr><td>SINTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SINTET(
!></td><td>--></td><td>VECTEUR DES   SINUS DES DIRECTIONS
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETA(
!></td><td>--></td><td>VECTEUR DES DIRECTIONS DE DISCRETISATION
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
                        SUBROUTINE QTRIA2
     &( F     , XK    , FREQ  , DFREQ , DEPTH , TETA  , SINTET, COSTET ,
     &  KSPB  , BDISPB, BDSSPB, RAISF , GRAVIT, NF    , NPLAN , NPOIN2 ,
     &  NBD   , INDI  , TSTOT , TSDER )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BDISPB         |---| 
C| BDSSPB         |---| 
C| COSTET         |---| 
C| COSTET(        |-->| VECTEUR DES COSINUS DES DIRECTIONS
C| DEPTH          |---| 
C| DEPTH(         |-->| TABLEAU DES PROFONDEURS (METRES)
C| DFREQ          |---| 
C| DFREQ(         |-->| TABLEAU DES PAS DE FREQUENCE
C| F             |---| 
C| F(             |---| SPECTRE DIRECTIONNEL DE VARIANCE
C| FREQ           |---| 
C| FREQ(          |-->| TABLEAU DES FREQUENCES DE DISCRETISATION
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| INDI           |---| 
C| KSPB           |---| 
C| NBD            |---| 
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
C| RAISF          |-->| RAISON FREQUENTIELLE POUR DISCRETISATION
C| SINTET         |---| 
C| SINTET(        |-->| VECTEUR DES   SINUS DES DIRECTIONS
C| TETA           |---| 
C| TETA(          |-->| VECTEUR DES DIRECTIONS DE DISCRETISATION
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
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NF, NPOIN2, NPLAN
      DOUBLE PRECISION  F(NPOIN2,NPLAN,NF), XK(NPOIN2,NF)
      DOUBLE PRECISION  RAISF , DFREQ(NF) , FREQ(NF), DEPTH(NPOIN2)
      DOUBLE PRECISION  TETA(NPLAN) , SINTET(NPLAN) , COSTET(NPLAN)
      DOUBLE PRECISION  TSTOT(NPOIN2,NPLAN,NF), TSDER(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION  GRAVIT , BMS , BMSP
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  IFF, JFF, IPL, JPL , IPO
      INTEGER  IFR, IPP, IPM
      DOUBLE PRECISION  DTETA, FR1 , AP2  , XK1   , XK3 , DEP
      DOUBLE PRECISION  TETA2, XK2 , K2NL , NRJ2
      DOUBLE PRECISION  FREQ0, FREQ1, FREQ2, FREQ3, LRAISF, RAISM1
      DOUBLE PRECISION  VR1 , VR2 , VR3 , TK1 , TK2 , TK3
      DOUBLE PRECISION  BK1 , BK2 , BK3 , D12 , D21 , DEP2
      DOUBLE PRECISION  FILT , BISP , BDISPB , BDSSPB
      DOUBLE PRECISION  PI  , DEUPI , DEUPI2 , KSPB
      DOUBLE PRECISION  VAR1 , XC1 , XC2 , XC3
      PARAMETER(PI=3.141592654D0, DEUPI=2.D0*PI )
C
      INTEGER           NBD, INDI(NBD), IP1, IP3
C
C
C.....EXTERNAL FUNCTIONS
C     """""""""""""""""""
      DOUBLE PRECISION KERBOU
      EXTERNAL         KERBOU
C
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
C
      DTETA = TETA(2)-TETA(1)
      BMS  = 1.D0/15.D0
      BMSP = BMS + 1.D0/3.D0
      DEUPI2 = DEUPI*DEUPI
      FREQ0  = FREQ(1)
      LRAISF = DLOG(RAISF)
      RAISM1 = RAISF-1.D0
C
      DO 200 IFF = 1,NF
       FREQ3 = FREQ(IFF)
       DO 201 JFF = 1,IFF-1
        FREQ1 = FREQ(JFF)
        FREQ2 = FREQ3-FREQ1
        IF(FREQ2.LE.FREQ0) THEN
          GOTO 201
        END IF
        FR1    = 1.D0 + DLOG(FREQ2/FREQ0)/LRAISF
        IFR    = IDINT(FR1)
        FR1    = FR1 - DBLE(IFR)
        FR1    = (RAISF**FR1-1.D0)/RAISM1
        DO 202 IP3 = 1,NBD
         IPL = INDI(IP3)
         DO 203 IP1 = 1,NBD
          JPL = INDI(IP1)
          DO 205 IPO = 1,NPOIN2
C         COMPUTES K2
C         --------------------
          DEP = DEPTH(IPO)
          XK1 = XK(IPO,JFF)
          XK3 = XK(IPO,IFF)
          K2NL   = DSQRT((XK3*COSTET(IPL)-XK1*COSTET(JPL))**2
     &             +(XK3*SINTET(IPL)-XK1*SINTET(JPL))**2)
          XK2    = (1.D0-FR1)*XK(IPO,IFR) + FR1*XK(IPO,IFR+1)
C
          TETA2=DATAN2(XK3*SINTET(IPL)-XK1*SINTET(JPL)
     &                ,XK3*COSTET(IPL)-XK1*COSTET(JPL))
          IF(TETA2.LT.0.D0) TETA2 = DEUPI + TETA2
C
          IF(TETA2.LT.BDISPB .OR. TETA2.GT.BDSSPB) THEN
C         INTERACTIONS BETWEEN COMPONENTS WHICH DIRECTIONS ARE NOT
C         WITHIN THE ANGULAR SECTOR DEFINED BY THE USER (VARIABLES
C         BDISPB AND BDSSPB) ARE NOT TAKEN INTO ACCOUNT
             GOTO 205
          END IF
C
          AP2    = (TETA2-TETA(1))/DTETA
          IPM    = IDINT(AP2)
          AP2    = AP2 - DBLE(IPM)
          IPM    = IPM + 1
          IPP    = IPM + 1
          IF(IPP.EQ.NPLAN+1) IPP = 1
C
C.........COMPUTES COUPLING COEFFICIENTS
C         """"""""""""""""""""""""""""""""""""
C         R(P-M,M)
          VR1 = KERBOU
     &(   XK1 , XK2   , FREQ1 , FREQ2 , DEP , TETA(JPL) , TETA2     )
C         R(M-P,P)
          VR2 = KERBOU
     &(   -XK1, XK3   , -FREQ1, FREQ3 , DEP , TETA(JPL) , TETA(IPL) )
C         R(-M,P)
          VR3 = KERBOU
     &(   -XK2  , XK3 , -FREQ2, FREQ3 , DEP , TETA2     , TETA(IPL) )
C
          FILT = KSPB/((XK2-K2NL)**2+KSPB*KSPB)
          FILT = -0.5D0*FILT/XK2
C
          DEP2 = DEP*DEP
          VAR1 = 2.D0*BMS*DEP2
          XC1  = VAR1*XK3*XK3
          XC2  = VAR1*XK2*XK2
          XC3  = VAR1*XK1*XK1
          VAR1 = BMSP*DEUPI2*DEP2
          TK1 = (GRAVIT*DEP*(1.D0+XC1)-VAR1*FREQ3*FREQ3)
          TK2 = (GRAVIT*DEP*(1.D0+XC2)-VAR1*FREQ2*FREQ2)
          TK3 = (GRAVIT*DEP*(1.D0+XC3)-VAR1*FREQ1*FREQ1)
C
          BK1 = DEUPI*FREQ3*(1.D0+3.D0*XC1)
          BK2 = DEUPI*FREQ2*(1.D0+3.D0*XC2)
          BK3 = DEUPI*FREQ1*(1.D0+3.D0*XC3)
C
C
C.........TAKES THE SOURCE TERM INTO ACCOUNT
C         """"""""""""""""""""""""""""""""
C
          NRJ2  = (1.D0-AP2)*((1.D0-FR1)*F(IPO,IPM,IFR)+FR1*
     &            F(IPO,IPM,IFR+1)) + AP2*((1.D0-FR1)*F(IPO,IPP,IFR)
     &            +FR1*F(IPO,IPP,IFR+1))
C
          BISP  = FILT*
     &            ((VR2/TK2)*F(IPO,IPL,IFF)*F(IPO,JPL,JFF)
     &            +(VR3/TK3)*F(IPO,IPL,IFF)*NRJ2
     &            -(VR1/TK1)*F(IPO,JPL,JFF)*NRJ2)
C
C          D12   = FILT*((VR2/TK2)*F(IPO,JPL,JFF)
C     *                 +(VR3/TK3)*NRJ2)
C          D21   = FILT*((VR2/TK2)*F(IPO,IPL,IFF)
C     *                 -(VR1/TK1)*NRJ2)
          VR1   = DFREQ(JFF)*DTETA*VR1/BK1
          VR3   = 2.D0*DFREQ(IFF)*DTETA*VR3/BK3
C
          TSTOT(IPO,IPL,IFF) = TSTOT(IPO,IPL,IFF) + VR1*BISP
C          TSDER(IPO,IPL,IFF) = TSDER(IPO,IPL,IFF) + VR1*D12
          TSTOT(IPO,JPL,JFF) = TSTOT(IPO,JPL,JFF) - VR3*BISP
C          TSDER(IPO,JPL,JFF) = TSDER(IPO,JPL,JFF) - VR3*D21
 205      CONTINUE
 203     CONTINUE
 202    CONTINUE
 201   CONTINUE
 200  CONTINUE
C
      RETURN
      END
C
C#######################################################################
C