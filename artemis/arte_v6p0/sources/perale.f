C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DISCRETISES AN ENERGY SPECTRUM IN NPALE BANDS
!>                OF EQUAL ENERGY. THE RESULT IS A LIST OF
!>                PERIODS CORRESPONDING TO EACH BAND.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_ARTEMIS
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> GAMMA, NPALE, NPOIN, NPRIV, PALE, PERPIC, PMAX, PMIN, PRIVE, TRA01
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU<hr>
!> COEFHE : FP, GAM, DELTA
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> B, B1, B2, DF, FMAX, FMIN, I, K, NPAS, SUMB, SUMICI, VAR
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PERALE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> SPE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS()

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
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
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
!>          <tr><td>GAMMA
!></td><td>--></td><td>COEFFICIENT DANS LA FORMULE DU SPECTRE
!>    </td></tr>
!>          <tr><td>NPALE
!></td><td>--></td><td>NOMBRE DE PERIODES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td><-></td><td>NOMBRE DE TABLEAUX PRIVES
!>    </td></tr>
!>          <tr><td>PALE
!></td><td><--</td><td>PERIODES DE DISCRETISATION DU SPECTRE
!>    </td></tr>
!>          <tr><td>PERPIC
!></td><td>--></td><td>PERIODE DU PIC DU SPECTRE
!>    </td></tr>
!>          <tr><td>PMAX
!></td><td>--></td><td>FREQUENCE MAXIMUM DU SPECTRE
!>    </td></tr>
!>          <tr><td>PMIN
!></td><td>--></td><td>FREQUENCE MINIMUM DU SPECTRE
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td><-></td><td>TABLEAU PRIVE DE L'UTILISATEUR
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PERALE
     &(PALE,GAMMA,PERPIC,NPALE,TRA01,NPOIN,PRIVE,NPRIV,PMIN,PMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| GAMMA          |-->| COEFFICIENT DANS LA FORMULE DU SPECTRE
C| NPALE          |-->| NOMBRE DE PERIODES DE DISCRETISATION
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPRIV          |<->| NOMBRE DE TABLEAUX PRIVES
C| PALE           |<--| PERIODES DE DISCRETISATION DU SPECTRE
C| PERPIC         |-->| PERIODE DU PIC DU SPECTRE
C| PMAX           |-->| FREQUENCE MAXIMUM DU SPECTRE
C| PMIN           |-->| FREQUENCE MINIMUM DU SPECTRE
C| PRIVE          |<->| TABLEAU PRIVE DE L'UTILISATEUR
C| TRA01          |<->| TABLEAU DE TRAVAIL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_ARTEMIS, EX_PERALE=> PERALE
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPALE,NPOIN,NPRIV,NPAS,I,K
C
      DOUBLE PRECISION PALE(NPALE),TRA01(NPOIN)
      DOUBLE PRECISION PERPIC,GAMMA ,SUMB  ,SUMICI   ,DF    ,VAR
      DOUBLE PRECISION PMIN  ,PMAX  ,FMIN  ,FMAX
      DOUBLE PRECISION B     ,B1    ,B2
C
      TYPE(BIEF_OBJ) :: PRIVE
C
      DOUBLE PRECISION FP,GAM,DELTA
      COMMON /COEFHE/ FP,GAM,DELTA
C
C      DOUBLE PRECISION SPE
C      EXTERNAL SPE
C
      INTRINSIC LOG,FLOAT
C
C-----------------------------------------------------------------------
C
C PEAK FREQUENCY
      FP   = 1.D0 / PERPIC
      FMIN = 1.D0 / PMAX
      FMAX = 1.D0 / PMIN
      IF (FMAX.GE.99.D0) THEN
         FMAX = 2.5D0 * FP
      ENDIF
C
C GAMMA IS IN THE COMMON STATEMENT OF FUNCTION SPE (CANNOT BE
C CALLED GAMMA BECAUSE IT IS AN ARGUMENT OF THIS SUBROUTINE)
      GAM  = GAMMA
C
C-----------------------------------------------------------------------
C
      IF (GAMMA.GT.0.99D0 .AND. GAMMA.LT.1.01D0) THEN
C
C
C        PIERSON-MOSKOWITZ SPECTRUM
C        ----------------------------
C
         B1 = EXP(-1.25D0 * (FP/FMAX)**4)
         B2 = EXP(-1.25D0 * (FP/FMIN)**4)
         B  = B1 - B2
         DO 10 I=1,NPALE
            PALE(NPALE-I+1) = PERPIC *
     &    (-0.8D0*LOG( B2 + B*FLOAT(2*I-1)/FLOAT(2*NPALE) ))**(0.25D0)
10       CONTINUE
C
      ELSE
C
C
C        JONSWAP SPECTRUM
C        ------------------
C
C        THE FREQUENCIES LIMITING THE SPECTRUM TO THE LEFT AND RIGHT
C        ARE GIVEN BY KEYWORDS IN THE ARGUMENTS
C
      IF (FMAX.LE.FP) THEN
         FMAX = 2.5D0 * FP
         WRITE(LU,110) FMAX
 110     FORMAT(/,1X,'(PERALE) : FMAX < FP ??? =>',1X,
     &          'CORRECTION : FMAX =',1X,F5.3,' HZ',/)
      ENDIF
C
C        NUMBER OF INTEGRATION INTERVALS FOR THE TRAPEZOIDS METHOD
C
         NPAS = 2000*NPALE
C
C        WIDTH OF AN INTEGRATION INTERVAL
C
         DF = (FMAX-FMIN)/FLOAT(NPAS)
C
C        COEFFICIENT FOR THE FUNCTION OF THE SPECTRUM (COMPUTED HERE
C        SO THAT IT'S NOT RECOMPUTED WHEN CALLS SPE)
C
         DELTA = 0.0624D0 * FP**4 /
     &           ( 0.230D0 + 0.0336D0*GAM - 0.185D0 / (1.9D0+GAM) )
C
C        SURFACE OF THE SPECTRUM (TRAPEZOIDS METHOD)
C
         SUMB = (SPE(FMIN) + SPE(FMAX))/2.D0
         DO 20 I = 2,NPAS-1
            SUMB = SUMB + SPE(FMIN+FLOAT(I)*DF)
20       CONTINUE
C
C        DIVIDES THE SPECTRUM INTO 2*NPALE BANDS OF EQUAL ENERGY
C
         SUMB = SUMB/FLOAT(2*NPALE)
C
C        IDENTIFIES THE FREQUENCIES EVERY (2*I-1)*SUMB (I=1,NPALE)
C
         SUMICI = SPE(FMIN)/2.D0
         I   = 1
         DO 30 K=1,NPAS
            VAR = SPE(FMIN+DF*FLOAT(K))
            SUMICI = SUMICI + VAR/2.D0
            IF (SUMICI.GT.SUMB*FLOAT(2*I-1)) THEN
               PALE(NPALE-I+1) = 1.D0 / ( FMIN + DF*(FLOAT(K)-0.5D0) )
               I = I + 1
               IF (I.GT.NPALE) RETURN
            ENDIF
            SUMICI = SUMICI + VAR/2.D0
30       CONTINUE
C
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C