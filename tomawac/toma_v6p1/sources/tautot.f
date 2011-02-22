C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE TOTAL STRESS FROM THE WIND VELOCITY
!>                UVENT AT THE ELEVATION ZVENT (IN PRINCIPLE ZVENT=10 M)
!>                AND FROM THE WAVE STRESS TAUW.
!><br>            THEORY DEVELOPED BY JANSSEN (1989 AND 1991) AND USED
!>                IN WAM-CYCLE 4 (SUBROUTINE STRESS OF PREPROC).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   TAUT IS COMPUTED FROM UVENT AND TAUW (SOLVES THE EQUATION
!>          IMPLICITLY USING AN ITERATIVE METHOD - NEWTON).

!>  @reference JANSSEN P.A.E.M (1989) :
!>                     "WIND-INDUCED STRESS AND THE DRAG OF AIR FLOW
!>                      OVER SEA WAVES". JPO, VOL 19, PP 745-754.

!>  @reference JANSSEN P.A.E.M (1991) :
!>                     "QUASI-LINEAR THEORY OF WIND-WAVE GENERATION
!>                      APPLIED TO WAVE FORECASTING". JPO, VOL 21, PP 1631-1642.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALPHA, CDRAG, GRAVIT, ITR, ITRMAX, ITRMIN, SEUIL, TAUT, TAUW, UVENT, XKAPPA, ZVENT
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, DFOLD, DIFF, FOLD, TAUMIN, TAUN, TAUO, USTN, USTO, X, XNUAIR, ZE
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>USTAR1()

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
!>      <td><center> 1.0                                       </center>
!> </td><td> 25/04/95
!> </td><td> M. BENOIT (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALPHA
!></td><td>--></td><td>CONSTANTE DE LA LOI DE CHARNOCK
!>    </td></tr>
!>          <tr><td>CDRAG
!></td><td>--></td><td>COEFFICIENT DE TRAINEE
!>    </td></tr>
!>          <tr><td>GRAVIT
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>ITR
!></td><td><--</td><td>NOMBRE D'ITERATIONS EFFECTUES
!>    </td></tr>
!>          <tr><td>ITRMAX
!></td><td>--></td><td>NOMBRE MAXIMAL D'ITERATIONS SOUHAITE
!>    </td></tr>
!>          <tr><td>ITRMIN
!></td><td>--></td><td>NOMBRE MINIMAL D'ITERATIONS SOUHAITE
!>    </td></tr>
!>          <tr><td>SEUIL
!></td><td>--></td><td>SEUIL DE CONVERGENCE METHODE DE NEWTON
!>    </td></tr>
!>          <tr><td>TAUT
!></td><td><--</td><td>CONTRAINTE TOTALE
!>    </td></tr>
!>          <tr><td>TAUW
!></td><td>--></td><td>CONTRAINTE DUE A LA HOULE
!>    </td></tr>
!>          <tr><td>UVENT
!></td><td>--></td><td>VITESSE DU VENT A LA COTE ZVENT (M/S)
!>    </td></tr>
!>          <tr><td>XKAPPA
!></td><td>--></td><td>CONSTANTE DE VON KARMAN
!>    </td></tr>
!>          <tr><td>ZVENT
!></td><td>--></td><td>COTE A LAQUELLE EST MESURE LE VENT (M)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TAUTOT
     &( TAUT  , UVENT , TAUW  , CDRAG , ALPHA , XKAPPA, ZVENT , SEUIL ,
     &  GRAVIT, ITR   , ITRMIN, ITRMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALPHA          |-->| CONSTANTE DE LA LOI DE CHARNOCK
C| CDRAG          |-->| COEFFICIENT DE TRAINEE
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| ITR            |<--| NOMBRE D'ITERATIONS EFFECTUES
C| ITRMAX         |-->| NOMBRE MAXIMAL D'ITERATIONS SOUHAITE
C| ITRMIN         |-->| NOMBRE MINIMAL D'ITERATIONS SOUHAITE
C| SEUIL          |-->| SEUIL DE CONVERGENCE METHODE DE NEWTON
C| TAUT           |<--| CONTRAINTE TOTALE
C| TAUW           |-->| CONTRAINTE DUE A LA HOULE
C| UVENT          |-->| VITESSE DU VENT A LA COTE ZVENT (M/S)
C| XKAPPA         |-->| CONSTANTE DE VON KARMAN
C| ZVENT          |-->| COTE A LAQUELLE EST MESURE LE VENT (M)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  ITRMIN, ITRMAX, ITR
      DOUBLE PRECISION TAUT  , UVENT , TAUW  , ALPHA , XKAPPA , ZVENT
      DOUBLE PRECISION SEUIL , CDRAG , GRAVIT
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      DOUBLE PRECISION TAUMIN, XNUAIR, AUX   , USTO  , TAUO   , TAUN
      DOUBLE PRECISION USTN  , X     , ZE    , DIFF  , FOLD   , DFOLD
C
C
      ITR   =0
      TAUMIN=1.D-5
      XNUAIR=1.D-5
C
C.....INITIAL VALUES
C     """"""""""""""""""
      USTO  =UVENT*SQRT(CDRAG)
      TAUO  =MAX(USTO**2,TAUW+TAUMIN)
C
  190 CONTINUE
      ITR   = ITR+1
C
C.....ITERATION BY THE METHOD OF NEWTON
C     """""""""""""""""""""""""""""""""""
      USTO  = SQRT(TAUO)
      X     = TAUW/TAUO
      ZE    = MAX(0.1D0*XNUAIR/USTO,ALPHA*TAUO/(GRAVIT*SQRT(1.D0-X)))
      AUX   = DLOG(ZVENT/ZE)
      FOLD  = USTO-XKAPPA*UVENT/AUX
      DFOLD = 1.D0-2.D0*XKAPPA*UVENT*(1.D0-1.5D0*X)/AUX**2/USTO/(1.D0-X)
      USTN  = USTO-FOLD/DFOLD
      TAUN  = MAX(USTN**2,TAUW+TAUMIN)
C
C.....CONVERGENCE CRITERIA
C     """"""""""""""""""""""""
      DIFF=ABS(TAUN-TAUO)/TAUO
      TAUO=TAUN
      IF (ITR.LT.ITRMIN) GOTO 190
      IF ((DIFF.GT.SEUIL).AND.(ITR.LT.ITRMAX)) GOTO 190
C
C.....APPLIES THE SOLUTION
C     """""""""""""""""""""""""""""""""""
      TAUT=TAUN
C
      RETURN
      END
C
C#######################################################################
C