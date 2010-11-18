C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES A TMA FREQUENCY SPECTRUM BASED
!>                ON A SERIES OF FREQUENCIES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AL, DEPTH, DEUPI, E2FMIN, FP, FPMIN, FREQ, GAMMA, GRAVIT, NF, SIGMAA, SIGMAB, SPEC
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ARG1, ARG2, ARG3, ARG4, COEF, FF, JF, OMEGH, SIG
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SPEINI()

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
!> </td><td> 10/01/96
!> </td><td> M. BENOIT (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AL
!></td><td>--></td><td>CONSTANTE DE PHILLIPS (ALPHA)
!>    </td></tr>
!>          <tr><td>DEPTH
!></td><td>--></td><td>PROFONDEUR D'EAU AU POINT CONSIDERE (M)
!>    </td></tr>
!>          <tr><td>DEUPI
!></td><td>--></td><td>2.PI
!>    </td></tr>
!>          <tr><td>E2FMIN
!></td><td>--></td><td>SEUIL MINIMUM DE SPECTRE CONSIDERE
!>    </td></tr>
!>          <tr><td>FP
!></td><td>--></td><td>FREQUENCE DE PIC DU SPECTRE JONSWAP
!>    </td></tr>
!>          <tr><td>FPMIN
!></td><td>--></td><td>VALEUR MINIMUM DE LA FREQUENCE DE PIC
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ(
!></td><td>--></td><td>FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>GAMMA
!></td><td>--></td><td>FACTEUR DE FORME DE PIC JONSWAP
!>    </td></tr>
!>          <tr><td>GRAVIT
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>SIGMAA
!></td><td>--></td><td>VALEUR DE SIGMA JONSWAP POUR F
!>    </td></tr>
!>          <tr><td>SIGMAB
!></td><td>--></td><td>VALEUR DE SIGMA JONSWAP POUR F > FP
!>    </td></tr>
!>          <tr><td>SPEC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SPEC(
!></td><td><--</td><td>VALEURS DU SPECTRE TMA
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SPETMA
     &( SPEC  , FREQ  , NF    , AL    , FP     , GAMMA , SIGMAA, SIGMAB,
     &  DEUPI , GRAVIT, E2FMIN, FPMIN , DEPTH  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AL             |-->| CONSTANTE DE PHILLIPS (ALPHA)
C| DEPTH          |-->| PROFONDEUR D'EAU AU POINT CONSIDERE (M)
C| DEUPI          |-->| 2.PI
C| E2FMIN         |-->| SEUIL MINIMUM DE SPECTRE CONSIDERE
C| FP             |-->| FREQUENCE DE PIC DU SPECTRE JONSWAP
C| FPMIN          |-->| VALEUR MINIMUM DE LA FREQUENCE DE PIC
C| FREQ           |---| 
C| FREQ(          |-->| FREQUENCES DE DISCRETISATION
C| GAMMA          |-->| FACTEUR DE FORME DE PIC JONSWAP
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| SIGMAA         |-->| VALEUR DE SIGMA JONSWAP POUR F
C| SIGMAB         |-->| VALEUR DE SIGMA JONSWAP POUR F > FP
C| SPEC           |---| 
C| SPEC(          |<--| VALEURS DU SPECTRE TMA
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NF
      DOUBLE PRECISION GRAVIT, SIGMAA, SIGMAB, GAMMA , DEUPI , FPMIN
      DOUBLE PRECISION FP    , E2FMIN, AL    , DEPTH
      DOUBLE PRECISION SPEC(NF)      , FREQ(NF)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  JF
      DOUBLE PRECISION COEF  , ARG1   , ARG2  , ARG3  , SIG   , FF
      DOUBLE PRECISION ARG4  , OMEGH
C
C
      IF (FP.GT.FPMIN) THEN
        COEF=AL*GRAVIT**2/DEUPI**4
        DO 100 JF=1,NF
          FF=FREQ(JF)
          IF (FF.LT.FP) THEN
            SIG=SIGMAA
          ELSE
            SIG=SIGMAB
          ENDIF
          ARG1=0.5D0*((FF-FP)/(SIG*FP))**2
          IF (ARG1.LT.99.D0) THEN
            ARG1=GAMMA**EXP(-ARG1)
          ELSE
            ARG1=1.D0
          ENDIF
          ARG2=1.25D0*(FP/FF)**4
          IF (ARG2.LT.99.D0) THEN
            ARG2=EXP(-ARG2)
          ELSE
            ARG2=0.D0
          ENDIF
          ARG3=COEF/FF**5
          OMEGH=DEUPI*FF*SQRT(DEPTH/GRAVIT)
          IF (OMEGH.LT.1.D0) THEN
            ARG4=0.5D0*OMEGH*OMEGH
          ELSEIF (OMEGH.LT.2.D0) THEN
            ARG4=1.D0-0.5D0*(2.D0-OMEGH)**2
          ELSE
            ARG4=1.D0
          ENDIF
          SPEC(JF)=ARG1*ARG2*ARG3*ARG4
          IF (SPEC(JF).LT.E2FMIN) SPEC(JF)=0.D0
  100   CONTINUE
      ELSE
        DO 150 JF=1,NF
          SPEC(JF)=0.D0
  150   CONTINUE
      ENDIF
C
      RETURN
      END
C
C#######################################################################
C