C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CONVERTS A SPECTRUM SPECIFIED IN RELATIVE
!>                FREQUENCY FR(-,-,-) INTO A SPECTRUM IN ABSOLUTE
!>                FREQUENCY FA(-,-,-).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> COSTET, DFREQ, FA, FR, FREQ, GRADEB, GRAPRD, KNEW, LT, NEWF, NEWF1, NF, NPLAN, NPOIN2, RAISF, SINTET, TAUX1, TAUX2, UC, VC, XK
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUXI, DEUPI, F0, FNEW, IMP, IP, JF, JP, KH, NEWM, NEWM1, UK, UNSLRF, Y, Z
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAC()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 12/01//2006
!> </td><td> M. BENOIT (LNHE)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>COSTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COSTET(
!></td><td>--></td><td>COSINUS DES DIRECTIONS DE PROPAGATION
!>    </td></tr>
!>          <tr><td>DFREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DFREQ(
!></td><td>--></td><td>ECARTS DE FREQUENCES DISCRETISES
!>    </td></tr>
!>          <tr><td>FA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FA(
!></td><td><--</td><td>SPECTRE EN FREQUENCE ABSOLUE
!>    </td></tr>
!>          <tr><td>FR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FR(
!></td><td>--></td><td>SPECTRE EN FREQUENCE RELATIVE
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ(
!></td><td>--></td><td>FREQUENCES DISCRETISEES
!>    </td></tr>
!>          <tr><td>GRADEB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAPRD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KNEW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KNEW(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>LT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NEWF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NEWF(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>NEWF1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NEWF1(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS DE PROPAGATION
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>RAISF
!></td><td>--></td><td>RAISON FREQUENTIELLE
!>    </td></tr>
!>          <tr><td>SINTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SINTET(
!></td><td>--></td><td>SINUS DES DIRECTIONS DE PROPAGATION
!>    </td></tr>
!>          <tr><td>TAUX1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUX1(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>TAUX2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUX2(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>UC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UC(
!></td><td>--></td><td>COMPOSANTE OUEST-EST DU CHAMP DE COURANT
!>    </td></tr>
!>          <tr><td>VC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VC(
!></td><td>--></td><td>COMPOSANTE SUD-NORD  DU CHAMP DE COURANT
!>    </td></tr>
!>          <tr><td>XK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XK(
!></td><td>--></td><td>NOMBRE D'ONDE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TRANSF
     &( FA    , FR    , FREQ  , DFREQ , COSTET, SINTET, UC    , VC    ,
     &  XK    , KNEW  , NEWF  , NEWF1 , TAUX1 , TAUX2 , NPOIN2, NPLAN ,
     &  NF    , RAISF , LT    , GRADEB, GRAPRD)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| COSTET         |---| 
C| COSTET(        |-->| COSINUS DES DIRECTIONS DE PROPAGATION
C| DFREQ          |---| 
C| DFREQ(         |-->| ECARTS DE FREQUENCES DISCRETISES
C| FA             |---| 
C| FA(            |<--| SPECTRE EN FREQUENCE ABSOLUE
C| FR             |---| 
C| FR(            |-->| SPECTRE EN FREQUENCE RELATIVE
C| FREQ           |---| 
C| FREQ(          |-->| FREQUENCES DISCRETISEES
C| GRADEB         |---| 
C| GRAPRD         |---| 
C| KNEW           |---| 
C| KNEW(          |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| LT             |---| 
C| NEWF           |---| 
C| NEWF(          |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| NEWF1          |---| 
C| NEWF1(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| NF             |-->| NOMBRE DE FREQUENCES
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE PROPAGATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| RAISF          |-->| RAISON FREQUENTIELLE
C| SINTET         |---| 
C| SINTET(        |-->| SINUS DES DIRECTIONS DE PROPAGATION
C| TAUX1          |---| 
C| TAUX1(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| TAUX2          |---| 
C| TAUX2(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| UC             |---| 
C| UC(            |-->| COMPOSANTE OUEST-EST DU CHAMP DE COURANT
C| VC             |---| 
C| VC(            |-->| COMPOSANTE SUD-NORD  DU CHAMP DE COURANT
C| XK             |---| 
C| XK(            |-->| NOMBRE D'ONDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER          NPOIN2, NPLAN , NF    , LT    , GRADEB, GRAPRD
      INTEGER          KNEW(NPOIN2)  , NEWF(NPOIN2)  , NEWF1(NPOIN2)
      DOUBLE PRECISION RAISF
      DOUBLE PRECISION FA(NPOIN2,NPLAN,NF),FR(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION FREQ(NF),DFREQ(NF),COSTET(NPLAN),SINTET(NPLAN)
      DOUBLE PRECISION UC(NPOIN2),VC(NPOIN2),TAUX1(NPOIN2),TAUX2(NPOIN2)
      DOUBLE PRECISION XK(NPOIN2,NF)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER          IP    , JP    , JF    , NEWM  , NEWM1 , KH
      DOUBLE PRECISION F0    , UK    , DEUPI , AUXI  , Y     , Z
      DOUBLE PRECISION FNEW  , UNSLRF
      LOGICAL          IMP
C
C
C-----------------------------------------------------------------------
C     CHANGES ONLY THE END DATES
C-----------------------------------------------------------------------
      IMP=.FALSE.
      IF ((LT.GE.GRADEB).AND.(MOD(LT-GRADEB,GRAPRD).EQ.0)) IMP=.TRUE.
      IF (.NOT.(IMP)) RETURN
C-----------------------------------------------------------------------
C
C
      DEUPI=2.D0*3.141592654D0
      F0=FREQ(1)
      UNSLRF=1.0D0/DLOG(RAISF)
C
      CALL OV( 'X=C     ' , FA , Y , Z , 0.D0 , NPOIN2*NPLAN*NF)
C
      DO JF=1,NF
C
        DO JP=1,NPLAN
C
          DO IP=1,NPOIN2
C
C           ---------------------------------------------------------
C           COMPUTES THE DIFFERENCE BETWEEN ABSOLUTE AND RELATIVE FREQUENCIES
C                                            -> ->
C                 Z = FREQ_ABS - FREQ_REL = (K .U)/(2.PI)
C           THE SPECTRUM IS PROJECTED ONTO THE ABSOLUTE FREQUENCIES
C           ONLY IF THE RELATIVE VARIATION Z/FREQ_REL IS SIGNIFICANT
C           ---------------------------------------------------------
            UK=SINTET(JP)*UC(IP)+COSTET(JP)*VC(IP)
            Z=UK*XK(IP,JF)/DEUPI
C
            IF (DABS(Z)/FREQ(JF).LT.1.0D-3) THEN
              KNEW (IP)=JP
              NEWF (IP)=JF
              NEWF1(IP)=-1
              TAUX1(IP)=FR(IP,JP,JF)
              TAUX2(IP)=0.0D0
            ELSE
C
C             -------------------------------------------------------
C             COMPUTES FNEW AND KNEW
C             -------------------------------------------------------
              FNEW = FREQ(JF)+Z
              IF (FNEW.GT.0.D0) THEN
                KNEW(IP)=JP
              ELSE
                KNEW(IP)=1+MOD(JP+NPLAN/2-1,NPLAN)
                FNEW=-FNEW
              ENDIF
C
C             -------------------------------------------------------
C             COMPUTES NEWF: INDEX OF THE DISCRETISED FREQUENCY
C             IMMEDIATELY LOWER THAN FNEW
C             -------------------------------------------------------
              IF (FNEW.LT.F0/RAISF) THEN
                NEWF(IP)=-1
              ELSE
                NEWF(IP)=INT(1.0D0+DLOG(FNEW/F0)*UNSLRF)
              ENDIF
C
C             -------------------------------------------------------
C             COMPUTES THE COEFFICIENTS AND INDICES FOR THE PROJECTION
C             -------------------------------------------------------
              IF ((NEWF(IP).LT.NF).AND.(NEWF(IP).GE.1)) THEN
                NEWF1(IP)=NEWF(IP)+1
                AUXI=FR(IP,JP,JF)*DFREQ(JF)
     &               /(FREQ(NEWF1(IP))-FREQ(NEWF(IP)))
                TAUX1(IP)=AUXI*(FREQ(NEWF1(IP))-FNEW)/DFREQ(NEWF(IP))
                TAUX2(IP)=AUXI*(FNEW-FREQ(NEWF(IP)))/DFREQ(NEWF1(IP))
              ELSEIF (NEWF(IP).EQ.0) THEN
                AUXI=FR(IP,JP,JF)*DFREQ(JF)/(F0*(1.D0-1.D0/RAISF))
                TAUX2(IP)=AUXI*(FNEW-F0/RAISF)/DFREQ(1)
                NEWF (IP)=-1
                NEWF1(IP)= 1
              ELSEIF (NEWF(IP).EQ.NF) THEN
                AUXI=FR(IP,JP,JF)*DFREQ(JF)/(FREQ(NF)*(RAISF-1.D0))
                TAUX1(IP)=AUXI*(FREQ(NF)*RAISF-FNEW)/DFREQ(NF)
                NEWF1(IP)=-1
              ELSE
                NEWF (IP)=-1
                NEWF1(IP)=-1
              ENDIF
C
            ENDIF
C
          ENDDO
C
C
C         -------------------------------------------------------
C         PROJECTS THE SPECTRUM
C         -------------------------------------------------------
          DO IP=1,NPOIN2
            NEWM =NEWF (IP)
            NEWM1=NEWF1(IP)
            KH=KNEW(IP)
            IF (NEWM .NE.-1) FA(IP,KH,NEWM )=FA(IP,KH,NEWM )+TAUX1(IP)
            IF (NEWM1.NE.-1) FA(IP,KH,NEWM1)=FA(IP,KH,NEWM1)+TAUX2(IP)
          ENDDO
C
        ENDDO
C
      ENDDO
C
      RETURN
      END
C
C#######################################################################
C