C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GIVES THE VARIABLE NAMES FOR THE RESULTS AND GEOMETRY
!>                FILES (TEXT) AND FOR THE PREVIOUS COMPUTATION
!>                RESULTS FILE (TEXTPR).<br>
!>                TEXT AND TEXTPR ARE GENERALLY EQUAL EXCEPT IF THE
!>                PREVIOUS COMPUTATION COMES FROM ANOTHER SOFTWARE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>INTERFACE_ARTEMIS
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> MNEMO, TEXTE, TEXTPR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_NOMVAR_ARTEMIS
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>LECDON_ARTEMIS()

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
!>    <td> 13/01/1998                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>MNEMO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TEXTE
!></td><td><--</td><td>NOM DES VARIABLES
!>    </td></tr>
!>          <tr><td>TEXTPR
!></td><td><--</td><td>NOM DES VARIABLES DU CALCUL PRECEDENT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE NOMVAR_ARTEMIS
     &(TEXTE,TEXTPR,MNEMO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| MNEMO          |---| 
C| TEXTE          |<--| NOM DES VARIABLES
C| TEXTPR         |<--| NOM DES VARIABLES DU CALCUL PRECEDENT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_ARTEMIS, EX_NOMVAR_ARTEMIS => NOMVAR_ARTEMIS
      IMPLICIT NONE

      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      CHARACTER*32 TEXTE(26),TEXTPR(26)
      CHARACTER*8  MNEMO(26)
C
C-----------------------------------------------------------------------
C
C  ENGLISH
C
      IF(LNG.EQ.2) THEN
C
      TEXTE (1 ) = 'WAVE HEIGHT     M               '
      TEXTE (2 ) = 'WAVE PHASE      RAD             '
      TEXTE (3 ) = 'U0 SURFACE      M/S             '
      TEXTE (4 ) = 'V0 SURFACE      M/S             '
      TEXTE (5 ) = 'FREE SURFACE    M               '
      TEXTE (6 ) = 'BOTTOM          M               '
      TEXTE (7 ) = 'STILL WATER H   M               '
      TEXTE (8 ) = 'PHASE VELOCITY  M/S             '
      TEXTE (9 ) = 'GROUP VELOCITY  M/S             '
      TEXTE (10) = 'WAVE NUMBER     1/M             '
      TEXTE (11) = 'REAL POTENTIAL  M2/S            '
      TEXTE (12) = 'IMAG POTENTIAL  M2/S            '
      TEXTE (13) = 'PRIVATE 1       UNIT   ??       '
      TEXTE (14) = 'PRIVATE 2       UNIT   ??       '
      TEXTE (15) = 'PRIVATE 3       UNIT   ??       '
      TEXTE (16) = 'PRIVATE 4       UNIT   ??       '
      TEXTE (17) = 'T01             S               '
      TEXTE (18) = 'T02             S               '
      TEXTE (19) = 'TM              S               '
      TEXTE (20) = 'FORCE_FX        M/S2            '
      TEXTE (21) = 'FORCE_FY        M/S2            '
      TEXTE (22) = 'WAVE INCIDENCE  DEG             '
      TEXTE (23) = 'QB                              '
      TEXTE (24) = 'STRESS_SXX      M3/S2           '
      TEXTE (25) = 'STRESS_SXY      M3/S2           '
      TEXTE (26) = 'STRESS_SYY      M3/S2           '
C
C TEXTPR IS USED TO READ PREVIOUS COMPUTATION FILES.
C IN GENERAL TEXTPR=TEXTE UNLESS ANOTHER CODE WAS USED TO
C GENERATE THE PREVIOUS RESULT, IN WHICH CASE THE OUTPUT
C VARIABLE NAMES HAVE TO BE WRITTEN HERE.
C
      TEXTPR (1 ) = 'WAVE HEIGHT     M               '
      TEXTPR (2 ) = 'WAVE PHASE      RAD             '
      TEXTPR (3 ) = 'U0 SURFACE      M/S             '
      TEXTPR (4 ) = 'V0 SURFACE      M/S             '
      TEXTPR (5 ) = 'FREE SURFACE    M               '
      TEXTPR (6 ) = 'BOTTOM          M               '
      TEXTPR (7 ) = 'STILL WATER H   M               '
      TEXTPR (8 ) = 'PHASE VELOCITY  M/S             '
      TEXTPR (9 ) = 'GROUP VELOCITY  M/S             '
      TEXTPR (10) = 'WAVE NUMBER     1/M             '
      TEXTPR (11) = 'REAL POTENTIAL  M2/S            '
      TEXTPR (12) = 'IMAG POTENTIAL  M2/S            '
      TEXTPR (13) = 'PRIVATE 1       UNIT   ??       '
      TEXTPR (14) = 'PRIVATE 2       UNIT   ??       '
      TEXTPR (15) = 'PRIVATE 3       UNIT   ??       '
      TEXTPR (16) = 'PRIVATE 4       UNIT   ??       '
      TEXTPR (17) = 'T01             S               '
      TEXTPR (18) = 'T02             S               '
      TEXTPR (19) = 'TM              S               '
      TEXTPR (20) = 'FORCE_FX        M/S2            '
      TEXTPR (21) = 'FORCE_FY        M/S2            '
      TEXTPR (22) = 'WAVE INCIDENCE  DEG             '
      TEXTPR (23) = 'QB                              '
      TEXTPR (24) = 'STRESS_SXX      M3/S2           '
      TEXTPR (25) = 'STRESS_SXY      M3/S2           '
      TEXTPR (26) = 'STRESS_SYY      M3/S2           '
C
C-----------------------------------------------------------------------
C
C  FRENCH OR OTHER
C
      ELSE
C
      TEXTE (1 ) = 'HAUTEUR HOULE   M               '
      TEXTE (2 ) = 'PHASE HOULE     RAD             '
      TEXTE (3 ) = 'U0 SURFACE      M/S             '
      TEXTE (4 ) = 'V0 SURFACE      M/S             '
      TEXTE (5 ) = 'SURFACE LIBRE   M               '
      TEXTE (6 ) = 'FOND            M               '
      TEXTE (7 ) = 'H EAU REPOS     M               '
      TEXTE (8 ) = 'VITESSE PHASE   M/S             '
      TEXTE (9 ) = 'VITESSE GROUPE  M/S             '
      TEXTE (10) = 'NOMBRE ONDE     1/M             '
      TEXTE (11) = 'POTENTIEL REEL  M2/S            '
      TEXTE (12) = 'POTENTIEL IMAG  M2/S            '
      TEXTE (13) = 'PRIVE 1         UNITES ??       '
      TEXTE (14) = 'PRIVE 2         UNITES ??       '
      TEXTE (15) = 'PRIVE 3         UNITES ??       '
      TEXTE (16) = 'PRIVE 4         UNITES ??       '
      TEXTE (17) = 'T01             S               '
      TEXTE (18) = 'T02             S               '
      TEXTE (19) = 'TM              S               '
      TEXTE (20) = 'FORCE_FX        M/S2            '
      TEXTE (21) = 'FORCE_FY        M/S2            '
      TEXTE (22) = 'INCIDENCE HOULE DEG             '
      TEXTE (23) = 'QB                              '
      TEXTE (24) = 'CONTRAINTE_SXX  M3/S2           '
      TEXTE (25) = 'CONTRAINTE_SXY  M3/S2           '
      TEXTE (26) = 'CONTRAINTE_SYY  M3/S2           '
C
C TEXTPR SERT A LA LECTURE DES FICHIERS DE CALCULS PRECEDENTS
C A PRIORI TEXTPR=TEXTE MAIS ON PEUT ESSAYER DE FAIRE UNE SUITE
C DE CALCUL A PARTIR D'UN AUTRE CODE.
C
      TEXTPR (1 ) = 'HAUTEUR HOULE   M               '
      TEXTPR (2 ) = 'PHASE HOULE     RAD             '
      TEXTPR (3 ) = 'U0 SURFACE      M/S             '
      TEXTPR (4 ) = 'V0 SURFACE      M/S             '
      TEXTPR (5 ) = 'SURFACE LIBRE   M               '
      TEXTPR (6 ) = 'FOND            M               '
      TEXTPR (7 ) = 'H EAU REPOS     M               '
      TEXTPR (8 ) = 'VITESSE PHASE   M/S             '
      TEXTPR (9 ) = 'VITESSE GROUPE  M/S             '
      TEXTPR (10) = 'NOMBRE ONDE     1/M             '
      TEXTPR (11) = 'POTENTIEL REEL  M2/S            '
      TEXTPR (12) = 'POTENTIEL IMAG  M2/S            '
      TEXTPR (13) = 'PRIVE 1         UNITES ??       '
      TEXTPR (14) = 'PRIVE 2         UNITES ??       '
      TEXTPR (15) = 'PRIVE 3         UNITES ??       '
      TEXTPR (16) = 'PRIVE 4         UNITES ??       '
      TEXTPR (17) = 'T01             S               '
      TEXTPR (18) = 'T02             S               '
      TEXTPR (19) = 'TM              S               '
      TEXTPR (20) = 'FORCE_FX        M/S2            '
      TEXTPR (21) = 'FORCE_FY        M/S2            '
      TEXTPR (22) = 'INCIDENCE HOULE DEG             '
      TEXTPR (23) = 'QB                              '
      TEXTPR (24) = 'CONTRAINTE_SXX  M3/S2           '
      TEXTPR (25) = 'CONTRAINTE_SXY  M3/S2           '
      TEXTPR (26) = 'CONTRAINTE_SYY  M3/S2           '
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C   ALIASES FOR THE VARIABLES IN THE STEERING FILE
C
C     USES THE OLD : LETVAR/&#039 DATED; APUVSBHCGKIJDEFOLMNWXT???? ' /
C
      MNEMO(1)    = 'HS      '
      MNEMO(2)    = 'PHAS    '
      MNEMO(3)    = 'U0      '
      MNEMO(4)    = 'V0      '
      MNEMO(5)    = 'ZS      '
      MNEMO(6)    = 'ZF      '
      MNEMO(7)    = 'HW      '
      MNEMO(8)    = 'C       '
      MNEMO(9)    = 'CG      '
      MNEMO(10)   = 'K       '
      MNEMO(11)   = 'PHIR    '
      MNEMO(12)   = 'PHII    '
      MNEMO(13)   = 'D       '
      MNEMO(14)   = 'E       '
      MNEMO(15)   = 'F       '
      MNEMO(16)   = 'G       '
      MNEMO(17)   = 'T01     '
      MNEMO(18)   = 'T02     '
      MNEMO(19)   = 'TM      '
      MNEMO(20)   = 'FX      '
      MNEMO(21)   = 'FY      '
      MNEMO(22)   = 'INC     '
      MNEMO(23)   = 'QB      '
      MNEMO(24)   = 'SXX     '
      MNEMO(25)   = 'SXY     '
      MNEMO(26)   = 'SYY     '
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C