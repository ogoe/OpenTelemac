C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GIVES THE VARIABLE NAMES FOR THE RESULTS AND GEOMETRY
!>                FILES (IN TEXTE) AND FOR THE PREVIOUS COMPUTATION
!>                RESULTS FILE (IN TEXTPR).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  TEXTE AND TEXTPR ARE GENERALLY THE SAME EXCEPT IF THE
!>         PREVIOUS COMPUTATION COMES FROM ANOTHER SOFTWARE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> MNEMO, NAMETRAC, NTRAC, TEXTE, TEXTPR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, I_IN_2_LETTERS, NEXT
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>LECDON_TELEMAC3D()

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
!>      <td><center> 5.7                                       </center>
!> </td><td> 15/09/06
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>MNEMO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NAMETRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TEXTE
!></td><td><--</td><td>SEE ABOVE
!>    </td></tr>
!>          <tr><td>TEXTPR
!></td><td><--</td><td>SEE ABOVE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE NOMVAR_2D_IN_3D
     &(TEXTE,TEXTPR,MNEMO,NTRAC,NAMETRAC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| MNEMO          |---| 
C| NAMETRAC       |---| 
C| NTRAC          |---| 
C| TEXTE          |<--| SEE ABOVE
C| TEXTPR         |<--| SEE ABOVE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(*),TEXTPR(*)
      CHARACTER(LEN=8) , INTENT(INOUT) :: MNEMO(*)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMETRAC(32)
      INTEGER, INTENT(IN) :: NTRAC
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=2) I_IN_2_LETTERS(32)
      DATA I_IN_2_LETTERS /'1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 ',
     &                     '10','11','12','13','14','15','16','17','18',
     &                     '19','20','21','22','23','24','25','26','27',
     &                     '28','29','30','31','32'/
C
      INTEGER I,NEXT
C
C-----------------------------------------------------------------------
C
C  ENGLISH
C
      IF(LNG.EQ.2) THEN
C
      TEXTE (1 ) = 'VELOCITY U      M/S             '
      TEXTE (2 ) = 'VELOCITY V      M/S             '
      TEXTE (3 ) = 'CELERITY        M/S             '
      TEXTE (4 ) = 'WATER DEPTH     M               '
      TEXTE (5 ) = 'FREE SURFACE    M               '
      TEXTE (6 ) = 'BOTTOM          M               '
      TEXTE (7 ) = 'FROUDE NUMBER                   '
      TEXTE (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTE (9 ) = 'TRACER                          '
      TEXTE (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      TEXTE (12) = 'VISCOSITY       M2/S            '
      TEXTE (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTE (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTE (15) = 'SCALAR VELOCITY M/S             '
      TEXTE (16) = 'WIND ALONG X    M/S             '
      TEXTE (17) = 'WIND ALONG Y    M/S             '
      TEXTE (18) = 'AIR PRESSURE    PASCAL          '
      TEXTE (19) = 'BOTTOM FRICTION                 '
      TEXTE (20) = 'DRIFT ALONG X   M               '
      TEXTE (21) = 'DRIFT ALONG Y   M               '
      TEXTE (22) = 'COURANT NUMBER                  '
      TEXTE (23) = 'RIGID BED       M               '
      TEXTE (24) = 'FRESH DEPOSITS  M               '
      TEXTE (25) = 'EROSION FLUX    UNIT   ??       '
      TEXTE (26) = 'DEPOSITION PROBA                '
      TEXTE (27) = 'PRIVE 1         ??              '
      TEXTE (28) = 'PRIVE 2         ??              '
      TEXTE (29) = 'PRIVE 3         ??              '
      TEXTE (30) = 'PRIVE 4         ??              '
      TEXTE (31) = 'FRICTION VELOCITM/S             '
      TEXTE (32) = 'SOLID DISCHARGE M2/S            '
      TEXTE (33) = 'SOLID DIS IN X  M2/S            '
      TEXTE (34) = 'SOLID DIS IN Y  M2/S            '
C
C TEXTPR IS USED TO READ PREVIOUS COMPUTATION FILES.
C IN GENERAL TEXTPR=TEXTE BUT YOU CAN FOLLOW UP A COMPUTATION
C FROM ANOTHER CODE WITH DIFFERENT VARIABLE NAMES, WHICH MUST
C BE GIVEN HERE:
C
      TEXTPR (1 ) = 'VELOCITY U      M/S             '
      TEXTPR (2 ) = 'VELOCITY V      M/S             '
      TEXTPR (3 ) = 'CELERITY        M/S             '
      TEXTPR (4 ) = 'WATER DEPTH     M               '
      TEXTPR (5 ) = 'FREE SURFACE    M               '
      TEXTPR (6 ) = 'BOTTOM          M               '
      TEXTPR (7 ) = 'FROUDE NUMBER                   '
      TEXTPR (8 ) = 'SCALAR FLOWRATE M2/S            '
      TEXTPR (9 ) = 'TRACER                          '
      TEXTPR (10) = 'TURBULENT ENERG.JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      TEXTPR (12) = 'VISCOSITY       M2/S            '
      TEXTPR (13) = 'FLOWRATE ALONG XM2/S            '
      TEXTPR (14) = 'FLOWRATE ALONG YM2/S            '
      TEXTPR (15) = 'SCALAR VELOCITY M/S             '
      TEXTPR (16) = 'WIND ALONG X    M/S             '
      TEXTPR (17) = 'WIND ALONG Y    M/S             '
      TEXTPR (18) = 'AIR PRESSURE    PASCAL          '
      TEXTPR (19) = 'BOTTOM FRICTION                 '
      TEXTPR (20) = 'DRIFT ALONG X   M               '
      TEXTPR (21) = 'DRIFT ALONG Y   M               '
      TEXTPR (22) = 'COURANT NUMBER                  '
      TEXTPR (23) = 'RIGID BED       M               '
      TEXTPR (24) = 'FRESH DEPOSITS  M               '
      TEXTPR (25) = 'EROSION FLUX    UNIT   ??       '
      TEXTPR (26) = 'DEPOSITION PROBA                '
      TEXTPR (27) = 'PRIVE 1         ??              '
      TEXTPR (28) = 'PRIVE 2         ??              '
      TEXTPR (29) = 'PRIVE 3         ??              '
      TEXTPR (30) = 'PRIVE 4         ??              '
      TEXTPR (31) = 'FRICTION VELOCITM/S             '
      TEXTPR (32) = 'SOLID DISCHARGE M2/S            '
      TEXTPR (33) = 'SOLID DIS IN X  M2/S            '
      TEXTPR (34) = 'SOLID DIS IN Y  M2/S            '
C
C-----------------------------------------------------------------------
C
C  FRANCAIS OU AUTRE
C
      ELSE
C
      TEXTE (1 ) = 'VITESSE U       M/S             '
      TEXTE (2 ) = 'VITESSE V       M/S             '
      TEXTE (3 ) = 'CELERITE        M/S             '
      TEXTE (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTE (5 ) = 'SURFACE LIBRE   M               '
      TEXTE (6 ) = 'FOND            M               '
      TEXTE (7 ) = 'FROUDE                          '
      TEXTE (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTE (9 ) = 'TRACEUR                         '
      TEXTE (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTE (11) = 'DISSIPATION     WATT/KG         '
      TEXTE (12) = 'VISCOSITE TURB. M2/S            '
      TEXTE (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTE (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTE (15) = 'VITESSE SCALAIREM/S             '
      TEXTE (16) = 'VENT X          M/S             '
      TEXTE (17) = 'VENT Y          M/S             '
      TEXTE (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTE (19) = 'FROTTEMENT                      '
      TEXTE (20) = 'DERIVE EN X     M               '
      TEXTE (21) = 'DERIVE EN Y     M               '
      TEXTE (22) = 'NBRE DE COURANT                 '
      TEXTE (23) = 'FOND RIGIDE     M               '
      TEXTE (24) = 'DEPOT FRAIS     M               '
      TEXTE (25) = 'FLUX D''EROSION  UNITES ??       '
      TEXTE (26) = 'PROBA DE DEPOT                  '
      TEXTE (27) = 'PRIVE 1         ??              '
      TEXTE (28) = 'PRIVE 2         ??              '
      TEXTE (29) = 'PRIVE 3         ??              '
      TEXTE (30) = 'PRIVE 4         ??              '
      TEXTE (31) = 'VITESSE DE FROT.M/S             '
      TEXTE (32) = 'DEBIT SOLIDE    M2/S            '
      TEXTE (33) = 'DEBIT SOL EN X  M2/S            '
      TEXTE (34) = 'DEBIT SOL EN Y  M2/S            '
C
C TEXTPR SERT A LA LECTURE DES FICHIERS DE CALCULS PRECEDENTS
C A PRIORI TEXTPR=TEXTE MAIS ON PEUT ESSAYER DE FAIRE UNE SUITE
C DE CALCUL A PARTIR D'UN AUTRE CODE.
C
      TEXTPR (1 ) = 'VITESSE U       M/S             '
      TEXTPR (2 ) = 'VITESSE V       M/S             '
      TEXTPR (3 ) = 'CELERITE        M/S             '
      TEXTPR (4 ) = 'HAUTEUR D''EAU   M               '
      TEXTPR (5 ) = 'SURFACE LIBRE   M               '
      TEXTPR (6 ) = 'FOND            M               '
      TEXTPR (7 ) = 'FROUDE                          '
      TEXTPR (8 ) = 'DEBIT SCALAIRE  M2/S            '
      TEXTPR (9 ) = 'TRACEUR                         '
      TEXTPR (10) = 'ENERGIE TURBUL. JOULE/KG        '
      TEXTPR (11) = 'DISSIPATION     WATT/KG         '
      TEXTPR (12) = 'VISCOSITE TURB. M2/S            '
      TEXTPR (13) = 'DEBIT SUIVANT X M2/S            '
      TEXTPR (14) = 'DEBIT SUIVANT Y M2/S            '
      TEXTPR (15) = 'VITESSE SCALAIREM/S             '
      TEXTPR (16) = 'VENT X          M/S             '
      TEXTPR (17) = 'VENT Y          M/S             '
      TEXTPR (18) = 'PRESSION ATMOS. PASCAL          '
      TEXTPR (19) = 'FROTTEMENT                      '
      TEXTPR (20) = 'DERIVE EN X     M               '
      TEXTPR (21) = 'DERIVE EN Y     M               '
      TEXTPR (22) = 'NBRE DE COURANT                 '
      TEXTPR (23) = 'FOND RIGIDE     M               '
      TEXTPR (24) = 'DEPOT FRAIS     M               '
      TEXTPR (25) = 'FLUX D''EROSION  UNITES ??       '
      TEXTPR (26) = 'PROBA DE DEPOT                  '
      TEXTPR (27) = 'PRIVE 1         ??              '
      TEXTPR (28) = 'PRIVE 2         ??              '
      TEXTPR (29) = 'PRIVE 3         ??              '
      TEXTPR (30) = 'PRIVE 4         ??              '
      TEXTPR (31) = 'VITESSE DE FROT.M/S             '
      TEXTPR (32) = 'DEBIT SOLIDE    M2/S            '
      TEXTPR (33) = 'DEBIT SOL EN X  M2/S            '
      TEXTPR (34) = 'DEBIT SOL EN Y  M2/S            '
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C   ALIASES FOR THE VARIABLES IN THE STEERING FILE
C
C     UVCHSBFQTKEDIJMXYPWAGLNORZ
C     VELOCITY COMPONENT U
      MNEMO(1)   = 'U       '
C     VELOCITY COMPONENT V
      MNEMO(2)   = 'V       '
C     CELERITY
      MNEMO(3)   = 'C       '
C     WATER DEPTH
      MNEMO(4)   = 'H       '
C     FREE SURFACE ELEVATION
      MNEMO(5)   = 'S       '
C     BOTTOM ELEVATION
      MNEMO(6)   = 'B       '
C     FROUDE
      MNEMO(7)   = 'F       '
C     FLOW RATE
      MNEMO(8)   = 'Q       '
C     TRACER
      MNEMO(9)   = 'T       '
C     TURBULENT ENERGY
      MNEMO(10)   = 'K       '
C     DISSIPATION
      MNEMO(11)   = 'E       '
C     TURBULENT VISCOSITY
      MNEMO(12)   = 'D       '
C     FLOWRATE ALONG X
      MNEMO(13)   = 'I       '
C     FLOWRATE ALONG Y
      MNEMO(14)   = 'J       '
C     SPEED
      MNEMO(15)   = 'M       '
C     WIND COMPONENT X
      MNEMO(16)   = 'X       '
C     WIND COMPONENT Y
      MNEMO(17)   = 'Y       '
C     ATMOSPHERIC PRESSURE
      MNEMO(18)   = 'P       '
C     FRICTION
      MNEMO(19)   = 'W       '
C     DRIFT IN X
      MNEMO(20)   = 'A       '
C     DRIFT IN Y
      MNEMO(21)   = 'G       '
C     COURANT NUMBER
      MNEMO(22)   = 'L       '
C     RIGID BOTTOM
      MNEMO(23)   = 'RB      '
C     FRESH DEPOSIT
      MNEMO(24)   = 'FD      '
C     EROSION FLUX
      MNEMO(25)   = 'EF      '
C     PROBABILITY OF DEPOSITION
      MNEMO(26)   = 'DP      '
C     VARIABLE 27
      MNEMO(27)   = 'PRIVE1  '
C     VARIABLE 28
      MNEMO(28)   = 'PRIVE2  '
C     VARIABLE 29
      MNEMO(29)   = 'PRIVE3  '
C     VARIABLE 30
      MNEMO(30)   = 'PRIVE4  '
C     VARIABLE 31
      MNEMO(31)   = 'US      '
C     SOLID DISCHARGE 32
      MNEMO(32)   = 'QS      '
C     SOLID DISCHARGE ALONG X 33
      MNEMO(33)   = 'QSX     '
C     SOLID DISCHARGE ALONG Y 34
      MNEMO(34)   = 'QSY     '
C
C-----------------------------------------------------------------------
C
      NEXT = 35
C
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          TEXTE(NEXT+I-1) = NAMETRAC(I)
          MNEMO(NEXT+I-1) = 'TA'//I_IN_2_LETTERS(I)//'    '
        ENDDO
      ENDIF
C
      IF(NEXT+NTRAC-1.GT.100) THEN
        IF(LNG.EQ.1) WRITE(LU,98)
98      FORMAT(1X,'NOMVAR_2D_IN_3D : MAXVAR=100 TROP PETIT')
        IF(LNG.EQ.1) WRITE(LU,99)
99      FORMAT(1X,'NOMVAR_2D_IN_3D : MAXVAR=100 TOO SMALL')
      ENDIF
C
      DO I=NEXT,100
        TEXTPR(I)=TEXTE(I)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C