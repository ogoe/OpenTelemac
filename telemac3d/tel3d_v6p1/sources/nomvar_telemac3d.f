C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GIVES THE VARIABLE NAMES FOR THE RESULTS AND GEOMETRY
!>                FILES (IN TEXTE) AND FOR THE PREVIOUS COMPUTATION
!>                RESULTS FILE (IN TEXTPR).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  TEXTE AND TEXTPR ARE GENERALLY THE SAME EXCEPT IF THE
!>         PREVIOUS COMPUTATION COMES FROM ANOTHER SOFTWARE.

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
!>      <td><center> 5.5                                       </center>
!> </td><td> 30/08/2000
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE NOMVAR_TELEMAC3D
     &(TEXT3,TEXTP3,MNEMO,NTRAC,NAMETRAC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| MNEMO          |---| 
C| NAMETRAC       |---| 
C| NTRAC          |---| 
C| TEXT3          |---| 
C| TEXTE          |<--| SEE ABOVE
C| TEXTP3         |---| 
C| TEXTPR         |<--| SEE ABOVE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NTRAC
C                        100 = MAXVAR IN DECLARATIONS_TELEMAC3D
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXT3(100),TEXTP3(100)
      CHARACTER(LEN=32), INTENT(IN)    :: NAMETRAC(32)
      CHARACTER(LEN=8), INTENT(INOUT)  ::  MNEMO(100)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,NEXT
C
      CHARACTER(LEN=2) I_IN_2_LETTERS(32)
      DATA I_IN_2_LETTERS /'1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 ',
     &                     '10','11','12','13','14','15','16','17','18',
     &                     '19','20','21','22','23','24','25','26','27',
     &                     '28','29','30','31','32'/
C
C-----------------------------------------------------------------------
C
C     INITIALISES
C
      DO I=1,100
        MNEMO(I) ='????????'
        TEXT3(I) ='????????????????????????????????'
        TEXTP3(I)='????????????????????????????????'
      ENDDO
C
C     ENGLISH
C
      IF(LNG.EQ.2) THEN
C
      TEXT3 (1 ) = 'ELEVATION Z     M               '
      TEXT3 (2 ) = 'VELOCITY U      M/S             '
      TEXT3 (3 ) = 'VELOCITY V      M/S             '
      TEXT3 (4 ) = 'VELOCITY W      M/S             '
      TEXT3 (5 ) = 'NUX FOR VELOCITYM2/S            '
      TEXT3 (6 ) = 'NUY FOR VELOCITYM2/S            '
      TEXT3 (7 ) = 'NUZ FOR VELOCITYM2/S            '
      TEXT3 (8 ) = 'TURBULENT ENERGYJOULE/KG        '
      TEXT3 (9 ) = 'DISSIPATION     WATT/KG         '
      TEXT3 (10) = 'RICHARDSON NUMB                 '
      TEXT3 (11) = 'RELATIVE DENSITY                '
      TEXT3 (12) = 'DYNAMIC PRESSUREPA              '
      TEXT3 (13) = 'HYDROSTATIC PRESPA              '
C
C TEXTPR IS USED FOR READING PREVIOUS COMPUTATION FILES.
C IN GENERAL TEXTPR=TEXTE BUT YOU CAN FOLLOW UP A COMPUTATION
C FROM ANOTHER CODE WITH DIFFERENT VARIABLE NAMES, WHICH MUST
C BE GIVEN HERE:
C
      TEXTP3 (1 ) = 'ELEVATION Z     M               '
      TEXTP3 (2 ) = 'VELOCITY U      M/S             '
      TEXTP3 (3 ) = 'VELOCITY V      M/S             '
      TEXTP3 (4 ) = 'VELOCITY W      M/S             '
      TEXTP3 (5 ) = 'NUX FOR VELOCITYM2/S            '
      TEXTP3 (6 ) = 'NUY FOR VELOCITYM2/S            '
      TEXTP3 (7 ) = 'NUZ FOR VELOCITYM2/S            '
      TEXTP3 (8 ) = 'TURBULENT ENERGYJOULE/KG        '
      TEXTP3 (9 ) = 'DISSIPATION     WATT/KG         '
      TEXTP3 (10) = 'RICHARDSON NUMB                 '
      TEXTP3 (11) = 'RELATIVE DENSITY                '
      TEXTP3 (12) = 'DYNAMIC PRESSUREPA              '
      TEXTP3 (13) = 'HYDROSTATIC PRESPA              '
C
C-----------------------------------------------------------------------
C
C  FRANCAIS OU AUTRE
C
      ELSE
C
      TEXT3 (1 ) = 'COTE Z          M               '
      TEXT3 (2 ) = 'VITESSE U       M/S             '
      TEXT3 (3 ) = 'VITESSE V       M/S             '
      TEXT3 (4 ) = 'VITESSE W       M/S             '
      TEXT3 (5 ) = 'NUX POUR VITESSEM2/S            '
      TEXT3 (6 ) = 'NUY POUR VITESSEM2/S            '
      TEXT3 (7 ) = 'NUZ POUR VITESSEM2/S            '
      TEXT3 (8 ) = 'ENERGIE TURBULENJOULE/KG        '
      TEXT3 (9 ) = 'DISSIPATION     WATT/KG         '
      TEXT3 (10) = 'NB DE RICHARDSON                '
      TEXT3 (11) = 'DENSITE RELATIVE                '
      TEXT3 (12) = 'PRESSION DYNAMIQPA              '
      TEXT3 (13) = 'PRESSION HYDROSTPA              '
C
C TEXTPR SERT A LA LECTURE DES FICHIERS DE CALCULS PRECEDENTS
C A PRIORI TEXTPR=TEXTE MAIS ON PEUT ESSAYER DE FAIRE UNE SUITE
C DE CALCUL A PARTIR D'UN AUTRE CODE.
C
      TEXTP3 (1 ) = 'COTE Z          M               '
      TEXTP3 (2 ) = 'VITESSE U       M/S             '
      TEXTP3 (3 ) = 'VITESSE V       M/S             '
      TEXTP3 (4 ) = 'VITESSE W       M/S             '
      TEXTP3 (5 ) = 'NUX POUR VITESSEM2/S            '
      TEXTP3 (6 ) = 'NUY POUR VITESSEM2/S            '
      TEXTP3 (7 ) = 'NUZ POUR VITESSEM2/S            '
      TEXTP3 (8 ) = 'ENERGIE TURBULENJOULE/KG        '
      TEXTP3 (9 ) = 'DISSIPATION     WATT/KG         '
      TEXTP3 (10) = 'NB DE RICHARDSON                '
      TEXTP3 (11) = 'DENSITE RELATIVE                '
      TEXTP3 (12) = 'PRESSION DYNAMIQPA              '
      TEXTP3 (13) = 'PRESSION HYDROSTPA              '
C
      ENDIF
C
C WHATEVER THE LANGUAGE: TRACERS
C
      NEXT = 14
C
      IF(NTRAC.GT.0) THEN
        DO I=1,NTRAC
          TEXT3(NEXT+I-1) = NAMETRAC(I)
          MNEMO(NEXT+I-1) = 'TA'//I_IN_2_LETTERS(I)//'    '
        ENDDO
        DO I=1,NTRAC
          TEXT3(NEXT+NTRAC+3*(I-1)  ) = 'NUX '//NAMETRAC(I)(1:12)//
     &                                'M2/S            '
          MNEMO(NEXT+NTRAC+3*(I-1)  ) = 'NAX'//I_IN_2_LETTERS(I)//'   '
          TEXT3(NEXT+NTRAC+3*(I-1)+1) = 'NUY '//NAMETRAC(I)(1:12)//
     &                                'M2/S            '
          MNEMO(NEXT+NTRAC+3*(I-1)+1) = 'NAY'//I_IN_2_LETTERS(I)//'   '
          TEXT3(NEXT+NTRAC+3*(I-1)+2) = 'NUZ '//NAMETRAC(I)(1:12)//
     &                                'M2/S            '
          MNEMO(NEXT+NTRAC+3*(I-1)+2) = 'NAZ'//I_IN_2_LETTERS(I)//'   '
        ENDDO
      ENDIF
C
      IF(NEXT+4*NTRAC-1.GT.100) THEN
        IF(LNG.EQ.1) WRITE(LU,98)
98      FORMAT(1X,'NOMVAR_TELEMAC3D : MAXVAR=100 TROP PETIT')
        IF(LNG.EQ.1) WRITE(LU,99)
99      FORMAT(1X,'NOMVAR_TELEMAC3D : MAXVAR=100 TOO SMALL')
      ENDIF
C
      DO I=NEXT,100
        TEXTP3(I)=TEXT3(I)
      ENDDO
C
C-----------------------------------------------------------------------
C
C   ALIASES FOR THE VARIABLES IN THE STEERING FILE
C
C
      MNEMO(1)   = 'Z       '
      MNEMO(2)   = 'U       '
      MNEMO(3)   = 'V       '
      MNEMO(4)   = 'W       '
      MNEMO(5)   = 'NUX     '
      MNEMO(6)   = 'NUY     '
      MNEMO(7)   = 'NUZ     '
      MNEMO(8)   = 'K       '
      MNEMO(9)   = 'EPS     '
      MNEMO(10)  = 'RI      '
      MNEMO(11)  = 'RHO     '
      MNEMO(12)  = 'DP      '
      MNEMO(13)  = 'PH      '
C
C     MNEMO FOR TRACERS IS DONE ABOVE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
