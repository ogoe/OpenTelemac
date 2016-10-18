!                    *************************
                     SUBROUTINE NOMVAR_TOMAWAC
!                    *************************
!
     &(TEXTE,TEXTPR,MNEMO,MAXVAR)
!
!***********************************************************************
! TOMAWAC   V6P1                                   21/06/2011
!***********************************************************************
!
!brief    GIVES THE VARIABLE NAMES FOR THE RESULTS AND GEOMETRY
!+                FILES (TEXTE) AND FOR THE PREVIOUS COMPUTATION
!+                RESULTS FILE (TEXTPR).
!+
!+
!+            TEXTE AND TEXTPR ARE GENERALLY EQUAL EXCEPT IF THE
!+                PREVIOUS COMPUTATION COMES FROM ANOTHER SOFTWARE.
!
!history  MICHEL BENOIT (EDF R&D LNHE)
!+        06/12/2004
!+        V5P5
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  G.MATTAROLO (EDF - LNHE)
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MAXVAR         |-->| MAXIMUM NUMBER OF OUTPUT VARIABLES
!| MNEMO          |<--| ALIASES FOR THE VARIABLES IN THE STEERING FILE
!| TEXTE          |<--| NAMES OF VARIABLES
!| TEXTPR         |<--| NAMES OF VARIABLES OF THE PREVIOUS COMPUTATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_NOMVAR_TOMAWAC => NOMVAR_TOMAWAC
      IMPLICIT NONE
!
!
      INTEGER, INTENT(IN)         ::      MAXVAR
      CHARACTER(LEN=32), INTENT(INOUT) :: TEXTE(MAXVAR),TEXTPR(MAXVAR)
      CHARACTER(LEN=8) , INTENT(INOUT) :: MNEMO(MAXVAR)
!
!-----------------------------------------------------------------------
!
!  ENGLISH
!
      IF (LNG.EQ.2) THEN
!
      TEXTE (1 ) = 'VARIANCE M0     M2              '
      TEXTE (2 ) = 'WAVE HEIGHT HM0 M               '
      TEXTE (3 ) = 'MEAN DIRECTION  DEG             '
      TEXTE (4 ) = 'WAVE SPREAD     DEG             '
      TEXTE (5 ) = 'BOTTOM          M               '
      TEXTE (6 ) = 'WATER DEPTH     M               '
      TEXTE (7 ) = 'VELOCITY U      M/S             '
      TEXTE (8 ) = 'VELOCITY V      M/S             '
      TEXTE (9 ) = 'WIND ALONG X    M/S             '
      TEXTE (10) = 'WIND ALONG Y    M/S             '
      TEXTE (11) = 'FORCE FX        M/S2            '
      TEXTE (12) = 'FORCE FY        M/S2            '
      TEXTE (13) = 'STRESS SXX      M3/S2           '
      TEXTE (14) = 'STRESS SXY      M3/S2           '
      TEXTE (15) = 'STRESS SYY      M3/S2           '
      TEXTE (16) = 'BOTTOM VELOCITY M/S             '
      TEXTE (17) = 'PRIVATE 1       UNIT   ??       '
      TEXTE (18) = 'MEAN FREQ FMOY  HZ              '
      TEXTE (19) = 'MEAN FREQ FM01  HZ              '
      TEXTE (20) = 'MEAN FREQ FM02  HZ              '
      TEXTE (21) = 'PEAK FREQ FPD   HZ              '
      TEXTE (22) = 'PEAK FREQ FPR5  HZ              '
      TEXTE (23) = 'PEAK FREQ FPR8  HZ              '
      TEXTE (24) = 'USTAR           M/S             '
      TEXTE (25) = 'CD                              '
      TEXTE (26) = 'Z0              M               '
      TEXTE (27) = 'WAVE STRESS     KG/(M.S2)       '
      TEXTE (28) = 'MEAN PERIOD TMOYS               '
      TEXTE (29) = 'MEAN PERIOD TM01S               '
      TEXTE (30) = 'MEAN PERIOD TM02S               '
      TEXTE (31) = 'PEAK PERIOD TPD S               '
      TEXTE (32) = 'PEAK PERIOD TPR5S               '
      TEXTE (33) = 'PEAK PERIOD TPR8S               '
      TEXTE (34) = 'WAVE POWER      KW/M            '
      TEXTE (35) = 'BETA                            '
!
! TEXTPR IS USED TO READ PREVIOUS COMPUTATION FILES.
! IN GENERAL TEXTPR=TEXTE UNLESS ANOTHER CODE WAS USED TO
! GENERATE THE PREVIOUS RESULT, IN WHICH CASE THE OUTPUT
! VARIABLE NAMES HAVE TO BE WRITTEN HERE.
!
      TEXTPR (1 ) = 'VARIANCE M0     M2              '
      TEXTPR (2 ) = 'WAVE HEIGHT HM0 M               '
      TEXTPR (3 ) = 'MEAN DIRECTION  DEG             '
      TEXTPR (4 ) = 'WAVE SPREAD     DEG             '
      TEXTPR (5 ) = 'BOTTOM          M               '
      TEXTPR (6 ) = 'WATER DEPTH     M               '
      TEXTPR (7 ) = 'VELOCITY U      M/S             '
      TEXTPR (8 ) = 'VELOCITY V      M/S             '
      TEXTPR (9 ) = 'WIND ALONG X    M/S             '
      TEXTPR (10) = 'WIND ALONG Y    M/S             '
      TEXTPR (11) = 'FORCE FX        M/S2            '
      TEXTPR (12) = 'FORCE FY        M/S2            '
      TEXTPR (13) = 'STRESS SXX      M3/S2           '
      TEXTPR (14) = 'STRESS SXY      M3/S2           '
      TEXTPR (15) = 'STRESS SYY      M3/S2           '
      TEXTPR (16) = 'BOTTOM VELOCITY M/S             '
      TEXTPR (17) = 'PRIVATE 1       UNIT   ??       '
      TEXTPR (18) = 'MEAN FREQ FMOY  HZ              '
      TEXTPR (19) = 'MEAN FREQ FM01  HZ              '
      TEXTPR (20) = 'MEAN FREQ FM02  HZ              '
      TEXTPR (21) = 'PEAK FREQ FPD   HZ              '
      TEXTPR (22) = 'PEAK FREQ FPR5  HZ              '
      TEXTPR (23) = 'PEAK FREQ FPR8  HZ              '
      TEXTPR (24) = 'USTAR           M/S             '
      TEXTPR (25) = 'CD                              '
      TEXTPR (26) = 'Z0              M               '
      TEXTPR (27) = 'WAVE STRESS     KG/(M.S2)       '
      TEXTPR (28) = 'MEAN PERIOD TMOYS               '
      TEXTPR (29) = 'MEAN PERIOD TM01S               '
      TEXTPR (30) = 'MEAN PERIOD TM02S               '
      TEXTPR (31) = 'PEAK PERIOD TPD S               '
      TEXTPR (32) = 'PEAK PERIOD TPR5S               '
      TEXTPR (33) = 'PEAK PERIOD TPR8S               '
      TEXTPR (34) = 'WAVE POWER      KW/M            '
      TEXTPR (35) = 'BETA                            '
!
!-----------------------------------------------------------------------
!
!  FRENCH OR OTHER
!
      ELSE
!
      TEXTE (1 ) = 'VARIANCE M0     M2              '
      TEXTE (2 ) = 'HAUTEUR HM0     M               '
      TEXTE (3 ) = 'DIRECTION MOY   DEG             '
      TEXTE (4 ) = 'ETALEMENT DIREC DEG             '
      TEXTE (5 ) = 'FOND            M               '
      TEXTE (6 ) = 'HAUTEUR D''EAU   M               '
      TEXTE (7 ) = 'VITESSE U       M/S             '
      TEXTE (8 ) = 'VITESSE V       M/S             '
      TEXTE (9 ) = 'VENT X          M/S             '
      TEXTE (10) = 'VENT Y          M/S             '
      TEXTE (11) = 'FORCE FX        M/S2            '
      TEXTE (12) = 'FORCE FY        M/S2            '
      TEXTE (13) = 'CONTRAINTE SXX  M3/S2           '
      TEXTE (14) = 'CONTRAINTE SXY  M3/S2           '
      TEXTE (15) = 'CONTRAINTE SYY  M3/S2           '
      TEXTE (16) = 'VITESSE FOND    M/S             '
      TEXTE (17) = 'PRIVE 1         UNIT   ??       '
      TEXTE (18) = 'FREQ MOY FMOY   HZ              '
      TEXTE (19) = 'FREQ MOY FM01   HZ              '
      TEXTE (20) = 'FREQ MOY FM02   HZ              '
      TEXTE (21) = 'FREQ PIC FPD    HZ              '
      TEXTE (22) = 'FREQ PIC FPR5   HZ              '
      TEXTE (23) = 'FREQ PIC FPR8   HZ              '
      TEXTE (24) = 'USTAR           M/S             '
      TEXTE (25) = 'CD                              '
      TEXTE (26) = 'Z0              M               '
      TEXTE (27) = 'CONTRAINTE SURF KG/(M.S2)       '
      TEXTE (28) = 'PERIODE MOY TMOYS               '
      TEXTE (29) = 'PERIODE MOY TM01S               '
      TEXTE (30) = 'PERIODE MOY TM02S               '
      TEXTE (31) = 'PERIODE PIC TPD S               '
      TEXTE (32) = 'PERIODE PIC TPR5S               '
      TEXTE (33) = 'PERIODE PIC TPR8S               '
      TEXTE (34) = 'PUISSANCE HOULE KW/M            '
      TEXTE (35) = 'BETA                            '
!
! TEXTPR SERT A LA LECTURE DES FICHIERS DE CALCULS PRECEDENTS
! A PRIORI TEXTPR=TEXTE MAIS ON PEUT ESSAYER DE FAIRE UNE SUITE
! DE CALCUL A PARTIR D'UN AUTRE CODE.
!
      TEXTPR (1 ) = 'VARIANCE M0     M2              '
      TEXTPR (2 ) = 'HAUTEUR HM0     M               '
      TEXTPR (3 ) = 'DIRECTION MOY   DEG             '
      TEXTPR (4 ) = 'ETALEMENT DIREC DEG             '
      TEXTPR (5 ) = 'FOND            M               '
      TEXTPR (6 ) = 'HAUTEUR D''EAU   M               '
      TEXTPR (7 ) = 'VITESSE U       M/S             '
      TEXTPR (8 ) = 'VITESSE V       M/S             '
      TEXTPR (9 ) = 'VENT X          M/S             '
      TEXTPR (10) = 'VENT Y          M/S             '
      TEXTPR (11) = 'FORCE FX        M/S2            '
      TEXTPR (12) = 'FORCE FY        M/S2            '
      TEXTPR (13) = 'CONTRAINTE SXX  M3/S2           '
      TEXTPR (14) = 'CONTRAINTE SXY  M3/S2           '
      TEXTPR (15) = 'CONTRAINTE SYY  M3/S2           '
      TEXTPR (16) = 'VITESSE FOND    M/S             '
      TEXTPR (17) = 'PRIVE 1         UNIT   ??       '
      TEXTPR (18) = 'FREQ MOY FMOY   HZ              '
      TEXTPR (19) = 'FREQ MOY FM01   HZ              '
      TEXTPR (20) = 'FREQ MOY FM02   HZ              '
      TEXTPR (21) = 'FREQ PIC FPD    HZ              '
      TEXTPR (22) = 'FREQ PIC FPR5   HZ              '
      TEXTPR (23) = 'FREQ PIC FPR8   HZ              '
      TEXTPR (24) = 'USTAR           M/S             '
      TEXTPR (25) = 'CD                              '
      TEXTPR (26) = 'Z0              M               '
      TEXTPR (27) = 'CONTRAINTE SURF KG/(M.S2)       '
      TEXTPR (28) = 'PERIODE MOY TMOYS               '
      TEXTPR (29) = 'PERIODE MOY TM01S               '
      TEXTPR (30) = 'PERIODE MOY TM02S               '
      TEXTPR (31) = 'PERIODE PIC TPD S               '
      TEXTPR (32) = 'PERIODE PIC TPR5S               '
      TEXTPR (33) = 'PERIODE PIC TPR8S               '
      TEXTPR (34) = 'PUISSANCE HOULE KW/M            '
      TEXTPR (35) = 'BETA                            '
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!   ALIASES FOR THE VARIABLES IN THE STEERING FILE
!
      MNEMO(1)    = 'M0      '
      MNEMO(2)    = 'HM0     '
      MNEMO(3)    = 'DMOY    '
      MNEMO(4)    = 'SPD     '
      MNEMO(5)    = 'ZF      '
      MNEMO(6)    = 'WD      '
      MNEMO(7)    = 'UX      '
      MNEMO(8)    = 'UY      '
      MNEMO(9)    = 'VX      '
      MNEMO(10)   = 'VY      '
      MNEMO(11)   = 'FX      '
      MNEMO(12)   = 'FY      '
      MNEMO(13)   = 'SXX     '
      MNEMO(14)   = 'SXY     '
      MNEMO(15)   = 'SYY     '
      MNEMO(16)   = 'UWB     '
      MNEMO(17)   = 'PRI     '
      MNEMO(18)   = 'FMOY    '
      MNEMO(19)   = 'FM01    '
      MNEMO(20)   = 'FM02    '
      MNEMO(21)   = 'FPD     '
      MNEMO(22)   = 'FPR5    '
      MNEMO(23)   = 'FPR8    '
      MNEMO(24)   = 'US      '
      MNEMO(25)   = 'CD      '
      MNEMO(26)   = 'Z0      '
      MNEMO(27)   = 'WS      '
      MNEMO(28)   = 'TMOY    '
      MNEMO(29)   = 'TM01    '
      MNEMO(30)   = 'TM02    '
      MNEMO(31)   = 'TPD     '
      MNEMO(32)   = 'TPR5    '
      MNEMO(33)   = 'TPR8    '
      MNEMO(34)   = 'POW     '
      MNEMO(35)   = 'BETA    '
!
!-----------------------------------------------------------------------
!
      RETURN
      END
