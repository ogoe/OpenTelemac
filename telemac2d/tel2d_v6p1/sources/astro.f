!                    ****************
                     SUBROUTINE ASTRO
!                    ****************
!
     &(YEAR,MONTH,DAY,HOUR,MIN,SEC,AT,ARL,ARS,DL,DS,AL,AS)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE ASTRONOMICAL TERMS NECESSARY FOR THE
!+                COMPUTATION OF THE TIDAL FORCING TERMS.
!code
!+ DESCRIPTION OF THE ASTRONOMICAL PARAMETERS:
!+
!+  NOTE: "PERIODE" IS THE PERIOD OF THE PARAMETER EXPRESSED IN
!+          JOUR/ANNEE/ CENTURY JULIENS
!+
!+ .____.__________________________________________.________.___________.
!+ |CODE|                   ROLE                   |NOTATION| PERIOD    |
!+ |____|__________________________________________|________|___________|
!+ |    |             FONCTION DU TEMPS            |        |           |
!+ | H  |   LONGITUDE SOLAIRE MOYENNE              |   H,L  | 365,25  J |
!+ | S  |   LONGITUDE LUNAIRE MOYENNE              |    S   | 27,32   J |
!+ | P  |   LONGITUDE DU PERIGEE LUNAIRE MOYENNE   |    P   | 8,85    A |
!+ | O  |   LONGITUDE DU NOEUD LUNAIRE ASCENDANT   | N,OMEGA| 18,61   A |
!+ | OM |   INCLINAISON DE L'EQUATEUR SUR          |  OMEGA | 27665,7 S |
!+ |    |     L'ECLIPTIQUE                         |        |           |
!+ | L  |   LONGITUDE VRAIE DE LA LUNE             |    L   |           |
!+ | CR |   PARALLAXE VRAIE DE LA LUNE             |   C/R  |           |
!+ | DL |   DECLINAISON DE LA LUNE                 |  DELTA |           |
!+ | AL |   ASCENSION DROITE DE LA LUNE            |  ALPHA |           |
!+ | NU |   ASCENSION DROITE DE G (EQUATEUR-ORBITE)|    NU  |           |
!+ | ET |   EXCENTRICITE DE L'ORBITE TERRESTRE     |    E   |           |
!+ | MA |   ANOMALIE MOYENNE DU SOLEIL             |    M   | 365,26  J |
!+ | EA |   ANOMALIE EXCENTRIQUE DU SOLEIL         |    E   |           |
!+ | TS |   DISTANCE TERRE-SOLEIL (UA)             |    R   |           |
!+ | TL |   DISTANCE TERRE-LUNE   (KM)             |    R   |           |
!+ | ARL|   RAPPORT RT / TL                        |   A/R  |           |
!+ | ARS|   RAPPORT RT / TS                        |   A/R  |           |
!+ | VS |   ANOMALIE VRAIE DU SOLEIL               |    V   |           |
!+ | LS |   LONGITUDE VRAIE DU SOLEIL              |  TETA  |           |
!+ | AS |   ASCENSION DROITE DU SOLEIL             |  ALPHA |           |
!+ | DS |   DECLINAISON DU SOLEIL                  |  DELTA |           |
!+ |    |                                          |        |           |
!+ |    |               CONSTANTES                 |        |           |
!+ | I0 |   INCLINAISON DE L'ORBITE LUNAIRE        |   I    |           |
!+ |    |     SUR L'ECLIPTIQUE                     |        |           |
!+ | E  |   EXENTRICITE DE L'ORBITE LUNAIRE        |   E    |           |
!+ | M  |   RAPPORT DU MOUVEMENT MOYEN DU SOLEIL   |   M    |           |
!+ |    |     AU MOUVEMENT MOYEN DE LA LUNE        |        |           |
!+ | UA |   UNITE ASTRONOMIQUE EN KM               |   UA   |           |
!+ | RT |   RAYON MOYEN DE LA TERRE                |   A    |           |
!+ | C  |   DEMI GRAND AXE DE L'ORBITE LUNAIRE (KM)|  C,C   |           |
!+ | AC |   RAPPORT RT / C                         |  A/C   |           |
!+ |____|__________________________________________|________|___________|
!+
!+ OTHER CONSTANTS:
!+
!+   PI:   VALUE OF PI
!+   API:  TRANSFORMS DEGREES INTO RADIANS
!+
!+ INTERMEDIATE VARIABLES
!+
!+   KSI,I,X,EA1:  INTERMEDIATE VARIABLES USED FOR THE CALCULATION OF
!+                 THE RISE.
!+                 RIGHT OF THE MOON (A) AND THE LUNAR DECLINATION (D)
!
!history  E. DAVID (LHF)    ;
!+        01/03/1994
!+        V5P2
!+   F LEPEINTRE (LNH) 30 87 78 54; 
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AL             |---| 
!| ARS            |---| 
!| AS             |---| 
!| AT             |-->| TEMPS
!| DS             |---| 
!| HOUR           |---| 
!| MIN            |---| 
!| SEC            |---| 
!| YEAR,MONTH,DAY |-->| DATE DU CALCUL DES TERMES ASTROS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: YEAR,MONTH,DAY,HOUR,MIN,SEC
      DOUBLE PRECISION, INTENT(IN)    :: AT
      DOUBLE PRECISION, INTENT(INOUT) :: ARL,ARS,DL,DS,AL,AS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION T,H,S,P,O,OM,L,CR,ET,MA,EA,TS,VS,LS,JULTIM
      DOUBLE PRECISION I0,E,M,UA,RT,C,AC,PI,API,EA1,KSI,I,X,NU
!
      INTRINSIC ACOS,ASIN,ATAN,COS,SIN,SQRT,TAN,ABS,MOD
!
      DOUBLE PRECISION DMO,ATANC
      EXTERNAL         DMO,ATANC, JULTIM
!
!-----------------------------------------------------------------------
!
      PI  = ACOS (-1.D0)
      API = PI / 180.D0
      I0  = 5.145576994D0 * API
      E   = 0.05490D0
      M   = 0.074804D0
      UA  = 149503899.D0
      RT  = 6378.D0
      C   = 384403.D0
      AC  = RT / C
!
      T   = JULTIM(YEAR,MONTH,DAY,HOUR,MIN,SEC,AT)
!
      H   = DMO ( 279.69668D0 + 36000.76892D0       * T
     &                        + 0.0003025D0         * T * T )
      S   = DMO ( 270.434164D0 + 481267.8831D0      * T
     &                         - 0.001133D0         * T * T
     &                         + 0.0000019D0        * T * T * T )
      P   = DMO ( 334.328019444D0 + 4069.03220556D0 * T
     &                             - 0.01034D0      * T * T
     &                             + 0.0000125D0    * T * T * T )
      O   = DMO ( 259.183275D0 - 1934.1420D0        * T
     &                      + 0.002078D0            * T * T
     &                      + 0.0000022D0           * T * T * T )
      OM  = DMO ( 23.452294D0 - 0.0130125D0         * T
     &                        - 0.00000164D0        * T * T
     &                        + 0.000000503D0       * T * T * T )
      L   = S + 2*E*SIN(S-P) +  5.D0/4.D0 *E*E*SIN(2*(S-P)) +
     &                         15.D0/4.D0 *M*E*SIN(S-2*H+P) +
     &                         11.D0/8.D0 *M*M*SIN(2*(S-H))
      CR  = 1.D0 + E*COS(S-P) +            E*E*COS(2*(S-P)) +
     &                         15.D0/8.D0 *M*E*COS(S-2*H+P) +
     &                                     M*M*COS(2*(S-H))
      KSI=MOD(O-ATAN(SIN(O)/(SIN(I0)/TAN(OM)+COS(I0)*COS(O))),PI)
!
! KSI VARIES FROM -12 TO +12 DEGREES IN 18.7 YEARS
!
      IF (KSI.GT.+API*13.D0) KSI=KSI-PI
      IF (KSI.LT.-API*13.D0) KSI=KSI+PI
!
! CALCULATES I
!
      I   = ACOS( COS(OM)*COS(I0) - SIN(OM)*SIN(I0)*COS(O) )
!
! CALCULATES X: TAN(X) = TAN(L-KSI) * COS(I)
!     WITH X BETWEEN 0 AND 2 PI
!
      X   = ATANC( TAN(L-KSI) * COS(I) , L )
!
      ET  = 0.01675104D0 - 0.0000418D0 * T - 0.000000126D0 * T * T
      MA  = DMO ( 358.47583D0 + 35999.04975D0 * T
     &                        - 0.00015D0     * T * T
     &                        + 0.0000033D0   * T * T * T )
      EA1 = MA
10    CONTINUE
      EA  = MA + ET * SIN (EA1)
      IF (ABS(EA-EA1).GT.1.D-12) THEN
        EA1=EA
        GOTO 10
      ENDIF
      TS  = 1.0000002D0 * ( 1.D0-ET*COS(EA) ) * UA
      VS  = 2.D0 * ATAN( SQRT((1.D0+ET)/(1.D0-ET)) * TAN(EA/2.D0) )
      LS  = H + VS - MA
!
! OUTPUT PARAMETERS
!
      ARL = AC * CR
      ARS = RT / TS
      DL  = ASIN ( SIN(L-KSI) * SIN(I) )
      DS  = ASIN ( SIN(OM)*SIN(LS) )
      NU  = ATANC(SIN(O)/(SIN(OM)/TAN(I0)+COS(OM)*COS(O)),O)
      AL  = X + NU
      AS  = ATAN ( (COS(OM)*SIN(LS) / COS(LS)) )
!
!-----------------------------------------------------------------------
!
      RETURN
      END