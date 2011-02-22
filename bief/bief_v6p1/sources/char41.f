C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       TRACES BACK OR FORTH THE CHARACTERISTIC CURVES
!>                FOR TELEMAC-3D PRISMS
!>                IN TIME INTERVAL DT
!>                USING A FINITE ELEMENT DISCRETISATION.
!>  @code
!>  DISCRETISATION :
!>
!>     THE DOMAIN IS APPROXIMATED BY A FINITE ELEMENT DISCRETISATION.
!>     A LOCAL APPROXIMATION IS DEFINED FOR THE VELOCITY :
!>     THE VALUE AT ANY POINT OF AN ELEMENT ONLY DEPENDS ON THE VALUES
!>     AT THE NODES OF THIS ELEMENT.
!>
!>
!>  RESTRICTIONS AND ASSUMPTIONS:
!>
!>     THE ADVECTION FIELD U IS ASSUMED INDEPENDENT OF TIME.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DT, DX, DY, DZ, ELT, ETA, IBOR, IKLE2, ISO_USELESS, NELEM2, NPLAN, NPLOT, NPOIN2, NRK, NSP, SENS, SHP, SHZ, SURDET, TEST, U, V, W, X, XPLOT, Y, YPLOT, Z, ZPLOT, ZSTAR
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A1, DELTAZ, DENOM, DX1, DXP, DY1, DYP, EPSDZ, EPSILO, I1, I2, I3, IEL, IELE, IET, IET2, IFA, IPLOT, ISO, ISOH, ISOV, ISP, ISUI, PAS, PAS2, XP, YP, ZP
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CHAR41
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CARACT(), DERI3D()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 16/02/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 23/01/2009
!> </td><td>
!> </td><td> CORRECTED A BUG WHEN IS CROSSING A PLANE (SEE PAS2)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 28/08/2008
!> </td><td>
!> </td><td> INVERTED THE LOOPS ON NSP AND IPLOT
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 12/10/2005
!> </td><td>
!> </td><td> CORRECTED A BUG (SEE VARIABLE IELE, WHICH WAS PREVIOUSLY
!>           IEL AND WAS OVERWRITING ANOTHER IEL)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 08/11/2004
!> </td><td>
!> </td><td> ADAPTED TO THE GENERALISED SIGMA TRANSFORM
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 28/04/1993
!> </td><td> J-M JANIN (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS.
!>    </td></tr>
!>          <tr><td>DX,DY,DZ
!></td><td>---</td><td>STOCKAGE DES SOUS-PAS .
!>    </td></tr>
!>          <tr><td>ELT
!></td><td><-></td><td>NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
!>                  NOEUD.
!>    </td></tr>
!>          <tr><td>ETA
!></td><td><-></td><td>NUMEROS DES ETAGES CHOISIS POUR CHAQUE NOEUD.
!>    </td></tr>
!>          <tr><td>IBOR
!></td><td>--></td><td>NUMEROS 2D DES ELEMENTS AYANT UNE FACE COMMUNE
!>                  AVEC L'ELEMENT .  SI IFABOR
!>                  ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>TRANSITION ENTRE LES NUMEROTATIONS LOCALE
!>                  ET GLOBALE DU MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>ISO
!></td><td>---</td><td>STOCKAGE BINAIRE DE LA FACE DE SORTIE.
!>    </td></tr>
!>          <tr><td>ISO_USELESS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS.
!>    </td></tr>
!>          <tr><td>NPLOT
!></td><td>--></td><td>NOMBRE DE DERIVANTS.
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>NRK
!></td><td>--></td><td>NOMBRE DE SOUS-PAS DE RUNGE-KUTTA.
!>    </td></tr>
!>          <tr><td>NSP
!></td><td>---</td><td>NOMBRE DE SOUS-PAS DE RUNGE KUTTA.
!>    </td></tr>
!>          <tr><td>SENS
!></td><td>--></td><td>DESCENTE OU REMONTEE DES CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>SHP
!></td><td><-></td><td>COORDONNEES BARYCENTRIQUES 2D AU PIED DES
!>                  COURBES CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>SHZ
!></td><td><-></td><td>COORDONNEES BARYCENTRIQUES SUIVANT Z DES
!>                  NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
!>    </td></tr>
!>          <tr><td>SURDET
!></td><td>--></td><td>VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
!>    </td></tr>
!>          <tr><td>TEST
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTE DE LA VITESSE DU CONVECTEUR
!>    </td></tr>
!>          <tr><td>X,Y,ZSTAR
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>XPLOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YPLOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>COTE DANS LE MAILLAGE REEL
!>    </td></tr>
!>          <tr><td>ZPLOT
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CHAR41
     & ( U , V , W , DT , NRK , X , Y , ZSTAR , Z , IKLE2 , IBOR ,
     &   XPLOT , YPLOT , ZPLOT , DX , DY , DZ , SHP , SHZ , ELT , ETA ,
     &   NSP , NPLOT , NPOIN2 , NELEM2 , NPLAN , SURDET ,
     &   SENS , ISO_USELESS , TEST )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DT             |-->| PAS DE TEMPS.
C| DX,DY,DZ       |---| STOCKAGE DES SOUS-PAS .
C| ELT            |<->| NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
C|                |   | NOEUD.
C| ETA            |<->| NUMEROS DES ETAGES CHOISIS POUR CHAQUE NOEUD.
C| IBOR           |-->| NUMEROS 2D DES ELEMENTS AYANT UNE FACE COMMUNE
C|                |   | AVEC L'ELEMENT .  SI IFABOR
C|                |   | ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE
C| IKLE2          |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
C|                |   | ET GLOBALE DU MAILLAGE 2D.
C| ISO            |---| STOCKAGE BINAIRE DE LA FACE DE SORTIE.
C| ISO_USELESS    |---| 
C| NELEM2         |-->| NOMBRE D'ELEMENTS DU MAILLAGE 2D.
C| NPLAN          |-->| NOMBRE DE PLANS.
C| NPLOT          |-->| NOMBRE DE DERIVANTS.
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D.
C| NRK            |-->| NOMBRE DE SOUS-PAS DE RUNGE-KUTTA.
C| NSP            |---| NOMBRE DE SOUS-PAS DE RUNGE KUTTA.
C| SENS           |-->| DESCENTE OU REMONTEE DES CARACTERISTIQUES.
C| SHP            |<->| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
C|                |   | COURBES CARACTERISTIQUES.
C| SHZ            |<->| COORDONNEES BARYCENTRIQUES SUIVANT Z DES
C|                |   | NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
C| SURDET         |-->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
C| TEST           |---| 
C| U,V,W          |-->| COMPOSANTE DE LA VITESSE DU CONVECTEUR
C| X,Y,ZSTAR      |-->| COORDONNEES DES POINTS DU MAILLAGE.
C| XPLOT          |---| 
C| YPLOT          |---| 
C| Z             |-->| COTE DANS LE MAILLAGE REEL
C| ZPLOT          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CHAR41 => CHAR41
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN)    :: SENS,NPLAN
      INTEGER         , INTENT(IN)    :: NPOIN2,NELEM2,NPLOT,NRK
      INTEGER         , INTENT(IN)    :: IKLE2(NELEM2,3)
      INTEGER         , INTENT(INOUT) :: ELT(NPLOT),NSP(NPLOT)
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN2,NPLAN),V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: W(NPOIN2,NPLAN),SURDET(NELEM2)
      DOUBLE PRECISION, INTENT(INOUT) :: XPLOT(NPLOT),YPLOT(NPLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: ZPLOT(NPLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPLOT),SHZ(NPLOT)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2),DT
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN2,NPLAN),ZSTAR(NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPLOT),DY(NPLOT),TEST(NPLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: DZ(NPLOT)
      INTEGER         , INTENT(IN)    :: IBOR(NELEM2,5,NPLAN-1)
      INTEGER         , INTENT(INOUT) :: ETA(NPLOT),ISO_USELESS(NPLOT)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELE,ISO
      INTEGER IPLOT,ISP,I1,I2,I3,IEL,IET,IET2,ISOH,ISOV,IFA,ISUI(3)
C
      DOUBLE PRECISION PAS,EPSILO,A1,DX1,DY1,DXP,DYP,XP,YP,ZP,DENOM
      DOUBLE PRECISION DELTAZ,EPSDZ,PAS2
C
      INTRINSIC ABS , INT , MAX , SQRT
C
      DATA ISUI   / 2 , 3 , 1 /
      DATA EPSILO / -1.D-6 /
      DATA EPSDZ  / 1.D-4 /
C
C-----------------------------------------------------------------------
C  COMPUTES THE NUMBER OF SUB-STEPS, THE SAME FOR ALL THE POINTS
C-----------------------------------------------------------------------
C
      DO 10 IPLOT = 1 , NPLOT
C
        IEL = ELT(IPLOT)
        IET = ETA(IPLOT)
C
        I1 = IKLE2(IEL,1)
        I2 = IKLE2(IEL,2)
        I3 = IKLE2(IEL,3)
C
        DXP = U(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT))
     &      + U(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT))
     &      + U(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT))
     &      + U(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT)
     &      + U(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT)
     &      + U(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT)
C
        DYP = V(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT))
     &      + V(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT))
     &      + V(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT))
     &      + V(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT)
     &      + V(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT)
     &      + V(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT)
C
        NSP(IPLOT)=MAX(1,INT(NRK*DT*SQRT((DXP**2+DYP**2)*SURDET(IEL))))
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C  REPEATS FOR ALL R-K STEPS
C-----------------------------------------------------------------------
C
      DO IPLOT=1,NPLOT
C
      PAS = SENS * DT / NSP(IPLOT)
C
      DO ISP = 1 , NSP(IPLOT)
C
C-----------------------------------------------------------------------
C       LOCATES THE ARRIVAL POINT OF THE CHARACTERISTIC CURVE
C-----------------------------------------------------------------------
C
        ISO = 0
C
        PAS2=PAS
C
        IEL = ELT(IPLOT)
        IET = ETA(IPLOT)
C
        I1 = IKLE2(IEL,1)
        I2 = IKLE2(IEL,2)
        I3 = IKLE2(IEL,3)
C
        DX(IPLOT) = ( U(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT))
     &              + U(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT))
     &              + U(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT))
     &              + U(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT)
     &              + U(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT)
     &              + U(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT) ) * PAS
C
               DY(IPLOT) = ( V(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT))
     &                     + V(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT))
     &                     + V(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT))
     &                     + V(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT)
     &                     + V(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT)
     &                     + V(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT) ) * PAS
C
               DELTAZ =  (Z(I1,IET+1)-Z(I1,IET))*SHP(1,IPLOT)
     &                 + (Z(I2,IET+1)-Z(I2,IET))*SHP(2,IPLOT)
     &                 + (Z(I3,IET+1)-Z(I3,IET))*SHP(3,IPLOT)
!
               IF(DELTAZ.GT.EPSDZ) THEN
               DZ(IPLOT) = ( W(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT))
     &                     + W(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT))
     &                     + W(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT))
     &                     + W(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT)
     &                     + W(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT)
     &                     + W(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT) )
     &                     * PAS * (ZSTAR(IET+1)-ZSTAR(IET)) / DELTAZ
               ELSE
                 DZ(IPLOT) = 0.D0
               ENDIF
C
               XP = XPLOT(IPLOT) + DX(IPLOT)
               YP = YPLOT(IPLOT) + DY(IPLOT)
               ZP = ZPLOT(IPLOT) + DZ(IPLOT)
C
               SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                        -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL)
               SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                        -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL)
               SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                        -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL)
               SHZ(IPLOT) = (ZP-ZSTAR(IET)) / (ZSTAR(IET+1)-ZSTAR(IET))
C
               XPLOT(IPLOT) = XP
               YPLOT(IPLOT) = YP
               ZPLOT(IPLOT) = ZP
C
               IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2)
               IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3)
               IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4)
C
               IF(SHZ(IPLOT).LT.EPSILO)      ISO=IBSET(ISO,0)
               IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1)
C
C-----------------------------------------------------------------------
C  SPECIAL TREATMENT FOR THE CHARACTERISTICS LEAVING
C  THE START ELEMENT
C-----------------------------------------------------------------------
C
50          CONTINUE
C
C
           IF(ISO.NE.0) THEN
C
C-----------------------------------------------------------------------
C  AT THIS POINT, KNOWS THE CURVE LEFT THE ELEMENT
C-----------------------------------------------------------------------
C
               ISOH = IAND(ISO,28)
               ISOV = IAND(ISO, 3)
               IEL = ELT(IPLOT)
               IET = ETA(IPLOT)
               XP = XPLOT(IPLOT)
               YP = YPLOT(IPLOT)
               ZP = ZPLOT(IPLOT)
C
               IF(ISOH.NE.0) THEN
                  IF (ISOH.EQ.4) THEN
                     IFA = 2
                  ELSEIF (ISOH.EQ.8) THEN
                     IFA = 3
                  ELSEIF (ISOH.EQ.16) THEN
                     IFA = 1
                  ELSEIF (ISOH.EQ.12) THEN
                     IFA = 2
                     IF (DX(IPLOT)*(Y(IKLE2(IEL,3))-YP).LT.
     &                   DY(IPLOT)*(X(IKLE2(IEL,3))-XP)) IFA = 3
                  ELSEIF (ISOH.EQ.24) THEN
                     IFA = 3
                     IF (DX(IPLOT)*(Y(IKLE2(IEL,1))-YP).LT.
     &                   DY(IPLOT)*(X(IKLE2(IEL,1))-XP)) IFA = 1
                  ELSE
                     IFA = 1
                     IF (DX(IPLOT)*(Y(IKLE2(IEL,2))-YP).LT.
     &                   DY(IPLOT)*(X(IKLE2(IEL,2))-XP)) IFA = 2
                  ENDIF
                  IF(ISOV.GT.0) THEN
                     IF(ABS(DZ(IPLOT)).GT.EPSDZ) THEN
                       A1 = (ZP-ZSTAR(IET+ISOV-1)) / DZ(IPLOT)
                     ELSE
                       A1 = 0.D0
                     ENDIF
                     I1 = IKLE2(IEL,IFA)
                     I2 = IKLE2(IEL,ISUI(IFA))
                     IF ((X(I2)-X(I1))*(YP-A1*DY(IPLOT)-Y(I1)).GT.
     &                 (Y(I2)-Y(I1))*(XP-A1*DX(IPLOT)-X(I1))) IFA=ISOV+3
                  ENDIF
               ELSE
                  IFA = ISOV + 3
               ENDIF
C
               IEL = IBOR(IEL,IFA,IET)
C
               IF (IFA.LE.3) THEN
C
C-----------------------------------------------------------------------
C  AT THIS POINT, KNOWS THE PRISM EXIT SIDE IS QUADRANGULAR
C  =================================================================
C-----------------------------------------------------------------------
C
                  IF(IEL.GT.0) THEN
C
C-----------------------------------------------------------------------
C  AT THIS POINT, KNOWS THE EXIT SIDE IS INTERIOR (WRT DOMAIN)
C  GOES TO ADJACENT ELEMENT
C-----------------------------------------------------------------------
C
                    I1 = IKLE2(IEL,1)
                    I2 = IKLE2(IEL,2)
                    I3 = IKLE2(IEL,3)
C
                    ELT(IPLOT) = IEL
                    SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                            -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL)
                    SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                            -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL)
                    SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                            -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL)
C
                    ISO = ISOV
C
                    IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2)
                    IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3)
                    IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4)
C
                    GOTO 50
C
                  ENDIF
C
                  DXP = DX(IPLOT)
                  DYP = DY(IPLOT)
                  I1  = IKLE2(ELT(IPLOT),IFA)
                  I2  = IKLE2(ELT(IPLOT),ISUI(IFA))
                  DX1 = X(I2) - X(I1)
                  DY1 = Y(I2) - Y(I1)
C
                  IF(IEL.EQ.-1) THEN
C
C-----------------------------------------------------------------------
C  AT THIS POINT, KNOWS THE EXIT SIDE IS A SOLID BOUNDARY
C  PROJECTS THE BALANCE ONTO THE BOUNDARY AND MOVES ON
C-----------------------------------------------------------------------
C
                    A1 = (DXP*DX1+DYP*DY1) / (DX1**2+DY1**2)
C                   MOVES ALONG THE BOUNDARY, STARTING FROM
C                   THE EXIT POINT
                    DX(IPLOT) = A1 * DX1
                    DY(IPLOT) = A1 * DY1
C
                    A1=((XP-X(I1))*DX1+(YP-Y(I1))*DY1)/(DX1**2+DY1**2)
                    SHP(          IFA  ,IPLOT) = 1.D0 - A1
                    SHP(     ISUI(IFA) ,IPLOT) = A1
                    SHP(ISUI(ISUI(IFA)),IPLOT) = 0.D0
                    XPLOT(IPLOT) = X(I1) + A1 * DX1
                    YPLOT(IPLOT) = Y(I1) + A1 * DY1
C
                    ISO = ISOV
C
                    IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2)
                    IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3)
                    IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4)
C
                    GOTO 50
C
                  ENDIF
C
C-----------------------------------------------------------------------
C  AT THIS POINT, KNOWS THE EXIT SIDE IS A LIQUID BOUNDARY
C  STOPS TRACING BACK THE CHARACTERISTICS CURVE (ELT SIGN)
C
C     OR
C
C  AT THIS POINT, KNOWS THE EXIT SIDE IS AN INTERFACE BETWEEN SUB-DOMAINS
C  THE INTERFACE POINT WILL BE TREATED IN THE NEIGHBOURING SUB-DOMAIN;
C  MERELY SETS TEST TO 0
C-----------------------------------------------------------------------
C
C>>>>
C                 A1 = (DXP*(YP-Y(I1))-DYP*(XP-X(I1)))/(DXP*DY1-DYP*DX1)
                  DENOM = DXP*DY1-DYP*DX1
                  IF(ABS(DENOM).GT.1.D-8) THEN
                     A1 = (DXP*(YP-Y(I1))-DYP*(XP-X(I1))) / DENOM
                  ELSE
                     A1 = 0.D0
                  ENDIF
C<<<<
                  IF (A1.GT.1.D0) A1 = 1.D0
                  IF (A1.LT.0.D0) A1 = 0.D0
                  SHP(          IFA  ,IPLOT) = 1.D0 - A1
                  SHP(     ISUI(IFA) ,IPLOT) = A1
                  SHP(ISUI(ISUI(IFA)),IPLOT) = 0.D0
                  XPLOT(IPLOT) = X(I1) + A1 * DX1
                  YPLOT(IPLOT) = Y(I1) + A1 * DY1
                  IF(ABS(DXP).GT.ABS(DYP)) THEN
                     A1 = (XP-XPLOT(IPLOT))/DXP
                  ELSE
                     A1 = (YP-YPLOT(IPLOT))/DYP
                  ENDIF
                  ZPLOT(IPLOT) = ZP - A1*DZ(IPLOT)
                  SHZ(IPLOT) = (ZPLOT(IPLOT)-ZSTAR(IET))
     &                       / (ZSTAR(IET+1)-ZSTAR(IET))
                  ELT(IPLOT) = - SENS * ELT(IPLOT)
                  NSP(IPLOT) = ISP
C
                  IF(IEL.EQ.-2.AND.NCSIZE.GT.1) TEST(IPLOT) = 0.D0
C
               ELSE
C
C-----------------------------------------------------------------------
C  CASE WHERE IFA = 4 OR 5
C  AT THIS POINT, KNOWS THE PRISM EXIT SIDE IS TRIANGULAR
C  =====================================================================
C-----------------------------------------------------------------------
C
                  IFA = IFA - 4
C                 HENCE IFA NOW EQUALS 0 OR 1
C
                  IF (IEL.EQ.1) THEN
C
C-----------------------------------------------------------------------
C  AT THIS POINT, KNOWS THE EXIT SIDE IS INTERIOR (WRT DOMAIN)
C  AND DOES NOT NEED TO UPDATE THE VELOCITIES
C  GOES TO ADJACENT ELEMENT
C-----------------------------------------------------------------------
C
                     ETA(IPLOT) = IET + IFA + IFA - 1
                     SHZ(IPLOT) = (ZP-ZSTAR(ETA(IPLOT)))
     &                   / (ZSTAR(ETA(IPLOT)+1)-ZSTAR(ETA(IPLOT)))
                     ISO = ISOH
                     IF(SHZ(IPLOT).LT.EPSILO)      ISO=IBSET(ISO,0)
                     IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1)
                     GOTO 50
C
                  ENDIF
C
                  IF(IEL.EQ.-1) THEN
C
C-----------------------------------------------------------------------
C  AT THIS POINT, KNOWS THE EXIT SIDE IS A SOLID BOUNDARY
C  PROJECTS THE BALANCE ONTO THE BOUNDARY AND MOVES ON
C-----------------------------------------------------------------------
C
                     ZPLOT(IPLOT) = ZSTAR(IET+IFA)
                     DZ   (IPLOT) = 0.D0
                     SHZ  (IPLOT) = IFA
C
                     ISO = ISOH
                     IF(ISOH.NE.0) GOTO 50
C
                  ELSE
C
C-----------------------------------------------------------------------
C  AT THIS POINT, KNOWS THE EXIT SIDE IS A LIQUID BOUNDARY (CASE 0)
C  STOPS TRACING BACK THE CHARACTERISTICS CURVE (ELT SIGN)
C  OR KNOWS THAT HAVE JUST CROSSED A PLANE AND UPDATE OF THE VELOCITIES
C  IS REQUIRED (CASE 2)
C-----------------------------------------------------------------------
C
                     IF(ABS(DZ(IPLOT)).GT.EPSDZ) THEN
                       A1 = (ZP-ZSTAR(IET+IFA)) / DZ(IPLOT)
                     ELSE
                       A1 = 0.D0
                     ENDIF
C                    RETREATS UNTIL THE CROSSING POINT
                     XP = XP - A1*DX(IPLOT)
                     YP = YP - A1*DY(IPLOT)
                     ZP = ZSTAR(IET+IFA)
                     IELE = ELT(IPLOT)
                     I1 = IKLE2(IELE,1)
                     I2 = IKLE2(IELE,2)
                     I3 = IKLE2(IELE,3)
C
                     SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                           -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IELE)
                     SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                           -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IELE)
                     SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                           -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IELE)
C
                     IF(IEL.EQ.2) THEN
C
C-----------------------------------------------------------------------
C  AT THIS POINT, KNOWS THE EXIT SIDE BELONGS TO A PLANE WHERE UPDATE
C  OF THE VELOCITIES IS REQUIRED
C-----------------------------------------------------------------------
C
C                       IF IFA = 1 EXITS THROUGH THE TOP
C                       IF IFA = 0 EXITS THROUGH THE BOTTOM
C                       THEN NEW IET IS  IET+1 IF IFA = 1
C                                    AND IET-1 IF IFA = 0
C                       THIS IS SUMMARISED BY IET=IET+2*IFA-1
C
C                       RECOMPUTED VELOCITIES MUST BE TAKEN AT IET2=IET+IFA
C                       I.E. BOTTOM IF EXITS THROUGH THE BOTTOM
C                           AND TOP IF EXITS THROUGH THE TOP
C
                        IET2   = IET + IFA
                        IET    = IET + IFA + IFA - 1
C                       REDUCES REMAINING TIME
                        PAS2 = PAS2 * A1
C
                        DX(IPLOT) = ( U(I1,IET2)*SHP(1,IPLOT)
     &                              + U(I2,IET2)*SHP(2,IPLOT)
     &                              + U(I3,IET2)*SHP(3,IPLOT) ) * PAS2
C
                        DY(IPLOT) = ( V(I1,IET2)*SHP(1,IPLOT)
     &                              + V(I2,IET2)*SHP(2,IPLOT)
     &                              + V(I3,IET2)*SHP(3,IPLOT) ) * PAS2
C
                        DELTAZ =  (Z(I1,IET+1)-Z(I1,IET))*SHP(1,IPLOT)
     &                          + (Z(I2,IET+1)-Z(I2,IET))*SHP(2,IPLOT)
     &                          + (Z(I3,IET+1)-Z(I3,IET))*SHP(3,IPLOT)
C
                        IF(DELTAZ.GT.EPSDZ) THEN
                          DZ(IPLOT)=(W(I1,IET2)*SHP(1,IPLOT)
     &                             + W(I2,IET2)*SHP(2,IPLOT)
     &                             + W(I3,IET2)*SHP(3,IPLOT))*PAS2
     &                             *(ZSTAR(IET+1)-ZSTAR(IET))/DELTAZ
                        ELSE
                          DZ(IPLOT) = 0.D0
                        ENDIF
C
                        XP = XP + DX(IPLOT)
                        YP = YP + DY(IPLOT)
                        ZP = ZP + DZ(IPLOT)
C
                        SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                        -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IELE)
                        SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                        -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IELE)
                        SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                        -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IELE)
                        SHZ(IPLOT)=(ZP-ZSTAR(IET))/
     &                                         (ZSTAR(IET+1)-ZSTAR(IET))
C
                        XPLOT(IPLOT) = XP
                        YPLOT(IPLOT) = YP
                        ZPLOT(IPLOT) = ZP
                        ETA(IPLOT) = IET
C
                        ISO = 0
C
                        IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2)
                        IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3)
                        IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4)
C
                        IF(SHZ(IPLOT).LT.EPSILO)      ISO=IBSET(ISO,0)
                        IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1)
C
                        GOTO 50
C
                     ENDIF
C
                     XPLOT(IPLOT) = XP
                     YPLOT(IPLOT) = YP
                     ZPLOT(IPLOT) = ZP
                     SHZ  (IPLOT) = IFA
                     ELT  (IPLOT) = - SENS * ELT(IPLOT)
                     EXIT
C
                  ENDIF
C
               ENDIF
C
            ENDIF
C
      ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C