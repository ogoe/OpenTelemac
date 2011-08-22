!                    *****************
                     SUBROUTINE CHAR41
!                    *****************
!
     & ( U , V , W , DT , NRK , X , Y , ZSTAR , Z , IKLE2 , IBOR ,
     &   XPLOT , YPLOT , ZPLOT , DX , DY , DZ , SHP , SHZ , ELT , ETA ,
     &   NSP , NPLOT , NPOIN2 , NELEM2 , NPLAN , SURDET ,
     &   SENS , ISO_USELESS , TEST )
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    TRACES BACK OR FORTH THE CHARACTERISTIC CURVES
!+                FOR TELEMAC-3D PRISMS
!+                IN TIME INTERVAL DT
!+                USING A FINITE ELEMENT DISCRETISATION.
!code
!+  DISCRETISATION :
!+
!+     THE DOMAIN IS APPROXIMATED BY A FINITE ELEMENT DISCRETISATION.
!+     A LOCAL APPROXIMATION IS DEFINED FOR THE VELOCITY :
!+     THE VALUE AT ANY POINT OF AN ELEMENT ONLY DEPENDS ON THE VALUES
!+     AT THE NODES OF THIS ELEMENT.
!+
!+
!+  RESTRICTIONS AND ASSUMPTIONS:
!+
!+     THE ADVECTION FIELD U IS ASSUMED INDEPENDENT OF TIME.
!
!history  J-M JANIN (LNH)
!+        28/04/1993
!+
!+
!
!history
!+        08/11/2004
!+
!+   ADAPTED TO THE GENERALISED SIGMA TRANSFORM
!
!history
!+        12/10/2005
!+
!+   CORRECTED A BUG (SEE VARIABLE IELE, WHICH WAS PREVIOUSLY
!
!history
!+        28/08/2008
!+
!+   INVERTED THE LOOPS ON NSP AND IPLOT
!
!history
!+        23/01/2009
!+
!+   CORRECTED A BUG WHEN IS CROSSING A PLANE (SEE PAS2)
!
!history  J-M HERVOUET (LNHE)
!+        16/02/2010
!+        V6P0
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| PAS DE TEMPS.
!| DX,DY,DZ       |---| STOCKAGE DES SOUS-PAS .
!| ELT            |<->| NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
!|                |   | NOEUD.
!| ETA            |<->| NUMEROS DES ETAGES CHOISIS POUR CHAQUE NOEUD.
!| IBOR           |-->| NUMEROS 2D DES ELEMENTS AYANT UNE FACE COMMUNE
!|                |   | AVEC L'ELEMENT .  SI IFABOR
!|                |   | ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE
!| IKLE2          |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
!|                |   | ET GLOBALE DU MAILLAGE 2D.
!| ISO_USELESS    |---|
!| NELEM2         |-->| NOMBRE D'ELEMENTS DU MAILLAGE 2D.
!| NPLAN          |-->| NOMBRE DE PLANS.
!| NPLOT          |-->| NOMBRE DE DERIVANTS.
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D.
!| NRK            |-->| NOMBRE DE SOUS-PAS DE RUNGE-KUTTA.
!| NSP            |---| NOMBRE DE SOUS-PAS DE RUNGE KUTTA.
!| SENS           |-->| DESCENTE OU REMONTEE DES CARACTERISTIQUES.
!| SHP            |<->| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
!|                |   | COURBES CARACTERISTIQUES.
!| SHZ            |<->| COORDONNEES BARYCENTRIQUES SUIVANT Z DES
!|                |   | NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
!| SURDET         |-->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
!| TEST           |---|
!| U,V,W          |-->| COMPOSANTE DE LA VITESSE DU CONVECTEUR
!| X,Y,ZSTAR      |-->| COORDONNEES DES POINTS DU MAILLAGE.
!| XPLOT          |---|
!| YPLOT          |---|
!| Z              |-->| COTE DANS LE MAILLAGE REEL
!| ZPLOT          |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CHAR41 => CHAR41
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
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
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELE,ISO
      INTEGER IPLOT,ISP,I1,I2,I3,IEL,IET,IET2,ISOH,ISOV,IFA,ISUI(3)
!
      DOUBLE PRECISION PAS,EPSILO,A1,DX1,DY1,DXP,DYP,XP,YP,ZP,DENOM
      DOUBLE PRECISION DELTAZ,EPSDZ,PAS2
!
      INTRINSIC ABS , INT , MAX , SQRT
!
      DATA ISUI   / 2 , 3 , 1 /
      DATA EPSILO / -1.D-6 /
      DATA EPSDZ  / 1.D-4 /
!
!-----------------------------------------------------------------------
!  COMPUTES THE NUMBER OF SUB-STEPS, THE SAME FOR ALL THE POINTS
!-----------------------------------------------------------------------
!
      DO 10 IPLOT = 1 , NPLOT
!
        IEL = ELT(IPLOT)
        IET = ETA(IPLOT)
!
        I1 = IKLE2(IEL,1)
        I2 = IKLE2(IEL,2)
        I3 = IKLE2(IEL,3)
!
        DXP = U(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT))
     &      + U(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT))
     &      + U(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT))
     &      + U(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT)
     &      + U(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT)
     &      + U(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT)
!
        DYP = V(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT))
     &      + V(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT))
     &      + V(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT))
     &      + V(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT)
     &      + V(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT)
     &      + V(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT)
!
        NSP(IPLOT)=MAX(1,INT(NRK*DT*SQRT((DXP**2+DYP**2)*SURDET(IEL))))
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!  REPEATS FOR ALL R-K STEPS
!-----------------------------------------------------------------------
!
      DO IPLOT=1,NPLOT
!
      PAS = SENS * DT / NSP(IPLOT)
!
      DO ISP = 1 , NSP(IPLOT)
!
!-----------------------------------------------------------------------
!       LOCATES THE ARRIVAL POINT OF THE CHARACTERISTIC CURVE
!-----------------------------------------------------------------------
!
        ISO = 0
!
        PAS2=PAS
!
        IEL = ELT(IPLOT)
        IET = ETA(IPLOT)
!
        I1 = IKLE2(IEL,1)
        I2 = IKLE2(IEL,2)
        I3 = IKLE2(IEL,3)
!
        DX(IPLOT) = ( U(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT))
     &              + U(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT))
     &              + U(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT))
     &              + U(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT)
     &              + U(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT)
     &              + U(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT) ) * PAS
!
               DY(IPLOT) = ( V(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT))
     &                     + V(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT))
     &                     + V(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT))
     &                     + V(I1,IET+1)*SHP(1,IPLOT)*SHZ(IPLOT)
     &                     + V(I2,IET+1)*SHP(2,IPLOT)*SHZ(IPLOT)
     &                     + V(I3,IET+1)*SHP(3,IPLOT)*SHZ(IPLOT) ) * PAS
!
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
!
               XP = XPLOT(IPLOT) + DX(IPLOT)
               YP = YPLOT(IPLOT) + DY(IPLOT)
               ZP = ZPLOT(IPLOT) + DZ(IPLOT)
!
               SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                        -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL)
               SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                        -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL)
               SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                        -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL)
               SHZ(IPLOT) = (ZP-ZSTAR(IET)) / (ZSTAR(IET+1)-ZSTAR(IET))
!
               XPLOT(IPLOT) = XP
               YPLOT(IPLOT) = YP
               ZPLOT(IPLOT) = ZP
!
               IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2)
               IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3)
               IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4)
!
               IF(SHZ(IPLOT).LT.EPSILO)      ISO=IBSET(ISO,0)
               IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1)
!
!-----------------------------------------------------------------------
!  SPECIAL TREATMENT FOR THE CHARACTERISTICS LEAVING
!  THE START ELEMENT
!-----------------------------------------------------------------------
!
50          CONTINUE
!
!
           IF(ISO.NE.0) THEN
!
!-----------------------------------------------------------------------
!  AT THIS POINT, KNOWS THE CURVE LEFT THE ELEMENT
!-----------------------------------------------------------------------
!
               ISOH = IAND(ISO,28)
               ISOV = IAND(ISO, 3)
               IEL = ELT(IPLOT)
               IET = ETA(IPLOT)
               XP = XPLOT(IPLOT)
               YP = YPLOT(IPLOT)
               ZP = ZPLOT(IPLOT)
!
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
!
               IEL = IBOR(IEL,IFA,IET)
!
               IF (IFA.LE.3) THEN
!
!-----------------------------------------------------------------------
!  AT THIS POINT, KNOWS THE PRISM EXIT SIDE IS QUADRANGULAR
!  =================================================================
!-----------------------------------------------------------------------
!
                  IF(IEL.GT.0) THEN
!
!-----------------------------------------------------------------------
!  AT THIS POINT, KNOWS THE EXIT SIDE IS INTERIOR (WRT DOMAIN)
!  GOES TO ADJACENT ELEMENT
!-----------------------------------------------------------------------
!
                    I1 = IKLE2(IEL,1)
                    I2 = IKLE2(IEL,2)
                    I3 = IKLE2(IEL,3)
!
                    ELT(IPLOT) = IEL
                    SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                            -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL)
                    SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                            -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL)
                    SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                            -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL)
!
                    ISO = ISOV
!
                    IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2)
                    IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3)
                    IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4)
!
                    GOTO 50
!
                  ENDIF
!
                  DXP = DX(IPLOT)
                  DYP = DY(IPLOT)
                  I1  = IKLE2(ELT(IPLOT),IFA)
                  I2  = IKLE2(ELT(IPLOT),ISUI(IFA))
                  DX1 = X(I2) - X(I1)
                  DY1 = Y(I2) - Y(I1)
!
                  IF(IEL.EQ.-1) THEN
!
!-----------------------------------------------------------------------
!  AT THIS POINT, KNOWS THE EXIT SIDE IS A SOLID BOUNDARY
!  PROJECTS THE BALANCE ONTO THE BOUNDARY AND MOVES ON
!-----------------------------------------------------------------------
!
                    A1 = (DXP*DX1+DYP*DY1) / (DX1**2+DY1**2)
!                   MOVES ALONG THE BOUNDARY, STARTING FROM
!                   THE EXIT POINT
                    DX(IPLOT) = A1 * DX1
                    DY(IPLOT) = A1 * DY1
!
                    A1=((XP-X(I1))*DX1+(YP-Y(I1))*DY1)/(DX1**2+DY1**2)
                    SHP(          IFA  ,IPLOT) = 1.D0 - A1
                    SHP(     ISUI(IFA) ,IPLOT) = A1
                    SHP(ISUI(ISUI(IFA)),IPLOT) = 0.D0
                    XPLOT(IPLOT) = X(I1) + A1 * DX1
                    YPLOT(IPLOT) = Y(I1) + A1 * DY1
!
                    ISO = ISOV
!
                    IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2)
                    IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3)
                    IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4)
!
                    GOTO 50
!
                  ENDIF
!
!-----------------------------------------------------------------------
!  AT THIS POINT, KNOWS THE EXIT SIDE IS A LIQUID BOUNDARY
!  STOPS TRACING BACK THE CHARACTERISTICS CURVE (ELT SIGN)
!
!     OR
!
!  AT THIS POINT, KNOWS THE EXIT SIDE IS AN INTERFACE BETWEEN SUB-DOMAINS
!  THE INTERFACE POINT WILL BE TREATED IN THE NEIGHBOURING SUB-DOMAIN;
!  MERELY SETS TEST TO 0
!-----------------------------------------------------------------------
!
!>>>>
!                 A1 = (DXP*(YP-Y(I1))-DYP*(XP-X(I1)))/(DXP*DY1-DYP*DX1)
                  DENOM = DXP*DY1-DYP*DX1
                  IF(ABS(DENOM).GT.1.D-8) THEN
                     A1 = (DXP*(YP-Y(I1))-DYP*(XP-X(I1))) / DENOM
                  ELSE
                     A1 = 0.D0
                  ENDIF
!<<<<
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
!
                  IF(IEL.EQ.-2.AND.NCSIZE.GT.1) TEST(IPLOT) = 0.D0
!
               ELSE
!
!-----------------------------------------------------------------------
!  CASE WHERE IFA = 4 OR 5
!  AT THIS POINT, KNOWS THE PRISM EXIT SIDE IS TRIANGULAR
!  =====================================================================
!-----------------------------------------------------------------------
!
                  IFA = IFA - 4
!                 HENCE IFA NOW EQUALS 0 OR 1
!
                  IF (IEL.EQ.1) THEN
!
!-----------------------------------------------------------------------
!  AT THIS POINT, KNOWS THE EXIT SIDE IS INTERIOR (WRT DOMAIN)
!  AND DOES NOT NEED TO UPDATE THE VELOCITIES
!  GOES TO ADJACENT ELEMENT
!-----------------------------------------------------------------------
!
                     ETA(IPLOT) = IET + IFA + IFA - 1
                     SHZ(IPLOT) = (ZP-ZSTAR(ETA(IPLOT)))
     &                   / (ZSTAR(ETA(IPLOT)+1)-ZSTAR(ETA(IPLOT)))
                     ISO = ISOH
                     IF(SHZ(IPLOT).LT.EPSILO)      ISO=IBSET(ISO,0)
                     IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1)
                     GOTO 50
!
                  ENDIF
!
                  IF(IEL.EQ.-1) THEN
!
!-----------------------------------------------------------------------
!  AT THIS POINT, KNOWS THE EXIT SIDE IS A SOLID BOUNDARY
!  PROJECTS THE BALANCE ONTO THE BOUNDARY AND MOVES ON
!-----------------------------------------------------------------------
!
                     ZPLOT(IPLOT) = ZSTAR(IET+IFA)
                     DZ   (IPLOT) = 0.D0
                     SHZ  (IPLOT) = IFA
!
                     ISO = ISOH
                     IF(ISOH.NE.0) GOTO 50
!
                  ELSE
!
!-----------------------------------------------------------------------
!  AT THIS POINT, KNOWS THE EXIT SIDE IS A LIQUID BOUNDARY (CASE 0)
!  STOPS TRACING BACK THE CHARACTERISTICS CURVE (ELT SIGN)
!  OR KNOWS THAT HAVE JUST CROSSED A PLANE AND UPDATE OF THE VELOCITIES
!  IS REQUIRED (CASE 2)
!-----------------------------------------------------------------------
!
                     IF(ABS(DZ(IPLOT)).GT.EPSDZ) THEN
                       A1 = (ZP-ZSTAR(IET+IFA)) / DZ(IPLOT)
                     ELSE
                       A1 = 0.D0
                     ENDIF
!                    RETREATS UNTIL THE CROSSING POINT
                     XP = XP - A1*DX(IPLOT)
                     YP = YP - A1*DY(IPLOT)
                     ZP = ZSTAR(IET+IFA)
                     IELE = ELT(IPLOT)
                     I1 = IKLE2(IELE,1)
                     I2 = IKLE2(IELE,2)
                     I3 = IKLE2(IELE,3)
!
                     SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                           -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IELE)
                     SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                           -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IELE)
                     SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                           -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IELE)
!
                     IF(IEL.EQ.2) THEN
!
!-----------------------------------------------------------------------
!  AT THIS POINT, KNOWS THE EXIT SIDE BELONGS TO A PLANE WHERE UPDATE
!  OF THE VELOCITIES IS REQUIRED
!-----------------------------------------------------------------------
!
!                       IF IFA = 1 EXITS THROUGH THE TOP
!                       IF IFA = 0 EXITS THROUGH THE BOTTOM
!                       THEN NEW IET IS  IET+1 IF IFA = 1
!                                    AND IET-1 IF IFA = 0
!                       THIS IS SUMMARISED BY IET=IET+2*IFA-1
!
!                       RECOMPUTED VELOCITIES MUST BE TAKEN AT IET2=IET+IFA
!                       I.E. BOTTOM IF EXITS THROUGH THE BOTTOM
!                           AND TOP IF EXITS THROUGH THE TOP
!
                        IET2   = IET + IFA
                        IET    = IET + IFA + IFA - 1
!                       REDUCES REMAINING TIME
                        PAS2 = PAS2 * A1
!
                        DX(IPLOT) = ( U(I1,IET2)*SHP(1,IPLOT)
     &                              + U(I2,IET2)*SHP(2,IPLOT)
     &                              + U(I3,IET2)*SHP(3,IPLOT) ) * PAS2
!
                        DY(IPLOT) = ( V(I1,IET2)*SHP(1,IPLOT)
     &                              + V(I2,IET2)*SHP(2,IPLOT)
     &                              + V(I3,IET2)*SHP(3,IPLOT) ) * PAS2
!
                        DELTAZ =  (Z(I1,IET+1)-Z(I1,IET))*SHP(1,IPLOT)
     &                          + (Z(I2,IET+1)-Z(I2,IET))*SHP(2,IPLOT)
     &                          + (Z(I3,IET+1)-Z(I3,IET))*SHP(3,IPLOT)
!
                        IF(DELTAZ.GT.EPSDZ) THEN
                          DZ(IPLOT)=(W(I1,IET2)*SHP(1,IPLOT)
     &                             + W(I2,IET2)*SHP(2,IPLOT)
     &                             + W(I3,IET2)*SHP(3,IPLOT))*PAS2
     &                             *(ZSTAR(IET+1)-ZSTAR(IET))/DELTAZ
                        ELSE
                          DZ(IPLOT) = 0.D0
                        ENDIF
!
                        XP = XP + DX(IPLOT)
                        YP = YP + DY(IPLOT)
                        ZP = ZP + DZ(IPLOT)
!
                        SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                        -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IELE)
                        SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                        -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IELE)
                        SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                        -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IELE)
                        SHZ(IPLOT)=(ZP-ZSTAR(IET))/
     &                                         (ZSTAR(IET+1)-ZSTAR(IET))
!
                        XPLOT(IPLOT) = XP
                        YPLOT(IPLOT) = YP
                        ZPLOT(IPLOT) = ZP
                        ETA(IPLOT) = IET
!
                        ISO = 0
!
                        IF(SHP(1,IPLOT).LT.EPSILO) ISO=IBSET(ISO,2)
                        IF(SHP(2,IPLOT).LT.EPSILO) ISO=IBSET(ISO,3)
                        IF(SHP(3,IPLOT).LT.EPSILO) ISO=IBSET(ISO,4)
!
                        IF(SHZ(IPLOT).LT.EPSILO)      ISO=IBSET(ISO,0)
                        IF(SHZ(IPLOT).GT.1.D0-EPSILO) ISO=IBSET(ISO,1)
!
                        GOTO 50
!
                     ENDIF
!
                     XPLOT(IPLOT) = XP
                     YPLOT(IPLOT) = YP
                     ZPLOT(IPLOT) = ZP
                     SHZ  (IPLOT) = IFA
                     ELT  (IPLOT) = - SENS * ELT(IPLOT)
                     EXIT
!
                  ENDIF
!
               ENDIF
!
            ENDIF
!
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END