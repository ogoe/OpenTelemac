!                    *****************
                     SUBROUTINE CHAR13
!                    *****************
!
     & ( U , V , DT , NRK , X , Y , IKLE , IFABOR ,
     &   XPLOT , YPLOT , DX , DY , SHP , ELT , NSP ,
     &   NPLOT , NPOIN , NELEM , NELMAX , SURDET , SENS , TEST)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    TRACES BACK OR FORTH THE CHARACTERISTIC CURVES
!+                FOR P1 QUADRILATERALS
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
!+     THE DERIVATIVE IS ASSUMED PUNCTUAL HENCE NONDISPERSIVE.
!
!history  J-M HERVOUET (LNHE)
!+        08/08/08
!+        V5P9
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
!| DX,DY          |---| STOCKAGE DES SOUS-PAS .
!| ELT            |<->| NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
!|                |   | CARACTERISTIQUES.
!| IFABOR         |-->| NUMEROS DES ELEMENTS AYANT UNE FACE COMMUNE
!|                |   | AVEC L'ELEMENT .  SI IFABOR
!|                |   | ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE
!| IKLE           |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
!|                |   | ET GLOBALE.
!| NELEM          |-->| NOMBRE D'ELEMENTS.
!| NELMAX         |-->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
!| NPLOT          |-->| NOMBRE DE DERIVANTS.
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
!| NRK            |-->| NOMBRE DE SOUS-PAS DE RUNGE-KUTTA.
!| NSP            |---| NOMBRE DE SOUS-PAS DE RUNGE KUTTA.
!| SENS           |-->| DESCENTE OU REMONTEE DES CARACTERISTIQUES.
!| SHP            |<->| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
!|                |   | COURBES CARACTERISTIQUES.
!| SURDET         |-->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
!| TEST           |---|
!| U,V            |-->| COMPOSANTE DE LA VITESSE DU CONVECTEUR
!| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE.
!| XPLOT,YPLOT    |<->| POSITIONS SUCCESSIVES DES DERIVANTS.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CHAR13 => CHAR13
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: SENS
      INTEGER         , INTENT(IN)    :: NPOIN,NELEM,NELMAX,NPLOT,NRK
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,6),IFABOR(NELMAX,3)
      INTEGER         , INTENT(INOUT) :: ELT(NPLOT),NSP(NPLOT)
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),SURDET(NELEM)
      DOUBLE PRECISION, INTENT(INOUT) :: XPLOT(NPLOT),YPLOT(NPLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPLOT)
      DOUBLE PRECISION, INTENT(IN)    :: DT
      DOUBLE PRECISION, INTENT(IN)    :: X(*),Y(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPLOT),DY(NPLOT),TEST(NPLOT)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPLOT,NSPMAX,ISP,I1,I2,I3,I4,I5,I6,IEL,ISO,IFA,ISUI(3)
!
      DOUBLE PRECISION PAS,EPSILO,A1,DX1,DY1,DXP,DYP,XP,YP,DENOM
!
      DATA ISUI   / 2 , 3 , 1 /
      DATA EPSILO / -1.D-6 /
!
      INTRINSIC INT,MAX,MIN,SQRT
!
!***********************************************************************
!  COMPUTES THE NUMBER OF SUB-STEPS, THE SAME FOR ALL THE POINTS
!-----------------------------------------------------------------------
!
      NSPMAX = 1
!
      DO 10 IPLOT = 1 ,NPLOT
!
         NSP(IPLOT) = 0
         IEL = ELT(IPLOT)
!
         IF(IEL.GT.0) THEN
!
            I1 = IKLE(IEL,1)
            I2 = IKLE(IEL,2)
            I3 = IKLE(IEL,3)
            I4 = IKLE(IEL,4)
            I5 = IKLE(IEL,5)
            I6 = IKLE(IEL,6)
!
            DXP = U(I1)*(2.D0*SHP(1,IPLOT)-1.D0)*SHP(1,IPLOT)
     &          + U(I2)*(2.D0*SHP(2,IPLOT)-1.D0)*SHP(2,IPLOT)
     &          + U(I3)*(2.D0*SHP(3,IPLOT)-1.D0)*SHP(3,IPLOT)
     &          + U(I4)*4.D0*SHP(1,IPLOT)*SHP(2,IPLOT)
     &          + U(I5)*4.D0*SHP(2,IPLOT)*SHP(3,IPLOT)
     &          + U(I6)*4.D0*SHP(3,IPLOT)*SHP(1,IPLOT)
            DYP = V(I1)*(2.D0*SHP(1,IPLOT)-1.D0)*SHP(1,IPLOT)
     &          + V(I2)*(2.D0*SHP(2,IPLOT)-1.D0)*SHP(2,IPLOT)
     &          + V(I3)*(2.D0*SHP(3,IPLOT)-1.D0)*SHP(3,IPLOT)
     &          + V(I4)*4.D0*SHP(1,IPLOT)*SHP(2,IPLOT)
     &          + V(I5)*4.D0*SHP(2,IPLOT)*SHP(3,IPLOT)
     &          + V(I6)*4.D0*SHP(3,IPLOT)*SHP(1,IPLOT)
!
           NSP(IPLOT) = MAX ( 1 ,
     &         INT(NRK*DT*SQRT((DXP*DXP+DYP*DYP)*SURDET(IEL))) )
            NSPMAX = MAX ( NSPMAX , NSP(IPLOT) )
!
         ENDIF
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!  REPEATS FOR ALL R-K STEPS
!-----------------------------------------------------------------------
!
       DO 20 ISP = 1 , NSPMAX
!
!-----------------------------------------------------------------------
!        LOCATES THE ARRIVAL POINT OF THE CHARACTERISTIC CURVE
!-----------------------------------------------------------------------
!
        DO 30 IPLOT = 1 ,NPLOT
!
         IF (ISP.LE.NSP(IPLOT)) THEN
!
               IEL = ELT(IPLOT)
               I1 = IKLE(IEL,1)
               I2 = IKLE(IEL,2)
               I3 = IKLE(IEL,3)
               I4 = IKLE(IEL,4)
               I5 = IKLE(IEL,5)
               I6 = IKLE(IEL,6)
               PAS = SENS * DT / NSP(IPLOT)
!
          DX(IPLOT) = ( U(I1)*(2.D0*SHP(1,IPLOT)-1.D0)*SHP(1,IPLOT)
     &                + U(I2)*(2.D0*SHP(2,IPLOT)-1.D0)*SHP(2,IPLOT)
     &                + U(I3)*(2.D0*SHP(3,IPLOT)-1.D0)*SHP(3,IPLOT)
     &                + U(I4)*4.D0*SHP(1,IPLOT)*SHP(2,IPLOT)
     &                + U(I5)*4.D0*SHP(2,IPLOT)*SHP(3,IPLOT)
     &                + U(I6)*4.D0*SHP(3,IPLOT)*SHP(1,IPLOT)) * PAS
          DY(IPLOT) = ( V(I1)*(2.D0*SHP(1,IPLOT)-1.D0)*SHP(1,IPLOT)
     &                + V(I2)*(2.D0*SHP(2,IPLOT)-1.D0)*SHP(2,IPLOT)
     &                + V(I3)*(2.D0*SHP(3,IPLOT)-1.D0)*SHP(3,IPLOT)
     &                + V(I4)*4.D0*SHP(1,IPLOT)*SHP(2,IPLOT)
     &                + V(I5)*4.D0*SHP(2,IPLOT)*SHP(3,IPLOT)
     &                + V(I6)*4.D0*SHP(3,IPLOT)*SHP(1,IPLOT)) * PAS
!
               XP = XPLOT(IPLOT) + DX(IPLOT)
               YP = YPLOT(IPLOT) + DY(IPLOT)
!
               SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                        -(Y(I3)-Y(I2))*(XP-X(I2)))* SURDET(IEL)
               SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                        -(Y(I1)-Y(I3))*(XP-X(I3)))* SURDET(IEL)
               SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                        -(Y(I2)-Y(I1))*(XP-X(I1)))* SURDET(IEL)
!
               XPLOT(IPLOT) = XP
               YPLOT(IPLOT) = YP
!
            ENDIF
!
30        CONTINUE
!
!-----------------------------------------------------------------------
!  SPECIAL TREATMENT FOR THE CHARACTERISTICS LEAVING
!  THE START ELEMENT
!-----------------------------------------------------------------------
!
         DO 40 IPLOT = 1 , NPLOT
!
         IF(ISP.LE.NSP(IPLOT)) THEN
!
50       CONTINUE
!
            ISO = 0
            IF (SHP(1,IPLOT).LT.EPSILO) ISO = 1
            IF (SHP(2,IPLOT).LT.EPSILO) ISO = ISO + 2
            IF (SHP(3,IPLOT).LT.EPSILO) ISO = ISO + 4
!
            IF (ISO.NE.0) THEN
!
!-----------------------------------------------------------------------
!  AT THIS POINT, KNOWS THE CURVE LEFT THE ELEMENT
!-----------------------------------------------------------------------
!
               IEL =ELT(IPLOT)
               XP = XPLOT(IPLOT)
               YP = YPLOT(IPLOT)
!
               IF     (ISO.EQ.1) THEN
                  IFA = 2
               ELSEIF (ISO.EQ.2) THEN
                  IFA = 3
               ELSEIF (ISO.EQ.4) THEN
                  IFA = 1
               ELSEIF (ISO.EQ.3) THEN
                  IFA = 2
                  IF (DX(IPLOT)*(Y(IKLE(IEL,3))-YP).LT.
     &                DY(IPLOT)*(X(IKLE(IEL,3))-XP)) IFA = 3
               ELSEIF (ISO.EQ.6) THEN
                  IFA = 3
                  IF (DX(IPLOT)*(Y(IKLE(IEL,1))-YP).LT.
     &                DY(IPLOT)*(X(IKLE(IEL,1))-XP)) IFA = 1
               ELSE
                  IFA = 1
                  IF (DX(IPLOT)*(Y(IKLE(IEL,2))-YP).LT.
     &                DY(IPLOT)*(X(IKLE(IEL,2))-XP)) IFA = 2
               ENDIF
!
               IEL = IFABOR(IEL,IFA)
!
               IF (IEL.GT.0) THEN
!
!-----------------------------------------------------------------------
!  AT THIS POINT, KNOWS THE EXIT SIDE IS INTERIOR (WRT DOMAIN)
!  GOES TO ADJACENT ELEMENT
!-----------------------------------------------------------------------
!
               I1 = IKLE(IEL,1)
               I2 = IKLE(IEL,2)
               I3 = IKLE(IEL,3)
               I4 = IKLE(IEL,4)
               I5 = IKLE(IEL,5)
               I6 = IKLE(IEL,6)
!
               ELT(IPLOT) = IEL
               SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                     -(Y(I3)-Y(I2))*(XP-X(I2)))* SURDET(IEL)
               SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                     -(Y(I1)-Y(I3))*(XP-X(I3)))* SURDET(IEL)
               SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                     -(Y(I2)-Y(I1))*(XP-X(I1)))* SURDET(IEL)
!
               GOTO 50
!
               ENDIF
!
               DXP = DX(IPLOT)
               DYP = DY(IPLOT)
               I1  = IKLE(ELT(IPLOT),IFA)
               I2  = IKLE(ELT(IPLOT),ISUI(IFA))
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
                  A1 = (DXP*DX1 + DYP*DY1) / (DX1*DX1 + DY1*DY1)
                  DX(IPLOT) = A1 * DX1
                  DY(IPLOT) = A1 * DY1
!
                A1 = ((XP-X(I1))*DX1+(YP-Y(I1))*DY1)/(DX1*DX1+DY1*DY1)
                  SHP(IFA,IPLOT) = 1.D0-A1
                  SHP(     ISUI(IFA)   ,IPLOT) = A1
                  SHP(ISUI(ISUI(IFA))  ,IPLOT) = 0.D0
                  XPLOT(IPLOT) = X(I1) + A1 * DX1
                  YPLOT(IPLOT) = Y(I1) + A1 * DY1
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
               DENOM = DXP*DY1-DYP*DX1
               IF(DENOM.NE.0.D0) THEN
                 A1  = (DXP*(YP-Y(I1))-DYP*(XP-X(I1))) / DENOM
               ELSE
                 A1  = 0.D0
               ENDIF
               A1 = MAX(MIN(A1,1.D0),0.D0)
                SHP(IFA,IPLOT) = 1.D0-A1
                SHP(     ISUI(IFA)   ,IPLOT) = A1
                SHP(ISUI(ISUI(IFA))  ,IPLOT) = 0.D0
               XPLOT(IPLOT) = X(I1) + A1 * DX1
               YPLOT(IPLOT) = Y(I1) + A1 * DY1
               ELT(IPLOT) = - SENS * ELT(IPLOT)
               NSP(IPLOT) = ISP
!
               IF(IEL.EQ.-2.AND.NCSIZE.GT.1) THEN
                 IF(LNG.EQ.1) THEN
                 WRITE(LU,*) 'CHAR13 : PARALLELISME NON TRAITE'
                 ENDIF
                 IF(LNG.EQ.2) THEN
                 WRITE(LU,*) 'CHAR13: PARALLELISM NOT TREATED'
                 ENDIF
                 CALL PLANTE(1)
                 STOP
               ENDIF
!
            ENDIF
!
         ENDIF
!
40       CONTINUE
!
!
20       CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END