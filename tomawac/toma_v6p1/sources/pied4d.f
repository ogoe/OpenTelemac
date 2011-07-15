!                    *****************
                     SUBROUTINE PIED4D
!                    *****************
!
     &  (U , V , T , W , DT , NRK , X , Y , TETA , FREQ , IKLE2 ,
     &   IFABOR , ETAS , XPLOT , YPLOT , TPLOT , FPLOT , DX , DY , DW ,
     &   DF , SHP1 , SHP2 , SHP3 , SHT , SHF , ELT , ETA , FRE , NSP ,
     &   NPLOT , NPOIN2 , NELEM2 , NPLAN , NF , SURDET , SENS ,
     &   ISO )
!
!***********************************************************************
! TOMAWAC   V6P1                                   21/06/2011
!***********************************************************************
!
!brief    TRACES IN TIME THE CHARACTERISTICS CURVES FOR TOMAWAC
!+               "HYPER PRISMS", WITHIN THE TIME INTERVAL DT, USING AN
!+                HYBRID DISCRETISATION FINITE ELEMENTS+FINITE DIFF (2D).
!
!note     DISCRETISATION : THE DOMAIN IS APPROXIMATED USING A FINITE
!+         ELEMENT DISCRETISATION. A LOCAL APPROXIMATION IS USED FOR THE
!+         VELOCITY : THE VALUE IN ONE POINT OF AN ELEMENT ONLY DEPENDS
!+         ON THE VALUES AT THE NODES OF THIS ELEMENT.
!note  RESTRICTIONS AND ASSUMPTIONS : THE ADVECTION FIELD U IS
!+         ASSUMED NOT TO VARY WITH TIME.
!
!history  F MARCOS (LNH)
!+        01/02/95
!+        V1P0
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
!+        21/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DT             |-->| TIME STEP
!| DX,DY,DW,DF    |<--| STORED SUB-INCREMENT TIME STEPS
!| ELT            |<->| NUMBERS OF THE ELEMENTS 2D OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETA            |<->| NUMBERS OF THE LAYERS OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETAS           |<->| WORK TABLE INDICATING THE NUMBER OF THE HIGHER
!|                |   | LAYER
!| FPLOT          |<->| STARTING FREQUENCIES OF THE POINTS TO BE 
!|                |   | ADVECTED
!| FRE            |<->| NUMBER OF THE FREQUENCIES OF THE
!|                |   | POINTS TO BE ADVECTED
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID,
!|                |   | SOLID OR PERIODIC BOUNDARY
!| IKLE2          |-->| TRANSITION BETWEEN LOCAL AND GLOBAL NUMBERING
!|                |   | OF THE 2D MESH
!| ISO            |<->| BINARY STORING OF THE EXIT FACE OF THE ELEMENT
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPLOT          |-->| NUMBER OF POINTS TO BE ADVECTED
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NRK            |-->| NUMBER OF RUNGE-KUTTA SUB-ITERATIONS
!| NSP            |<--| NUMBER OF RUNGE-KUTTA SUB-ITERATIONS
!| SENS           |-->| INDICATES TRACE-DOWN OR TRACE-BACK
!|                |   | OF THE CHARACTERISTICS
!| SHF            |<->| BARYCENTRIC COORDINATES ALONG F OF THE
!|                |   | NODES IN THEIR ASSOCIATED LAYER "FRE"
!| SHP1,SHP2,SHP3 |<->| BARYCENTRIC COORDINATES OF THE NODES IN
!|                |   | THEIR ASSOCIATED 2D ELEMENT "ELT"
!| SHT            |<->| BARYCENTRIC COORDINATES ALONG TETA OF THE 
!|                |   | NODES IN THEIR ASSOCIATED LAYER "ETA"
!| SURDET         |-->| 1/DET. OF ELEMENTS 2D FOR ISOPARAM. TRANSF.
!| TPLOT          |<->| STARTING DIRECTIONS OF THE POINTS TO BE
!|                |   | ADVECTED
!| U,V,T,W        |-->| COMPONENTS OF THE ADVECTION VELOCITY
!| X,Y,TETA,FREQ  |-->| COORDINATES OF THE MESH
!| XPLOT          |<->| STARTING ABSCISSAE OF THE POINTS TO BE
!|                |   | ADVECTED
!| YPLOT          |<->| STARTING ORDINATES OF THE POINTS TO BE
!|                |   | ADVECTED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NPOIN2,NELEM2,NPLAN,NPLOT,NSPMAX,NRK,SENS,NF
!
      DOUBLE PRECISION U(NPOIN2,NPLAN,NF),V(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION T(NPOIN2,NPLAN,NF),W(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION XPLOT(NPLOT),YPLOT(NPLOT)
      DOUBLE PRECISION TPLOT(NPLOT),FPLOT(NPLOT)
      DOUBLE PRECISION SURDET(NELEM2),SHT(NPLOT),SHF(NPLOT)
      DOUBLE PRECISION SHP1(NPLOT),SHP2(NPLOT),SHP3(NPLOT)
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2),TETA(NPLAN+1),FREQ(NF)
      DOUBLE PRECISION DX(NPLOT),DY(NPLOT),DW(NPLOT),DF(NPLOT)
      DOUBLE PRECISION PAS,DT,EPSILO,A1,A2
      DOUBLE PRECISION DX1,DY1,DXP,DYP,DTP,DFP,XP,YP,TP,FP
!
      INTEGER IKLE2(NELEM2,3),IFABOR(NELEM2,7),ETAS(NPLAN)
      INTEGER ELT(NPLOT),ETA(NPLOT),FRE(NPLOT),NSP(NPLOT),ISO(NPLOT)
      INTEGER IPLOT,ISP,I1,I2,I3,IEL,IET,IFR,IFA,ISUI(3)
      INTEGER ISOH,ISOT,ISOF,ISOV
!
      INTRINSIC ABS , INT , MAX , SQRT
!
      DATA ISUI   / 2 , 3 , 1 /
      DATA EPSILO / -1.D-6 /
!
!-----------------------------------------------------------------------
!  COMPUTES THE MAXIMUM NUMBER OF SUB-ITERATIONS
!-----------------------------------------------------------------------
!
      NSPMAX = 1
!
      DO 10 IPLOT = 1 , NPLOT
!
         NSP(IPLOT) = 0
         IEL = ELT(IPLOT)
!
         IF (IEL.GT.0) THEN
!
            IET = ETA(IPLOT)
          IFR = FRE(IPLOT)
!
            I1 = IKLE2(IEL,1)
            I2 = IKLE2(IEL,2)
            I3 = IKLE2(IEL,3)
!
         DXP =(1.D0-SHF(IPLOT))*
     &              ( U(I1,IET  ,IFR)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &          + U(I2,IET  ,IFR)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &          + U(I3,IET  ,IFR)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &          + U(I1,ETAS(IET),IFR)*SHP1(IPLOT)*SHT(IPLOT)
     &          + U(I2,ETAS(IET),IFR)*SHP2(IPLOT)*SHT(IPLOT)
     &          + U(I3,ETAS(IET),IFR)*SHP3(IPLOT)*SHT(IPLOT))
     &        + SHF(IPLOT)*
     &              ( U(I1,IET  ,IFR+1)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &          + U(I2,IET  ,IFR+1)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &          + U(I3,IET  ,IFR+1)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &          + U(I1,ETAS(IET),IFR+1)*SHP1(IPLOT)*SHT(IPLOT)
     &          + U(I2,ETAS(IET),IFR+1)*SHP2(IPLOT)*SHT(IPLOT)
     &          + U(I3,ETAS(IET),IFR+1)*SHP3(IPLOT)*SHT(IPLOT))
!
         DYP =(1.D0-SHF(IPLOT))*
     &              ( V(I1,IET  ,IFR)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &          + V(I2,IET  ,IFR)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &          + V(I3,IET  ,IFR)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &          + V(I1,ETAS(IET),IFR)*SHP1(IPLOT)*SHT(IPLOT)
     &          + V(I2,ETAS(IET),IFR)*SHP2(IPLOT)*SHT(IPLOT)
     &          + V(I3,ETAS(IET),IFR)*SHP3(IPLOT)*SHT(IPLOT))
     &        + SHF(IPLOT)*
     &              ( V(I1,IET  ,IFR+1)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &          + V(I2,IET  ,IFR+1)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &          + V(I3,IET  ,IFR+1)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &          + V(I1,ETAS(IET),IFR+1)*SHP1(IPLOT)*SHT(IPLOT)
     &          + V(I2,ETAS(IET),IFR+1)*SHP2(IPLOT)*SHT(IPLOT)
     &          + V(I3,ETAS(IET),IFR+1)*SHP3(IPLOT)*SHT(IPLOT))
!
         DTP =(1.D0-SHF(IPLOT))*
     &              ( T(I1,IET  ,IFR)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &          + T(I2,IET  ,IFR)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &          + T(I3,IET  ,IFR)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &          + T(I1,ETAS(IET),IFR)*SHP1(IPLOT)*SHT(IPLOT)
     &          + T(I2,ETAS(IET),IFR)*SHP2(IPLOT)*SHT(IPLOT)
     &          + T(I3,ETAS(IET),IFR)*SHP3(IPLOT)*SHT(IPLOT))
     &        + SHF(IPLOT)*
     &              ( T(I1,IET  ,IFR+1)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &          + T(I2,IET  ,IFR+1)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &          + T(I3,IET  ,IFR+1)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &          + T(I1,ETAS(IET),IFR+1)*SHP1(IPLOT)*SHT(IPLOT)
     &          + T(I2,ETAS(IET),IFR+1)*SHP2(IPLOT)*SHT(IPLOT)
     &          + T(I3,ETAS(IET),IFR+1)*SHP3(IPLOT)*SHT(IPLOT))
!
         DFP =(1.D0-SHF(IPLOT))*
     &              ( W(I1,IET  ,IFR)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &          + W(I2,IET  ,IFR)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &          + W(I3,IET  ,IFR)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &          + W(I1,ETAS(IET),IFR)*SHP1(IPLOT)*SHT(IPLOT)
     &          + W(I2,ETAS(IET),IFR)*SHP2(IPLOT)*SHT(IPLOT)
     &          + W(I3,ETAS(IET),IFR)*SHP3(IPLOT)*SHT(IPLOT))
     &        + SHF(IPLOT)*
     &              ( W(I1,IET  ,IFR+1)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &          + W(I2,IET  ,IFR+1)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &          + W(I3,IET  ,IFR+1)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &          + W(I1,ETAS(IET),IFR+1)*SHP1(IPLOT)*SHT(IPLOT)
     &          + W(I2,ETAS(IET),IFR+1)*SHP2(IPLOT)*SHT(IPLOT)
     &          + W(I3,ETAS(IET),IFR+1)*SHP3(IPLOT)*SHT(IPLOT))
!
         NSP(IPLOT)= MAX( INT(NRK*DT*ABS(DTP/(TETA(IET)-TETA(IET+1)))),
     &                   INT(NRK*DT*ABS(DFP/(FREQ(IFR)-FREQ(IFR+1)))) )
         NSP(IPLOT)= MAX( NSP(IPLOT),
     &               INT(NRK*DT*SQRT((DXP*DXP+DYP*DYP)*SURDET(IEL))) )
!
          NSP(IPLOT) = MAX (1,NSP(IPLOT))
!
            NSPMAX = MAX ( NSPMAX , NSP(IPLOT) )
!
         ENDIF
!
10    CONTINUE
      IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'NOMBRE MAX DE SOUS PAS :',NSPMAX
      ELSE
         WRITE(LU,*) 'NUMBER OF SUB-ITERATIONS :',NSPMAX
      ENDIF
!
!-----------------------------------------------------------------------
!  LOOP ON NUMBER OF SUB-ITERATIONS
!-----------------------------------------------------------------------
!
      DO 20 ISP = 1 , NSPMAX
!
!-----------------------------------------------------------------------
!  LOCATES THE END POINT OF ALL THE CHARACTERISTICS
!-----------------------------------------------------------------------
!
         DO 30 IPLOT = 1 , NPLOT
!
            ISO(IPLOT) = 0
            IF (ISP.LE.NSP(IPLOT)) THEN
!
               IEL = ELT(IPLOT)
               IET = ETA(IPLOT)
               IFR = FRE(IPLOT)
!
               I1 = IKLE2(IEL,1)
               I2 = IKLE2(IEL,2)
               I3 = IKLE2(IEL,3)
               PAS = SENS * DT / NSP(IPLOT)
!
         DX(IPLOT) = ( (1.D0-SHF(IPLOT))*
     &          ( U(I1,IET  ,IFR)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &      + U(I2,IET  ,IFR)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &      + U(I3,IET  ,IFR)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &      + U(I1,ETAS(IET),IFR)*SHP1(IPLOT)*SHT(IPLOT)
     &      + U(I2,ETAS(IET),IFR)*SHP2(IPLOT)*SHT(IPLOT)
     &      + U(I3,ETAS(IET),IFR)*SHP3(IPLOT)*SHT(IPLOT))
     &        + SHF(IPLOT)*
     &          ( U(I1,IET  ,IFR+1)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &      + U(I2,IET  ,IFR+1)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &      + U(I3,IET  ,IFR+1)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &      + U(I1,ETAS(IET),IFR+1)*SHP1(IPLOT)*SHT(IPLOT)
     &      + U(I2,ETAS(IET),IFR+1)*SHP2(IPLOT)*SHT(IPLOT)
     &      + U(I3,ETAS(IET),IFR+1)*SHP3(IPLOT)*SHT(IPLOT)) )*PAS
!
         DY(IPLOT) = ( (1.D0-SHF(IPLOT))*
     &          ( V(I1,IET  ,IFR)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &      + V(I2,IET  ,IFR)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &      + V(I3,IET  ,IFR)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &      + V(I1,ETAS(IET),IFR)*SHP1(IPLOT)*SHT(IPLOT)
     &      + V(I2,ETAS(IET),IFR)*SHP2(IPLOT)*SHT(IPLOT)
     &      + V(I3,ETAS(IET),IFR)*SHP3(IPLOT)*SHT(IPLOT))
     &        + SHF(IPLOT)*
     &          ( V(I1,IET  ,IFR+1)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &      + V(I2,IET  ,IFR+1)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &      + V(I3,IET  ,IFR+1)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &      + V(I1,ETAS(IET),IFR+1)*SHP1(IPLOT)*SHT(IPLOT)
     &      + V(I2,ETAS(IET),IFR+1)*SHP2(IPLOT)*SHT(IPLOT)
     &      + V(I3,ETAS(IET),IFR+1)*SHP3(IPLOT)*SHT(IPLOT)) )*PAS
!
         DW(IPLOT) = ( (1.D0-SHF(IPLOT))*
     &          ( T(I1,IET  ,IFR)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &      + T(I2,IET  ,IFR)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &      + T(I3,IET  ,IFR)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &      + T(I1,ETAS(IET),IFR)*SHP1(IPLOT)*SHT(IPLOT)
     &      + T(I2,ETAS(IET),IFR)*SHP2(IPLOT)*SHT(IPLOT)
     &      + T(I3,ETAS(IET),IFR)*SHP3(IPLOT)*SHT(IPLOT))
     &        + SHF(IPLOT)*
     &          ( T(I1,IET  ,IFR+1)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &      + T(I2,IET  ,IFR+1)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &      + T(I3,IET  ,IFR+1)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &      + T(I1,ETAS(IET),IFR+1)*SHP1(IPLOT)*SHT(IPLOT)
     &      + T(I2,ETAS(IET),IFR+1)*SHP2(IPLOT)*SHT(IPLOT)
     &      + T(I3,ETAS(IET),IFR+1)*SHP3(IPLOT)*SHT(IPLOT)) )*PAS
!
         DF(IPLOT) = ( (1.D0-SHF(IPLOT))*
     &          ( W(I1,IET  ,IFR)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &      + W(I2,IET  ,IFR)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &      + W(I3,IET  ,IFR)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &      + W(I1,ETAS(IET),IFR)*SHP1(IPLOT)*SHT(IPLOT)
     &      + W(I2,ETAS(IET),IFR)*SHP2(IPLOT)*SHT(IPLOT)
     &      + W(I3,ETAS(IET),IFR)*SHP3(IPLOT)*SHT(IPLOT))
     &        + SHF(IPLOT)*
     &          ( W(I1,IET  ,IFR+1)*SHP1(IPLOT)*(1.D0-SHT(IPLOT))
     &      + W(I2,IET  ,IFR+1)*SHP2(IPLOT)*(1.D0-SHT(IPLOT))
     &      + W(I3,IET  ,IFR+1)*SHP3(IPLOT)*(1.D0-SHT(IPLOT))
     &      + W(I1,ETAS(IET),IFR+1)*SHP1(IPLOT)*SHT(IPLOT)
     &      + W(I2,ETAS(IET),IFR+1)*SHP2(IPLOT)*SHT(IPLOT)
     &      + W(I3,ETAS(IET),IFR+1)*SHP3(IPLOT)*SHT(IPLOT)) )*PAS
!
               XP = XPLOT(IPLOT) + DX(IPLOT)
               YP = YPLOT(IPLOT) + DY(IPLOT)
               TP = TPLOT(IPLOT) + DW(IPLOT)
               FP = FPLOT(IPLOT) + DF(IPLOT)
!
               SHP1(IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                        -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL)
               SHP2(IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                        -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL)
               SHP3(IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                        -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL)
               SHT(IPLOT) = (TP-TETA(IET)) / (TETA(IET+1)-TETA(IET))
               SHF(IPLOT) = (FP-FREQ(IFR)) / (FREQ(IFR+1)-FREQ(IFR))
!             IF (ABS(SHT(IPLOT)).GT.2.5D0 ) THEN
!         WRITE(LU,*) 'SHT***',IPLOT,IET,SHT(IPLOT)
!         WRITE(LU,*) TETA(IET),TETA(IET+1),ZP
!         WRITE(LU,*) DZ(IPLOT),ZPLOT(IPLOT)
!         STOP
!              ENDIF
!
               XPLOT(IPLOT) = XP
               YPLOT(IPLOT) = YP
               TPLOT(IPLOT) = TP
               FPLOT(IPLOT) = FP
!
               IF (SHP1(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),4)
               IF (SHP2(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),5)
               IF (SHP3(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),6)
!
               IF  (SHT(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),0)
               IF  (SHT(IPLOT).GT.1.D0-EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),1)
!
               IF  (SHF(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),2)
               IF  (SHF(IPLOT).GT.1.D0-EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),3)
!
            ENDIF
!
30       CONTINUE
!
!-----------------------------------------------------------------------
!  TREATS DIFFERENTLY THE CHARACTERISTICS ISSUED FROM
!  THE START ELEMENT
!-----------------------------------------------------------------------
!
         DO 40 IPLOT = 1 , NPLOT
!
50          CONTINUE
!
            IF (ISO(IPLOT).NE.0) THEN
!
!-----------------------------------------------------------------------
!  HERE: LEFT THE ELEMENT
!-----------------------------------------------------------------------
!
              ISOT = IAND(ISO(IPLOT), 3)
              ISOF = IAND(ISO(IPLOT),12)/4
            ISOV = IAND(ISO(IPLOT),15)
              ISOH = IAND(ISO(IPLOT),112)
              IEL = ELT(IPLOT)
              IET = ETA(IPLOT)
              IFR = FRE(IPLOT)
              XP = XPLOT(IPLOT)
              YP = YPLOT(IPLOT)
              TP = TPLOT(IPLOT)
              FP = FPLOT(IPLOT)
!
              IF (ISOH.NE.0) THEN
!
                IF (ISOH.EQ.16) THEN
                   IFA = 2
                ELSEIF (ISOH.EQ.32) THEN
                   IFA = 3
                ELSEIF (ISOH.EQ.64) THEN
                   IFA = 1
                ELSEIF (ISOH.EQ.48) THEN
                   IFA = 2
                   IF (DX(IPLOT)*(Y(IKLE2(IEL,3))-YP).LT.
     &                 DY(IPLOT)*(X(IKLE2(IEL,3))-XP)) IFA = 3
                ELSEIF (ISOH.EQ.96) THEN
                   IFA = 3
                   IF (DX(IPLOT)*(Y(IKLE2(IEL,1))-YP).LT.
     &                 DY(IPLOT)*(X(IKLE2(IEL,1))-XP)) IFA = 1
                ELSE
                   IFA = 1
                   IF (DX(IPLOT)*(Y(IKLE2(IEL,2))-YP).LT.
     &                 DY(IPLOT)*(X(IKLE2(IEL,2))-XP)) IFA = 2
                ENDIF
!
                IF (ISOV.GT.0) THEN
                  I1 = IKLE2(IEL,IFA)
                  I2 = IKLE2(IEL,ISUI(IFA))
                  IF (ISOF.GT.0) THEN
      	      IF (ISOT.GT.0) THEN
      	        A1=(FP-FREQ(IFR+ISOF-1))/DF(IPLOT)
      	        A2=(TP-TETA(IET+ISOT-1))/DW(IPLOT)
      	        IF (A1.LT.A2) THEN
                          IF ((X(I2)-X(I1))*(YP-A1*DY(IPLOT)-Y(I1)).GT.
     &             (Y(I2)-Y(I1))*(XP-A1*DX(IPLOT)-X(I1))) IFA=ISOF+5
      	        ELSE
                          IF ((X(I2)-X(I1))*(YP-A2*DY(IPLOT)-Y(I1)).GT.
     &             (Y(I2)-Y(I1))*(XP-A2*DX(IPLOT)-X(I1))) IFA=ISOT+3
      		ENDIF
      	      ELSE
                        A1 = (FP-FREQ(IFR+ISOF-1)) / DF(IPLOT)
                        IF ((X(I2)-X(I1))*(YP-A1*DY(IPLOT)-Y(I1)).GT.
     &             (Y(I2)-Y(I1))*(XP-A1*DX(IPLOT)-X(I1))) IFA=ISOF+5
      	       ENDIF
      	    ELSE
                      A1 = (TP-TETA(IET+ISOT-1)) / DW(IPLOT)
                      IF ((X(I2)-X(I1))*(YP-A1*DY(IPLOT)-Y(I1)).GT.
     &             (Y(I2)-Y(I1))*(XP-A1*DX(IPLOT)-X(I1))) IFA=ISOT+3
      	    ENDIF
                ENDIF
!
               ELSEIF (ISOT.GT.0) THEN
!
      	  IFA = ISOT + 3
!
                  IF (ISOF.GT.0) THEN
      	    A1=(FP-FREQ(IFR+ISOF-1))/DF(IPLOT)
      	    A2=(TP-TETA(IET+ISOT-1))/DW(IPLOT)
      	    IF (A1.LT.A2) IFA = ISOF + 5
      	  ENDIF
               ELSE
      	  IFA = ISOF + 5
             ENDIF
!
               IEL = IFABOR(IEL,IFA)
!
               IF (IFA.LE.3) THEN
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE OF THE PRISM IS A RECTANGULAR FACE
!  =================================================================
!-----------------------------------------------------------------------
!
                  IF (IEL.GT.0) THEN
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE IS AN INTERIOR FACE
!  MOVES TO THE ADJACENT ELEMENT
!-----------------------------------------------------------------------
!
                     I1 = IKLE2(IEL,1)
                     I2 = IKLE2(IEL,2)
                     I3 = IKLE2(IEL,3)
!
                     ELT(IPLOT) = IEL
                     SHP1(IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                           -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL)
                     SHP2(IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                           -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL)
                     SHP3(IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                           -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL)
!
                     ISO(IPLOT) = ISOV
!
         IF (SHP1(IPLOT).LT.EPSILO) ISO(IPLOT)=IBSET(ISO(IPLOT),4)
         IF (SHP2(IPLOT).LT.EPSILO) ISO(IPLOT)=IBSET(ISO(IPLOT),5)
         IF (SHP3(IPLOT).LT.EPSILO) ISO(IPLOT)=IBSET(ISO(IPLOT),6)
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
                  IF (IEL.EQ.-1) THEN
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE IS A SOLID BOUNDARY
!  SETS SHP TO 0, END OF TRACING BACK
!-----------------------------------------------------------------------
!
                     SHP1(IPLOT) = 0.D0
                     SHP2(IPLOT) = 0.D0
                     SHP3(IPLOT) = 0.D0
                     ELT(IPLOT) = - SENS * ELT(IPLOT)
                     NSP(IPLOT) = ISP
      	     GOTO 40
!
                  ENDIF
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE IS A LIQUID BOUNDARY
!  ENDS TRACING BACK (SIGN OF ELT)
!-----------------------------------------------------------------------
!
                  A1 = (DXP*(YP-Y(I1))-DYP*(XP-X(I1)))/(DXP*DY1-DYP*DX1)
                  IF (A1.GT.1.D0) A1 = 1.D0
                  IF (A1.LT.0.D0) A1 = 0.D0
      	  IF (IFA.EQ.1) THEN
      	    SHP1(IPLOT) = 1.D0 - A1
      	    SHP2(IPLOT) = A1
      	    SHP3(IPLOT) = 0.D0
                  ELSEIF (IFA.EQ.2) THEN
      	    SHP2(IPLOT) = 1.D0 - A1
      	    SHP3(IPLOT) = A1
      	    SHP1(IPLOT) = 0.D0
                  ELSE
      	    SHP3(IPLOT) = 1.D0 - A1
      	    SHP1(IPLOT) = A1
      	    SHP2(IPLOT) = 0.D0
                  ENDIF
                  XPLOT(IPLOT) = X(I1) + A1 * DX1
                  YPLOT(IPLOT) = Y(I1) + A1 * DY1
                  IF (ABS(DXP).GT.ABS(DYP)) THEN
                     A1 = (XP-XPLOT(IPLOT))/DXP
                  ELSE
                     A1 = (YP-YPLOT(IPLOT))/DYP
                  ENDIF
                  TPLOT(IPLOT) = TP - A1*DW(IPLOT)
                  SHT(IPLOT) = (TPLOT(IPLOT)-TETA(IET))
     &                       / (TETA(IET+1)-TETA(IET))
                  FPLOT(IPLOT) = FP - A1*DF(IPLOT)
                  SHF(IPLOT) = (FPLOT(IPLOT)-FREQ(IFR))
     &                       / (FREQ(IFR+1)-FREQ(IFR))
                  ELT(IPLOT) = - SENS * ELT(IPLOT)
                  NSP(IPLOT) = ISP
!
               ELSEIF (IFA.LE.5) THEN
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE OF THE PRISM IS A TRIANGULAR FACE TETA
!  =====================================================================
!-----------------------------------------------------------------------
!
                  IFA = IFA - 4
!
                  IF (IEL.EQ.1) THEN
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE IS AN INTERIOR FACE
!  MOVES TO THE ADJACENT ELEMENT
!-----------------------------------------------------------------------
!
                     ETA(IPLOT) = IET + IFA + IFA - 1
      	     IF (ETA(IPLOT).EQ.NPLAN+1) THEN
      		 ETA(IPLOT)=1
      		 TP=TP-2*3.14159265D0
      		 TPLOT(IPLOT)=TP
                     ENDIF
      	     IF (ETA(IPLOT).EQ.0) THEN
      		 ETA(IPLOT) = NPLAN
      		 TP=TP+2*3.14159265D0
      		 TPLOT(IPLOT)=TP
                     ENDIF
                     SHT(IPLOT) = (TP-TETA(ETA(IPLOT)))
     &                   / (TETA(ETA(IPLOT)+1)-TETA(ETA(IPLOT)))
!
                     ISO(IPLOT) = ISOH+ISOF*4
!
               IF (SHT(IPLOT).LT.EPSILO)
     &             ISO(IPLOT)=IBSET(ISO(IPLOT),0)
               IF (SHT(IPLOT).GT.1.D0-EPSILO)
     &             ISO(IPLOT)=IBSET(ISO(IPLOT),1)
!
                     GOTO 50
!
                  ELSE
!
        IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'PROBLEME DANS PIED4D',IEL,IPLOT
        ELSE
         WRITE(LU,*) 'PROBLEM IN PIED4D',IEL,IPLOT
        ENDIF
        WRITE(LU,*) 'SHP',SHP1(IPLOT),SHP2(IPLOT),SHP3(IPLOT)
        WRITE(LU,*) 'SHT',SHT(IPLOT)
        WRITE(LU,*) 'DXYZ',DX(IPLOT),DY(IPLOT),DW(IPLOT)
        WRITE(LU,*) 'XYZ',XPLOT(IPLOT),YPLOT(IPLOT),TPLOT(IPLOT)
        STOP
                  ENDIF
!
               ELSE
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE OF THE PRISM IS A TRIANGULAR FACE FREQ
!  =====================================================================
!-----------------------------------------------------------------------
!
                  IFA = IFA - 6
!
                  IF ((IFA.EQ.1).AND.(IFR.EQ.NF-1)) IEL=-1
                  IF ((IFA.EQ.0).AND.(IFR.EQ.1)) IEL=-1
                  IF (IEL.EQ.1) THEN
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE IS AN INTERIOR FACE
!  MOVES TO THE ADJACENT ELEMENT
!-----------------------------------------------------------------------
!
                     FRE(IPLOT) = IFR + IFA + IFA - 1
                     SHF(IPLOT) = (FP-FREQ(FRE(IPLOT)))
     &                   / (FREQ(FRE(IPLOT)+1)-FREQ(FRE(IPLOT)))
!
                     ISO(IPLOT) = ISOH+ISOT
!
               IF (SHF(IPLOT).LT.EPSILO)
     &             ISO(IPLOT)=IBSET(ISO(IPLOT),2)
               IF (SHF(IPLOT).GT.1.D0-EPSILO)
     &             ISO(IPLOT)=IBSET(ISO(IPLOT),3)
!
                     GOTO 50
!
                  ELSE
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE IS THE MIN OR MAX FREQUENCY
!  PROJECTS THE RELICAT ON THE BOUNDAY AND CONTINUES
!-----------------------------------------------------------------------
!
                    FPLOT(IPLOT)=FREQ(IFR+IFA)
      	    DF(IPLOT)=0.D0
      	    SHF(IPLOT)=IFA
      	    ISO(IPLOT) = ISOH +ISOT
      	    IF(ISO(IPLOT).NE.0) GOTO 50
!
                  ENDIF
!
               ENDIF
!
            ENDIF
!
40       CONTINUE
!
20    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
