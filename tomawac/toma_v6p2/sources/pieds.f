!                    ****************
                     SUBROUTINE PIEDS
!                    ****************
!
     &  (U , V , W , DT , NRK , X , Y , TETA , IKLE2 , IFABOR , ETAS ,
     &   XPLOT , YPLOT , ZPLOT , DX , DY , DZ , SHP1 , SHP2 , SHP3 ,
     &   SHZ , ELT , ETA , NSP , NPLOT , NPOIN2 , NELEM2 , NPLAN ,
     &   IFF , SURDET , SENS , ISO )
!
!***********************************************************************
! TOMAWAC   V6P1                                   22/06/2011
!***********************************************************************
!
!brief    TRACES IN TIME THE CHARACTERISTICS CURVES FOR TELEMAC-3D
!+                PRISMS, WITHIN THE TIME INTERVAL DT, USING A FINITE
!+                ELEMENTS DISCRETISATION.
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
!| DX,DY,DZ       |<--| STORED SUB-INCREMENTS TIME STEPS
!| ELT            |<->| NUMBERS OF THE ELEMENTS 2D OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETA            |<->| NUMBERS OF THE LAYERS OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETAS           |<->| WORK TABLE INDICATING THE NUMBER OF THE HIGHER
!|                |   | LAYER
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID,
!|                |   | SOLID OR PERIODIC BOUNDARY
!| IFF            |-->| FREQUENCY INDEX
!| IKLE2          |-->| TRANSITION BETWEEN LOCAL AND GLOBAL NUMBERING
!|                |   | OF THE 2D MESH
!| ISO            |<->| BINARY STORING OF THE EXIT FACE OF THE ELEMENT
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPLOT          |-->| NUMBER OF POINTS TO BE ADVECTED
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NRK            |-->| NUMBER OF RUNGE-KUTTA SUB-ITERATIONS
!| NSP            |<--| NUMBER OF RUNGE-KUTTA SUB-ITERATIONS
!| SENS           |-->| DESCENTE OU REMONTEE DES CARACTERISTIQUES.
!| SHP1,SHP2,SHP3 |<->| BARYCENTRIC COORDINATES OF THE NODES IN
!|                |   | THEIR ASSOCIATED 2D ELEMENT "ELT"
!| SHZ            |<->| BARYCENTRIC COORDINATES ALONG TETA OF THE 
!|                |   | NODES IN THEIR ASSOCIATED LAYER "ETA"
!| SURDET         |-->| 1/DET. OF ELEMENTS 2D FOR ISOPARAM. TRANSF.
!| U,V,W          |-->| COMPONENTS OF THE ADVECTION VELOCITY
!| X,Y,TETA       |-->| COORDINATES OF THE MESH
!| XPLOT          |<->| STARTING ABSCISSAE OF THE POINTS TO BE
!|                |   | ADVECTED
!| YPLOT          |<->| STARTING ORDINATES OF THE POINTS TO BE
!|                |   | ADVECTED
!| ZPLOT          |<->| STARTING DIRECTIONS OF THE POINTS TO BE
!|                |   | ADVECTED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NPOIN2,NELEM2,NPLAN,NPLOT,NSPMAX,NRK,SENS,IFF
!
      DOUBLE PRECISION U(NPOIN2,NPLAN),V(NPOIN2,NPLAN)
      DOUBLE PRECISION W(NPOIN2,NPLAN)
      DOUBLE PRECISION XPLOT(NPLOT),YPLOT(NPLOT),ZPLOT(NPLOT)
      DOUBLE PRECISION SURDET(NELEM2),SHZ(NPLOT)
      DOUBLE PRECISION SHP1(NPLOT),SHP2(NPLOT),SHP3(NPLOT)
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2),TETA(NPLAN+1)
      DOUBLE PRECISION DX(NPLOT),DY(NPLOT),DZ(NPLOT)
      DOUBLE PRECISION PAS,DT,A1,DX1,DY1,DXP,DYP,DZP,XP,YP,ZP
      DOUBLE PRECISION EPSILO, EPSI, EPM1
!
      INTEGER IKLE2(NELEM2,3),IFABOR(NELEM2,5),ETAS(NPLAN)
      INTEGER ELT(NPLOT),ETA(NPLOT),NSP(NPLOT),ISO(NPLOT)
      INTEGER IPLOT,ISP,I1,I2,I3,IEL,IET,ISOH,ISOV,IFA,ISUI(3)
!
      INTRINSIC ABS , INT , MAX , SQRT
!
      DATA ISUI   / 2 , 3 , 1 /
      DATA EPSILO / -1.D-6 /
      DATA EPSI   / 1.D-12 /
!
!-----------------------------------------------------------------------
!    COMPUTES THE NUMBER OF SUB-ITERATIONS
!    (THE SAME AT ALL THE NODES FOR A GIVEN FREQUENCY)
!-----------------------------------------------------------------------
!
      NSPMAX = 1
      EPM1=1.D0-EPSI
!
      DO 10 IPLOT = 1 , NPLOT
!
         NSP(IPLOT) = 0
         IEL = ELT(IPLOT)
!
         IF (IEL.GT.0) THEN
!
            IET = ETA(IPLOT)
!
            I1 = IKLE2(IEL,1)
            I2 = IKLE2(IEL,2)
            I3 = IKLE2(IEL,3)
!
         DXP = U(I1,IET  )*SHP1(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + U(I2,IET  )*SHP2(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + U(I3,IET  )*SHP3(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + U(I1,ETAS(IET))*SHP1(IPLOT)*SHZ(IPLOT)
     &       + U(I2,ETAS(IET))*SHP2(IPLOT)*SHZ(IPLOT)
     &       + U(I3,ETAS(IET))*SHP3(IPLOT)*SHZ(IPLOT)
!
         DYP = V(I1,IET  )*SHP1(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + V(I2,IET  )*SHP2(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + V(I3,IET  )*SHP3(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + V(I1,ETAS(IET))*SHP1(IPLOT)*SHZ(IPLOT)
     &       + V(I2,ETAS(IET))*SHP2(IPLOT)*SHZ(IPLOT)
     &       + V(I3,ETAS(IET))*SHP3(IPLOT)*SHZ(IPLOT)
!
         DZP = W(I1,IET  )*SHP1(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + W(I2,IET  )*SHP2(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + W(I3,IET  )*SHP3(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + W(I1,ETAS(IET))*SHP1(IPLOT)*SHZ(IPLOT)
     &       + W(I2,ETAS(IET))*SHP2(IPLOT)*SHZ(IPLOT)
     &       + W(I3,ETAS(IET))*SHP3(IPLOT)*SHZ(IPLOT)
!
         NSP(IPLOT)= MAX(INT(NRK*DT*ABS(DZP/(TETA(IET)-TETA(IET+1)))),
     &         INT(NRK*DT*SQRT((DXP*DXP+DYP*DYP)*SURDET(IEL))) )
!
            NSP(IPLOT) = MAX (1,NSP(IPLOT))
!
            NSPMAX = MAX ( NSPMAX , NSP(IPLOT) )
!
         ENDIF
!
10    CONTINUE
      IF (LNG.EQ.1) THEN
        WRITE(LU,*)
     &     '   FREQUENCE',IFF,', NOMBRE DE SOUS PAS RUNGE KUTTA :'
     &        ,NSPMAX
      ELSE
        WRITE(LU,*)
     &     '   FREQUENCY',IFF,', NUMBER OF RUNGE KUTTA SUB TIME-STEP
     &S :',NSPMAX
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
!
               I1 = IKLE2(IEL,1)
               I2 = IKLE2(IEL,2)
               I3 = IKLE2(IEL,3)
               PAS = SENS * DT / NSP(IPLOT)
!
               DX(IPLOT) =
     & ( U(I1,IET  )*SHP1(IPLOT)*(1.D0-SHZ(IPLOT))
     & + U(I2,IET  )*SHP2(IPLOT)*(1.D0-SHZ(IPLOT))
     & + U(I3,IET  )*SHP3(IPLOT)*(1.D0-SHZ(IPLOT))
     & + U(I1,ETAS(IET))*SHP1(IPLOT)*SHZ(IPLOT)
     & + U(I2,ETAS(IET))*SHP2(IPLOT)*SHZ(IPLOT)
     & + U(I3,ETAS(IET))*SHP3(IPLOT)*SHZ(IPLOT) ) * PAS
!
               DY(IPLOT) =
     & ( V(I1,IET  )*SHP1(IPLOT)*(1.D0-SHZ(IPLOT))
     & + V(I2,IET  )*SHP2(IPLOT)*(1.D0-SHZ(IPLOT))
     & + V(I3,IET  )*SHP3(IPLOT)*(1.D0-SHZ(IPLOT))
     & + V(I1,ETAS(IET))*SHP1(IPLOT)*SHZ(IPLOT)
     & + V(I2,ETAS(IET))*SHP2(IPLOT)*SHZ(IPLOT)
     & + V(I3,ETAS(IET))*SHP3(IPLOT)*SHZ(IPLOT) ) * PAS
!
               DZ(IPLOT) =
     & ( W(I1,IET  )*SHP1(IPLOT)*(1.D0-SHZ(IPLOT))
     & + W(I2,IET  )*SHP2(IPLOT)*(1.D0-SHZ(IPLOT))
     & + W(I3,IET  )*SHP3(IPLOT)*(1.D0-SHZ(IPLOT))
     & + W(I1,ETAS(IET))*SHP1(IPLOT)*SHZ(IPLOT)
     & + W(I2,ETAS(IET))*SHP2(IPLOT)*SHZ(IPLOT)
     & + W(I3,ETAS(IET))*SHP3(IPLOT)*SHZ(IPLOT) ) * PAS
!
               XP = XPLOT(IPLOT) + DX(IPLOT)
               YP = YPLOT(IPLOT) + DY(IPLOT)
               ZP = ZPLOT(IPLOT) + DZ(IPLOT)
!
               SHP1(IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                        -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL)
               SHP2(IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                        -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL)
               SHP3(IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                        -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL)
               SHZ(IPLOT) = (ZP-TETA(IET)) / (TETA(IET+1)-TETA(IET))
!               IF (ABS(SHZ(IPLOT)).GT.2.5D0 ) THEN
!                  WRITE(LU,*)'SHZ***',IPLOT,IET,SHZ(IPLOT)
!                  WRITE(LU,*)TETA(IET),TETA(IET+1),ZP
!                  WRITE(LU,*)DZ(IPLOT),ZPLOT(IPLOT)
!                  STOP
!              ENDIF
!
               XPLOT(IPLOT) = XP
               YPLOT(IPLOT) = YP
               ZPLOT(IPLOT) = ZP
!
               IF (SHP1(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),2)
               IF (SHP2(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),3)
               IF (SHP3(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),4)
!
               IF  (SHZ(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),0)
               IF  (SHZ(IPLOT).GT.1.D0-EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),1)
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
               ISOH = IAND(ISO(IPLOT),28)
               ISOV = IAND(ISO(IPLOT), 3)
               IEL = ELT(IPLOT)
               IET = ETA(IPLOT)
               XP = XPLOT(IPLOT)
               YP = YPLOT(IPLOT)
               ZP = ZPLOT(IPLOT)
!
               IF (ISOH.NE.0) THEN
!
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
!
                  IF (ISOV.GT.0) THEN
                     A1 = (ZP-TETA(IET+ISOV-1)) / DZ(IPLOT)
                     I1 = IKLE2(IEL,IFA)
                     I2 = IKLE2(IEL,ISUI(IFA))
                     IF ((X(I2)-X(I1))*(YP-A1*DY(IPLOT)-Y(I1)).GT.
     &                 (Y(I2)-Y(I1))*(XP-A1*DX(IPLOT)-X(I1))) IFA=ISOV+3
                  ENDIF
!
               ELSE
!
                  IFA = ISOV + 3
!
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
         IF (SHP1(IPLOT).LT.EPSILO) ISO(IPLOT)=IBSET(ISO(IPLOT),2)
         IF (SHP2(IPLOT).LT.EPSILO) ISO(IPLOT)=IBSET(ISO(IPLOT),3)
         IF (SHP3(IPLOT).LT.EPSILO) ISO(IPLOT)=IBSET(ISO(IPLOT),4)
!
                     GOTO 50
!
                  ENDIF
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
!FBG                  IF (A1.GT.1.D0) A1 = 1.D0
!FBG                  IF (A1.LT.0.D0) A1 = 0.D0
                  IF (A1.GT.EPM1) A1 = 1.D0
                  IF (A1.LT.EPSI) A1 = 0.D0
!FGB
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
                  IF (A1.GT.EPM1) A1 = 1.D0
                  IF (A1.LT.EPSI) A1 = 0.D0
                  ZPLOT(IPLOT) = ZP - A1*DZ(IPLOT)
                  SHZ(IPLOT) = (ZPLOT(IPLOT)-TETA(IET))
     &                       / (TETA(IET+1)-TETA(IET))
                  ELT(IPLOT) = - SENS * ELT(IPLOT)
                  NSP(IPLOT) = ISP
!
               ELSE
!
!-----------------------------------------------------------------------
!  HERE: THE EXIT FACE OF THE PRISM IS A TRIANGULAR FACE
!  ===============================================================
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
                         ZP=ZP-2*3.14159265D0
                         ZPLOT(IPLOT)=ZP
                     ENDIF
                     IF (ETA(IPLOT).EQ.0) THEN
                         ETA(IPLOT) = NPLAN
                         ZP=ZP+2*3.14159265D0
                         ZPLOT(IPLOT)=ZP
                     ENDIF
                     SHZ(IPLOT) = (ZP-TETA(ETA(IPLOT)))
     &                   / (TETA(ETA(IPLOT)+1)-TETA(ETA(IPLOT)))
!
                     ISO(IPLOT) = ISOH
!
               IF (SHZ(IPLOT).LT.EPSILO)
     &             ISO(IPLOT)=IBSET(ISO(IPLOT),0)
               IF (SHZ(IPLOT).GT.1.D0-EPSILO)
     &             ISO(IPLOT)=IBSET(ISO(IPLOT),1)
!
                     GOTO 50
!
                  ELSE
!
!         WRITE(LU,*)'YA UN PROBLEME',IEL,IPLOT
!         WRITE(LU,*)'SHP',SHP1(IPLOT),SHP2(IPLOT),SHP3(IPLOT)
!         WRITE(LU,*)'SHZ',SHZ(IPLOT)
!         WRITE(LU,*)'DXYZ',DX(IPLOT),DY(IPLOT),DZ(IPLOT)
!         WRITE(LU,*)'XYZ',XPLOT(IPLOT),YPLOT(IPLOT),ZPLOT(IPLOT)
                  STOP
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
