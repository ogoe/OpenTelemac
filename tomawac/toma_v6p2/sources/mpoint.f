!                       ***************** 
                        SUBROUTINE MPOINT 
!                       ***************** 
! 
     &( U     , V     , W     , DT    , X     , Y     , TETA  , IKLE2 , 
     &  IFABOR, ETAS  , XPLOT , YPLOT , ZPLOT , DX    , DY    , DZ    , 
     &  SHP   , SHZ   , ELT   , ETA   , NSP   , NPLOT , 
     &  NPOIN2, NELEM2, NPLAN , IFF   , SURDET, SENS  , ISO ) 
! 
!*********************************************************************** 
! TOMAWAC   V6P2                                   25/06/2012 
!*********************************************************************** 
! 
!brief    MIDDLE POINT METHOD TO FIND THE FOOT OF 
!+              CARACTERISTICS IN THE SEMILAGRANGIAN METHOD 
! 
!note  RESTRICTIONS AND ASSUMPTIONS : THE ADVECTION FIELD U IS 
!+         ASSUMED NOT TO VARY WITH TIME. 
! 
!history  E. KRIEZI (LNH) 
!+        04/12/2006 
!+        V5P5 
! 
! 
!history  G.MATTAROLO (EDF - LNHE) 
!+        23/06/2012 
!+        V6P2 
!+   Modification for V6P2 
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
!| NSP            |<--| NUMBER OF RUNGE-KUTTA SUB-ITERATIONS 
!| SENS           |-->| DESCENTE OU REMONTEE DES CARACTERISTIQUES. 
!| SHP            |<->| BARYCENTRIC COORDINATES OF THE NODES IN 
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
!.....VARIABLES IN ARGUMENT 
!     """""""""""""""""""" 
! 
      INTEGER NPOIN2, NELEM2, NPLAN, NPLOT, SENS, IFF 
      INTEGER ELT(NPLOT), ETA(NPLOT), NSP(NPLOT), ISO(NPLOT) 
      INTEGER IKLE2(NELEM2,3), IFABOR(NELEM2,5), ETAS(NPLAN) 
      DOUBLE PRECISION DT 
      DOUBLE PRECISION U(NPOIN2,NPLAN), V(NPOIN2,NPLAN) 
      DOUBLE PRECISION W(NPOIN2,NPLAN) 
      DOUBLE PRECISION XPLOT(NPLOT), YPLOT(NPLOT), ZPLOT(NPLOT) 
      DOUBLE PRECISION SHZ(NPLOT), SURDET(NELEM2) 
      DOUBLE PRECISION SHP(3,NPLOT)
      DOUBLE PRECISION X(NPOIN2), Y(NPOIN2), TETA(NPLAN+1) 
      DOUBLE PRECISION DX(NPLOT), DY(NPLOT), DZ(NPLOT) 
! 
!.....LOCAL VARIABLES 
!     """"""""""""""" 
      INTEGER NSPMAX, IPLOT, ISP, I1, I2, I3 
      INTEGER IEL, IET, ISOH, ISOV, IFA, ISUI(3) 
      INTEGER,ALLOCATABLE:: I1_P(:), I1_T(:) 
      DOUBLE PRECISION EPSILO, EPSI, EPM1, COEF1, COEF2, DEUPI 
      DOUBLE PRECISION,ALLOCATABLE:: U_MP(:), V_MP(:), W_MP(:) 
      DOUBLE PRECISION,ALLOCATABLE:: XPLOT_IN(:), YPLOT_IN(:) 
      DOUBLE PRECISION,ALLOCATABLE:: ZPLOT_IN(:) 
      DOUBLE PRECISION PAS, A1, DX1, DY1, DTHETA 
      DOUBLE PRECISION DXP, DYP, DZP, XP, YP, ZP 
 
      LOGICAL DEJA 
! 
      INTRINSIC ABS , INT , MAX , SQRT 
! 
      DATA ISUI   / 2 , 3 , 1 / 
      DATA EPSILO / -1.D-6 / 
      DATA EPSI   / 1.D-12 / 
      DATA DEJA/.FALSE./ 
       
      SAVE 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! 
      IF(.NOT.DEJA)THEN 
         ALLOCATE(I1_P(NPLOT)) 
         ALLOCATE(I1_T(NPLOT)) 
         ALLOCATE(U_MP(NPLOT)) 
         ALLOCATE(V_MP(NPLOT)) 
         ALLOCATE(W_MP(NPLOT)) 
         ALLOCATE(XPLOT_IN(NPLOT)) 
         ALLOCATE(YPLOT_IN(NPLOT)) 
         ALLOCATE(ZPLOT_IN(NPLOT)) 
         DEJA=.TRUE. 
      ENDIF 
 
      DEUPI = 2.D0* 3.14159265358978D0 
      EPM1=1.D0-EPSI 
! 
!----------------------------------------------------------------------- 
!  MIDLE POINT METHOD TO FIND THE FOOT OF CARACTERISTICS  
!  IN THE SEMILAGRANGIAN METHODE WE USED 
!----------------------------------------------------------------------- 
! 3 iterations are enough to converge 
      NSPMAX =3 
      COEF1=3.D0/2.D0 
      COEF2=1.D0/2.D0 
! 
      DTHETA=abs(TETA(2)-TETA(3)) 
! 
!        write(6,*)  'FREQUENCY ',IFF 
! 
       XPLOT_in=XPLOT 
       YPLOT_in=YPLOT 
       ZPLOT_in=ZPLOT 
! 
        DO IPLOT = 1 , NPLOT 
               IET = ETA(IPLOT) 
               I1_P(IPLOT) =IPLOT-(IET-1)*NPOIN2 
               I1_T(IPLOT)=IET 
        ENDDO 
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
CMIC$ DO PARALLEL VECTOR 
         DO 30 IPLOT = 1 , NPLOT 
! 
            ISO(IPLOT) = 0 
! 
            IEL = ELT(IPLOT) 
            IET = ETA(IPLOT) 
 
! 
            I1 = IKLE2(IEL,1) 
            I2 = IKLE2(IEL,2) 
            I3 = IKLE2(IEL,3) 
            PAS = -1.D0*DT 
! 
            IF(ISP.EQ.1) THEN 
               U_mp(IPLOT) = U(I1_P(IPLOT),I1_T(IPLOT)) 
               V_mp(IPLOT) = V(I1_P(IPLOT),I1_T(IPLOT)) 
               W_mp(IPLOT) = W(I1_P(IPLOT),I1_T(IPLOT)) 
               
            ELSE 
               U_mp(IPLOT) = COEF1*U(I1_P(IPLOT),I1_T(IPLOT))- 
     & ( U(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT)) 
     & + U(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT)) 
     & + U(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT)) 
     & + U(I1,ETAS(IET))*SHP(1,IPLOT)*SHZ(IPLOT) 
     & + U(I2,ETAS(IET))*SHP(2,IPLOT)*SHZ(IPLOT) 
     & + U(I3,ETAS(IET))*SHP(3,IPLOT)*SHZ(IPLOT) ) * COEF2  
! 
! 
               V_mp(IPLOT) = COEF1*V(I1_P(IPLOT),I1_T(IPLOT))- 
     & ( V(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT)) 
     & + V(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT)) 
     & + V(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT)) 
     & + V(I1,ETAS(IET))*SHP(1,IPLOT)*SHZ(IPLOT) 
     & + V(I2,ETAS(IET))*SHP(2,IPLOT)*SHZ(IPLOT) 
     & + V(I3,ETAS(IET))*SHP(3,IPLOT)*SHZ(IPLOT) ) * COEF2 
! 
! 
               W_mp(IPLOT) = COEF1*W(I1_P(IPLOT),I1_T(IPLOT))- 
     & ( W(I1,IET  )*SHP(1,IPLOT)*(1.D0-SHZ(IPLOT)) 
     & + W(I2,IET  )*SHP(2,IPLOT)*(1.D0-SHZ(IPLOT)) 
     & + W(I3,IET  )*SHP(3,IPLOT)*(1.D0-SHZ(IPLOT)) 
     & + W(I1,ETAS(IET))*SHP(1,IPLOT)*SHZ(IPLOT) 
     & + W(I2,ETAS(IET))*SHP(2,IPLOT)*SHZ(IPLOT) 
     & + W(I3,ETAS(IET))*SHP(3,IPLOT)*SHZ(IPLOT) ) *COEF2 
! 
             ENDIF 
! 
             DX(IPLOT)=U_mp(IPLOT)*PAS 
             DY(IPLOT)=V_mp(IPLOT)*PAS 
             DZ(IPLOT)=W_mp(IPLOT)*PAS 
! 
             XP = XPLOT_in(IPLOT) + DX(IPLOT) 
             YP = YPLOT_in(IPLOT) + DY(IPLOT) 
             ZP = ZPLOT_in(IPLOT) + DZ(IPLOT) 
! 
             IF (ZP.GT.DEUPI) ZP=ZP-int(ZP/DEUPI)*DEUPI 
             IF (ZP.LT.0.D0) ZP=ZP+(int(abs(ZP)/DEUPI)+1)*DEUPI 
! 
             SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                      -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL) 
             SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                      -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL) 
             SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                      -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL) 
! 
             ZPLOT(IPLOT) = ZP 
             IF(IET.EQ.NPLAN)THEN 
               SHZ(IPLOT) = (ZP-TETA(IET)) / (DTHETA)   
             ELSEIF(IET.EQ.1)THEN 
               SHZ(IPLOT) = (ZP-TETA(IET)) / (TETA(IET+1)-TETA(IET))                 
             ELSE 
               SHZ(IPLOT) = (ZP-TETA(IET)) / (TETA(IET+1)-TETA(IET)) 
             ENDIF 
! 
             IF(SHZ(IPLOT).GT.1.D0-EPSILO) THEN 
                IET =IET+INT(SHZ(IPLOT)) 
 
               IF(IET.GE.NPLAN+1) IET=IET-NPLAN 
               IF (IET.LE.-1) IET=IET+NPLAN+1 
               IF(IET.EQ.0) IET=NPLAN 
!                
     
               ETA(IPLOT)=IET 
! 
               IF (IET.EQ.NPLAN) THEN 
                SHZ(IPLOT) = (ZP-TETA(IET)) / (DTHETA)               
               ELSE 
                SHZ(IPLOT) = (ZP-TETA(IET)) / (TETA(IET+1)-TETA(IET)) 
               ENDIF     
! 
            ELSEIF(SHZ(IPLOT).LT.EPSILO) THEN 
               IET=IET-(INT(ABS(SHZ(IPLOT)))+1) 
! 
               IF (IET.LE.-1) IET=IET+NPLAN+1 
               IF(IET.GE.NPLAN+1) IET=IET-NPLAN 
               IF(IET.EQ.0) IET=NPLAN 
! 
               ETA(IPLOT)=IET 
! 
               ZP= ZPLOT(IPLOT)  
               IF (IET.EQ.NPLAN) THEN 
                 SHZ(IPLOT) = (ZP-TETA(IET)) / (DTHETA)               
               ELSE 
                SHZ(IPLOT) = (ZP-TETA(IET)) / (TETA(IET+1)-TETA(IET)) 
               ENDIF          
            ENDIF 
! 
            IF(SHZ(IPLOT).GE.EPM1) SHZ(IPLOT)=1.D0 
            IF(SHZ(IPLOT).LE.EPSI) SHZ(IPLOT)=0.D0  
! 
            XPLOT(IPLOT) = XP 
            YPLOT(IPLOT) = YP 
! 
            IF (SHP(1,IPLOT).LT.EPSILO) 
     &           ISO(IPLOT)=IBSET(ISO(IPLOT),2) 
            IF (SHP(2,IPLOT).LT.EPSILO) 
     &           ISO(IPLOT)=IBSET(ISO(IPLOT),3) 
            IF (SHP(3,IPLOT).LT.EPSILO) 
     &           ISO(IPLOT)=IBSET(ISO(IPLOT),4) 
! 
! 
30       CONTINUE 
CMIC$ END DO 
! 
!----------------------------------------------------------------------- 
!  TREATS DIFFERENTLY THE CHARACTERISTICS ISSUED FROM 
!  THE START ELEMENT 
!----------------------------------------------------------------------- 
! 
CMIC$ DO PARALLEL VECTOR 
         DO 40 IPLOT = 1 , NPLOT 
! 
50        CONTINUE 
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
                    IF (DZ(IPLOT).EQ.0.D0) THEN 
                     A1=0.0 
                    ELSE 
! E.KRIEZI                   A1 = (ZP-TETA(IET+ISOV-1)) / DZ(IPLOT) 
                     A1 = (ZP-TETA(IET)) / DZ(IPLOT) 
                     I1 = IKLE2(IEL,IFA) 
                     I2 = IKLE2(IEL,ISUI(IFA)) 
                     IF ((X(I2)-X(I1))*(YP-A1*DY(IPLOT)-Y(I1)).GT. 
     &                 (Y(I2)-Y(I1))*(XP-A1*DX(IPLOT)-X(I1))) IFA=ISOV+3 
                    ENDIF 
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
                     SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2)) 
     &                           -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL) 
                     SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3)) 
     &                           -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL) 
                     SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1)) 
     &                           -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL) 
! 
                     ISO(IPLOT) = ISOV 
! 
                     IF (SHP(1,IPLOT).LT.EPSILO) 
     &	                ISO(IPLOT)=IBSET(ISO(IPLOT),2) 
                     IF (SHP(2,IPLOT).LT.EPSILO) 
     &		        ISO(IPLOT)=IBSET(ISO(IPLOT),3) 
                     IF (SHP(3,IPLOT).LT.EPSILO) 
     &  		ISO(IPLOT)=IBSET(ISO(IPLOT),4) 
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
                     SHP(1,IPLOT) = 0.D0 
                     SHP(2,IPLOT) = 0.D0 
                     SHP(3,IPLOT) = 0.D0 
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
                  IF((DXP*DY1-DYP*DX1).EQ.0.D0) THEN 
                   A1=0 
                   SHP(1,IPLOT) = 0.D0 
                   SHP(2,IPLOT) = 0.D0 
                   SHP(3,IPLOT) = 0.D0 
                   ELT(IPLOT) = - SENS * ELT(IPLOT) 
                   NSP(IPLOT) = ISP 
                   GOTO 40 
                  ELSE 
                   A1=(DXP*(YP-Y(I1))-DYP*(XP-X(I1)))/(DXP*DY1-DYP*DX1) 
                  ENDIF 
!                 
CFBG                  IF (A1.GT.1.D0) A1 = 1.D0 
CFBG                  IF (A1.LT.0.D0) A1 = 0.D0 
                  IF (A1.GT.EPM1) A1 = 1.D0 
                  IF (A1.LT.EPSI) A1 = 0.D0 
CFGB 
                  IF (IFA.EQ.1) THEN 
                    SHP(1,IPLOT) = 1.D0 - A1 
                    SHP(2,IPLOT) = A1 
                    SHP(3,IPLOT) = 0.D0 
                  ELSEIF (IFA.EQ.2) THEN 
                    SHP(2,IPLOT) = 1.D0 - A1 
                    SHP(3,IPLOT) = A1 
                    SHP(1,IPLOT) = 0.D0 
                  ELSE 
                    SHP(3,IPLOT) = 1.D0 - A1 
                    SHP(1,IPLOT) = A1 
                    SHP(2,IPLOT) = 0.D0 
                  ENDIF 
                  XPLOT(IPLOT) = X(I1) + A1 * DX1 
                  YPLOT(IPLOT) = Y(I1) + A1 * DY1 
                  IF ((ABS(DXP).GT.ABS(DYP)).AND.(DXP.NE.0.D0)) THEN 
                     A1 = (XP-XPLOT(IPLOT))/DXP 
                  ELSEIF((ABS(DYP).GT.ABS(DXP)).AND.(DYP.NE.0.D0))THEN 
                     A1 = (YP-YPLOT(IPLOT))/DYP 
                  ELSEIF ((DXP.EQ.0.D0).OR.(DYP.EQ.0.D0)) THEN 
                     SHP(1,IPLOT) = 0.D0 
                     SHP(2,IPLOT) = 0.D0 
                     SHP(3,IPLOT) = 0.D0 
                     ELT(IPLOT) = - SENS * ELT(IPLOT) 
                     NSP(IPLOT) = ISP 
                     GOTO 40 
                  ENDIF 
                  IF (A1.GT.EPM1) A1 = 1.D0 
                  IF (A1.LT.EPSI) A1 = 0.D0 
                  ZPLOT(IPLOT) = ZP - A1*DZ(IPLOT) 
! 
                  IF (ZPLOT(IPLOT).GT.DEUPI) ZPLOT(IPLOT)= 
     &              ZPLOT(IPLOT)-int(ZPLOT(IPLOT)/DEUPI)*DEUPI 
                  IF (ZPLOT(IPLOT).LT.0.D0) ZPLOT(IPLOT)= 
     &              ZPLOT(IPLOT)+(int(abs(ZPLOT(IPLOT))/DEUPI)+1)*DEUPI 
  
! 
!__________E.KRIEZI SHZ------------------ 
                  ZP= ZPLOT(IPLOT) 
                  IF(IET.EQ.NPLAN)THEN 
                    SHZ(IPLOT) = (ZP-TETA(IET))/(DTHETA) 
                  ELSEIF(IET.EQ.1)THEN 
                    SHZ(IPLOT) = (ZP-TETA(IET))/(TETA(IET+1)-TETA(IET)) 
                  ELSE 
                    SHZ(IPLOT) = (ZP-TETA(IET))/(TETA(IET+1)-TETA(IET)) 
                  ENDIF 
! 
                  IF(SHZ(IPLOT).GT.1.D0-EPSILO) THEN 
                    IET =IET+INT(SHZ(IPLOT)) 
                    IF(IET.GE.NPLAN+1) IET=IET-NPLAN 
                    IF (IET.LE.-1) IET=IET+NPLAN+1 
                    IF(IET.EQ.0) IET=NPLAN 
                    ETA(IPLOT)=IET 
                    IF (IET.EQ.NPLAN) THEN 
                      SHZ(IPLOT) = (ZP-TETA(IET)) / (DTHETA)               
                    ELSE 
                      SHZ(IPLOT) = 
     &		       (ZP-TETA(IET))/(TETA(IET+1)-TETA(IET)) 
                    ENDIF     
                  ELSEIF(SHZ(IPLOT).LT.EPSILO) THEN 
                    IET=IET-(INT(ABS(SHZ(IPLOT)))+1) 
                    IF (IET.LE.-1) IET=IET+NPLAN+1 
                    IF(IET.GE.NPLAN+1) IET=IET-NPLAN 
                    IF(IET.EQ.0) IET=NPLAN 
                    ETA(IPLOT)=IET 
                    ZP= ZPLOT(IPLOT) 
                    IF (IET.EQ.NPLAN) THEN 
                      SHZ(IPLOT) = (ZP-TETA(IET)) / (DTHETA) 
                    ELSE 
                      SHZ(IPLOT) = 
     &		       (ZP-TETA(IET)) / (TETA(IET+1)-TETA(IET)) 
                    ENDIF          
                  ENDIF 
                  IF(SHZ(IPLOT).GE.EPM1) SHZ(IPLOT)=1.D0 
                  IF(SHZ(IPLOT).LE.EPSI) SHZ(IPLOT)=0.D0 
!________________________________________________________ 
!     
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
                     IET=ETA(IPLOT)  
                     IF (IET.EQ.0) THEN 
                       IET=NPLAN 
                       ZP=ZP+2*3.14159265D0 
                     ELSEIF (IET.LE.-1) THEN 
                       IET=IET+int(abs(IET)/NPLAN)*NPLAN+1 
                       ZP=ZP+(int(abs(ZP)/DEUPI)+1)*DEUPI 
                     ELSEIF(IET.GE.NPLAN+1) THEN 
                       ZP=ZP-int(ZP/DEUPI)*DEUPI 
                       IET=IET-int(abs(IET)/NPLAN)*NPLAN 
                     ENDIF 
! 
                     IF (IET.EQ.NPLAN) THEN 
                       SHZ(IPLOT) = (ZP-TETA(IET)) / (DTHETA) 
                     ELSE 
                       SHZ(IPLOT) = (ZP-TETA(IET)) /  
     &                          (TETA(IET+1)-TETA(IET)) 
                     ENDIF     
                     ETA(IPLOT) = IET 
                     ZPLOT(IPLOT)=ZP 
! 
                     ISO(IPLOT) = ISOH 
! 
                     IF (SHZ(IPLOT).LT.EPSILO) 
     &                 ISO(IPLOT)=IBSET(ISO(IPLOT),0) 
                     IF (SHZ(IPLOT).GT.1.D0-EPSILO) 
     &                 ISO(IPLOT)=IBSET(ISO(IPLOT),1) 
! 
                     GOTO 50 
! 
                  ELSE 
! 
!         WRITE(LU,*)'YA UN PROBLEME',IEL,IPLOT 
!         WRITE(LU,*)'SHP',SHP(1,IPLOT),SHP(2,IPLOT),SHP(3,IPLOT) 
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
20    CONTINUE 
! 
!----------------------------------------------------------------------- 
! 
      RETURN 
      END
