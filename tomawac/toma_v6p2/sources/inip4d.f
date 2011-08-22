!                    *****************
                     SUBROUTINE INIP4D
!                    *****************
!
     &( U , V , T , W , X , Y , SHP1 ,SHP2 , SHP3 , SHT , SHF , ELT ,
     & ETA , FRE , XCONV , YCONV , TCONV, FCONV , TETA , FREQ ,IKLE2 ,
     & NPOIN2 , NELEM2 , NPLAN  , IFF , NF  ,IFABOR,GOODELT)
!
!***********************************************************************
! TOMAWAC   V6P1                                   20/06/2011
!***********************************************************************
!
!brief    FOR TOMAWAC "HYPER PRISMS", AND BEFORE TRACING BACK IN
!+                TIME THE CHARACTERISTICS CURVES, SETS THE BARYCENTRIC
!+                COORDINATES FOR ALL THE NODES IN THE MESH IN THE ELEMENT
!+                TOWARDS WHICH THE CURVE POINTS.
!+
!+           (SUBROUTINE INSPIRED FROM GSHP41 OF BIEF)
!
!history  F MARCOS (LNH)
!+        01/02/93
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
!+        20/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ELT            |<--| NUMBERS OF THE ELEMENTS 2D OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETA            |<--| NUMBERS OF THE LAYERS OF THE
!|                |   | POINTS TO BE ADVECTED
!| FCONV,TCONV,   |<--| COORDINATES OF THE INITIAL POINT OF THE
!|   XCONV,YCONV  |   | CHARACETRISTIC
!| FRE            |<--| NUMBER OF THE FREQUENCIES OF THE
!|                |   | POINTS TO BE ADVECTED
!| FREQ           |-->| DISCRETIZED FREQUENCIES
!| GOODELT        |<->| CHARACTERISTIC IDENTIFIER
!|                |   | = 1 CORRECT ELEMENT FOR TRACING BACK THE CHARACT.
!|                |   | = 2001 CORRECT ELEMENT AT 2 PROCS BOUNDARY
!|                |   | = 2000 WRONG ELEMENT AT 2 PROCS BOUNDARY
!|                |   | = 1101 CORRECT ELEMENT AT 2 PROCS BOUNDARY + 
!|                |   |   SOLID BOUNDARY
!|                |   | = 1100 WRONG ELEMENT AT 2 PROCS BOUNDARY + 
!|                |   |   SOLID BOUNDARY
!|                |   | = 1011 CORRECT ELEMENT AT 2 PROCS BOUNDARY + 
!|                |   |   LIQUID BOUNDARY
!|                |   | = 1010 WRONG ELEMENT AT 2 PROCS BOUNDARY + 
!|                |   |   LIQUID BOUNDARY
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID,
!| IFF            |-->| FREQUENCY INDEX
!| IKLE2          |-->| TRANSITION BETWEEN LOCAL AND GLOBAL NUMBERING
!|                |   | OF THE 2D MESH
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| SHF            |-->| BARYCENTRIC COORDINATES ALONG F OF THE 
!|                |   | NODES IN THEIR ASSOCIATED FREQUENCIES "FRE"
!| SHP1,SHP2,SHP3 |<--| BARYCENTRIC COORDINATES OF THE NODES IN
!|                |   | THEIR ASSOCIATED 2D ELEMENT "ELT"
!| SHT            |<--| BARYCENTRIC COORDINATES ALONG TETA OF THE 
!|                |   | NODES IN THEIR ASSOCIATED LAYER "ETA"
!| TETA           |-->| DISCRETIZED DIRECTIONS
!| U,V,T,W        |-->| ADVECTION FIELD COMPONENTS
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER NPOIN2,NELEM2,NPLAN,NF
      INTEGER N1,N2,N3,IPOIN,IELEM,IPLAN,IP,IFF
!
      DOUBLE PRECISION U(NPOIN2,NPLAN),V(NPOIN2,NPLAN)
      DOUBLE PRECISION T(NPOIN2,NPLAN),W(NPOIN2,NPLAN)
      DOUBLE PRECISION TETA(*),FREQ(NF)
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION XCONV(NPOIN2,NPLAN),YCONV(NPOIN2,NPLAN)
      DOUBLE PRECISION TCONV(NPOIN2,NPLAN),FCONV(NPOIN2,NPLAN)
      DOUBLE PRECISION SHP1(NPOIN2,NPLAN),SHP2(NPOIN2,NPLAN)
      DOUBLE PRECISION SHP3(NPOIN2,NPLAN),SHT(NPOIN2,NPLAN)
      DOUBLE PRECISION SHF(NPOIN2,NPLAN)
      DOUBLE PRECISION DET1,DET2,EPS
      INTEGER IFABOR(NELEM2,7),GOODELT(NPOIN2,NPLAN)
!
      INTEGER IKLE2(NELEM2,3),ELT(NPOIN2,NPLAN),ETA(NPOIN2,NPLAN)
      INTEGER FRE(NPOIN2,NPLAN)
!
!      DATA EPS / 1.D-6 /
!      DATA EPS / 1.D-12 /
       DATA EPS /0.D0 /
!-----------------------------------------------------------------------
!
!  INITIALISES THE POINTS TO ADVECT
!
      GOODELT = 0
      DO 60 IP=1,NPLAN
        DO 90 IPOIN=1,NPOIN2
          XCONV(IPOIN,IP)=X(IPOIN)
          YCONV(IPOIN,IP)=Y(IPOIN)
          TCONV(IPOIN,IP)=TETA(IP)
          FCONV(IPOIN,IP)=FREQ(IFF)
90        CONTINUE
60      CONTINUE
!
!-----------------------------------------------------------------------
        DO 10 IPLAN=1,NPLAN
!
!-----------------------------------------------------------------------
!  INITIALLY FILLS IN THE SHP AND ELT
!  (NOTE: FOR LATERAL BOUNDARY POINTS, THERE MAY NOT BE AN ELEMENT
!  TOWARDS WHICH -(U, V) POINTS).
!
         DO 20 IELEM = 1,NELEM2
!
            N1=IKLE2(IELEM,1)
               ELT(N1,IPLAN) = IELEM
               SHP1(N1,IPLAN) = 1.D0
               SHP2(N1,IPLAN) = 0.D0
               SHP3(N1,IPLAN) = 0.D0
            N1=IKLE2(IELEM,2)
               ELT(N1,IPLAN) = IELEM
               SHP1(N1,IPLAN) = 0.D0
               SHP2(N1,IPLAN) = 1.D0
               SHP3(N1,IPLAN) = 0.D0
            N1=IKLE2(IELEM,3)
               ELT(N1,IPLAN) = IELEM
               SHP1(N1,IPLAN) = 0.D0
               SHP2(N1,IPLAN) = 0.D0
               SHP3(N1,IPLAN) = 1.D0
20       CONTINUE
!
!-----------------------------------------------------------------------
!  FILLS IN THE SHP AND ELT, ELEMENT BY ELEMENT, FOR THE POINTS IN
!  THE ELEMENT FOR WHICH -(U, V) POINTS TOWARDS THIS ELEMENT.
!
        DO 450 IELEM=1,NELEM2
!
          N1=IKLE2(IELEM,1)
          N2=IKLE2(IELEM,2)
          N3=IKLE2(IELEM,3)
!
! ON THE EDGE OF EACH PROC
! ----------------------------------------------
!
          IF ((IFABOR(IELEM,1)==-2)) THEN
             ELT(N1,IPLAN) = IELEM
             SHP1(N1,IPLAN) = 1.D0
             SHP2(N1,IPLAN) = 0.D0
             SHP3(N1,IPLAN) = 0.D0
             ELT(N2,IPLAN) = IELEM
             SHP1(N2,IPLAN) = 0.D0
             SHP2(N2,IPLAN) = 1.D0
             SHP3(N2,IPLAN) = 0.D0
          ENDIF
!
          IF ((IFABOR(IELEM,2)==-2)) THEN
             ELT(N2,IPLAN) = IELEM
             SHP1(N2,IPLAN) = 0.D0
             SHP2(N2,IPLAN) = 1.D0
             SHP3(N2,IPLAN) = 0.D0
             ELT(N3,IPLAN) = IELEM
             SHP1(N3,IPLAN) = 0.D0
             SHP2(N3,IPLAN) = 0.D0
             SHP3(N3,IPLAN) = 1.D0
          ENDIF
!
          IF ((IFABOR(IELEM,3)==-2)) THEN
             ELT(N3,IPLAN) = IELEM
             SHP1(N3,IPLAN) = 0.D0
             SHP2(N3,IPLAN) = 0.D0
             SHP3(N3,IPLAN) = 1.D0
             ELT(N1,IPLAN) = IELEM
             SHP1(N1,IPLAN) = 1.D0
             SHP2(N1,IPLAN) = 0.D0
             SHP3(N1,IPLAN) = 0.D0
          ENDIF
!
450       CONTINUE
!
        DO 50 IELEM=1,NELEM2
          N1=IKLE2(IELEM,1)
          N2=IKLE2(IELEM,2)
          N3=IKLE2(IELEM,3)
!
! DET1 = (NINI+1,UNILAG)  DET2 = (UNILAG,NINI-1)
! ----------------------------------------------
!
      DET1=(X(N2)-X(N1))*V(N1,IPLAN)-(Y(N2)-Y(N1))*U(N1,IPLAN)
      DET2=(Y(N3)-Y(N1))*U(N1,IPLAN)-(X(N3)-X(N1))*V(N1,IPLAN)
          IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N1,IPLAN) = IELEM
             SHP1(N1,IPLAN) = 1.D0
             SHP2(N1,IPLAN) = 0.D0
             SHP3(N1,IPLAN) = 0.D0
             GOODELT(N1,IPLAN) = 1
          ENDIF
!
      DET1=(X(N3)-X(N2))*V(N2,IPLAN)-(Y(N3)-Y(N2))*U(N2,IPLAN)
      DET2=(Y(N1)-Y(N2))*U(N2,IPLAN)-(X(N1)-X(N2))*V(N2,IPLAN)
          IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N2,IPLAN) = IELEM
             SHP1(N2,IPLAN) = 0.D0
             SHP2(N2,IPLAN) = 1.D0
             SHP3(N2,IPLAN) = 0.D0
             GOODELT(N2,IPLAN) = 1
          ENDIF
!
      DET1=(X(N1)-X(N3))*V(N3,IPLAN)-(Y(N1)-Y(N3))*U(N3,IPLAN)
      DET2=(Y(N2)-Y(N3))*U(N3,IPLAN)-(X(N2)-X(N3))*V(N3,IPLAN)
          IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N3,IPLAN) = IELEM
             SHP1(N3,IPLAN) = 0.D0
             SHP2(N3,IPLAN) = 0.D0
             SHP3(N3,IPLAN) = 1.D0
             GOODELT(N3,IPLAN) = 1
          ENDIF
!
50       CONTINUE
         DO 230 IELEM = 1,NELEM2
            N1=IKLE2(IELEM,1)
            N2=IKLE2(IELEM,2)
            N3=IKLE2(IELEM,3)
          IF (IFABOR(IELEM,1)==0) GOODELT(N1,IPLAN)=
     &                                        GOODELT(N1,IPLAN)+10
          IF (IFABOR(IELEM,1)==0) GOODELT(N2,IPLAN)=
     &                                        GOODELT(N2,IPLAN)+10
          IF (IFABOR(IELEM,2)==0) GOODELT(N2,IPLAN)=
     &                                        GOODELT(N2,IPLAN)+10
          IF (IFABOR(IELEM,2)==0) GOODELT(N3,IPLAN)=
     &                                        GOODELT(N3,IPLAN)+10
          IF (IFABOR(IELEM,3)==0) GOODELT(N3,IPLAN)=
     &                                        GOODELT(N3,IPLAN)+10
          IF (IFABOR(IELEM,3)==0) GOODELT(N1,IPLAN)=
     &                                        GOODELT(N1,IPLAN)+10
          IF (IFABOR(IELEM,1)==-1) GOODELT(N1,IPLAN)=
     &                                        GOODELT(N1,IPLAN)+100
          IF (IFABOR(IELEM,1)==-1) GOODELT(N2,IPLAN)=
     &                                        GOODELT(N2,IPLAN)+100
          IF (IFABOR(IELEM,2)==-1) GOODELT(N2,IPLAN)=
     &                                        GOODELT(N2,IPLAN)+100
          IF (IFABOR(IELEM,2)==-1) GOODELT(N3,IPLAN)=
     &                                        GOODELT(N3,IPLAN)+100
          IF (IFABOR(IELEM,3)==-1) GOODELT(N3,IPLAN)=
     &                                        GOODELT(N3,IPLAN)+100
          IF (IFABOR(IELEM,3)==-1) GOODELT(N1,IPLAN)=
     &                                        GOODELT(N1,IPLAN)+100
          IF (IFABOR(IELEM,1)==-2) GOODELT(N1,IPLAN)=
     &                                       GOODELT(N1,IPLAN)+1000
          IF (IFABOR(IELEM,1)==-2) GOODELT(N2,IPLAN)=
     &                                       GOODELT(N2,IPLAN)+1000
          IF (IFABOR(IELEM,2)==-2) GOODELT(N2,IPLAN)=
     &                                       GOODELT(N2,IPLAN)+1000
          IF (IFABOR(IELEM,2)==-2) GOODELT(N3,IPLAN)=
     &                                       GOODELT(N3,IPLAN)+1000
          IF (IFABOR(IELEM,3)==-2) GOODELT(N1,IPLAN)=
     &                                       GOODELT(N1,IPLAN)+1000
          IF (IFABOR(IELEM,3)==-2) GOODELT(N3,IPLAN)=
     &                                       GOODELT(N3,IPLAN)+1000
230      CONTINUE
!
!
!-----------------------------------------------------------------------
!  FILLS IN THE SHT, ETA, SHF AND FRE, POINT BY POINT
!
         DO 70 IPOIN=1,NPOIN2
!
           IF (T(IPOIN,IPLAN).GT.0.D0) THEN
            IF (IPLAN.EQ.1) THEN
                ETA(IPOIN,1) = NPLAN
                SHT(IPOIN,1) = 1.D0
                TCONV(IPOIN,1)=TETA(NPLAN+1)
              ELSE
                ETA(IPOIN,IPLAN) = IPLAN-1
                SHT(IPOIN,IPLAN) = 1.D0
              ENDIF
           ELSE
              ETA(IPOIN,IPLAN) = IPLAN
              SHT(IPOIN,IPLAN) = 0.D0
           ENDIF
!
           IF (((W(IPOIN,IPLAN).GT.0.D0).AND.(IFF.NE.1)).OR.
     &             (IFF.EQ.NF)) THEN
            FRE(IPOIN,IPLAN) = IFF-1
            SHF(IPOIN,IPLAN) = 1.D0
           ELSE
            FRE(IPOIN,IPLAN) = IFF
            SHF(IPOIN,IPLAN) = 0.D0
           ENDIF
70       CONTINUE
!
!-----------------------------------------------------------------------
!
10      CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
