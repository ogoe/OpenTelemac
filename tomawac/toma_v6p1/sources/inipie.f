!                    *****************
                     SUBROUTINE INIPIE
!                    *****************
!
     &( U , V , W , X , Y , SHP1 ,SHP2 , SHP3 , SHZ , ELT , ETA ,
     & XCONV , YCONV , ZCONV, TETA , IKLE2 , NPOIN2 , NELEM2 , NPLAN  ,
     & ELI , KNOGL , KNI , NELE2L , NPOI2L ,IFABOR,GOODELT)
!
!***********************************************************************
! TOMAWAC   V6P1                                   20/06/2011
!***********************************************************************
!
!brief    FOR COWADIS "PRISMS", AND BEFORE TRACING BACK IN TIME
!+                THE CHARACTERISTICS CURVES, SETS THE BARYCENTRIC
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
!| ELI            |-->| WORK TABLE
!| ELT            |<--| NUMBERS OF THE ELEMENTS 2D OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETA            |<--| NUMBERS OF THE LAYERS OF THE
!|                |   | POINTS TO BE ADVECTED
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
!| IFABOR1        |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID,
!|                |   | SOLID OR PERIODIC BOUNDARY
!| IKLE2          |-->| TRANSITION BETWEEN LOCAL AND GLOBAL NUMBERING
!|                |   | OF THE 2D MESH
!| KNI            |-->| WORK TABLE
!| KNOGL          |-->| ARRAY LINKING GLOBAL TO LOCAL INDEXES IN PARALL
!| NELE2L         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOI2L         |-->| NUMBER OF POINTS IN 2D MESH
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| SHP1,SHP2,SHP3 |<--| BARYCENTRIC COORDINATES OF THE NODES IN
!|                |   | THEIR ASSOCIATED 2D ELEMENT "ELT"
!| SHZ            |<--| BARYCENTRIC COORDINATES ALONG TETA OF THE 
!|                |   | NODES IN THEIR ASSOCIATED LAYER "ETA"
!| TETA           |-->| DISCRETIZED DIRECTIONS
!| U,V,W          |-->| ADVECTION FIELD COMPONENTS
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| XCONV,YCONV,   |<--| COORDINATES OF THE INITIAL POINT OF THE
!|       ZCONV    |   | CHARACETRISTIC
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER NPOIN2,NELEM2,NELE2L,NPLAN,NPOI2L
!
      DOUBLE PRECISION U(NPOIN2,NPLAN),V(NPOIN2,NPLAN)
      DOUBLE PRECISION W(NPOIN2,NPLAN),TETA(*)
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION XCONV(NPOI2L,NPLAN),YCONV(NPOI2L,NPLAN)
      DOUBLE PRECISION ZCONV(NPOI2L,NPLAN)
      DOUBLE PRECISION SHP1(NPOI2L,NPLAN),SHP2(NPOI2L,NPLAN)
      DOUBLE PRECISION SHP3(NPOI2L,NPLAN),SHZ(NPOI2L,NPLAN)
      DOUBLE PRECISION DET1,DET2,EPS
!
      INTEGER ELI(NELE2L),KNOGL(NPOIN2),KNI(NPOI2L)
      INTEGER IKLE2(NELEM2,3),ELT(NPOI2L,NPLAN),ETA(NPOI2L,NPLAN)
      INTEGER N1L,N2L,N3L,N1G,N2G,N3G,IPOIL,IPOIG,IELEM,IEL2,IPLAN,IP
      INTEGER IFABOR(NELEM2,5),GOODELT(NPOI2L,NPLAN)
      DOUBLE PRECISION EPS2
!
!      DATA EPS / 1.D-6 /
      DATA EPS / 0.D0 /
!-----------------------------------------------------------------------
!
!  INITIALISES THE POINTS TO ADVECT
!
      GOODELT = 0
      IF (NPOI2L.NE.NPOIN2) THEN
        DO 60 IP=1,NPLAN
          DO 90 IPOIL=1,NPOI2L
            IPOIG=KNI(IPOIL)
            XCONV(IPOIL,IP)=X(IPOIG)
            YCONV(IPOIL,IP)=Y(IPOIG)
            ZCONV(IPOIL,IP)=TETA(IP)
90        CONTINUE
60      CONTINUE
      ELSE
        DO 160 IP=1,NPLAN
          DO 190 IPOIG=1,NPOIN2
            XCONV(IPOIG,IP)=X(IPOIG)
            YCONV(IPOIG,IP)=Y(IPOIG)
            ZCONV(IPOIG,IP)=TETA(IP)
190       CONTINUE
160     CONTINUE
      ENDIF
!
!-----------------------------------------------------------------------
!
      DO 10 IPLAN=1,NPLAN
!
      IF (NELE2L.NE.NELEM2) THEN
!***********************************************************************
!     IN PARALLEL MODE
!***********************************************************************
!-----------------------------------------------------------------------
!  INITIALLY FILLS IN THE SHP AND ELT
!  (NOTE: FOR LATERAL BOUNDARY POINTS, THERE MAY NOT BE AN ELEMENT
!  TOWARDS WHICH -(U, V) POINTS).
!
         DO 20 IEL2 = 1,NELE2L
            IELEM=ELI(IEL2)
!
            N1G=IKLE2(IELEM,1)
            N1L=KNOGL(N1G)
!            WRITE(LU,*)'IEL2' ,IELEM,N1G,N1L
               ELT(N1L,IPLAN) = IELEM
               SHP1(N1L,IPLAN) = 1.D0
               SHP2(N1L,IPLAN) = 0.D0
               SHP3(N1L,IPLAN) = 0.D0
            N1G=IKLE2(IELEM,2)
            N1L=KNOGL(N1G)
!            WRITE(LU,*)'IEL2' ,IELEM,N1G,N1L
               ELT(N1L,IPLAN) = IELEM
               SHP1(N1L,IPLAN) = 0.D0
               SHP2(N1L,IPLAN) = 1.D0
               SHP3(N1L,IPLAN) = 0.D0
            N1G=IKLE2(IELEM,3)
            N1L=KNOGL(N1G)
!            WRITE(LU,*)'IEL2' ,IELEM,N1G,N1L
               ELT(N1L,IPLAN) = IELEM
               SHP1(N1L,IPLAN) = 0.D0
               SHP2(N1L,IPLAN) = 0.D0
               SHP3(N1L,IPLAN) = 1.D0
20       CONTINUE
!
!-----------------------------------------------------------------------
!  FILLS IN THE SHP AND ELT, ELEMENT BY ELEMENT, FOR THE POINTS IN
!  THE ELEMENT FOR WHICH -(U, V) POINTS TOWARDS THIS ELEMENT.
!
        DO 50 IEL2=1,NELE2L
          IELEM=ELI(IEL2)
!
          N1G=IKLE2(IELEM,1)
          N1L=KNOGL(N1G)
          N2G=IKLE2(IELEM,2)
          N2L=KNOGL(N2G)
          N3G=IKLE2(IELEM,3)
          N3L=KNOGL(N3G)
!
! DET1 = (NINI+1,UNILAG)  DET2 = (UNILAG,NINI-1)
! ----------------------------------------------
!
      DET1=(X(N2G)-X(N1G))*V(N1G,IPLAN)-(Y(N2G)-Y(N1G))*U(N1G,IPLAN)
      DET2=(Y(N3G)-Y(N1G))*U(N1G,IPLAN)-(X(N3G)-X(N1G))*V(N1G,IPLAN)
          IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N1L,IPLAN) = IELEM
             SHP1(N1L,IPLAN) = 1.D0
             SHP2(N1L,IPLAN) = 0.D0
             SHP3(N1L,IPLAN) = 0.D0
          ENDIF
!
      DET1=(X(N3G)-X(N2G))*V(N2G,IPLAN)-(Y(N3G)-Y(N2G))*U(N2G,IPLAN)
      DET2=(Y(N1G)-Y(N2G))*U(N2G,IPLAN)-(X(N1G)-X(N2G))*V(N2G,IPLAN)
          IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N2L,IPLAN) = IELEM
             SHP1(N2L,IPLAN) = 0.D0
             SHP2(N2L,IPLAN) = 1.D0
             SHP3(N2L,IPLAN) = 0.D0
          ENDIF
!
      DET1=(X(N1G)-X(N3G))*V(N3G,IPLAN)-(Y(N1G)-Y(N3G))*U(N3G,IPLAN)
      DET2=(Y(N2G)-Y(N3G))*U(N3G,IPLAN)-(X(N2G)-X(N3G))*V(N3G,IPLAN)
          IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N3L,IPLAN) = IELEM
             SHP1(N3L,IPLAN) = 0.D0
             SHP2(N3L,IPLAN) = 0.D0
             SHP3(N3L,IPLAN) = 1.D0
          ENDIF
!
50       CONTINUE
!
!-----------------------------------------------------------------------
!  FILLS IN THE SHZ AND ETA, POINT BY POINT.
!
         DO 70 IPOIL=1,NPOI2L
            IPOIG=KNI(IPOIL)
!
           IF (W(IPOIG,IPLAN).GT.0.D0) THEN
              IF (IPLAN.EQ.1) THEN
                ETA(IPOIL,1) = NPLAN
                SHZ(IPOIL,1) = 1.D0
                ZCONV(IPOIL,1)=TETA(NPLAN+1)
              ELSE
                ETA(IPOIL,IPLAN) = IPLAN-1
                SHZ(IPOIL,IPLAN) = 1.D0
              ENDIF
           ELSE
              ETA(IPOIL,IPLAN) = IPLAN
              SHZ(IPOIL,IPLAN) = 0.D0
           ENDIF
!
70       CONTINUE
!
!***********************************************************************
!     END OF COMPUTATION IN PARALLEL MODE
!***********************************************************************
!
      ELSE
!
         DO 120 IELEM = 1,NELEM2
            N1G=IKLE2(IELEM,1)
               ELT(N1G,IPLAN) = IELEM
               SHP1(N1G,IPLAN) = 1.D0
               SHP2(N1G,IPLAN) = 0.D0
               SHP3(N1G,IPLAN) = 0.D0
            N1G=IKLE2(IELEM,2)
               ELT(N1G,IPLAN) = IELEM
               SHP1(N1G,IPLAN) = 0.D0
               SHP2(N1G,IPLAN) = 1.D0
               SHP3(N1G,IPLAN) = 0.D0
            N1G=IKLE2(IELEM,3)
               ELT(N1G,IPLAN) = IELEM
               SHP1(N1G,IPLAN) = 0.D0
               SHP2(N1G,IPLAN) = 0.D0
               SHP3(N1G,IPLAN) = 1.D0
120      CONTINUE
!
!
        DO 450 IELEM=1,NELEM2
!
          N1G=IKLE2(IELEM,1)
          N2G=IKLE2(IELEM,2)
          N3G=IKLE2(IELEM,3)
!
! ON THE EDGE OF EACH PROC
! ----------------------------------------------
!
          IF ((IFABOR(IELEM,1)==-2)) THEN
             ELT(N1G,IPLAN) = IELEM
             SHP1(N1G,IPLAN) = 1.D0
             SHP2(N1G,IPLAN) = 0.D0
             SHP3(N1G,IPLAN) = 0.D0
             ELT(N2G,IPLAN) = IELEM
             SHP1(N2G,IPLAN) = 0.D0
             SHP2(N2G,IPLAN) = 1.D0
             SHP3(N2G,IPLAN) = 0.D0
          ENDIF
!
          IF ((IFABOR(IELEM,2)==-2)) THEN
             ELT(N2G,IPLAN) = IELEM
             SHP1(N2G,IPLAN) = 0.D0
             SHP2(N2G,IPLAN) = 1.D0
             SHP3(N2G,IPLAN) = 0.D0
             ELT(N3G,IPLAN) = IELEM
             SHP1(N3G,IPLAN) = 0.D0
             SHP2(N3G,IPLAN) = 0.D0
             SHP3(N3G,IPLAN) = 1.D0
          ENDIF
!
          IF ((IFABOR(IELEM,3)==-2)) THEN
             ELT(N3G,IPLAN) = IELEM
             SHP1(N3G,IPLAN) = 0.D0
             SHP2(N3G,IPLAN) = 0.D0
             SHP3(N3G,IPLAN) = 1.D0
             ELT(N1G,IPLAN) = IELEM
             SHP1(N1G,IPLAN) = 1.D0
             SHP2(N1G,IPLAN) = 0.D0
             SHP3(N1G,IPLAN) = 0.D0
          ENDIF
!
450       CONTINUE
        DO 150 IELEM=1,NELEM2
!
          N1G=IKLE2(IELEM,1)
          N2G=IKLE2(IELEM,2)
          N3G=IKLE2(IELEM,3)
!
! DET1 = (NINI+1,UNILAG)  DET2 = (UNILAG,NINI-1)
! ----------------------------------------------
!
      DET1=(X(N2G)-X(N1G))*V(N1G,IPLAN)-(Y(N2G)-Y(N1G))*U(N1G,IPLAN)
      DET2=(Y(N3G)-Y(N1G))*U(N1G,IPLAN)-(X(N3G)-X(N1G))*V(N1G,IPLAN)
      IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N1G,IPLAN) = IELEM
             SHP1(N1G,IPLAN) = 1.D0
             SHP2(N1G,IPLAN) = 0.D0
             SHP3(N1G,IPLAN) = 0.D0
             GOODELT(N1G,IPLAN) = 1
      ENDIF
!
      DET1=(X(N3G)-X(N2G))*V(N2G,IPLAN)-(Y(N3G)-Y(N2G))*U(N2G,IPLAN)
      DET2=(Y(N1G)-Y(N2G))*U(N2G,IPLAN)-(X(N1G)-X(N2G))*V(N2G,IPLAN)
      IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N2G,IPLAN) = IELEM
             SHP1(N2G,IPLAN) = 0.D0
             SHP2(N2G,IPLAN) = 1.D0
             SHP3(N2G,IPLAN) = 0.D0
             GOODELT(N2G,IPLAN) = 1
      ENDIF
!
      DET1=(X(N1G)-X(N3G))*V(N3G,IPLAN)-(Y(N1G)-Y(N3G))*U(N3G,IPLAN)
      DET2=(Y(N2G)-Y(N3G))*U(N3G,IPLAN)-(X(N2G)-X(N3G))*V(N3G,IPLAN)
      IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N3G,IPLAN) = IELEM
             SHP1(N3G,IPLAN) = 0.D0
             SHP2(N3G,IPLAN) = 0.D0
             SHP3(N3G,IPLAN) = 1.D0
             GOODELT(N3G,IPLAN) = 1
      ENDIF
!
150       CONTINUE
!
         DO 230 IELEM = 1,NELEM2
            N1G=IKLE2(IELEM,1)
            N2G=IKLE2(IELEM,2)
            N3G=IKLE2(IELEM,3)
          IF (IFABOR(IELEM,1)==0) GOODELT(N1G,IPLAN)=
     &                                        GOODELT(N1G,IPLAN)+10
          IF (IFABOR(IELEM,1)==0) GOODELT(N2G,IPLAN)=
     &                                        GOODELT(N2G,IPLAN)+10
          IF (IFABOR(IELEM,2)==0) GOODELT(N2G,IPLAN)=
     &                                        GOODELT(N2G,IPLAN)+10
          IF (IFABOR(IELEM,2)==0) GOODELT(N3G,IPLAN)=
     &                                        GOODELT(N3G,IPLAN)+10
          IF (IFABOR(IELEM,3)==0) GOODELT(N3G,IPLAN)=
     &                                        GOODELT(N3G,IPLAN)+10
          IF (IFABOR(IELEM,3)==0) GOODELT(N1G,IPLAN)=
     &                                        GOODELT(N1G,IPLAN)+10
          IF (IFABOR(IELEM,1)==-1) GOODELT(N1G,IPLAN)=
     &                                        GOODELT(N1G,IPLAN)+100
          IF (IFABOR(IELEM,1)==-1) GOODELT(N2G,IPLAN)=
     &                                        GOODELT(N2G,IPLAN)+100
          IF (IFABOR(IELEM,2)==-1) GOODELT(N2G,IPLAN)=
     &                                        GOODELT(N2G,IPLAN)+100
          IF (IFABOR(IELEM,2)==-1) GOODELT(N3G,IPLAN)=
     &                                        GOODELT(N3G,IPLAN)+100
          IF (IFABOR(IELEM,3)==-1) GOODELT(N3G,IPLAN)=
     &                                        GOODELT(N3G,IPLAN)+100
          IF (IFABOR(IELEM,3)==-1) GOODELT(N1G,IPLAN)=
     &                                        GOODELT(N1G,IPLAN)+100
          IF (IFABOR(IELEM,1)==-2) GOODELT(N1G,IPLAN)=
     &                                       GOODELT(N1G,IPLAN)+1000
          IF (IFABOR(IELEM,1)==-2) GOODELT(N2G,IPLAN)=
     &                                       GOODELT(N2G,IPLAN)+1000
          IF (IFABOR(IELEM,2)==-2) GOODELT(N2G,IPLAN)=
     &                                       GOODELT(N2G,IPLAN)+1000
          IF (IFABOR(IELEM,2)==-2) GOODELT(N3G,IPLAN)=
     &                                       GOODELT(N3G,IPLAN)+1000
          IF (IFABOR(IELEM,3)==-2) GOODELT(N1G,IPLAN)=
     &                                       GOODELT(N1G,IPLAN)+1000
          IF (IFABOR(IELEM,3)==-2) GOODELT(N3G,IPLAN)=
     &                                       GOODELT(N3G,IPLAN)+1000
230      CONTINUE
!
!-----------------------------------------------------------------------
!  FILLS IN THE SHZ AND ETA, POINT BY POINT.
!
         DO 170 IPOIG=1,NPOIN2
!
           IF (W(IPOIG,IPLAN).GT.0.D0) THEN
              IF (IPLAN.EQ.1) THEN
                ETA(IPOIG,1) = NPLAN
                SHZ(IPOIG,1) = 1.D0
                ZCONV(IPOIG,1)=TETA(NPLAN+1)
              ELSE
                ETA(IPOIG,IPLAN) = IPLAN-1
                SHZ(IPOIG,IPLAN) = 1.D0
              ENDIF
           ELSE
              ETA(IPOIG,IPLAN) = IPLAN
              SHZ(IPOIG,IPLAN) = 0.D0
           ENDIF
!
170       CONTINUE
!
!-----------------------------------------------------------------------
!       END OF SCALAR COMPUTATION
!-----------------------------------------------------------------------
       ENDIF
!
10    CONTINUE
!
      RETURN
      END
