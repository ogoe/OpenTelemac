C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FOR COWADIS "PRISMS", AND BEFORE TRACING BACK IN TIME
!>                THE CHARACTERISTICS CURVES, SETS THE BARYCENTRIC
!>                COORDINATES FOR ALL THE NODES IN THE MESH IN THE ELEMENT
!>                TOWARDS WHICH THE CURVE POINTS.
!><br>           (SUBROUTINE INSPIRED FROM GSHP41 OF BIEF)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELI, ELT, ETA, GOODELT, IFABOR, IKLE2, KNI, KNOGL, NELE2L, NELEM2, NPLAN, NPOI2L, NPOIN2, SHP1, SHP2, SHP3, SHZ, TETA, U, V, W, X, XCONV, Y, YCONV, ZCONV
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DET1, DET2, EPS, EPS2, IEL2, IELEM, IP, IPLAN, IPOIG, IPOIL, N1G, N1L, N2G, N2L, N3G, N3L
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PREPRO()

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
!>      <td><center> 1.0                                       </center>
!> </td><td> 01/02/93
!> </td><td> F MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ELI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ELT
!></td><td><--</td><td>NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
!>                  NOEUD.
!>    </td></tr>
!>          <tr><td>ETA
!></td><td><--</td><td>NUMEROS DES ETAGES CHOISIS POUR CHAQUE NOEUD.
!>    </td></tr>
!>          <tr><td>GOODELT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>TRANSITION ENTRE LES NUMEROTATIONS LOCALE
!>                  ET GLOBALE
!>    </td></tr>
!>          <tr><td>KNI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KNOGL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELE2L
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS
!>    </td></tr>
!>          <tr><td>NPOI2L
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>SHP1
!></td><td><--</td><td>COORDONNEES BARYCENTRIQUES DES NOEUDS DANS
!>                  LEURS ELEMENTS 2D "ELT" ASSOCIES.
!>    </td></tr>
!>          <tr><td>SHP2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHP3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHZ
!></td><td><--</td><td>COORDONNEES BARYCENTRIQUES SUIVANT Z DES
!>                  NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>--></td><td>DIRECTIONS DE PROPAGATION
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES DU CHAMP CONVECTEUR
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>XCONV
!></td><td><--</td><td>POSITION INITIALE DES DERIVANT EN X
!>    </td></tr>
!>          <tr><td>YCONV
!></td><td><--</td><td>POSITION INITIALE DES DERIVANT EN Y
!>    </td></tr>
!>          <tr><td>ZCONV
!></td><td><--</td><td>POSITION INITIALE DES DERIVANT EN Z
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INIPIE
     &( U , V , W , X , Y , SHP1 ,SHP2 , SHP3 , SHZ , ELT , ETA ,
     & XCONV , YCONV , ZCONV, TETA , IKLE2 , NPOIN2 , NELEM2 , NPLAN  ,
     & ELI , KNOGL , KNI , NELE2L , NPOI2L ,IFABOR,GOODELT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ELI            |---| 
C| ELT            |<--| NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
C|                |   | NOEUD.
C| ETA            |<--| NUMEROS DES ETAGES CHOISIS POUR CHAQUE NOEUD.
C| GOODELT        |---| 
C| IFABOR         |---| 
C| IKLE2          |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
C|                |   | ET GLOBALE
C| KNI            |---| 
C| KNOGL          |---| 
C| NELE2L         |---| 
C| NELEM2         |-->| NOMBRE D'ELEMENTS DU MAILLAGE 2D.
C| NPLAN          |-->| NOMBRE DE DIRECTIONS
C| NPOI2L         |---| 
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D.
C| SHP1           |<--| COORDONNEES BARYCENTRIQUES DES NOEUDS DANS
C|                |   | LEURS ELEMENTS 2D "ELT" ASSOCIES.
C| SHP2           |---| 
C| SHP3           |---| 
C| SHZ            |<--| COORDONNEES BARYCENTRIQUES SUIVANT Z DES
C|                |   | NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
C| TETA           |-->| DIRECTIONS DE PROPAGATION
C| U,V,W          |-->| COMPOSANTES DU CHAMP CONVECTEUR
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE.
C| XCONV          |<--| POSITION INITIALE DES DERIVANT EN X
C| YCONV          |<--| POSITION INITIALE DES DERIVANT EN Y
C| ZCONV          |<--| POSITION INITIALE DES DERIVANT EN Z
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER NPOIN2,NELEM2,NELE2L,NPLAN,NPOI2L
C
      DOUBLE PRECISION U(NPOIN2,NPLAN),V(NPOIN2,NPLAN)
      DOUBLE PRECISION W(NPOIN2,NPLAN),TETA(*)
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION XCONV(NPOI2L,NPLAN),YCONV(NPOI2L,NPLAN)
      DOUBLE PRECISION ZCONV(NPOI2L,NPLAN)
      DOUBLE PRECISION SHP1(NPOI2L,NPLAN),SHP2(NPOI2L,NPLAN)
      DOUBLE PRECISION SHP3(NPOI2L,NPLAN),SHZ(NPOI2L,NPLAN)
      DOUBLE PRECISION DET1,DET2,EPS
C
      INTEGER ELI(NELE2L),KNOGL(NPOIN2),KNI(NPOI2L)
      INTEGER IKLE2(NELEM2,3),ELT(NPOI2L,NPLAN),ETA(NPOI2L,NPLAN)
      INTEGER N1L,N2L,N3L,N1G,N2G,N3G,IPOIL,IPOIG,IELEM,IEL2,IPLAN,IP
      INTEGER IFABOR(NELEM2,5),GOODELT(NPOI2L,NPLAN)
      DOUBLE PRECISION EPS2
C
C      DATA EPS / 1.D-6 /
      DATA EPS / 0.D0 /
C-----------------------------------------------------------------------
C
C  INITIALISES THE POINTS TO ADVECT
C
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
C
C-----------------------------------------------------------------------
C
      DO 10 IPLAN=1,NPLAN
C
      IF (NELE2L.NE.NELEM2) THEN
C***********************************************************************
C     IN PARALLEL MODE
C***********************************************************************
C-----------------------------------------------------------------------
C  INITIALLY FILLS IN THE SHP AND ELT
C  (NOTE: FOR LATERAL BOUNDARY POINTS, THERE MAY NOT BE AN ELEMENT
C  TOWARDS WHICH -(U, V) POINTS).
C
         DO 20 IEL2 = 1,NELE2L
            IELEM=ELI(IEL2)
C
            N1G=IKLE2(IELEM,1)
            N1L=KNOGL(N1G)
C            WRITE(LU,*)'IEL2' ,IELEM,N1G,N1L
               ELT(N1L,IPLAN) = IELEM
               SHP1(N1L,IPLAN) = 1.D0
               SHP2(N1L,IPLAN) = 0.D0
               SHP3(N1L,IPLAN) = 0.D0
            N1G=IKLE2(IELEM,2)
            N1L=KNOGL(N1G)
C            WRITE(LU,*)'IEL2' ,IELEM,N1G,N1L
               ELT(N1L,IPLAN) = IELEM
               SHP1(N1L,IPLAN) = 0.D0
               SHP2(N1L,IPLAN) = 1.D0
               SHP3(N1L,IPLAN) = 0.D0
            N1G=IKLE2(IELEM,3)
            N1L=KNOGL(N1G)
C            WRITE(LU,*)'IEL2' ,IELEM,N1G,N1L
               ELT(N1L,IPLAN) = IELEM
               SHP1(N1L,IPLAN) = 0.D0
               SHP2(N1L,IPLAN) = 0.D0
               SHP3(N1L,IPLAN) = 1.D0
20       CONTINUE
C
C-----------------------------------------------------------------------
C  FILLS IN THE SHP AND ELT, ELEMENT BY ELEMENT, FOR THE POINTS IN
C  THE ELEMENT FOR WHICH -(U, V) POINTS TOWARDS THIS ELEMENT.
C
        DO 50 IEL2=1,NELE2L
          IELEM=ELI(IEL2)
C
          N1G=IKLE2(IELEM,1)
          N1L=KNOGL(N1G)
          N2G=IKLE2(IELEM,2)
          N2L=KNOGL(N2G)
          N3G=IKLE2(IELEM,3)
          N3L=KNOGL(N3G)
C
C DET1 = (NINI+1,UNILAG)  DET2 = (UNILAG,NINI-1)
C ----------------------------------------------
C
      DET1=(X(N2G)-X(N1G))*V(N1G,IPLAN)-(Y(N2G)-Y(N1G))*U(N1G,IPLAN)
      DET2=(Y(N3G)-Y(N1G))*U(N1G,IPLAN)-(X(N3G)-X(N1G))*V(N1G,IPLAN)
          IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N1L,IPLAN) = IELEM
             SHP1(N1L,IPLAN) = 1.D0
             SHP2(N1L,IPLAN) = 0.D0
             SHP3(N1L,IPLAN) = 0.D0
          ENDIF
C
      DET1=(X(N3G)-X(N2G))*V(N2G,IPLAN)-(Y(N3G)-Y(N2G))*U(N2G,IPLAN)
      DET2=(Y(N1G)-Y(N2G))*U(N2G,IPLAN)-(X(N1G)-X(N2G))*V(N2G,IPLAN)
          IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N2L,IPLAN) = IELEM
             SHP1(N2L,IPLAN) = 0.D0
             SHP2(N2L,IPLAN) = 1.D0
             SHP3(N2L,IPLAN) = 0.D0
          ENDIF
C
      DET1=(X(N1G)-X(N3G))*V(N3G,IPLAN)-(Y(N1G)-Y(N3G))*U(N3G,IPLAN)
      DET2=(Y(N2G)-Y(N3G))*U(N3G,IPLAN)-(X(N2G)-X(N3G))*V(N3G,IPLAN)
          IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N3L,IPLAN) = IELEM
             SHP1(N3L,IPLAN) = 0.D0
             SHP2(N3L,IPLAN) = 0.D0
             SHP3(N3L,IPLAN) = 1.D0
          ENDIF
C
50       CONTINUE
C
C-----------------------------------------------------------------------
C  FILLS IN THE SHZ AND ETA, POINT BY POINT.
C
         DO 70 IPOIL=1,NPOI2L
            IPOIG=KNI(IPOIL)
C
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
C
70       CONTINUE
C
C***********************************************************************
C     END OF COMPUTATION IN PARALLEL MODE
C***********************************************************************
C
      ELSE
C
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
C
!
        DO 450 IELEM=1,NELEM2
C
          N1G=IKLE2(IELEM,1)
          N2G=IKLE2(IELEM,2)
          N3G=IKLE2(IELEM,3)
C
C ON THE EDGE OF EACH PROC
C ----------------------------------------------
C
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
C
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
C
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
C
450       CONTINUE

        DO 150 IELEM=1,NELEM2
C
          N1G=IKLE2(IELEM,1)
          N2G=IKLE2(IELEM,2)
          N3G=IKLE2(IELEM,3)
C
C DET1 = (NINI+1,UNILAG)  DET2 = (UNILAG,NINI-1)
C ----------------------------------------------
C
      DET1=(X(N2G)-X(N1G))*V(N1G,IPLAN)-(Y(N2G)-Y(N1G))*U(N1G,IPLAN)
      DET2=(Y(N3G)-Y(N1G))*U(N1G,IPLAN)-(X(N3G)-X(N1G))*V(N1G,IPLAN)
      IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N1G,IPLAN) = IELEM
             SHP1(N1G,IPLAN) = 1.D0
             SHP2(N1G,IPLAN) = 0.D0
             SHP3(N1G,IPLAN) = 0.D0
             GOODELT(N1G,IPLAN) = 1
      ENDIF
C
      DET1=(X(N3G)-X(N2G))*V(N2G,IPLAN)-(Y(N3G)-Y(N2G))*U(N2G,IPLAN)
      DET2=(Y(N1G)-Y(N2G))*U(N2G,IPLAN)-(X(N1G)-X(N2G))*V(N2G,IPLAN)
      IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N2G,IPLAN) = IELEM
             SHP1(N2G,IPLAN) = 0.D0
             SHP2(N2G,IPLAN) = 1.D0
             SHP3(N2G,IPLAN) = 0.D0
             GOODELT(N2G,IPLAN) = 1
      ENDIF
C
      DET1=(X(N1G)-X(N3G))*V(N3G,IPLAN)-(Y(N1G)-Y(N3G))*U(N3G,IPLAN)
      DET2=(Y(N2G)-Y(N3G))*U(N3G,IPLAN)-(X(N2G)-X(N3G))*V(N3G,IPLAN)
      IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N3G,IPLAN) = IELEM
             SHP1(N3G,IPLAN) = 0.D0
             SHP2(N3G,IPLAN) = 0.D0
             SHP3(N3G,IPLAN) = 1.D0
             GOODELT(N3G,IPLAN) = 1
      ENDIF

C
150       CONTINUE



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

C
C-----------------------------------------------------------------------
C  FILLS IN THE SHZ AND ETA, POINT BY POINT.
C
         DO 170 IPOIG=1,NPOIN2
C
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
C
170       CONTINUE
C
C-----------------------------------------------------------------------
C       END OF SCALAR COMPUTATION
C-----------------------------------------------------------------------
       ENDIF
C
10    CONTINUE
C
      RETURN
      END
C
C#######################################################################
C