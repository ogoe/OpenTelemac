C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FOR TOMAWAC "HYPER PRISMS", AND BEFORE TRACING BACK IN
!>                TIME THE CHARACTERISTICS CURVES, SETS THE BARYCENTRIC
!>                COORDINATES FOR ALL THE NODES IN THE MESH IN THE ELEMENT
!>                TOWARDS WHICH THE CURVE POINTS.
!><br>           (SUBROUTINE INSPIRED FROM GSHP41 OF BIEF)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELT, ETA, FCONV, FRE, FREQ, GOODELT, IFABOR, IFF, IKLE2, NELEM2, NF, NPLAN, NPOIN2, SHF, SHP1, SHP2, SHP3, SHT, T, TCONV, TETA, U, V, W, X, XCONV, Y, YCONV
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DET1, DET2, EPS, IELEM, IP, IPLAN, IPOIN, N1, N2, N3
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
!>          <tr><td>ELT
!></td><td><--</td><td>NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
!>                  NOEUD.
!>    </td></tr>
!>          <tr><td>ETA
!></td><td><--</td><td>NUMEROS DES DIREC. CHOISIS POUR CHAQUE NOEUD.
!>    </td></tr>
!>          <tr><td>FCONV
!></td><td><--</td><td>POSITION INITIALE DES DERIVANT EN F
!>    </td></tr>
!>          <tr><td>FRE
!></td><td><--</td><td>NUMEROS DES FREQ. CHOISIES POUR CHAQUE NOEUD.
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>--></td><td>FREQUENCES DE PROPAGATION
!>    </td></tr>
!>          <tr><td>GOODELT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IFF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>TRANSITION ENTRE LES NUMEROTATIONS LOCALE
!>                  ET GLOBALE
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>SHF
!></td><td>---</td><td>
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
!>          <tr><td>SHT
!></td><td><--</td><td>COORDONNEES BARYCENTRIQUES SUIVANT Z DES
!>                  NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
!>    </td></tr>
!>          <tr><td>TCONV
!></td><td><--</td><td>POSITION INITIALE DES DERIVANT EN TETA
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>--></td><td>DIRECTIONS DE PROPAGATION
!>    </td></tr>
!>          <tr><td>U,V,T,W
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
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INIP4D
     &( U , V , T , W , X , Y , SHP1 ,SHP2 , SHP3 , SHT , SHF , ELT ,
     & ETA , FRE , XCONV , YCONV , TCONV, FCONV , TETA , FREQ ,IKLE2 ,
     & NPOIN2 , NELEM2 , NPLAN  , IFF , NF  ,IFABOR,GOODELT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ELT            |<--| NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
C|                |   | NOEUD.
C| ETA            |<--| NUMEROS DES DIREC. CHOISIS POUR CHAQUE NOEUD.
C| FCONV          |<--| POSITION INITIALE DES DERIVANT EN F
C| FRE            |<--| NUMEROS DES FREQ. CHOISIES POUR CHAQUE NOEUD.
C| FREQ           |-->| FREQUENCES DE PROPAGATION
C| GOODELT        |---| 
C| IFABOR         |---| 
C| IFF            |---| 
C| IKLE2          |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
C|                |   | ET GLOBALE
C| NELEM2         |-->| NOMBRE D'ELEMENTS DU MAILLAGE 2D.
C| NF             |-->| NOMBRE DE FREQUENCES
C| NPLAN          |-->| NOMBRE DE DIRECTIONS
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D.
C| SHF            |---| 
C| SHP1           |<--| COORDONNEES BARYCENTRIQUES DES NOEUDS DANS
C|                |   | LEURS ELEMENTS 2D "ELT" ASSOCIES.
C| SHP2           |---| 
C| SHP3           |---| 
C| SHT            |<--| COORDONNEES BARYCENTRIQUES SUIVANT Z DES
C|                |   | NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
C| TCONV          |<--| POSITION INITIALE DES DERIVANT EN TETA
C| TETA           |-->| DIRECTIONS DE PROPAGATION
C| U,V,T,W        |-->| COMPOSANTES DU CHAMP CONVECTEUR
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE.
C| XCONV          |<--| POSITION INITIALE DES DERIVANT EN X
C| YCONV          |<--| POSITION INITIALE DES DERIVANT EN Y
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER NPOIN2,NELEM2,NPLAN,NF
      INTEGER N1,N2,N3,IPOIN,IELEM,IPLAN,IP,IFF
C
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
C
      INTEGER IKLE2(NELEM2,3),ELT(NPOIN2,NPLAN),ETA(NPOIN2,NPLAN)
      INTEGER FRE(NPOIN2,NPLAN)
C
C      DATA EPS / 1.D-6 /
C      DATA EPS / 1.D-12 /
       DATA EPS /0.D0 /
C-----------------------------------------------------------------------
C
C  INITIALISES THE POINTS TO ADVECT
C
      GOODELT = 0
      DO 60 IP=1,NPLAN
        DO 90 IPOIN=1,NPOIN2
          XCONV(IPOIN,IP)=X(IPOIN)
          YCONV(IPOIN,IP)=Y(IPOIN)
          TCONV(IPOIN,IP)=TETA(IP)
          FCONV(IPOIN,IP)=FREQ(IFF)
90        CONTINUE
60      CONTINUE
C
C-----------------------------------------------------------------------
        DO 10 IPLAN=1,NPLAN
C
C-----------------------------------------------------------------------
C  INITIALLY FILLS IN THE SHP AND ELT
C  (NOTE: FOR LATERAL BOUNDARY POINTS, THERE MAY NOT BE AN ELEMENT
C  TOWARDS WHICH -(U, V) POINTS).
C
         DO 20 IELEM = 1,NELEM2
C
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
C
C-----------------------------------------------------------------------
C  FILLS IN THE SHP AND ELT, ELEMENT BY ELEMENT, FOR THE POINTS IN
C  THE ELEMENT FOR WHICH -(U, V) POINTS TOWARDS THIS ELEMENT.
C
        DO 450 IELEM=1,NELEM2
C
          N1=IKLE2(IELEM,1)
          N2=IKLE2(IELEM,2)
          N3=IKLE2(IELEM,3)
C
C ON THE EDGE OF EACH PROC
C ----------------------------------------------
C
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
C
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
C
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
C
450       CONTINUE


        DO 50 IELEM=1,NELEM2
          N1=IKLE2(IELEM,1)
          N2=IKLE2(IELEM,2)
          N3=IKLE2(IELEM,3)
C
C DET1 = (NINI+1,UNILAG)  DET2 = (UNILAG,NINI-1)
C ----------------------------------------------
C
      DET1=(X(N2)-X(N1))*V(N1,IPLAN)-(Y(N2)-Y(N1))*U(N1,IPLAN)
      DET2=(Y(N3)-Y(N1))*U(N1,IPLAN)-(X(N3)-X(N1))*V(N1,IPLAN)
          IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N1,IPLAN) = IELEM
             SHP1(N1,IPLAN) = 1.D0
             SHP2(N1,IPLAN) = 0.D0
             SHP3(N1,IPLAN) = 0.D0
             GOODELT(N1,IPLAN) = 1
          ENDIF
C
      DET1=(X(N3)-X(N2))*V(N2,IPLAN)-(Y(N3)-Y(N2))*U(N2,IPLAN)
      DET2=(Y(N1)-Y(N2))*U(N2,IPLAN)-(X(N1)-X(N2))*V(N2,IPLAN)
          IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N2,IPLAN) = IELEM
             SHP1(N2,IPLAN) = 0.D0
             SHP2(N2,IPLAN) = 1.D0
             SHP3(N2,IPLAN) = 0.D0
             GOODELT(N2,IPLAN) = 1
          ENDIF
C
      DET1=(X(N1)-X(N3))*V(N3,IPLAN)-(Y(N1)-Y(N3))*U(N3,IPLAN)
      DET2=(Y(N2)-Y(N3))*U(N3,IPLAN)-(X(N2)-X(N3))*V(N3,IPLAN)
          IF (DET1.LE.EPS.AND.DET2.LE.EPS) THEN
             ELT(N3,IPLAN) = IELEM
             SHP1(N3,IPLAN) = 0.D0
             SHP2(N3,IPLAN) = 0.D0
             SHP3(N3,IPLAN) = 1.D0
             GOODELT(N3,IPLAN) = 1
          ENDIF
C
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



C
C-----------------------------------------------------------------------
C  FILLS IN THE SHT, ETA, SHF AND FRE, POINT BY POINT
C
         DO 70 IPOIN=1,NPOIN2
C
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
C
           IF (((W(IPOIN,IPLAN).GT.0.D0).AND.(IFF.NE.1)).OR.
     &             (IFF.EQ.NF)) THEN
            FRE(IPOIN,IPLAN) = IFF-1
            SHF(IPOIN,IPLAN) = 1.D0
           ELSE
            FRE(IPOIN,IPLAN) = IFF
            SHF(IPOIN,IPLAN) = 0.D0
           ENDIF

70       CONTINUE
C
C-----------------------------------------------------------------------
C
10      CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C