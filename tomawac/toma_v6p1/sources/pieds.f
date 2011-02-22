C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       TRACES IN TIME THE CHARACTERISTICS CURVES FOR TELEMAC-3D
!>                PRISMS, WITHIN THE TIME INTERVAL DT, USING A FINITE
!>                ELEMENTS DISCRETISATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  DISCRETISATION : THE DOMAIN IS APPROXIMATED USING A FINITE
!>         ELEMENT DISCRETISATION. A LOCAL APPROXIMATION IS USED FOR THE
!>         VELOCITY : THE VALUE IN ONE POINT OF AN ELEMENT ONLY DEPENDS
!>         ON THE VALUES AT THE NODES OF THIS ELEMENT.

!>  @note  RESTRICTIONS AND ASSUMPTIONS : THE ADVECTION FIELD U IS
!>         ASSUMED NOT TO VARY WITH TIME.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DT, DX, DY, DZ, ELT, ETA, ETAS, IFABOR, IFF, IKLE2, ISO, NELEM2, NPLAN, NPLOT, NPOIN2, NRK, NSP, SENS, SHP1, SHP2, SHP3, SHZ, SURDET, TETA, U, V, W, X, XPLOT, Y, YPLOT, ZPLOT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A1, DX1, DXP, DY1, DYP, DZP, EPM1, EPSI, EPSILO, I1, I2, I3, IEL, IET, IFA, IPLOT, ISOH, ISOV, ISP, ISUI, NSPMAX, PAS, XP, YP, ZP
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
!> </td><td> 01/02/95
!> </td><td> F MARCOS (LNH) 30 87 72 66
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
!>          <tr><td>ETAS
!></td><td><-></td><td>TABLEAU DE TRAVAIL DONNANT LE NUMERO DE
!>                  L'ETAGE SUPERIEUR
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>NUMEROS 2D DES ELEMENTS AYANT UNE FACE COMMUNE
!>                  AVEC L'ELEMENT .  SI IFABOR
!>                  ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE
!>    </td></tr>
!>          <tr><td>IFF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>TRANSITION ENTRE LES NUMEROTATIONS LOCALE
!>                  ET GLOBALE DU MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>ISO
!></td><td><-></td><td>INDIQUE PAR BIT LA FACE DE SORTIE DE L'ELEMEN
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE 2D.
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS
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
!>          <tr><td>SHP1
!></td><td>---</td><td>COORDONNEES BARYCENTRIQUES 2D AU PIED DES
!>                  COURBES CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>SHP2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHP3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SHZ
!></td><td><-></td><td>COORDONNEES BARYCENTRIQUES SUIVANT Z DES
!>                  NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
!>    </td></tr>
!>          <tr><td>SURDET
!></td><td>--></td><td>VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTE DE LA VITESSE DU CONVECTEUR
!>    </td></tr>
!>          <tr><td>X,Y,TETA
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>XPLOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YPLOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZPLOT
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PIEDS
     &  (U , V , W , DT , NRK , X , Y , TETA , IKLE2 , IFABOR , ETAS ,
     &   XPLOT , YPLOT , ZPLOT , DX , DY , DZ , SHP1 , SHP2 , SHP3 ,
     &   SHZ , ELT , ETA , NSP , NPLOT , NPOIN2 , NELEM2 , NPLAN ,
     &   IFF , SURDET , SENS , ISO )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DT             |-->| PAS DE TEMPS.
C| DX,DY,DZ       |---| STOCKAGE DES SOUS-PAS .
C| ELT            |<->| NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
C|                |   | NOEUD.
C| ETA            |<->| NUMEROS DES ETAGES CHOISIS POUR CHAQUE NOEUD.
C| ETAS           |<->| TABLEAU DE TRAVAIL DONNANT LE NUMERO DE
C|                |   | L'ETAGE SUPERIEUR
C| IFABOR         |-->| NUMEROS 2D DES ELEMENTS AYANT UNE FACE COMMUNE
C|                |   | AVEC L'ELEMENT .  SI IFABOR
C|                |   | ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE
C| IFF            |---| 
C| IKLE2          |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
C|                |   | ET GLOBALE DU MAILLAGE 2D.
C| ISO            |<->| INDIQUE PAR BIT LA FACE DE SORTIE DE L'ELEMEN
C| NELEM2         |-->| NOMBRE D'ELEMENTS DU MAILLAGE 2D.
C| NPLAN          |-->| NOMBRE DE DIRECTIONS
C| NPLOT          |-->| NOMBRE DE DERIVANTS.
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D.
C| NRK            |-->| NOMBRE DE SOUS-PAS DE RUNGE-KUTTA.
C| NSP            |---| NOMBRE DE SOUS-PAS DE RUNGE KUTTA.
C| SENS           |-->| DESCENTE OU REMONTEE DES CARACTERISTIQUES.
C| SHP1           |---| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
C|                |   | COURBES CARACTERISTIQUES.
C| SHP2           |---| 
C| SHP3           |---| 
C| SHZ            |<->| COORDONNEES BARYCENTRIQUES SUIVANT Z DES
C|                |   | NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
C| SURDET         |-->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
C| U,V,W          |-->| COMPOSANTE DE LA VITESSE DU CONVECTEUR
C| X,Y,TETA       |-->| COORDONNEES DES POINTS DU MAILLAGE.
C| XPLOT          |---| 
C| YPLOT          |---| 
C| ZPLOT          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NPOIN2,NELEM2,NPLAN,NPLOT,NSPMAX,NRK,SENS,IFF
C
      DOUBLE PRECISION U(NPOIN2,NPLAN),V(NPOIN2,NPLAN)
      DOUBLE PRECISION W(NPOIN2,NPLAN)
      DOUBLE PRECISION XPLOT(NPLOT),YPLOT(NPLOT),ZPLOT(NPLOT)
      DOUBLE PRECISION SURDET(NELEM2),SHZ(NPLOT)
      DOUBLE PRECISION SHP1(NPLOT),SHP2(NPLOT),SHP3(NPLOT)
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2),TETA(NPLAN+1)
      DOUBLE PRECISION DX(NPLOT),DY(NPLOT),DZ(NPLOT)
      DOUBLE PRECISION PAS,DT,A1,DX1,DY1,DXP,DYP,DZP,XP,YP,ZP
      DOUBLE PRECISION EPSILO, EPSI, EPM1
C
      INTEGER IKLE2(NELEM2,3),IFABOR(NELEM2,5),ETAS(NPLAN)
      INTEGER ELT(NPLOT),ETA(NPLOT),NSP(NPLOT),ISO(NPLOT)
      INTEGER IPLOT,ISP,I1,I2,I3,IEL,IET,ISOH,ISOV,IFA,ISUI(3)
C
      INTRINSIC ABS , INT , MAX , SQRT
C
      DATA ISUI   / 2 , 3 , 1 /
      DATA EPSILO / -1.D-6 /
      DATA EPSI   / 1.D-12 /
C
C-----------------------------------------------------------------------
C    COMPUTES THE NUMBER OF SUB-ITERATIONS
C    (THE SAME AT ALL THE NODES FOR A GIVEN FREQUENCY)
C-----------------------------------------------------------------------
C
      NSPMAX = 1
      EPM1=1.D0-EPSI
C
      DO 10 IPLOT = 1 , NPLOT
C
         NSP(IPLOT) = 0
         IEL = ELT(IPLOT)
C
         IF (IEL.GT.0) THEN
C
            IET = ETA(IPLOT)
C
            I1 = IKLE2(IEL,1)
            I2 = IKLE2(IEL,2)
            I3 = IKLE2(IEL,3)
C
         DXP = U(I1,IET  )*SHP1(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + U(I2,IET  )*SHP2(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + U(I3,IET  )*SHP3(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + U(I1,ETAS(IET))*SHP1(IPLOT)*SHZ(IPLOT)
     &       + U(I2,ETAS(IET))*SHP2(IPLOT)*SHZ(IPLOT)
     &       + U(I3,ETAS(IET))*SHP3(IPLOT)*SHZ(IPLOT)
C
         DYP = V(I1,IET  )*SHP1(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + V(I2,IET  )*SHP2(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + V(I3,IET  )*SHP3(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + V(I1,ETAS(IET))*SHP1(IPLOT)*SHZ(IPLOT)
     &       + V(I2,ETAS(IET))*SHP2(IPLOT)*SHZ(IPLOT)
     &       + V(I3,ETAS(IET))*SHP3(IPLOT)*SHZ(IPLOT)
C
         DZP = W(I1,IET  )*SHP1(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + W(I2,IET  )*SHP2(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + W(I3,IET  )*SHP3(IPLOT)*(1.D0-SHZ(IPLOT))
     &       + W(I1,ETAS(IET))*SHP1(IPLOT)*SHZ(IPLOT)
     &       + W(I2,ETAS(IET))*SHP2(IPLOT)*SHZ(IPLOT)
     &       + W(I3,ETAS(IET))*SHP3(IPLOT)*SHZ(IPLOT)
C
         NSP(IPLOT)= MAX(INT(NRK*DT*ABS(DZP/(TETA(IET)-TETA(IET+1)))),
     &         INT(NRK*DT*SQRT((DXP*DXP+DYP*DYP)*SURDET(IEL))) )
C
            NSP(IPLOT) = MAX (1,NSP(IPLOT))
C
            NSPMAX = MAX ( NSPMAX , NSP(IPLOT) )
C
         ENDIF
C
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
C
C-----------------------------------------------------------------------
C  LOOP ON NUMBER OF SUB-ITERATIONS
C-----------------------------------------------------------------------
C
      DO 20 ISP = 1 , NSPMAX
C
C-----------------------------------------------------------------------
C  LOCATES THE END POINT OF ALL THE CHARACTERISTICS
C-----------------------------------------------------------------------
C
         DO 30 IPLOT = 1 , NPLOT
C
            ISO(IPLOT) = 0
            IF (ISP.LE.NSP(IPLOT)) THEN
C
               IEL = ELT(IPLOT)
               IET = ETA(IPLOT)
C
               I1 = IKLE2(IEL,1)
               I2 = IKLE2(IEL,2)
               I3 = IKLE2(IEL,3)
               PAS = SENS * DT / NSP(IPLOT)
C
               DX(IPLOT) =
     & ( U(I1,IET  )*SHP1(IPLOT)*(1.D0-SHZ(IPLOT))
     & + U(I2,IET  )*SHP2(IPLOT)*(1.D0-SHZ(IPLOT))
     & + U(I3,IET  )*SHP3(IPLOT)*(1.D0-SHZ(IPLOT))
     & + U(I1,ETAS(IET))*SHP1(IPLOT)*SHZ(IPLOT)
     & + U(I2,ETAS(IET))*SHP2(IPLOT)*SHZ(IPLOT)
     & + U(I3,ETAS(IET))*SHP3(IPLOT)*SHZ(IPLOT) ) * PAS
C
               DY(IPLOT) =
     & ( V(I1,IET  )*SHP1(IPLOT)*(1.D0-SHZ(IPLOT))
     & + V(I2,IET  )*SHP2(IPLOT)*(1.D0-SHZ(IPLOT))
     & + V(I3,IET  )*SHP3(IPLOT)*(1.D0-SHZ(IPLOT))
     & + V(I1,ETAS(IET))*SHP1(IPLOT)*SHZ(IPLOT)
     & + V(I2,ETAS(IET))*SHP2(IPLOT)*SHZ(IPLOT)
     & + V(I3,ETAS(IET))*SHP3(IPLOT)*SHZ(IPLOT) ) * PAS
C
               DZ(IPLOT) =
     & ( W(I1,IET  )*SHP1(IPLOT)*(1.D0-SHZ(IPLOT))
     & + W(I2,IET  )*SHP2(IPLOT)*(1.D0-SHZ(IPLOT))
     & + W(I3,IET  )*SHP3(IPLOT)*(1.D0-SHZ(IPLOT))
     & + W(I1,ETAS(IET))*SHP1(IPLOT)*SHZ(IPLOT)
     & + W(I2,ETAS(IET))*SHP2(IPLOT)*SHZ(IPLOT)
     & + W(I3,ETAS(IET))*SHP3(IPLOT)*SHZ(IPLOT) ) * PAS
C
               XP = XPLOT(IPLOT) + DX(IPLOT)
               YP = YPLOT(IPLOT) + DY(IPLOT)
               ZP = ZPLOT(IPLOT) + DZ(IPLOT)
C
               SHP1(IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                        -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL)
               SHP2(IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                        -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL)
               SHP3(IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                        -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL)
               SHZ(IPLOT) = (ZP-TETA(IET)) / (TETA(IET+1)-TETA(IET))
C               IF (ABS(SHZ(IPLOT)).GT.2.5D0 ) THEN
C                  WRITE(LU,*)'SHZ***',IPLOT,IET,SHZ(IPLOT)
C                  WRITE(LU,*)TETA(IET),TETA(IET+1),ZP
C                  WRITE(LU,*)DZ(IPLOT),ZPLOT(IPLOT)
C                  STOP
C              ENDIF
C
               XPLOT(IPLOT) = XP
               YPLOT(IPLOT) = YP
               ZPLOT(IPLOT) = ZP
C
               IF (SHP1(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),2)
               IF (SHP2(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),3)
               IF (SHP3(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),4)
C
               IF  (SHZ(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),0)
               IF  (SHZ(IPLOT).GT.1.D0-EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),1)
C
            ENDIF
C
30       CONTINUE
C
C-----------------------------------------------------------------------
C  TREATS DIFFERENTLY THE CHARACTERISTICS ISSUED FROM
C  THE START ELEMENT
C-----------------------------------------------------------------------
C
         DO 40 IPLOT = 1 , NPLOT
C
50          CONTINUE
C
            IF (ISO(IPLOT).NE.0) THEN
C
C-----------------------------------------------------------------------
C  HERE: LEFT THE ELEMENT
C-----------------------------------------------------------------------
C
               ISOH = IAND(ISO(IPLOT),28)
               ISOV = IAND(ISO(IPLOT), 3)
               IEL = ELT(IPLOT)
               IET = ETA(IPLOT)
               XP = XPLOT(IPLOT)
               YP = YPLOT(IPLOT)
               ZP = ZPLOT(IPLOT)
C
               IF (ISOH.NE.0) THEN
C
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
C
                  IF (ISOV.GT.0) THEN
                     A1 = (ZP-TETA(IET+ISOV-1)) / DZ(IPLOT)
                     I1 = IKLE2(IEL,IFA)
                     I2 = IKLE2(IEL,ISUI(IFA))
                     IF ((X(I2)-X(I1))*(YP-A1*DY(IPLOT)-Y(I1)).GT.
     &                 (Y(I2)-Y(I1))*(XP-A1*DX(IPLOT)-X(I1))) IFA=ISOV+3
                  ENDIF
C
               ELSE
C
                  IFA = ISOV + 3
C
               ENDIF
C
               IEL = IFABOR(IEL,IFA)
C
               IF (IFA.LE.3) THEN

C
C-----------------------------------------------------------------------
C  HERE: THE EXIT FACE OF THE PRISM IS A RECTANGULAR FACE
C  =================================================================
C-----------------------------------------------------------------------
C
                  IF (IEL.GT.0) THEN
C
C-----------------------------------------------------------------------
C  HERE: THE EXIT FACE IS AN INTERIOR FACE
C  MOVES TO THE ADJACENT ELEMENT
C-----------------------------------------------------------------------
C
                     I1 = IKLE2(IEL,1)
                     I2 = IKLE2(IEL,2)
                     I3 = IKLE2(IEL,3)
C
                     ELT(IPLOT) = IEL
                     SHP1(IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                           -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL)
                     SHP2(IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                           -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL)
                     SHP3(IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                           -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL)
C
                     ISO(IPLOT) = ISOV
C
         IF (SHP1(IPLOT).LT.EPSILO) ISO(IPLOT)=IBSET(ISO(IPLOT),2)
         IF (SHP2(IPLOT).LT.EPSILO) ISO(IPLOT)=IBSET(ISO(IPLOT),3)
         IF (SHP3(IPLOT).LT.EPSILO) ISO(IPLOT)=IBSET(ISO(IPLOT),4)
C
                     GOTO 50
C
                  ENDIF
                  DXP = DX(IPLOT)
                  DYP = DY(IPLOT)
                  I1  = IKLE2(ELT(IPLOT),IFA)
                  I2  = IKLE2(ELT(IPLOT),ISUI(IFA))
                  DX1 = X(I2) - X(I1)
                  DY1 = Y(I2) - Y(I1)
C
                  IF (IEL.EQ.-1) THEN
C
C-----------------------------------------------------------------------
C  HERE: THE EXIT FACE IS A SOLID BOUNDARY
C  SETS SHP TO 0, END OF TRACING BACK
C-----------------------------------------------------------------------
C
                     SHP1(IPLOT) = 0.D0
                     SHP2(IPLOT) = 0.D0
                     SHP3(IPLOT) = 0.D0
                     ELT(IPLOT) = - SENS * ELT(IPLOT)
                     NSP(IPLOT) = ISP
                     GOTO 40
C
                  ENDIF
C
C-----------------------------------------------------------------------
C  HERE: THE EXIT FACE IS A LIQUID BOUNDARY
C  ENDS TRACING BACK (SIGN OF ELT)
C-----------------------------------------------------------------------
C
                  A1 = (DXP*(YP-Y(I1))-DYP*(XP-X(I1)))/(DXP*DY1-DYP*DX1)
CFBG                  IF (A1.GT.1.D0) A1 = 1.D0
CFBG                  IF (A1.LT.0.D0) A1 = 0.D0
                  IF (A1.GT.EPM1) A1 = 1.D0
                  IF (A1.LT.EPSI) A1 = 0.D0
CFGB
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
C
               ELSE
C
C-----------------------------------------------------------------------
C  HERE: THE EXIT FACE OF THE PRISM IS A TRIANGULAR FACE
C  ===============================================================
C-----------------------------------------------------------------------
C
                  IFA = IFA - 4
C
                  IF (IEL.EQ.1) THEN
C
C-----------------------------------------------------------------------
C  HERE: THE EXIT FACE IS AN INTERIOR FACE
C  MOVES TO THE ADJACENT ELEMENT
C-----------------------------------------------------------------------
C
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
C
                     ISO(IPLOT) = ISOH
C
               IF (SHZ(IPLOT).LT.EPSILO)
     &             ISO(IPLOT)=IBSET(ISO(IPLOT),0)
               IF (SHZ(IPLOT).GT.1.D0-EPSILO)
     &             ISO(IPLOT)=IBSET(ISO(IPLOT),1)
C
                     GOTO 50
C
                  ELSE
C
C         WRITE(LU,*)'YA UN PROBLEME',IEL,IPLOT
C         WRITE(LU,*)'SHP',SHP1(IPLOT),SHP2(IPLOT),SHP3(IPLOT)
C         WRITE(LU,*)'SHZ',SHZ(IPLOT)
C         WRITE(LU,*)'DXYZ',DX(IPLOT),DY(IPLOT),DZ(IPLOT)
C         WRITE(LU,*)'XYZ',XPLOT(IPLOT),YPLOT(IPLOT),ZPLOT(IPLOT)

                  STOP
                  ENDIF
C
               ENDIF
C
            ENDIF
C
40       CONTINUE
C
20    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C