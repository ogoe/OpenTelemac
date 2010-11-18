C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       TRACES IN TIME THE CHARACTERISTICS CURVES FOR TOMAWAC
!>               "HYPER PRISMS", WITHIN THE TIME INTERVAL DT, USING AN
!>                HYBRID DISCRETISATION FINITE ELEMENTS+FINITE DIFF (2D).

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
!>    </th><td> DF, DT, DW, DX, DY, ELT, ETA, ETAS, FPLOT, FRE, FREQ, IFABOR, IKLE2, ISO, NELEM2, NF, NPLAN, NPLOT, NPOIN2, NRK, NSP, SENS, SHF, SHP1, SHP2, SHP3, SHT, SURDET, T, TETA, TPLOT, U, V, W, X, XPLOT, Y, YPLOT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A1, A2, DFP, DTP, DX1, DXP, DY1, DYP, EPSILO, FP, I1, I2, I3, IEL, IET, IFA, IFR, IPLOT, ISOF, ISOH, ISOT, ISOV, ISP, ISUI, NSPMAX, PAS, TP, XP, YP
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
!>          <tr><td>DX,DY,DW,DF
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
!>          <tr><td>FPLOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FRE
!></td><td><-></td><td>NUMEROS DES FREQ. CHOISIES POUR CHAQUE NOEUD.
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>NUMEROS 2D DES ELEMENTS AYANT UNE FACE COMMUNE
!>                  AVEC L'ELEMENT .  SI IFABOR
!>                  ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE
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
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES
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
!>          <tr><td>SHF
!></td><td><-></td><td>COORDONNEES BARYCENTRIQUES SUIVANT F DES
!>                  NOEUDS DANS LEURS FREQUENCES "FRE" ASSOCIEES.
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
!>          <tr><td>SHT
!></td><td><-></td><td>COORDONNEES BARYCENTRIQUES SUIVANT TETA DES
!>                  NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
!>    </td></tr>
!>          <tr><td>SURDET
!></td><td>--></td><td>VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
!>    </td></tr>
!>          <tr><td>TPLOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V,T,W
!></td><td>--></td><td>COMPOSANTE DE LA VITESSE DU CONVECTEUR
!>    </td></tr>
!>          <tr><td>X,Y,TETA,FREQ
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>XPLOT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YPLOT
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PIED4D
     &  (U , V , T , W , DT , NRK , X , Y , TETA , FREQ , IKLE2 ,
     &   IFABOR , ETAS , XPLOT , YPLOT , TPLOT , FPLOT , DX , DY , DW ,
     &   DF , SHP1 , SHP2 , SHP3 , SHT , SHF , ELT , ETA , FRE , NSP ,
     &   NPLOT , NPOIN2 , NELEM2 , NPLAN , NF , SURDET , SENS ,
     &   ISO )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DT             |-->| PAS DE TEMPS.
C| DX,DY,DW,DF    |---| STOCKAGE DES SOUS-PAS .
C| ELT            |<->| NUMEROS DES ELEMENTS 2D CHOISIS POUR CHAQUE
C|                |   | NOEUD.
C| ETA            |<->| NUMEROS DES ETAGES CHOISIS POUR CHAQUE NOEUD.
C| ETAS           |<->| TABLEAU DE TRAVAIL DONNANT LE NUMERO DE
C|                |   | L'ETAGE SUPERIEUR
C| FPLOT          |---| 
C| FRE            |<->| NUMEROS DES FREQ. CHOISIES POUR CHAQUE NOEUD.
C| IFABOR         |-->| NUMEROS 2D DES ELEMENTS AYANT UNE FACE COMMUNE
C|                |   | AVEC L'ELEMENT .  SI IFABOR
C|                |   | ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE
C| IKLE2          |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
C|                |   | ET GLOBALE DU MAILLAGE 2D.
C| ISO            |<->| INDIQUE PAR BIT LA FACE DE SORTIE DE L'ELEMEN
C| NELEM2         |-->| NOMBRE D'ELEMENTS DU MAILLAGE 2D.
C| NF             |-->| NOMBRE DE FREQUENCES
C| NPLAN          |-->| NOMBRE DE DIRECTIONS
C| NPLOT          |-->| NOMBRE DE DERIVANTS.
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D.
C| NRK            |-->| NOMBRE DE SOUS-PAS DE RUNGE-KUTTA.
C| NSP            |---| NOMBRE DE SOUS-PAS DE RUNGE KUTTA.
C| SENS           |-->| DESCENTE OU REMONTEE DES CARACTERISTIQUES.
C| SHF            |<->| COORDONNEES BARYCENTRIQUES SUIVANT F DES
C|                |   | NOEUDS DANS LEURS FREQUENCES "FRE" ASSOCIEES.
C| SHP1           |---| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
C|                |   | COURBES CARACTERISTIQUES.
C| SHP2           |---| 
C| SHP3           |---| 
C| SHT            |<->| COORDONNEES BARYCENTRIQUES SUIVANT TETA DES
C|                |   | NOEUDS DANS LEURS ETAGES "ETA" ASSOCIES.
C| SURDET         |-->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
C| TPLOT          |---| 
C| U,V,T,W        |-->| COMPOSANTE DE LA VITESSE DU CONVECTEUR
C| X,Y,TETA,FREQ  |-->| COORDONNEES DES POINTS DU MAILLAGE.
C| XPLOT          |---| 
C| YPLOT          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NPOIN2,NELEM2,NPLAN,NPLOT,NSPMAX,NRK,SENS,NF
C
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
C
      INTEGER IKLE2(NELEM2,3),IFABOR(NELEM2,7),ETAS(NPLAN)
      INTEGER ELT(NPLOT),ETA(NPLOT),FRE(NPLOT),NSP(NPLOT),ISO(NPLOT)
      INTEGER IPLOT,ISP,I1,I2,I3,IEL,IET,IFR,IFA,ISUI(3)
      INTEGER ISOH,ISOT,ISOF,ISOV
C
      INTRINSIC ABS , INT , MAX , SQRT
C
      DATA ISUI   / 2 , 3 , 1 /
      DATA EPSILO / -1.D-6 /
C
C-----------------------------------------------------------------------
C  COMPUTES THE MAXIMUM NUMBER OF SUB-ITERATIONS
C-----------------------------------------------------------------------
C
      NSPMAX = 1
C
      DO 10 IPLOT = 1 , NPLOT
C
         NSP(IPLOT) = 0
         IEL = ELT(IPLOT)
C
         IF (IEL.GT.0) THEN
C
            IET = ETA(IPLOT)
          IFR = FRE(IPLOT)
C
            I1 = IKLE2(IEL,1)
            I2 = IKLE2(IEL,2)
            I3 = IKLE2(IEL,3)
C
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
C
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
C
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
C
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
C
         NSP(IPLOT)= MAX( INT(NRK*DT*ABS(DTP/(TETA(IET)-TETA(IET+1)))),
     &                   INT(NRK*DT*ABS(DFP/(FREQ(IFR)-FREQ(IFR+1)))) )
         NSP(IPLOT)= MAX( NSP(IPLOT),
     &               INT(NRK*DT*SQRT((DXP*DXP+DYP*DYP)*SURDET(IEL))) )
C
          NSP(IPLOT) = MAX (1,NSP(IPLOT))
C
            NSPMAX = MAX ( NSPMAX , NSP(IPLOT) )
C
         ENDIF
C
10    CONTINUE
      IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'NOMBRE MAX DE SOUS PAS :',NSPMAX
      ELSE
         WRITE(LU,*) 'NUMBER OF SUB-ITERATIONS :',NSPMAX
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
               IFR = FRE(IPLOT)
C
               I1 = IKLE2(IEL,1)
               I2 = IKLE2(IEL,2)
               I3 = IKLE2(IEL,3)
               PAS = SENS * DT / NSP(IPLOT)
C
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
C
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
C
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
C
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
C
               XP = XPLOT(IPLOT) + DX(IPLOT)
               YP = YPLOT(IPLOT) + DY(IPLOT)
               TP = TPLOT(IPLOT) + DW(IPLOT)
               FP = FPLOT(IPLOT) + DF(IPLOT)
C
               SHP1(IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                        -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL)
               SHP2(IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                        -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL)
               SHP3(IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                        -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL)
               SHT(IPLOT) = (TP-TETA(IET)) / (TETA(IET+1)-TETA(IET))
               SHF(IPLOT) = (FP-FREQ(IFR)) / (FREQ(IFR+1)-FREQ(IFR))
C             IF (ABS(SHT(IPLOT)).GT.2.5D0 ) THEN
C         WRITE(LU,*) 'SHT***',IPLOT,IET,SHT(IPLOT)
C         WRITE(LU,*) TETA(IET),TETA(IET+1),ZP
C         WRITE(LU,*) DZ(IPLOT),ZPLOT(IPLOT)
C         STOP
C              ENDIF
C
               XPLOT(IPLOT) = XP
               YPLOT(IPLOT) = YP
               TPLOT(IPLOT) = TP
               FPLOT(IPLOT) = FP
C
               IF (SHP1(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),4)
               IF (SHP2(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),5)
               IF (SHP3(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),6)
C
               IF  (SHT(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),0)
               IF  (SHT(IPLOT).GT.1.D0-EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),1)
C
               IF  (SHF(IPLOT).LT.EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),2)
               IF  (SHF(IPLOT).GT.1.D0-EPSILO)
     &              ISO(IPLOT)=IBSET(ISO(IPLOT),3)
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
C
              IF (ISOH.NE.0) THEN
C
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
C
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
C
               ELSEIF (ISOT.GT.0) THEN
C
      	  IFA = ISOT + 3
C
                  IF (ISOF.GT.0) THEN
      	    A1=(FP-FREQ(IFR+ISOF-1))/DF(IPLOT)
      	    A2=(TP-TETA(IET+ISOT-1))/DW(IPLOT)
      	    IF (A1.LT.A2) IFA = ISOF + 5
      	  ENDIF
               ELSE
      	  IFA = ISOF + 5
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
         IF (SHP1(IPLOT).LT.EPSILO) ISO(IPLOT)=IBSET(ISO(IPLOT),4)
         IF (SHP2(IPLOT).LT.EPSILO) ISO(IPLOT)=IBSET(ISO(IPLOT),5)
         IF (SHP3(IPLOT).LT.EPSILO) ISO(IPLOT)=IBSET(ISO(IPLOT),6)
C
                     GOTO 50
C
                  ENDIF
C
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
C
               ELSEIF (IFA.LE.5) THEN
C
C-----------------------------------------------------------------------
C  HERE: THE EXIT FACE OF THE PRISM IS A TRIANGULAR FACE TETA
C  =====================================================================
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
C
                     ISO(IPLOT) = ISOH+ISOF*4
C
               IF (SHT(IPLOT).LT.EPSILO)
     &             ISO(IPLOT)=IBSET(ISO(IPLOT),0)
               IF (SHT(IPLOT).GT.1.D0-EPSILO)
     &             ISO(IPLOT)=IBSET(ISO(IPLOT),1)
C
                     GOTO 50
C
                  ELSE
C
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
C
               ELSE
C
C-----------------------------------------------------------------------
C  HERE: THE EXIT FACE OF THE PRISM IS A TRIANGULAR FACE FREQ
C  =====================================================================
C-----------------------------------------------------------------------
C
                  IFA = IFA - 6
C
                  IF ((IFA.EQ.1).AND.(IFR.EQ.NF-1)) IEL=-1
                  IF ((IFA.EQ.0).AND.(IFR.EQ.1)) IEL=-1
                  IF (IEL.EQ.1) THEN
C
C-----------------------------------------------------------------------
C  HERE: THE EXIT FACE IS AN INTERIOR FACE
C  MOVES TO THE ADJACENT ELEMENT
C-----------------------------------------------------------------------
C
                     FRE(IPLOT) = IFR + IFA + IFA - 1
                     SHF(IPLOT) = (FP-FREQ(FRE(IPLOT)))
     &                   / (FREQ(FRE(IPLOT)+1)-FREQ(FRE(IPLOT)))
C
                     ISO(IPLOT) = ISOH+ISOT
C
               IF (SHF(IPLOT).LT.EPSILO)
     &             ISO(IPLOT)=IBSET(ISO(IPLOT),2)
               IF (SHF(IPLOT).GT.1.D0-EPSILO)
     &             ISO(IPLOT)=IBSET(ISO(IPLOT),3)
C
                     GOTO 50
C
                  ELSE
C
C-----------------------------------------------------------------------
C  HERE: THE EXIT FACE IS THE MIN OR MAX FREQUENCY
C  PROJECTS THE RELICAT ON THE BOUNDAY AND CONTINUES
C-----------------------------------------------------------------------
C
                    FPLOT(IPLOT)=FREQ(IFR+IFA)
      	    DF(IPLOT)=0.D0
      	    SHF(IPLOT)=IFA
      	    ISO(IPLOT) = ISOH +ISOT
      	    IF(ISO(IPLOT).NE.0) GOTO 50
C
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