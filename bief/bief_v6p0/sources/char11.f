C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       TRACES BACK OR FORTH THE CHARACTERISTIC CURVES
!>                FOR P1 QUADRILATERALS
!>                IN TIME INTERVAL DT
!>                USING A FINITE ELEMENT DISCRETISATION.
!>  @code
!>  DISCRETISATION :
!>
!>     THE DOMAIN IS APPROXIMATED BY A FINITE ELEMENT DISCRETISATION.
!>     A LOCAL APPROXIMATION IS DEFINED FOR THE VELOCITY :
!>     THE VALUE AT ANY POINT OF AN ELEMENT ONLY DEPENDS ON THE VALUES
!>     AT THE NODES OF THIS ELEMENT.
!>
!>
!>  RESTRICTIONS AND ASSUMPTIONS:
!>
!>     THE ADVECTION FIELD U IS ASSUMED INDEPENDENT OF TIME.
!>     THE DERIVATIVE IS ASSUMED PUNCTUAL HENCE NONDISPERSIVE.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  NSP, DX AND DY NOW USELESS

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DT, DX, DY, ELT, IFABOR, IKLE, NELEM, NELMAX, NPLOT, NPOIN, NRK, NSP, SENS, SHP, SURDET, TEST, U, V, X, XPLOT, Y, YPLOT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A1, DENOM, DX1, DXP, DXX, DY1, DYP, DYY, EPSILO, I1, I2, I3, IEL, IFA, IPLOT, ISO, ISP, ISUI, ISUI2, NSPP, PAS, XP, YP
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CHAR11
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CARACT(), CARAFR(), DERIVE(), DERLAG()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 22/10/2008
!> </td><td> J-M JANIN (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 28/08/2008
!> </td><td> JMH
!> </td><td> INVERSION OF LOOPS ON NSP AND NPLOT
!>          (THIS WAS MEANT FOR VECTOR MACHINES)
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
!>          <tr><td>DX,DY
!></td><td>---</td><td>STOCKAGE DES SOUS-PAS .
!>    </td></tr>
!>          <tr><td>ELT
!></td><td><-></td><td>NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
!>                  CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>--></td><td>NUMEROS DES ELEMENTS AYANT UNE FACE COMMUNE
!>                  AVEC L'ELEMENT .  SI IFABOR
!>                  ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>TRANSITION ENTRE LES NUMEROTATIONS LOCALE
!>                  ET GLOBALE.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NPLOT
!></td><td>--></td><td>NOMBRE DE DERIVANTS.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
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
!>          <tr><td>SHP
!></td><td><-></td><td>COORDONNEES BARYCENTRIQUES 2D AU PIED DES
!>                  COURBES CARACTERISTIQUES.
!>    </td></tr>
!>          <tr><td>SURDET
!></td><td>--></td><td>VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
!>    </td></tr>
!>          <tr><td>TEST
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTE DE LA VITESSE DU CONVECTEUR
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>XPLOT,YPLOT
!></td><td><-></td><td>POSITIONS SUCCESSIVES DES DERIVANTS.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CHAR11
     & ( U , V , DT , NRK , X , Y , IKLE , IFABOR ,
     &   XPLOT , YPLOT , DX , DY , SHP , ELT , NSP ,
     &   NPLOT , NPOIN , NELEM , NELMAX , SURDET , SENS , TEST )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DT             |-->| PAS DE TEMPS.
C| DX,DY          |---| STOCKAGE DES SOUS-PAS .
C| ELT            |<->| NUMEROS DES ELEMENTS 2D AU PIED DES COURBES
C|                |   | CARACTERISTIQUES.
C| IFABOR         |-->| NUMEROS DES ELEMENTS AYANT UNE FACE COMMUNE
C|                |   | AVEC L'ELEMENT .  SI IFABOR
C|                |   | ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE
C| IKLE           |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
C|                |   | ET GLOBALE.
C| NELEM          |-->| NOMBRE D'ELEMENTS.
C| NELMAX         |-->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
C| NPLOT          |-->| NOMBRE DE DERIVANTS.
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| NRK            |-->| NOMBRE DE SOUS-PAS DE RUNGE-KUTTA.
C| NSP            |---| NOMBRE DE SOUS-PAS DE RUNGE KUTTA.
C| SENS           |-->| DESCENTE OU REMONTEE DES CARACTERISTIQUES.
C| SHP            |<->| COORDONNEES BARYCENTRIQUES 2D AU PIED DES
C|                |   | COURBES CARACTERISTIQUES.
C| SURDET         |-->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
C| TEST           |---| 
C| U,V            |-->| COMPOSANTE DE LA VITESSE DU CONVECTEUR
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE.
C| XPLOT,YPLOT    |<->| POSITIONS SUCCESSIVES DES DERIVANTS.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CHAR11 => CHAR11
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN)    :: SENS
      INTEGER         , INTENT(IN)    :: NPOIN,NELEM,NELMAX,NPLOT,NRK
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,3),IFABOR(NELMAX,3)
      INTEGER         , INTENT(INOUT) :: ELT(NPLOT),NSP(NPLOT)
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),SURDET(NELEM)
      DOUBLE PRECISION, INTENT(INOUT) :: XPLOT(NPLOT),YPLOT(NPLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(3,NPLOT)
      DOUBLE PRECISION, INTENT(IN)    :: DT
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPLOT),DY(NPLOT),TEST(NPLOT)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPLOT,ISP,I1,I2,I3,IEL,ISO,IFA,ISUI(3),ISUI2(3),NSPP
C
      DOUBLE PRECISION PAS,EPSILO,A1,DX1,DY1,DXP,DYP,XP,YP,DENOM,DXX,DYY
C
      DATA ISUI   / 2 , 3 , 1 /
      DATA ISUI2  / 3 , 1 , 2 /
      DATA EPSILO / -1.D-6 /
C
      INTRINSIC INT,MAX,MIN,SQRT
C
C-----------------------------------------------------------------------
C  REPEATS FOR ALL THE POINTS AND R-K STEPS
C-----------------------------------------------------------------------
C
      DO IPLOT=1,NPLOT
C
      IEL = ELT(IPLOT)
      I1 = IKLE(IEL,1)
      I2 = IKLE(IEL,2)
      I3 = IKLE(IEL,3)
      DXP = U(I1)*SHP(1,IPLOT)+U(I2)*SHP(2,IPLOT)+U(I3)*SHP(3,IPLOT)
      DYP = V(I1)*SHP(1,IPLOT)+V(I2)*SHP(2,IPLOT)+V(I3)*SHP(3,IPLOT)
      NSPP=MAX(1,INT(NRK*DT*SQRT((DXP**2+DYP**2)*SURDET(IEL))))
      PAS = SENS * DT / NSPP
C
      DO ISP=1,NSPP
C
C-----------------------------------------------------------------------
C        LOCATES THE ARRIVAL POINT OF THE CHARACTERISTIC CURVE
C-----------------------------------------------------------------------
C
            IEL = ELT(IPLOT)
            I1 = IKLE(IEL,1)
            I2 = IKLE(IEL,2)
            I3 = IKLE(IEL,3)
C
            DXX = ( U(I1)*SHP(1,IPLOT)
     &            + U(I2)*SHP(2,IPLOT)
     &            + U(I3)*SHP(3,IPLOT) ) * PAS
            DYY = ( V(I1)*SHP(1,IPLOT)
     &            + V(I2)*SHP(2,IPLOT)
     &            + V(I3)*SHP(3,IPLOT) ) * PAS
C
            XP = XPLOT(IPLOT) + DXX
            YP = YPLOT(IPLOT) + DYY
C
            SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                     -(Y(I3)-Y(I2))*(XP-X(I2))) * SURDET(IEL)
            SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                     -(Y(I1)-Y(I3))*(XP-X(I3))) * SURDET(IEL)
            SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                     -(Y(I2)-Y(I1))*(XP-X(I1))) * SURDET(IEL)
C
            XPLOT(IPLOT) = XP
            YPLOT(IPLOT) = YP
C
C-----------------------------------------------------------------------
C  SPECIAL TREATMENT FOR THE CHARACTERISTICS LEAVING
C  THE START ELEMENT
C-----------------------------------------------------------------------
C
50       CONTINUE
C
            ISO = 0
            IF (SHP(1,IPLOT).LT.EPSILO) ISO = 1
            IF (SHP(2,IPLOT).LT.EPSILO) ISO = ISO + 2
            IF (SHP(3,IPLOT).LT.EPSILO) ISO = ISO + 4
C
            IF (ISO.NE.0) THEN
C
C-----------------------------------------------------------------------
C  AT THIS POINT, KNOWS THE CURVE LEFT THE ELEMENT
C-----------------------------------------------------------------------
C
               IEL = ELT(IPLOT)
               XP = XPLOT(IPLOT)
               YP = YPLOT(IPLOT)
C
C              THE 3 LINES FORMING THE TRIANGLE CUT THE PLANE INTO 7
C              ZONES, NUMBERED FROM 0 (INSIDE THE TRIANGLE) TO 6
C              ISO IS THE NUMBER. FOR ISO =1,2,4, THERE IS NO AMBIGUITY
C              AS TO THE EDGE CROSSED. FOR ISO = 3, IT CAN BE EDGE 2
C              OR 3, FOR ISO = 5 IT CAN BE EDGE 1 OR 2, FOR ISO = 6 IT
C              CAN BE EDGE 1 OR 3.
C              FOR CASES 3, 5 AND 6, AN INNER PRODUCT SHOWS IF THE DIRECTION
C              OF THE DISPLACEMENT (DX,DY) IS ON THE RIGHT OR ON THE LEFT
C              OF THE INTERSECTION BETWEEN THE TWO EDGES, SO IT GIVES
C              THE REAL EDGE THAT HAS BEEN CROSSED
C
               IF     (ISO.EQ.1) THEN
                  IFA = 2
               ELSEIF (ISO.EQ.2) THEN
                  IFA = 3
               ELSEIF (ISO.EQ.4) THEN
                  IFA = 1
               ELSEIF (ISO.EQ.3) THEN
                  IFA = 2
                  IF (DXX*(Y(IKLE(IEL,3))-YP).LT.
     &                DYY*(X(IKLE(IEL,3))-XP)) IFA = 3
               ELSEIF (ISO.EQ.6) THEN
                  IFA = 3
                  IF (DXX*(Y(IKLE(IEL,1))-YP).LT.
     &                DYY*(X(IKLE(IEL,1))-XP)) IFA = 1
               ELSE
                  IFA = 1
                  IF (DXX*(Y(IKLE(IEL,2))-YP).LT.
     &                DYY*(X(IKLE(IEL,2))-XP)) IFA = 2
               ENDIF
C
               IEL = IFABOR(IEL,IFA)
C
               IF (IEL.GT.0) THEN
C
C-----------------------------------------------------------------------
C  AT THIS POINT, KNOWS THE EXIT SIDE IS INTERIOR (WRT DOMAIN)
C  GOES TO ADJACENT ELEMENT
C-----------------------------------------------------------------------
C
                  I1 = IKLE(IEL,1)
                  I2 = IKLE(IEL,2)
                  I3 = IKLE(IEL,3)
C
                  ELT(IPLOT) = IEL
                  SHP(1,IPLOT) = ((X(I3)-X(I2))*(YP-Y(I2))
     &                           -(Y(I3)-Y(I2))*(XP-X(I2)))*SURDET(IEL)
                  SHP(2,IPLOT) = ((X(I1)-X(I3))*(YP-Y(I3))
     &                           -(Y(I1)-Y(I3))*(XP-X(I3)))*SURDET(IEL)
                  SHP(3,IPLOT) = ((X(I2)-X(I1))*(YP-Y(I1))
     &                           -(Y(I2)-Y(I1))*(XP-X(I1)))*SURDET(IEL)
C
                  GOTO 50
C
               ENDIF
C
               DXP = DXX
               DYP = DYY
               I1  = IKLE(ELT(IPLOT),IFA)
               I2  = IKLE(ELT(IPLOT),ISUI(IFA))
               DX1 = X(I2) - X(I1)
               DY1 = Y(I2) - Y(I1)
C
               IF(IEL.EQ.-1) THEN
C
C-----------------------------------------------------------------------
C  AT THIS POINT, KNOWS THE EXIT SIDE IS A SOLID BOUNDARY
C  PROJECTS THE BALANCE ONTO THE BOUNDARY AND MOVES ON
C-----------------------------------------------------------------------
C
                  A1 = (DXP*DX1 + DYP*DY1) / (DX1**2 + DY1**2)
                  DXX = A1 * DX1
                  DYY = A1 * DY1
C
                  A1 = ((XP-X(I1))*DX1+(YP-Y(I1))*DY1)/(DX1**2+DY1**2)
                  SHP(          IFA  ,IPLOT) = 1.D0 - A1
                  SHP(     ISUI(IFA) ,IPLOT) = A1
                  SHP(    ISUI2(IFA) ,IPLOT) = 0.D0
                  XPLOT(IPLOT) = X(I1) + A1 * DX1
                  YPLOT(IPLOT) = Y(I1) + A1 * DY1
C
                  GOTO 50
C
               ENDIF
C
C-----------------------------------------------------------------------
C  AT THIS POINT, KNOWS THE EXIT SIDE IS A LIQUID BOUNDARY
C  STOPS TRACING BACK THE CHARACTERISTICS CURVE (ELT SIGN)
C
C     OR
C
C  AT THIS POINT, KNOWS THE EXIT SIDE IS AN INTERFACE BETWEEN SUB-DOMAINS
C  THE INTERFACE POINT WILL BE TREATED IN THE NEIGHBOURING SUB-DOMAIN;
C  MERELY SETS TEST TO 0
C-----------------------------------------------------------------------
C
               DENOM = DXP*DY1-DYP*DX1
               IF(DENOM.NE.0.D0) THEN
                 A1  = (DXP*(YP-Y(I1))-DYP*(XP-X(I1))) / DENOM
               ELSE
                 A1  = 0.D0
               ENDIF
               A1 = MAX(MIN(A1,1.D0),0.D0)
               SHP(          IFA  ,IPLOT) = 1.D0 - A1
               SHP(     ISUI(IFA) ,IPLOT) = A1
               SHP(    ISUI2(IFA) ,IPLOT) = 0.D0
               XPLOT(IPLOT) = X(I1) + A1 * DX1
               YPLOT(IPLOT) = Y(I1) + A1 * DY1
               ELT(IPLOT) = - SENS * ELT(IPLOT)
C
C              THIS CAN ONLY HAPPEN IN PARALLEL
               IF(IEL.EQ.-2) TEST(IPLOT) = 0.D0
C
               EXIT
C
            ENDIF
C
      ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C