C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       - SETS THE BARYCENTRIC COORDINATES IN THE MESH,
!>                  AT THE START OF COMPUTATION FOR EACH FLOAT.
!><br>            - COMPUTES THE SUCCESSIVE POSITIONS OF THIS FLOAT
!>                 (SUBSEQUENT TIMESTEPS).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEBLAG, DT, DX, DY, ELTLAG, FINLAG, IELM, IFABOR, IKLE, LT, MASKEL, MASKPT, MSK, NBOR, NDP, NELBOR, NELEM, NELMAX, NLAG, NPOIN, NPTFR, NSP, NULONE, RESUX, RESUY, SHPLAG, SURDET, T8, U, V, X, XLAG, Y, YLAG
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, ILAG, IPOIN, JLAG, LTT, NRK, Z
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CHAR11(), GTSH11(), OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!> </td><td> 02/09/08
!> </td><td> J-M JANIN (LNH) 30 87 72 84
!> </td><td> CALLS GTSH11 INSTEAD OF GTSHP11
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DEBLAG
!></td><td>--></td><td>NUMEROS DES PAS DE TEMPS DE DEBUT DE CALCUL
!>                  DES DERIVES.
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS.
!>    </td></tr>
!>          <tr><td>DX,DY
!></td><td>---</td><td>STOCKAGE DES SOUS-PAS .
!>    </td></tr>
!>          <tr><td>ELTLAG
!></td><td><-></td><td>NUMEROS DES ELEMENTS DANS LESQUELS SE TROUVE
!>                  A CET INSTANT CHACUNE DES DERVIES.
!>    </td></tr>
!>          <tr><td>FINLAG
!></td><td>--></td><td>NUMEROS DES PAS DE TEMPS DE FIN DE CALCUL DES
!>                  DERIVES.
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE DE MAILLAGE.
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
!>          <tr><td>LT
!></td><td>--></td><td>NUMERO DU PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES POINTS.
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>NDP
!></td><td>--></td><td>NOMBRE DE POINTS PAR ELEMENT
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMERO DE L'ELEMENT ADJACENT AU K IEME
!>                  SEGMENT DE BORD.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NLAG
!></td><td>--></td><td>NOMBRE DE DERIVES.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>NSP
!></td><td>---</td><td>NOMBRE DE SOUS PAS DE RUNGE KUTTA.
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td>--></td><td>NUMERO LOCAL D'UN POINT DE BORD DANS
!>                  L'ELEMENT ADJACENT DONNE PAR NELBOR.
!>    </td></tr>
!>          <tr><td>RESUX,RESUY
!></td><td><--</td><td>RESULTAT POUR ECRITURE SUR FICHIER DE LA
!>                  DERNIERE DERIVE ACHEVEE.
!>    </td></tr>
!>          <tr><td>SHPLAG
!></td><td><-></td><td>COORDONNEES BARYCENTRIQUES INSTANTANNEES DES
!>                  DERIVES DANS LEURS ELEMENTS RESPECTIFS.
!>    </td></tr>
!>          <tr><td>SURDET
!></td><td>--></td><td>VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
!>    </td></tr>
!>          <tr><td>T8
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTE DE LA VITESSE
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>XLAG,YLAG
!></td><td><-></td><td>POSITIONS INSTANTANNEES DES DERIVES.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DERLAG
     &( U , V , DT , X , Y , IKLE , IFABOR , LT , IELM , NDP , NPOIN ,
     &  NELEM , NELMAX , SURDET , XLAG , YLAG , DX , DY ,
     &  NSP , SHPLAG , DEBLAG , FINLAG , ELTLAG , NLAG , RESUX , RESUY ,
     &  NBOR , NELBOR , NULONE , NPTFR , MSK,MASKEL,MASKPT,T8)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DEBLAG         |-->| NUMEROS DES PAS DE TEMPS DE DEBUT DE CALCUL
C|                |   | DES DERIVES.
C| DT             |-->| PAS DE TEMPS.
C| DX,DY          |---| STOCKAGE DES SOUS-PAS .
C| ELTLAG         |<->| NUMEROS DES ELEMENTS DANS LESQUELS SE TROUVE
C|                |   | A CET INSTANT CHACUNE DES DERVIES.
C| FINLAG         |-->| NUMEROS DES PAS DE TEMPS DE FIN DE CALCUL DES
C|                |   | DERIVES.
C| IELM           |-->| TYPE DE MAILLAGE.
C| IFABOR         |-->| NUMEROS DES ELEMENTS AYANT UNE FACE COMMUNE
C|                |   | AVEC L'ELEMENT .  SI IFABOR
C|                |   | ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE
C| IKLE           |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
C|                |   | ET GLOBALE.
C| LT             |-->| NUMERO DU PAS DE TEMPS
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MASKPT         |-->| TABLEAU DE MASQUAGE DES POINTS.
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD.
C| NDP            |-->| NOMBRE DE POINTS PAR ELEMENT
C| NELBOR         |-->| NUMERO DE L'ELEMENT ADJACENT AU K IEME
C|                |   | SEGMENT DE BORD.
C| NELEM          |-->| NOMBRE D'ELEMENTS.
C| NELMAX         |-->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
C| NLAG           |-->| NOMBRE DE DERIVES.
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
C| NSP            |---| NOMBRE DE SOUS PAS DE RUNGE KUTTA.
C| NULONE         |-->| NUMERO LOCAL D'UN POINT DE BORD DANS
C|                |   | L'ELEMENT ADJACENT DONNE PAR NELBOR.
C| RESUX,RESUY    |<--| RESULTAT POUR ECRITURE SUR FICHIER DE LA
C|                |   | DERNIERE DERIVE ACHEVEE.
C| SHPLAG         |<->| COORDONNEES BARYCENTRIQUES INSTANTANNEES DES
C|                |   | DERIVES DANS LEURS ELEMENTS RESPECTIFS.
C| SURDET         |-->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
C| T8             |---| 
C| U,V            |-->| COMPOSANTE DE LA VITESSE
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE.
C| XLAG,YLAG      |<->| POSITIONS INSTANTANNEES DES DERIVES.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF   !, EX_DERLAG => DERLAG
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN)    :: NPOIN,LT,IELM,NDP,NELEM,NLAG
      INTEGER         , INTENT(IN)    :: NPTFR,NELMAX
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),DT
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      INTEGER         , INTENT(IN)    :: IFABOR(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM)
      DOUBLE PRECISION, INTENT(INOUT) :: XLAG(NPOIN,NLAG)
      DOUBLE PRECISION, INTENT(INOUT) :: YLAG(NPOIN,NLAG)
      INTEGER         , INTENT(INOUT) :: DEBLAG(NLAG),FINLAG(NLAG)
      INTEGER         , INTENT(INOUT) :: ELTLAG(NPOIN,NLAG)
      DOUBLE PRECISION, INTENT(INOUT) :: T8(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPOIN),DY(NPOIN)
      INTEGER         , INTENT(INOUT) :: NSP(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: RESUX(NPOIN),RESUY(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPLAG(NDP,NPOIN,NLAG)
      INTEGER         , INTENT(IN)    :: NBOR(NPTFR),NELBOR(NPTFR)
      INTEGER         , INTENT(IN)    :: NULONE(NPTFR)
      LOGICAL         , INTENT(IN)    :: MSK
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELMAX),MASKPT(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ILAG,JLAG,LTT,NRK,IPOIN
C
      DOUBLE PRECISION Z(1),C
C
C-----------------------------------------------------------------------
C
      DO 10 ILAG=1,NLAG
C
        IF(LT.EQ.DEBLAG(ILAG)) THEN
C
C-----------------------------------------------------------------------
C
C   - SETS THE BARYCENTRIC COORDINATES IN THE MESH , AT THE START
C     OF COMPUTATION FOR EACH FLOAT
C
C-----------------------------------------------------------------------
C
          CALL OV( 'X=CY    ' , XLAG(1,ILAG) , U , Z , -1.D0 , NPOIN )
          CALL OV( 'X=CY    ' , YLAG(1,ILAG) , V , Z , -1.D0 , NPOIN )
C
          IF(IELM.EQ.11) THEN
C
C  P1 TRIANGLES
C  ============
C
C      FILLS THE SHP AND ELT (OPTIMISED)
C
            CALL GTSH11(XLAG(1,ILAG),YLAG(1,ILAG),X,Y,SHPLAG(1,1,ILAG),
     &                  ELTLAG(1,ILAG),IKLE,NSP,NSP,NPOIN,
     &                  NELEM,NELMAX,1,MSK,MASKEL)
C
          ELSE
           IF(LNG.EQ.1) THEN
             WRITE(LU,*) IELM,' : ELEMENT NON PREVU DANS DERLAG'
           ENDIF
           IF(LNG.EQ.2) THEN
             WRITE(LU,*) IELM,': ELEMENT NOT IMPLEMENTED IN DERLAG'
           ENDIF
           STOP
          ENDIF
C
          CALL OV( 'X=Y     ' , XLAG(1,ILAG) , X , Z , C , NPOIN )
          CALL OV( 'X=Y     ' , YLAG(1,ILAG) , Y , Z , C , NPOIN )
C
        ELSEIF(LT.GT.DEBLAG(ILAG).AND.LT.LE.FINLAG(ILAG)) THEN
C
C-----------------------------------------------------------------------
C
C   - COMPUTES THE SUCCESSIVE POSITIONS OF THIS FLOAT
C     (SUBSEQUENT TIMESTEPS)
C
C-----------------------------------------------------------------------
C
C NUMBER OF RUNGE-KUTTA SUB-STEPS, BY CROSSED ELEMENT
C ======================================================
C
          NRK     =  3
C
C  P1 TRIANGLES
C  ============
C
          CALL CHAR11( U , V , DT , NRK , X , Y , IKLE , IFABOR ,
     &                 XLAG(1,ILAG) , YLAG(1,ILAG) , DX , DY ,
     &                 SHPLAG(1,1,ILAG) , ELTLAG(1,ILAG) , NSP ,
     &                 NPOIN , NPOIN , NELEM , NELMAX , SURDET , 1 ,T8)
C
        ENDIF
C
C-----------------------------------------------------------------------
C
C   - CANCELS THE FLOATS LEAVING THE DOMAIN
C
C-----------------------------------------------------------------------
C
        IF(LT.EQ.FINLAG(ILAG)) THEN
          DO IPOIN=1,NPOIN
            IF(ELTLAG(IPOIN,ILAG).LE.0) THEN
              XLAG(IPOIN,ILAG) = X(IPOIN)
              YLAG(IPOIN,ILAG) = Y(IPOIN)
            ENDIF
          ENDDO
        ENDIF
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
C   - STORAGE FOR RESULTS OUTPUT OF THE LAST COMPUTED FLOAT
C
C-----------------------------------------------------------------------
C
      CALL OV( 'X=C     ' , RESUX , Y , Z , 0.D0 , NPOIN )
      CALL OV( 'X=C     ' , RESUY , Y , Z , 0.D0 , NPOIN )
      LTT=0
      JLAG=1
      DO ILAG=1,NLAG
        IF(FINLAG(ILAG).GT.LTT.AND.FINLAG(ILAG).LE.LT) THEN
          LTT=FINLAG(ILAG)
          JLAG=ILAG
        ENDIF
      ENDDO
      IF(LTT.NE.0) THEN
        CALL OV( 'X=Y-Z   ' , RESUX , XLAG(1,JLAG) , X , C , NPOIN )
        CALL OV( 'X=Y-Z   ' , RESUY , YLAG(1,JLAG) , Y , C , NPOIN )
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C