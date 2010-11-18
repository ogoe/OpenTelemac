C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       - COMPUTES THE BARYCENTRIC COORDINATES OF A FLOAT
!>                  IN THE MESH AT THE TIME OF RELEASE.
!><br>            - COMPUTES THE SUCCESSIVE POSITIONS OF THIS FLOAT
!>                  WHICH IS CARRIED WITHOUT FRICTION BY THE CURRENT
!>                 (SUBSEQUENT TIMESTEPS).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEBFLO, DT, ELTFLO, FINFLO, FLOPRD, IELM, IFABOR, IKLE, LT, NDP, NELEM, NELMAX, NFLOT, NITFLO, NPOIN, SHPFLO, SURDET, T8, U, V, X, XFLOT, Y, YFLOT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DET1, DET2, DET3, DX, DY, IELEM, IFLOT, LTP, LTT, N1, N2, N3, NRK, NSP
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DERIVE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CHAR11(), PLANTE()
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
!>      <td><center> 5.1                                       </center>
!> </td><td> 18/08/94
!> </td><td> J-M JANIN (LNH) 30 87 72 84
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DEBFLO
!></td><td>--></td><td>NUMEROS DES PAS DE TEMPS DE LARGAGE DE
!>                  CHAQUE FLOTTEUR.
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS.
!>    </td></tr>
!>          <tr><td>ELTFLO
!></td><td><-></td><td>NUMEROS DES ELEMENTS DANS LESQUELS SE TROUVE
!>                  A CET INSTANT CHACUN DES FLOTTEURS.
!>    </td></tr>
!>          <tr><td>FINFLO
!></td><td><-></td><td>NUMEROS DES PAS DE TEMPS DE FIN DE CALCUL DE
!>                  DERIVE POUR CHAQUE FLOTTEUR.
!>                  FORCE ICI SI UN FLOTTEUR SORT PAR UNE FR. LIQ.
!>    </td></tr>
!>          <tr><td>FLOPRD
!></td><td>--></td><td>NOMBRE DE PAS DE TEMPS ENTRE 2 ENREGITREMENTS
!>                  DES POSITIONS SUCCESSIVES DES FLOTTEURS.
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT.
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
!>          <tr><td>NDP
!></td><td>--></td><td>NOMBRE DE POINTS PAR ELEMENT
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>NFLOT
!></td><td>--></td><td>NOMBRE DE FLOTTEURS.
!>    </td></tr>
!>          <tr><td>NITFLO
!></td><td>--></td><td>NOMBRE MAXIMAL D'ENREGISTREMENTS DES
!>                  POSITIONS SUCCESSIVES DES FLOTTEURS.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>SHPFLO
!></td><td><-></td><td>COORDONNEES BARYCENTRIQUES INSTANTANNEES DES
!>                  FLOTTEURS DANS LEURS ELEMENTS RESPECTIFS.
!>    </td></tr>
!>          <tr><td>SURDET
!></td><td>--></td><td>VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
!>    </td></tr>
!>          <tr><td>T8
!></td><td>---</td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTE DE LA VITESSE
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>XFLOT,YFLOT
!></td><td><-></td><td>POSITIONS SUCCESSIVES DES FLOTTEURS.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DERIVE
     &( U , V , DT , X , Y , IKLE , IFABOR , LT , IELM , NDP , NPOIN ,
     &  NELEM , NELMAX , SURDET , XFLOT , YFLOT ,
     &  SHPFLO , DEBFLO , FINFLO , ELTFLO , NFLOT , NITFLO,FLOPRD,T8)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DEBFLO         |-->| NUMEROS DES PAS DE TEMPS DE LARGAGE DE
C|                |   | CHAQUE FLOTTEUR.
C| DT             |-->| PAS DE TEMPS.
C| ELTFLO         |<->| NUMEROS DES ELEMENTS DANS LESQUELS SE TROUVE
C|                |   | A CET INSTANT CHACUN DES FLOTTEURS.
C| FINFLO         |<->| NUMEROS DES PAS DE TEMPS DE FIN DE CALCUL DE
C|                |   | DERIVE POUR CHAQUE FLOTTEUR.
C|                |   | FORCE ICI SI UN FLOTTEUR SORT PAR UNE FR. LIQ.
C| FLOPRD         |-->| NOMBRE DE PAS DE TEMPS ENTRE 2 ENREGITREMENTS
C|                |   | DES POSITIONS SUCCESSIVES DES FLOTTEURS.
C| IELM           |-->| TYPE D'ELEMENT.
C| IFABOR         |-->| NUMEROS DES ELEMENTS AYANT UNE FACE COMMUNE
C|                |   | AVEC L'ELEMENT .  SI IFABOR
C|                |   | ON A UNE FACE LIQUIDE,SOLIDE,OU PERIODIQUE
C| IKLE           |-->| TRANSITION ENTRE LES NUMEROTATIONS LOCALE
C|                |   | ET GLOBALE.
C| LT             |-->| NUMERO DU PAS DE TEMPS
C| NDP            |-->| NOMBRE DE POINTS PAR ELEMENT
C| NELEM          |-->| NOMBRE D'ELEMENTS.
C| NELMAX         |-->| NOMBRE MAXIMAL D'ELEMENTS DANS LE MAILLAGE 2D
C| NFLOT          |-->| NOMBRE DE FLOTTEURS.
C| NITFLO         |-->| NOMBRE MAXIMAL D'ENREGISTREMENTS DES
C|                |   | POSITIONS SUCCESSIVES DES FLOTTEURS.
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| SHPFLO         |<->| COORDONNEES BARYCENTRIQUES INSTANTANNEES DES
C|                |   | FLOTTEURS DANS LEURS ELEMENTS RESPECTIFS.
C| SURDET         |-->| VARIABLE UTILISEE PAR LA TRANSFORMEE ISOPARAM.
C| T8             |---| TABLEAU DE TRAVAIL
C| U,V            |-->| COMPOSANTE DE LA VITESSE
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE.
C| XFLOT,YFLOT    |<->| POSITIONS SUCCESSIVES DES FLOTTEURS.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_DERIVE => DERIVE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN)    :: NPOIN,LT,IELM,NDP,NELEM
      INTEGER         , INTENT(IN)    :: NITFLO,FLOPRD,NELMAX,NFLOT
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),DT
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      INTEGER         , INTENT(IN)    :: IFABOR(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM)
      DOUBLE PRECISION, INTENT(INOUT) :: XFLOT(NITFLO,NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: YFLOT(NITFLO,NFLOT)
      INTEGER         , INTENT(INOUT) :: DEBFLO(NFLOT),FINFLO(NFLOT)
      INTEGER         , INTENT(INOUT) :: ELTFLO(NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPFLO(NDP,NFLOT)
      DOUBLE PRECISION, INTENT(INOUT) :: T8(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER LTP,IFLOT,N1,N2,N3,NRK,IELEM,LTT,NSP(1)
C
      DOUBLE PRECISION DET1,DET2,DET3,DX(1),DY(1)
C
C-----------------------------------------------------------------------
C
      LTT=(LT-1)/FLOPRD + 1
      LTP=(LT-2)/FLOPRD + 1
C
      DO 10 IFLOT=1,NFLOT
C
        IF(LT.EQ.DEBFLO(IFLOT)) THEN
C
C-----------------------------------------------------------------------
C
C   - COMPUTES THE BARYCENTRIC COORDINATES OF A FLOAT IN THE MESH
C     AT THE TIME OF RELEASE
C
C-----------------------------------------------------------------------
C
          XFLOT(LTT,IFLOT) = XFLOT(1,IFLOT)
          YFLOT(LTT,IFLOT) = YFLOT(1,IFLOT)
C
          IF(IELM.EQ.11) THEN
C
C P1 TRIANGLES MESH
C ========================
C
            DO 20 IELEM=1,NELEM
              N1=IKLE(IELEM,1)
              N2=IKLE(IELEM,2)
              N3=IKLE(IELEM,3)
C
C DET1 = (N2N3,N2FLOT)  DET2 = (N3N1,N3FLOT)  DET3 = (N1N2,N1FLOT)
C ----------------------------------------------------------------
C
              DET1=(X(N3)-X(N2))*(YFLOT(LTT,IFLOT)-Y(N2))
     &            -(Y(N3)-Y(N2))*(XFLOT(LTT,IFLOT)-X(N2))
              DET2=(X(N1)-X(N3))*(YFLOT(LTT,IFLOT)-Y(N3))
     &            -(Y(N1)-Y(N3))*(XFLOT(LTT,IFLOT)-X(N3))
              DET3=(X(N2)-X(N1))*(YFLOT(LTT,IFLOT)-Y(N1))
     &            -(Y(N2)-Y(N1))*(XFLOT(LTT,IFLOT)-X(N1))
              IF(DET1.GE.0.D0.AND.DET2.GE.0.D0.AND.DET3.GE.0.D0) GOTO 30
C
20          CONTINUE
C
            IF(LNG.EQ.1) WRITE(LU,33) IFLOT
            IF(LNG.EQ.2) WRITE(LU,34) IFLOT
33          FORMAT(1X,'ERREUR D''INTERPOLATION DANS DERIVE :',/,
     &             1X,'LARGAGE DU FLOTTEUR',I6,/,
     &             1X,'EN DEHORS DU DOMAINE DE CALCUL')
34          FORMAT(1X,'INTERPOLATION ERROR IN DERIVE :',/,
     &             1X,'DROP POINT OF FLOAT',I6,/,
     &             1X,'OUT OF THE DOMAIN')
            STOP
C
C ELEMENT CONTAINING THE POINT OF RELEASE, COMPUTES THE SHPFLO
C ---------------------------------------------------------------
C
30          CONTINUE
            SHPFLO(1,IFLOT) = DET1*SURDET(IELEM)
            SHPFLO(2,IFLOT) = DET2*SURDET(IELEM)
            SHPFLO(3,IFLOT) = DET3*SURDET(IELEM)
            ELTFLO (IFLOT)  = IELEM
C
          ELSE
            IF(LNG.EQ.1) WRITE(LU,123) IELM
            IF(LNG.EQ.2) WRITE(LU,124) IELM
123         FORMAT(1X,'DERIVE : TYPE D''ELEMENT NON PREVU : ',1I6)
124         FORMAT(1X,'DERIVE : UNEXPECTED TYPE OF ELEMENT: ',1I6)
            CALL PLANTE(1)
            STOP
          ENDIF
C
        ELSEIF(LT.GT.DEBFLO(IFLOT).AND.LT.LE.FINFLO(IFLOT)) THEN
C
C-----------------------------------------------------------------------
C
C   - COMPUTES THE SUCCESSIVE POSITIONS OF THIS FLOAT WHICH IS
C     CARRIED WITHOUT FRICTION BY THE CURRENT (SUBSEQUENT TIMESTEPS)
C
C-----------------------------------------------------------------------
C
C NUMBER OF RUNGE-KUTTA SUB-STEPS, BY CROSSED ELEMENT
C ======================================================
C
          NRK = 3
C
          XFLOT(LTT,IFLOT) = XFLOT(LTP,IFLOT)
          YFLOT(LTT,IFLOT) = YFLOT(LTP,IFLOT)
C
          IF(IELM.EQ.11) THEN
C
C  P1 TRIANGLES
C  ============
C
            CALL CHAR11( U , V , DT , NRK , X , Y , IKLE , IFABOR ,
     &                   XFLOT(LTT,IFLOT) , YFLOT(LTT,IFLOT) , DX , DY ,
     &                   SHPFLO(1,IFLOT) , ELTFLO(IFLOT) , NSP ,
     &                   1 , NPOIN , NELEM , NELMAX , SURDET ,  1,T8 )
C
          ELSE
C
            IF(LNG.EQ.1) WRITE(LU,123) IELM
            IF(LNG.EQ.2) WRITE(LU,124) IELM
            STOP
C
          ENDIF
C
C  CASE OF LOST FLOATS
C  ========================
C
          IF(ELTFLO(IFLOT).LE.0) FINFLO(IFLOT) = LT
C
        ENDIF
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C