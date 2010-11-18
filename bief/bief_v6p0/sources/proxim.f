C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       IDENTIFIES THE POINTS OF THE MESH CLOSEST TO A SET
!>                OF GIVEN POINTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE, IP, NELEM, NELMAX, NP, NPOIN, X, XP, Y, YP
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A12, A23, A31, ALERT, D2, DIST2, I, IELEM, K, X1, X2, X3, XX, Y1, Y2, Y3, YY
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PROXIM
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), P_DMAX(), P_DSUM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D(), TELEMAC3D()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 03/07/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IP
!></td><td><--</td><td>ADRESSES DES POINTS TROUVES.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NP
!></td><td>--></td><td>NOMBRE DE POINTS DONNES.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>XP,YP
!></td><td>--></td><td>COORDONNEES DES POINTS DONNES.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PROXIM
     &(IP,XP,YP,X,Y,NP,NPOIN,IKLE,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |---| 
C| IP             |<--| ADRESSES DES POINTS TROUVES.
C| NELEM          |---| 
C| NELMAX         |---| 
C| NP             |-->| NOMBRE DE POINTS DONNES.
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE
C| XP,YP          |-->| COORDONNEES DES POINTS DONNES.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PROXIM => PROXIM
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NP,NPOIN,NELEM,NELMAX
      INTEGER, INTENT(INOUT) :: IP(NP)
      INTEGER, INTENT(IN)    :: IKLE(NELMAX,3)
C
      DOUBLE PRECISION, INTENT(IN) :: XP(NP),YP(NP),X(NPOIN),Y(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,K,IELEM
      DOUBLE PRECISION X1,Y1,X2,Y2,X3,Y3,A31,A12,A23,DIST2,D2,ALERT
      DOUBLE PRECISION XX,YY
C
      INTRINSIC SQRT
C
      DOUBLE PRECISION P_DSUM,P_DMAX
      EXTERNAL         P_DSUM,P_DMAX
C
C-----------------------------------------------------------------------
C
      DO 10 K=1,NP
        IP(K)=0
        DIST2=1.D10
        ALERT=0.D0
        XX=-1.D10
        YY=-1.D10
C
C       LOOP ON THE TRIANGLES:
C
        DO 20 IELEM=1,NELEM
          X1=X(IKLE(IELEM,1))
          X2=X(IKLE(IELEM,2))
          X3=X(IKLE(IELEM,3))
          Y1=Y(IKLE(IELEM,1))
          Y2=Y(IKLE(IELEM,2))
          Y3=Y(IKLE(IELEM,3))
          A31=XP(K)*Y3-YP(K)*X3+X3*Y1-X1*Y3+X1*YP(K)-XP(K)*Y1
          A12=XP(K)*Y1-YP(K)*X1+X1*Y2-X2*Y1+X2*YP(K)-XP(K)*Y2
          A23=XP(K)*Y2-YP(K)*X2+X2*Y3-X3*Y2+X3*YP(K)-XP(K)*Y3
          IF(A31.GT.-1.D-6.AND.A12.GT.-1.D-6.AND.A23.GT.-1.D-6) THEN
C           TAKES THE NEAREST NODE
            DO I=1,3
              D2=(XP(K)-X(IKLE(IELEM,I)))**2+(YP(K)-Y(IKLE(IELEM,I)))**2
              IF(D2.LT.DIST2) THEN
                IP(K)=IKLE(IELEM,I)
                DIST2=D2
              ENDIF
            ENDDO
          ENDIF
20      CONTINUE
        IF(IP(K).EQ.0) THEN
          IF(LNG.EQ.1) WRITE(LU,*) 'POINT SOURCE ',K,' HORS DOMAINE'
          IF(LNG.EQ.2) WRITE(LU,*) 'SOURCE POINT ',K,' OUTSIDE DOMAIN'
          IF(NCSIZE.LE.1) THEN
            WRITE(LU,*) ' '
            IF(LNG.EQ.1) WRITE(LU,*) 'INTERDIT EN MODE SCALAIRE'
            IF(LNG.EQ.2) WRITE(LU,*) 'NOT ALLOWED IN SCALAR MODE'
            CALL PLANTE(1)
            STOP
          ENDIF
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'POINT SOURCE ',K,' ASSIMILE AU POINT ',IP(K)
            WRITE(LU,*) 'SITUE A ',SQRT(DIST2),' METRES'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'SOURCE POINT ',K,' PUT ON POINT ',IP(K)
            WRITE(LU,*) 'LOCATED AT ',SQRT(DIST2),' METRES'
          ENDIF
          IF(SQRT(DIST2).GT.1.D-6.AND.NCSIZE.GT.1) THEN
            XX=X(IP(K))
            YY=Y(IP(K))
            ALERT=1.D0
          ENDIF
        ENDIF
        IF(NCSIZE.GT.1) THEN
          XX=P_DMAX(XX)
          YY=P_DMAX(YY)
          ALERT=P_DSUM(ALERT)
        ENDIF
        IF(ALERT.GT.0.5D0) THEN
          WRITE(LU,*) ' '
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'EN MODE PARALLELE LES SOURCES PONCTUELLES'
            WRITE(LU,*) 'DOIVENT COINCIDER EXACTEMENT AVEC DES POINTS'
            WRITE(LU,*) 'DU MAILLAGE, POUR LA SOURCE ',K,' CHOISIR :'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'IN PARALLEL SOURCES MUST COINCIDE WITH'
            WRITE(LU,*) 'NODES IN THE MESH, FOR SOURCE',K,' CHOOSE:'
          ENDIF
          WRITE(LU,*) 'X=',XX,' Y=',YY
          CALL PLANTE(1)
          STOP
        ENDIF
10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C