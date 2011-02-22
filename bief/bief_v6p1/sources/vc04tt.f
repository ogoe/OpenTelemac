C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>                  /        D(PSII)       D(PSII)
!>      V  = XMUL  /     U * ------- + V * -------   D(OMEGA)
!>       I        /OMEGA       DX            DY<br>
!>                  /            D(PSII*)           D(PSII*)
!>         = XMUL  /     H * U * -------- + H * V * --------   D(OMEGA*)
!>                /OMEGA*           DX                 DY<br>
!>    PSII IS OF TYPE P1 TETRAHEDRON<br>
!>    REAL MESH HERE, CAN BE REGARDED AS A COMPUTATION
!>    IN A TRANSFORMED MESH, BUT WITH H IN THE INTEGRAL.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FORMUL, IKLE1, IKLE2, IKLE3, IKLE4, NELEM, NELMAX, SU, SV, SW, U, V, W, W1, W2, W3, W4, X, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I1, I2, I3, I4, IELEM, IELMU, IELMV, IELMW, Q1, Q2, Q3, Q4, U1, U2, U3, U4, V1, V2, V3, V4, X2, X3, X4, XSUR24, Y2, Y3, Y4, Z2, Z3, Z4
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_VC04TT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>VECTOS()

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
!>      <td><center> 5.3                                       </center>
!> </td><td> 22/03/02
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LA FORMULE.
!>    </td></tr>
!>          <tr><td>FORMUL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE1,
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>Q1,2,3
!></td><td><--</td><td>VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
!>    </td></tr>
!>          <tr><td>SF,SG,SH
!></td><td>--></td><td>STRUCTURES DES FONCTIONS F,G ET H
!>    </td></tr>
!>          <tr><td>SU,SV,SW
!></td><td>--></td><td>STRUCTURES DES FONCTIONS U,V ET W
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES ELEMENTS.
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR
!>                  INTERVENANT DANS LA FORMULE.
!>    </td></tr>
!>          <tr><td>W1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XEL,YEL,
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR.
!>    </td></tr>
!>          <tr><td>Y
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VC04TT
     &( XMUL,SU,SV,SW,U,V,W,X,Y,Z,
     &  IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NELMAX,
     &  W1,W2,W3,W4,FORMUL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE.
C| FORMUL         |---| 
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| Q1,2,3         |<--| VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
C| SF,SG,SH       |-->| STRUCTURES DES FONCTIONS F,G ET H
C| SU,SV,SW       |-->| STRUCTURES DES FONCTIONS U,V ET W
C| SURFAC         |-->| SURFACE DES ELEMENTS.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR
C|                |   | INTERVENANT DANS LA FORMULE.
C| W1             |---| 
C| W2             |---| 
C| W3             |---| 
C| W4             |---| 
C| X             |---| 
C| XEL,YEL,       |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C| Y             |---| 
C| Z             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_VC04TT => VC04TT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX)
      INTEGER, INTENT(IN) :: IKLE3(NELMAX),IKLE4(NELMAX)
C
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELMAX),W2(NELMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: W3(NELMAX),W4(NELMAX)
      DOUBLE PRECISION, INTENT(IN) :: XMUL
C
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
C
C     STRUCTURES OF U, V AND REAL DATA
C
      TYPE(BIEF_OBJ),   INTENT(IN) :: SU,SV,SW
      DOUBLE PRECISION, INTENT(IN) :: U(*),V(*),W(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION XSUR24,X2,X3,X4,Y2,Y3,Y4,Z2,Z3,Z4
      DOUBLE PRECISION U1,U2,U3,U4,V1,V2,V3,V4,Q1,Q2,Q3,Q4
      INTEGER I1,I2,I3,I4,IELEM,IELMU,IELMV,IELMW
C
C**********************************************************************
C
      XSUR24 = XMUL / 24.D0
C
C-----------------------------------------------------------------------
C
      IELMU=SU%ELM
      IELMV=SV%ELM
      IELMW=SW%ELM
C
C-----------------------------------------------------------------------
C
C   HORIZONTAL TERMS
C
      IF(FORMUL(14:16).EQ.'HOR') THEN
C
C   LOOP ON THE ELEMENTS
C
      IF((IELMU.EQ.31.AND.IELMV.EQ.31).OR.
     &   (IELMU.EQ.51.AND.IELMV.EQ.51)     ) THEN
C
C-----------------------------------------------------------------------
C
C  U AND V DISCRETISED IN P1 PRISM:
C
      DO 3 IELEM = 1 , NELEM
C
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
C
         X2 = X(I2) - X(I1)
         X3 = X(I3) - X(I1)
         X4 = X(I4) - X(I1)
C
         Y2 = Y(I2) - Y(I1)
         Y3 = Y(I3) - Y(I1)
         Y4 = Y(I4) - Y(I1)
C
         Z2 = Z(I2) - Z(I1)
         Z3 = Z(I3) - Z(I1)
         Z4 = Z(I4) - Z(I1)
C
         U1 = U(I1)
         U2 = U(I2)
         U3 = U(I3)
         U4 = U(I4)
         V1 = V(I1)
         V2 = V(I2)
         V3 = V(I3)
         V4 = V(I4)
C
         W1(IELEM) = XSUR24*(
     &U3*Z2*Y3+V4*X3*Z4-U4*Y3*Z4+U4*Z2*Y3+U4*Y4*Z3-U4*Z2*Y4-U4*
     &Y2*Z3+U1*Y4*Z3+V1*Z2*X4-V4*X2*Z4-U3*Z2*Y4-U2*Y2*Z3+V4*X2*Z3+V4*Z2*
     &X4-V4*X4*Z3+V2*Z2*X4-U2*Z2*Y4+U2*Y2*Z4+U2*Y4*Z3+U2*Z2*Y3-V1*Z2*X3+
     &V1*X2*Z3-V1*X4*Z3+V1*X3*Z4+U3*Y2*Z4+U3*Y4*Z3-V3*X2*Z4-V3*X4*Z3+V3*
     &X3*Z4-V3*Z2*X3+V3*X2*Z3+V3*Z2*X4+U1*Z2*Y3-U1*Y2*Z3+U1*Y2*Z4-V2*X2*
     &Z4-V2*X4*Z3-V2*Z2*X3+V2*X2*Z3-U3*Y2*Z3+V2*X3*Z4-V4*Z2*X3-U1*Y3*Z4+
     &U4*Y2*Z4-U3*Y3*Z4-V1*X2*Z4-U1*Z2*Y4-U2*Y3*Z4)
C
         W2(IELEM) = XSUR24*(
     &-U4*Y4*Z3+U4*Y3*Z4-V4*X3*Z4+V4*X4*Z3+U1*Y3*Z4-U1*Y4*Z3-V3
     &*X3*Z4+V3*X4*Z3+U3*Y3*Z4-U3*Y4*Z3-V1*X3*Z4+V1*X4*Z3+U2*Y3*Z4-U2*Y4
     &*Z3-V2*X3*Z4+V2*X4*Z3)
C
         W3(IELEM) = XSUR24*(
     &U4*Z2*Y4-U4*Y2*Z4-V4*Z2*X4+U3*Z2*Y4+V4*X2*Z4-V1*Z2*X4-U1*
     &Y2*Z4-V3*Z2*X4+V3*X2*Z4-U3*Y2*Z4+V1*X2*Z4-U2*Y2*Z4+U2*Z2*Y4-V2*Z2*
     &X4+V2*X2*Z4+U1*Z2*Y4)
C
         W4(IELEM) = XSUR24*(
     &U4*Y2*Z3-U4*Z2*Y3+V4*Z2*X3-V4*X2*Z3+U1*Y2*Z3-U1*Z2*Y3+U3*
     &Y2*Z3-U3*Z2*Y3-V3*X2*Z3+V3*Z2*X3-V1*X2*Z3+V1*Z2*X3-U2*Z2*Y3+U2*Y2*
     &Z3-V2*X2*Z3+V2*Z2*X3)
C
3     CONTINUE
C
C-----------------------------------------------------------------------
C
C     ELSEIF(IELMU.EQ.  ) THEN
C
C-----------------------------------------------------------------------
C
      ELSE
C
C-----------------------------------------------------------------------
C
       IF (LNG.EQ.1) WRITE(LU,101) IELMU,SU%NAME
       IF (LNG.EQ.2) WRITE(LU,102) IELMU,SU%NAME
101    FORMAT(1X,'VC04TT (BIEF) :',/,
     &        1X,'DISCRETISATION DE U ET V : ',1I6,' CAS NON PREVU',/,
     &        1X,'NOM REEL DE U : ',A6)
102    FORMAT(1X,'VC04TT (BIEF) :',/,
     &        1X,'DISCRETISATION OF U ET V : ',1I6,' NOT IMPLEMENTED',/,
     &        1X,'REAL NAME OF U : ',A6)
       CALL PLANTE(1)
       STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(FORMUL(14:16).EQ.'VER') THEN
C
      IF(IELMW.NE.31.AND.IELMW.NE.51) THEN
        IF (LNG.EQ.1) WRITE(LU,301)
        IF (LNG.EQ.2) WRITE(LU,302)
301     FORMAT(1X,'VC04TT (BIEF) :',/,
     &         1X,'CAS NON PREVU (IELMW.NE.31 ET .NE.51)')
302     FORMAT(1X,'VC04TT (BIEF) :',/,
     &         1X,'UNEXPECTED CASE (IELMW.NE.31 AND .NE.51)')
        CALL PLANTE(1)
        STOP
      ENDIF
C
      DO IELEM = 1 , NELEM
C
         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)
C
         X2 = X(I2) - X(I1)
         X3 = X(I3) - X(I1)
         X4 = X(I4) - X(I1)
C
         Y2 = Y(I2) - Y(I1)
         Y3 = Y(I3) - Y(I1)
         Y4 = Y(I4) - Y(I1)
C
         Q1 = W(I1)
         Q2 = W(I2)
         Q3 = W(I3)
         Q4 = W(I4)
C
         W1(IELEM) = (
     &         X4*Y3*Q4-Y2*X4*Q4+X2*Y4*Q4-X2*Y3*Q4-X3*Y4*Q4+Y2*X3*Q4-X2*
     &Y3*Q3-Y2*X4*Q3+X2*Y4*Q1-X3*Y4*Q3+X2*Y4*Q3+Y2*X3*Q3-X3*Y4*Q2+X4*Y3*
     &Q2+X2*Y4*Q2-Y2*X4*Q2-X2*Y3*Q2+Y2*X3*Q2+Y2*X3*Q1-Y2*X4*Q1-X2*Y3*Q1+
     &X4*Y3*Q3+X4*Y3*Q1-X3*Y4*Q1 )*XSUR24
C
         W2(IELEM) = (
     &         -X4*Y3*Q4+X3*Y4*Q4+X3*Y4*Q3+X3*Y4*Q2-X4*Y3*Q2-X4*Y3*Q3-X4
     &*Y3*Q1+X3*Y4*Q1 )*XSUR24
C
         W3(IELEM) = (
     &         Y2*X4*Q4-X2*Y4*Q4+Y2*X4*Q3-X2*Y4*Q1-X2*Y4*Q3-X2*Y4*Q2+Y2*
     &X4*Q2+Y2*X4*Q1 )*XSUR24
C
         W4(IELEM) = (
     &         X2*Y3*Q4-Y2*X3*Q4+X2*Y3*Q3-Y2*X3*Q3+X2*Y3*Q2-Y2*X3*Q2-Y2*
     &X3*Q1+X2*Y3*Q1 )*XSUR24
C
      ENDDO
C
      ELSE
C
C-----------------------------------------------------------------------
C
       IF (LNG.EQ.1) WRITE(LU,201) FORMUL
       IF (LNG.EQ.2) WRITE(LU,202) FORMUL
201    FORMAT(1X,'VC04TT (BIEF) :',/,
     &        1X,'IL MANQUE HOR OU VER EN FIN DE FORMULE : ',A16)
202    FORMAT(1X,'VC04TT (BIEF):',/,
     &        1X,'HOR OR VER LACKING AT THE END OF THE FORMULA : ',A16)
       CALL PLANTE(1)
       STOP
C
C-----------------------------------------------------------------------
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C