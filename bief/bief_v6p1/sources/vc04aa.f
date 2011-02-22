C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>                    /          DPSI     DPSI
!>      V  =  XMUL   /       ( U  --  + V  --  ) D(OMEGA)
!>       I          /OMEGA        DX       DY<br>
!>    PSI(I) IS A BASE OF TYPE P1 TRIANGLE
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  SEE TESTS ON FORMUL TO UNDERSTAND HOW U AND V ARE DEALT WITH:
!><br>     IF FORMUL(7:7).EQ.' ' : VELOCITY WITH COMPONENTS U AND V LINEAR.
!><br>     IF FORMUL(7:7).EQ.'2' : VELOCITY EQUALS U*GRAD(V) U AND V LINEAR
!>                                 BUT V MAY BE PIECE-WISE LINEAR SO V%DIMDISC
!>                                 IS LOOKED AT IN THIS CASE.

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE1, IKLE2, IKLE3, NELEM, NELMAX, SPECAD, SU, SV, U, V, W1, W2, W3, XEL, XMUL, YEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DET, GRADVX, GRADVY, IELEM, IELMU, IELMV, U1, U2, U3, V1, V2, V3, X2, X3, XSUR6, Y2, Y3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_VC04AA
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
!>      <td><center> 5.7                                       </center>
!> </td><td> 01/06/06
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE1,
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>SPECAD
!></td><td>---</td><td>
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
!>          <tr><td>W1,2,3
!></td><td>--></td><td>VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
!>    </td></tr>
!>          <tr><td>W2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XEL,YEL,
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VC04AA
     &( XMUL,SU,SV,U,V,XEL,YEL,
     &  IKLE1,IKLE2,IKLE3,NELEM,NELMAX,W1,W2,W3,SPECAD)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| IKLE2          |---| 
C| IKLE3          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SPECAD         |---| 
C| SU,SV,SW       |-->| STRUCTURES DES FONCTIONS U,V ET W
C| SURFAC         |-->| SURFACE DES ELEMENTS.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR
C|                |   | INTERVENANT DANS LA FORMULE.
C| W1,2,3         |-->| VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
C| W2             |---| 
C| W3             |---| 
C| XEL,YEL,       |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_VC04AA => VC04AA
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
C
      DOUBLE PRECISION, INTENT(IN)   :: XEL(NELMAX,*),YEL(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT):: W1(NELMAX),W2(NELMAX),W3(NELMAX)
      DOUBLE PRECISION, INTENT(IN)   :: XMUL
C
C     STRUCTURES OF F, U, V AND REAL DATA
C
      TYPE(BIEF_OBJ), INTENT(IN)    :: SU,SV
      DOUBLE PRECISION, INTENT(IN)  :: U(*),V(*)
C
      LOGICAL, INTENT(IN) :: SPECAD
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IELMU,IELMV
      DOUBLE PRECISION XSUR6,X2,Y2,X3,Y3,U1,U2,U3,V1,V2,V3
      DOUBLE PRECISION GRADVX,GRADVY,DET
C
C-----------------------------------------------------------------------
C
      IELMU=SU%ELM
      IELMV=SV%ELM
C
      XSUR6 = XMUL/6.D0
C
C-----------------------------------------------------------------------
C
      IF(.NOT.SPECAD) THEN
C
C     VELOCITY WITH LINEAR COMPONENTS U AND V
C
        IF(IELMU.NE.11.OR.IELMV.NE.11) THEN
          IF(LNG.EQ.1) WRITE(LU,200) IELMU,SU%NAME
          IF(LNG.EQ.1) WRITE(LU,300)
          IF(LNG.EQ.2) WRITE(LU,201) IELMU,SU%NAME
          IF(LNG.EQ.2) WRITE(LU,301)
200       FORMAT(1X,'VC04AA : DISCRETISATION DE U : ',1I6,
     &           1X,'NOM REEL : ',A6)
300       FORMAT(1X,'CAS NON PREVU')
201       FORMAT(1X,'VC04AA: DISCRETIZATION OF U:',1I6,
     &           1X,'REAL NAME: ',A6)
301       FORMAT(1X,'CASE NOT IMPLEMENTED')
          CALL PLANTE(1)
          STOP
        ENDIF
C
        DO IELEM = 1 , NELEM
          X2 = XEL(IELEM,2)
          X3 = XEL(IELEM,3)
          Y2 = YEL(IELEM,2)
          Y3 = YEL(IELEM,3)
          U1 = U(IKLE1(IELEM))
          U2 = U(IKLE2(IELEM))
          U3 = U(IKLE3(IELEM))
          V1 = V(IKLE1(IELEM))
          V2 = V(IKLE2(IELEM))
          V3 = V(IKLE3(IELEM))
          W1(IELEM) = (U3*Y2-U3*Y3-V3*X2+V3*X3+U2*Y2-U2*Y3
     &                -V2*X2+V2*X3+U1*Y2-U1*Y3-V1*X2+V1*X3)*XSUR6
          W2(IELEM) = (U1*Y3+U2*Y3-V1*X3-V2*X3+U3*Y3-V3*X3)*XSUR6
          W3(IELEM) = (-U1*Y2-U2*Y2+V1*X2+V2*X2-U3*Y2+V3*X2)*XSUR6
        ENDDO
C
C-----------------------------------------------------------------------
C
      ELSE
C
C       VELOCITY EQUALS U * GRAD(V)
C       WITH VARIOUS DISCRETISATIONS OF U AND V
C
        IF(SV%DIMDISC.EQ.0) THEN
C
        IF(IELMU.EQ.11) THEN
C       V LINEAR, U LINEAR
        DO IELEM = 1 , NELEM
          X2 = XEL(IELEM,2)
          X3 = XEL(IELEM,3)
          Y2 = YEL(IELEM,2)
          Y3 = YEL(IELEM,3)
          DET=X2*Y3-X3*Y2
          V1 = V(IKLE1(IELEM))
          V2 = V(IKLE2(IELEM))
          V3 = V(IKLE3(IELEM))
          GRADVX=((V2-V1)*Y3-(V3-V1)*Y2)/DET
          GRADVY=(X2*(V3-V1)-X3*(V2-V1))/DET
          U1 = U(IKLE1(IELEM))*GRADVX
          U2 = U(IKLE2(IELEM))*GRADVX
          U3 = U(IKLE3(IELEM))*GRADVX
          V1 = U(IKLE1(IELEM))*GRADVY
          V2 = U(IKLE2(IELEM))*GRADVY
          V3 = U(IKLE3(IELEM))*GRADVY
          W1(IELEM) = (U3*Y2-U3*Y3-V3*X2+V3*X3+U2*Y2-U2*Y3
     &                -V2*X2+V2*X3+U1*Y2-U1*Y3-V1*X2+V1*X3)*XSUR6
          W2(IELEM) = (U1*Y3+U2*Y3-V1*X3-V2*X3+U3*Y3-V3*X3)*XSUR6
          W3(IELEM) = (-U1*Y2-U2*Y2+V1*X2+V2*X2-U3*Y2+V3*X2)*XSUR6
        ENDDO
        ELSEIF(IELMU.EQ.10) THEN
C       V LINEAR, U PIECE-WISE CONSTANT
        DO IELEM = 1 , NELEM
          X2 = XEL(IELEM,2)
          X3 = XEL(IELEM,3)
          Y2 = YEL(IELEM,2)
          Y3 = YEL(IELEM,3)
          DET=X2*Y3-X3*Y2
          V1 = V(IKLE1(IELEM))
          V2 = V(IKLE2(IELEM))
          V3 = V(IKLE3(IELEM))
          GRADVX=((V2-V1)*Y3-(V3-V1)*Y2)/DET
          GRADVY=(X2*(V3-V1)-X3*(V2-V1))/DET
          U1 = U(IELEM)*GRADVX
          U2 = U(IELEM)*GRADVX
          U3 = U(IELEM)*GRADVX
          V1 = U(IELEM)*GRADVY
          V2 = U(IELEM)*GRADVY
          V3 = U(IELEM)*GRADVY
          W1(IELEM) = (U3*Y2-U3*Y3-V3*X2+V3*X3+U2*Y2-U2*Y3
     &                -V2*X2+V2*X3+U1*Y2-U1*Y3-V1*X2+V1*X3)*XSUR6
          W2(IELEM) = (U1*Y3+U2*Y3-V1*X3-V2*X3+U3*Y3-V3*X3)*XSUR6
          W3(IELEM) = (-U1*Y2-U2*Y2+V1*X2+V2*X2-U3*Y2+V3*X2)*XSUR6
        ENDDO
        ELSE
          WRITE(LU,*) 'WRONG DISCRETISATION OF U IN VC04AA'
          CALL PLANTE(1)
          STOP
        ENDIF
C
        ELSEIF(SV%DIMDISC.EQ.11) THEN
C
        IF(IELMU.EQ.11) THEN
C       V PIECE-WISE LINEAR, U LINEAR
        DO IELEM=1,NELEM
          X2 = XEL(IELEM,2)
          X3 = XEL(IELEM,3)
          Y2 = YEL(IELEM,2)
          Y3 = YEL(IELEM,3)
          DET=X2*Y3-X3*Y2
          V1 = V(IELEM         )
          V2 = V(IELEM+  NELMAX)
          V3 = V(IELEM+2*NELMAX)
          GRADVX=((V2-V1)*Y3-(V3-V1)*Y2)/DET
          GRADVY=(X2*(V3-V1)-X3*(V2-V1))/DET
          U1 = U(IKLE1(IELEM))*GRADVX
          U2 = U(IKLE2(IELEM))*GRADVX
          U3 = U(IKLE3(IELEM))*GRADVX
          V1 = U(IKLE1(IELEM))*GRADVY
          V2 = U(IKLE2(IELEM))*GRADVY
          V3 = U(IKLE3(IELEM))*GRADVY
          W1(IELEM) = (U3*Y2-U3*Y3-V3*X2+V3*X3+U2*Y2-U2*Y3
     &                -V2*X2+V2*X3+U1*Y2-U1*Y3-V1*X2+V1*X3)*XSUR6
          W2(IELEM) = (U1*Y3+U2*Y3-V1*X3-V2*X3+U3*Y3-V3*X3)*XSUR6
          W3(IELEM) = (-U1*Y2-U2*Y2+V1*X2+V2*X2-U3*Y2+V3*X2)*XSUR6
        ENDDO
        ELSEIF(IELMU.EQ.10) THEN
C       V PIECE-WISE LINEAR, U LINEAR
        DO IELEM=1,NELEM
          X2 = XEL(IELEM,2)
          X3 = XEL(IELEM,3)
          Y2 = YEL(IELEM,2)
          Y3 = YEL(IELEM,3)
          DET=X2*Y3-X3*Y2
          V1 = V(IELEM         )
          V2 = V(IELEM+  NELMAX)
          V3 = V(IELEM+2*NELMAX)
          GRADVX=((V2-V1)*Y3-(V3-V1)*Y2)/DET
          GRADVY=(X2*(V3-V1)-X3*(V2-V1))/DET
          U1 = U(IELEM)*GRADVX
          U2 = U(IELEM)*GRADVX
          U3 = U(IELEM)*GRADVX
          V1 = U(IELEM)*GRADVY
          V2 = U(IELEM)*GRADVY
          V3 = U(IELEM)*GRADVY
          W1(IELEM) = (U3*Y2-U3*Y3-V3*X2+V3*X3+U2*Y2-U2*Y3
     &                -V2*X2+V2*X3+U1*Y2-U1*Y3-V1*X2+V1*X3)*XSUR6
          W2(IELEM) = (U1*Y3+U2*Y3-V1*X3-V2*X3+U3*Y3-V3*X3)*XSUR6
          W3(IELEM) = (-U1*Y2-U2*Y2+V1*X2+V2*X2-U3*Y2+V3*X2)*XSUR6
        ENDDO
        ELSE
          WRITE(LU,*) 'WRONG DISCRETISATION OF U IN VC04AA'
          CALL PLANTE(1)
          STOP
        ENDIF
C
        ELSE
          IF(LNG.EQ.1) WRITE(LU,500) SV%DIMDISC
          IF(LNG.EQ.1) WRITE(LU,300)
          IF(LNG.EQ.2) WRITE(LU,501) SV%DIMDISC
          IF(LNG.EQ.2) WRITE(LU,301)
500       FORMAT(1X,'VC04AA : V%DIMDISC= ',1I6)
501       FORMAT(1X,'VC04AA: V%DIMDISC=',1I6)
          CALL PLANTE(1)
          STOP
        ENDIF
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