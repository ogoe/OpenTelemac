C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR:
!>  @code
!>                        /   1                  DF      DF
!>      V      =  XMUL   /   ---  PSI(IH)  * ( U --  + V -- )   D(OMEGA)
!>       IH,P*          /P*  DZ*                 DX      DY<br><br>
!>    PSI(I) IS A 2D P1 BASE ON THE BASE TRIANGLE OF THE PRISM!<br>
!>    F AND U ARE 3D LINEAR BY PRISM
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  THE RESULT IS IN W IN NOT ASSEMBLED FORM

!>  @warning  COMPUTATION IN THE TRANSFORMED MESH !

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, IKLE1, IKLE2, IKLE3, IKLE4, IKLE5, IKLE6, NELEM, NELMAX, SF, SU, SV, U, V, W1, W2, W3, X, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DEN, F1, F2, F3, F4, F5, F6, HU1I3S, HU1S3I, HU1SI, HU2I3S, HU2S3I, HU2SI, HU3I3S, HU3S3I, HU3SI, HV1I3S, HV1S3I, HV1SI, HV2I3S, HV2S3I, HV2SI, HV3I3S, HV3S3I, HV3SI, I1, I2, I3, I4, I5, I6, IELEM, IELMF, IELMU, IELMV, PX1, PX2, PX3, PXFI, PXFS, PY1, PY2, PY3, PYFI, PYFS, SHUI1, SHUI2, SHUI3, SHUS1, SHUS2, SHUS3, SHVI1, SHVI2, SHVI3, SHVS1, SHVS2, SHVS3, SUI, SUS, SVI, SVS, U1, U2, U3, U4, U5, U6, V1, V2, V3, V4, V5, V6, X2, X3, Y2, Y3
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
!>      <td><center> 5.5                                       </center>
!> </td><td> 15/11/04
!> </td><td> A. DECOENE (INRIA-LNHE)
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
!>          <tr><td>IKLE5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
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
!>          <tr><td>W1,2,3
!></td><td>--></td><td>VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
!>    </td></tr>
!>          <tr><td>W2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DES POINTS
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VC18PP
     &( XMUL,SF,SU,SV,F,U,V,X,Y,Z,
     &  IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,NELEM,NELMAX,
     &  W1,W2,W3)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE.
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| IKLE5          |---| 
C| IKLE6          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DES FONCTIONS F,G ET H
C| SU,SV,SW       |-->| STRUCTURES DES FONCTIONS U,V ET W
C| SURFAC         |-->| SURFACE DES ELEMENTS.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR
C|                |   | INTERVENANT DANS LA FORMULE.
C| W1,2,3         |-->| VECTEUR RESULTAT SOUS FORME NON ASSEMBLEE.
C| W2             |---| 
C| W3             |---| 
C| X,Y,Z          |-->| COORDONNEES DES POINTS
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF !, EX_VC18PP => VC18PP
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE1(NELMAX),IKLE2(NELMAX),IKLE3(NELMAX)
      INTEGER, INTENT(IN) :: IKLE4(NELMAX),IKLE5(NELMAX),IKLE6(NELMAX)
C
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),Z(*),XMUL
      DOUBLE PRECISION, INTENT(INOUT) ::W1(NELMAX),W2(NELMAX),W3(NELMAX)
C
C     STRUCTURES OF F, U, V AND REAL DATA
C
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SU,SV
      DOUBLE PRECISION, INTENT(IN) :: F(*),U(*),V(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IELMF,IELMU,IELMV
      DOUBLE PRECISION X2,X3,Y2,Y3,DEN
      DOUBLE PRECISION PX1,PX2,PX3,PY1,PY2,PY3
      DOUBLE PRECISION PXFI,PXFS,PYFI,PYFS
      DOUBLE PRECISION U1,U2,U3,U4,U5,U6,V1,V2,V3,V4,V5,V6
      DOUBLE PRECISION F1,F2,F3,F4,F5,F6
      DOUBLE PRECISION SUS,SUI
      DOUBLE PRECISION SHUI1,SHUS1,SHUI2,SHUS2,SHUI3,SHUS3
      DOUBLE PRECISION HU1S3I,HU1I3S,HU1SI,HU2S3I,HU2I3S,HU2SI
      DOUBLE PRECISION HU3S3I,HU3I3S,HU3SI
      DOUBLE PRECISION SVS,SVI
      DOUBLE PRECISION SHVI1,SHVS1,SHVI2,SHVS2,SHVI3,SHVS3
      DOUBLE PRECISION HV1S3I,HV1I3S,HV1SI,HV2S3I,HV2I3S,HV2SI
      DOUBLE PRECISION HV3S3I,HV3I3S,HV3SI
C
      INTEGER I1,I2,I3,I4,I5,I6
C
C**********************************************************************
C
      DEN = XMUL/1440.D0
C
      IELMF=SF%ELM
      IELMU=SU%ELM
      IELMV=SV%ELM
C
C-----------------------------------------------------------------------
C
C     FUNCTION F AND VECTOR U ARE LINEAR
C
      IF(IELMU.EQ.41.AND.IELMV.EQ.41.AND.IELMF.EQ.41) THEN
C
C        LOOP ON THE ELEMENTS
C
         DO 2 IELEM = 1,NELEM
C
            I1 = IKLE1(IELEM)
            I2 = IKLE2(IELEM)
            I3 = IKLE3(IELEM)
            I4 = IKLE4(IELEM)
            I5 = IKLE5(IELEM)
            I6 = IKLE6(IELEM)
C
            X2  =   X(I2) - X(I1)
            X3  =   X(I3) - X(I1)
            Y2  =   Y(I2) - Y(I1)
            Y3  =   Y(I3) - Y(I1)
C
            U1  =  U(I1)
            U2  =  U(I2)
            U3  =  U(I3)
            U4  =  U(I4)
            U5  =  U(I5)
            U6  =  U(I6)
            V1  =  V(I1)
            V2  =  V(I2)
            V3  =  V(I3)
            V4  =  V(I4)
            V5  =  V(I5)
            V6  =  V(I6)
            F1  =  F(I1)
            F2  =  F(I2)
            F3  =  F(I3)
            F4  =  F(I4)
            F5  =  F(I5)
            F6  =  F(I6)
C
C    INTERMEDIATE COMPUTATIONS
C
            PX2=Y3*DEN
            PX3=-Y2*DEN
            PX1=-PX2-PX3
            PY2=-X3*DEN
            PY3=X2*DEN
            PY1=-PY3-PY2
C
            SUS    = U6+U5+U4
            SUI    = U3+U2+U1
            SHUS1  = 5 * (SUS+U4)
            SHUI1  = 5 * (SUI+U1)
            SHUS2  = 5 * (SUS+U5)
            SHUI2  = 5 * (SUI+U2)
            SHUS3  = 5 * (SUS+U6)
            SHUI3  = 5 * (SUI+U3)
            HU1S3I = SHUS1 + 3*SHUI1
            HU1I3S = SHUI1 + 3*SHUS1
            HU1SI  = SHUS1 +   SHUI1
            HU2S3I = SHUS2 + 3*SHUI2
            HU2I3S = SHUI2 + 3*SHUS2
            HU2SI  = SHUS2 +   SHUI2
            HU3S3I = SHUS3 + 3*SHUI3
            HU3I3S = SHUI3 + 3*SHUS3
            HU3SI  = SHUS3 +   SHUI3
C
            SVS    = V6+V5+V4
            SVI    = V3+V2+V1
            SHVS1  = 5 * (SVS+V4)
            SHVI1  = 5 * (SVI+V1)
            SHVS2  = 5 * (SVS+V5)
            SHVI2  = 5 * (SVI+V2)
            SHVS3  = 5 * (SVS+V6)
            SHVI3  = 5 * (SVI+V3)
            HV1S3I = SHVS1 + 3*SHVI1
            HV1I3S = SHVI1 + 3*SHVS1
            HV1SI  = SHVS1 +   SHVI1
            HV2S3I = SHVS2 + 3*SHVI2
            HV2I3S = SHVI2 + 3*SHVS2
            HV2SI  = SHVS2 +   SHVI2
            HV3S3I = SHVS3 + 3*SHVI3
            HV3I3S = SHVI3 + 3*SHVS3
            HV3SI  = SHVS3 +   SHVI3
C
            PXFI=PX1*F1+PX2*F2+PX3*F3
            PXFS=PX1*F4+PX2*F5+PX3*F6
            PYFI=PY1*F1+PY2*F2+PY3*F3
            PYFS=PY1*F4+PY2*F5+PY3*F6
C
            W1(IELEM)= PXFI * (HU1S3I+HU1SI)
     &               + PYFI * (HV1S3I+HV1SI)
     &               + PXFS * (HU1SI+HU1I3S)
     &               + PYFS * (HV1SI+HV1I3S)
C
            W2(IELEM)= PXFI * (HU2S3I+HU2SI)
     &               + PYFI * (HV2S3I+HV2SI)
     &               + PXFS * (HU2SI+HU2I3S)
     &               + PYFS * (HV2SI+HV2I3S)
C
            W3(IELEM)= PXFI * (HU3S3I+HU3SI)
     &               + PYFI * (HV3S3I+HV3SI)
     &               + PXFS * (HU3SI+HU3I3S)
     &               + PYFS * (HV3SI+HV3I3S)
C
2        CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSE
C
C-----------------------------------------------------------------------
C
         IF (LNG.EQ.1) WRITE(LU,100) IELMF,SF%NAME
         IF (LNG.EQ.1) WRITE(LU,200) IELMU,SU%NAME
         IF (LNG.EQ.1) WRITE(LU,300)
         IF (LNG.EQ.2) WRITE(LU,101) IELMF,SF%NAME
         IF (LNG.EQ.2) WRITE(LU,201) IELMU,SU%NAME
         IF (LNG.EQ.2) WRITE(LU,301)
100      FORMAT(1X,'VC18PP (BIEF) :',/,
     &          1X,'DISCRETISATION DE F : ',1I6,
     &          1X,'NOM REEL : ',A6)
200      FORMAT(1X,'DISCRETISATION DE U : ',1I6,
     &          1X,'NOM REEL : ',A6)
300      FORMAT(1X,'CAS NON PREVU')
101      FORMAT(1X,'VC18PP (BIEF) :',/,
     &          1X,'DISCRETIZATION OF F:',1I6,
     &          1X,'REAL NAME: ',A6)
201      FORMAT(1X,'DISCRETIZATION OF U:',1I6,
     &          1X,'REAL NAME: ',A6)
301      FORMAT(1X,'CASE NOT IMPLEMENTED')
         CALL PLANTE(1)
         STOP
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