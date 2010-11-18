C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENTS OF THE FOLLOWING MATRIX:
!>  @code<br>
!>                  /                     D
!>  A(I,J)=-XMUL   /  PSI2(J) *    F    * --( PSI1(I) ) D(OMEGA)
!>                /OMEGA                  DX<br>
!>  BEWARE THE MINUS SIGN !!<br>
!>  PSI1 AND PSI2: BASES OF TYPE P1 PRISM
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  NEED TO CHECK THE SIGN; SEE USE IN DIFF3D!!!!!!!!!!!!!!!!

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, IKLE, NELEM, NELMAX, SF, SURFAC, T, XM, XMUL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I1, I2, I3, I4, I5, I6, IELEM, PZ1, Q1, Q2, Q3, Q4, Q5, Q6, W14, W25, W36, W41, W52, W63, XSU360
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT08PP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MATRIY()

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
!> </td><td> 28/11/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LE CALCUL DE LA
!>                  MATRICE.
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE1
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>SF,SG,SH
!></td><td>--></td><td>STRUCTURES DE F,G ET H.
!>    </td></tr>
!>          <tr><td>SU,SV,SW
!></td><td>--></td><td>STRUCTURES DE U,V ET W.
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES TRIANGLES.
!>    </td></tr>
!>          <tr><td>T,XM
!></td><td><--</td><td>ELEMENTS DE LA MATRICE
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
!>                  CALCUL DE LA MATRICE.
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT08PP
     &( T,XM,XMUL,SF,F,SURFAC,IKLE,NELEM,NELMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| IKLE           |---| 
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DE F,G ET H.
C| SU,SV,SW       |-->| STRUCTURES DE U,V ET W.
C| SURFAC         |-->| SURFACE DES TRIANGLES.
C| T,XM           |<--| ELEMENTS DE LA MATRICE
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR INTERVENANT DANS LE
C|                |   | CALCUL DE LA MATRICE.
C| X,Y,Z          |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT08PP => MT08PP
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
C
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6),XM(NELMAX,30)
C
      DOUBLE PRECISION, INTENT(IN) :: XMUL
      DOUBLE PRECISION, INTENT(IN) :: F(*)
C
C     STRUCTURE OF F
C
      TYPE(BIEF_OBJ), INTENT(IN) :: SF
C
      DOUBLE PRECISION, INTENT(IN) :: SURFAC(NELMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      DOUBLE PRECISION PZ1,XSU360
      DOUBLE PRECISION Q1,Q2,Q3,Q4,Q5,Q6
      DOUBLE PRECISION W14,W41,W25,W52,W63,W36
C
      INTEGER I1,I2,I3,I4,I5,I6,IELEM
C
C**********************************************************************
C
      XSU360 = XMUL/360.D0
C
      IF(SF%ELM.NE.41) THEN
        IF (LNG.EQ.1) WRITE(LU,1000) SF%ELM
        IF (LNG.EQ.2) WRITE(LU,1001) SF%ELM
1000    FORMAT(1X,'MT08PP (BIEF) : TYPE DE F NON PREVU : ',I6)
1001    FORMAT(1X,'MT08PP (BIEF) : TYPE OF F NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C   LOOP ON THE ELEMENTS
C
      DO IELEM=1,NELEM
C
         I1 = IKLE(IELEM,1)
         I2 = IKLE(IELEM,2)
         I3 = IKLE(IELEM,3)
         I4 = IKLE(IELEM,4)
         I5 = IKLE(IELEM,5)
         I6 = IKLE(IELEM,6)
C
         Q1  =  F(I1)
         Q2  =  F(I2)
         Q3  =  F(I3)
         Q4  =  F(I4)
         Q5  =  F(I5)
         Q6  =  F(I6)
C
C    INTERMEDIATE COMPUTATIONS
C
         PZ1=-XSU360*SURFAC(IELEM)
C
         W14 = Q1+2*Q4
         W41 = Q4+2*Q1
         W25 = Q2+2*Q5
         W52 = Q5+2*Q2
         W63 = Q6+2*Q3
         W36 = Q3+2*Q6
C
         T(IELEM,1)=PZ1*2*(3*W41+W52+W63)
         XM(IELEM,18)=-T(IELEM,1)
         XM(IELEM,16)=PZ1*(2*(W41+W52)+W63)
         XM(IELEM,19)=-XM(IELEM,16)
         XM(IELEM,1) = XM(IELEM,16)
         XM(IELEM,22)=-XM(IELEM,16)
         XM(IELEM,2)=PZ1*(2*(W41+W63)+W52)
         XM(IELEM,20)=-XM(IELEM,2)
         XM(IELEM,17)= XM(IELEM,2)
         XM(IELEM,25)=-XM(IELEM,2)
         T(IELEM,2)=PZ1*2*(W41+3*W52+W63)
         XM(IELEM,23)= -T(IELEM,2)
         XM(IELEM,21)=PZ1*(2*(W52+W63)+W41)
         XM(IELEM,24)=-XM(IELEM,21)
         XM(IELEM,6) = XM(IELEM,21)
         XM(IELEM,26)=-XM(IELEM,21)
         T(IELEM,3)=PZ1*2*(W41+W52+3*W63)
         XM(IELEM,27)=-T(IELEM,3)
         XM(IELEM,3)=PZ1*2*(3*W14+W25+W36)
         T(IELEM,4)=-XM(IELEM,3)
         XM(IELEM,7)=PZ1*(2*(W14+W25)+W36)
         XM(IELEM,28)=-XM(IELEM,7)
         XM(IELEM,4) = XM(IELEM,7)
         XM(IELEM,13)=-XM(IELEM,7)
         XM(IELEM,10)=PZ1*(2*(W14+W36)+W25)
         XM(IELEM,29)=-XM(IELEM,10)
         XM(IELEM,5) = XM(IELEM,10)
         XM(IELEM,14)=-XM(IELEM,10)
         XM(IELEM,8)=PZ1*2*(W14+3*W25+W36)
         T(IELEM,5)=-XM(IELEM,8)
         XM(IELEM,11)=PZ1*(2*(W25+W36)+W14)
         XM(IELEM,30)=-XM(IELEM,11)
         XM(IELEM,9) = XM(IELEM,11)
         XM(IELEM,15)=-XM(IELEM,11)
         XM(IELEM,12)=PZ1*2*(W14+W25+3*W36)
         T(IELEM,6)=-XM(IELEM,12)
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C