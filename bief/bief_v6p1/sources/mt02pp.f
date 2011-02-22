C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE DIFFUSION MATRIX.
!><br>            THE FUNCTION DIFFUSION COEFFICIENT IS HERE A P1
!>                DIAGONAL TENSOR.
!><br>            CASE OF THE PRISM.
!>  @code
!>     STORAGE CONVENTION FOR EXTRA-DIAGONAL TERMS:
!>
!>     XM(IELEM, 1)  ---->  M(1,2) = M(2,1)
!>     XM(IELEM, 2)  ---->  M(1,3) = M(3,1)
!>     XM(IELEM, 3)  ---->  M(1,4) = M(4,1)
!>     XM(IELEM, 4)  ---->  M(1,5) = M(5,1)
!>     XM(IELEM, 5)  ---->  M(1,6) = M(6,1)
!>     XM(IELEM, 6)  ---->  M(2,3) = M(3,2)
!>     XM(IELEM, 7)  ---->  M(2,4) = M(4,2)
!>     XM(IELEM, 8)  ---->  M(2,5) = M(5,2)
!>     XM(IELEM, 9)  ---->  M(2,6) = M(6,2)
!>     XM(IELEM,10)  ---->  M(3,4) = M(4,3)
!>     XM(IELEM,11)  ---->  M(3,5) = M(5,3)
!>     XM(IELEM,12)  ---->  M(3,6) = M(6,3)
!>     XM(IELEM,13)  ---->  M(4,5) = M(5,4)
!>     XM(IELEM,14)  ---->  M(4,6) = M(6,4)
!>     XM(IELEM,15)  ---->  M(5,6) = M(6,5)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

!>  @warning  JMH : THIS IS NOT DONE AS IN 2D, TO BE MODIFIED

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, FORMUL, G, H, IKLE, INCHYD, NELEM, NELMAX, SF, SG, SH, SURFAC, T, X, XM, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> D, D1, D12, D13, D2, D23, D3, EPSILON, H1, H2, H3, HH12, HH13, HH23, HHI12, HHI13, HHI23, HHS12, HHS13, HHS23, HRI1, HRI2, HRI3, HRS1, HRS2, HRS3, I1, I2, I3, I4, I5, I6, IELEM, II4, II5, II6, NF1, NF2, NF3, NF4, NF5, NF6, NG1, NG2, NG3, NG4, NG5, NG6, NH1, NH2, NH3, NPOU0, R, RI, RR11, RR12, RR13, RR22, RR23, RR33, RRI, RRRI, RRRS, RRS, RS, SH1, SHH, SNHH, SNHHI, SNHHS, SNHI, SNHI1, SNHI2, SNHI3, SNHS, SNHS1, SNHS2, SNHS3, SNI, SNS, XS06, XS2880
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT02PP
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
!>      <td><center> 6.0                                       </center>
!> </td><td> 20/05/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 15/03/2010
!> </td><td> JMH
!> </td><td> PARAMETER EPSILON ADDED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 06/02/07
!> </td><td>
!> </td><td> IF FORMUL(14:16)='MON' :
!>           MONOTONY ALSO ENSURED IN X AND Y (POSITIVE EXTRADIAGONAL
!>           TERMS REMOVED)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 22/08/07
!> </td><td>
!> </td><td> CAN OPTIONALLY TREAT (USES DIMDISC) A P0 VERTICAL
!>           DIFFUSION ON THE VERTICAL
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A11,A12
!></td><td><--</td><td>ELEMENTS DE LA MATRICE
!>    </td></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LE CALCUL DE LA
!>                  MATRICE.
!>    </td></tr>
!>          <tr><td>FORMUL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE1
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!>    </td></tr>
!>          <tr><td>INCHYD
!></td><td>---</td><td>
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
!>          <tr><td>SURFAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>FACTEUR MULTIPLICATIF
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MT02PP
     &(T,XM,XMUL,SF,SG,SH,F,G,H,X,Y,Z,SURFAC,IKLE,NELEM,NELMAX,INCHYD,
     & FORMUL,NPLAN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| FORMUL         |---| 
C| IKLE           |---| 
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| INCHYD         |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DE F,G ET H.
C| SURFAC         |---| 
C| T             |---| 
C| X,Y,Z          |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XM             |---| 
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT02PP => MT02PP
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPLAN
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
C
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6),XM(NELMAX,15)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
C
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
      DOUBLE PRECISION, INTENT(IN)    :: F(*),G(*),H(*)
C
C     STRUCTURES OF F, G, H
C
      TYPE(BIEF_OBJ), INTENT(IN)      :: SF,SG,SH
C
      DOUBLE PRECISION, INTENT(IN)    :: X(*),Y(*),Z(*)
C
      LOGICAL, INTENT(IN)             :: INCHYD
      CHARACTER(LEN=16), INTENT(IN)   :: FORMUL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     DECLARATIONS SPECIFIC TO THIS SUBROUTINE
C
      DOUBLE PRECISION H1,H2,H3,RI,RS,R,RRI,RRS,RRRI,RRRS
      DOUBLE PRECISION D1,D2,D3,D12,D13,D23,D
      DOUBLE PRECISION SH1,SHH,SNI,SNS,SNHI,SNHS,SNHHI,SNHHS,SNHH
      DOUBLE PRECISION SNHI1,SNHS1,SNHI2,SNHS2,SNHI3,SNHS3
      DOUBLE PRECISION HHI12,HHS12,HH12,HHI13,HHS13,HH13
      DOUBLE PRECISION HHI23,HHS23,HH23
      DOUBLE PRECISION HRI1,HRS1,HRI2,HRS2,HRI3,HRS3
      DOUBLE PRECISION RR11,RR22,RR33,RR12,RR13,RR23
      DOUBLE PRECISION NF1,NF2,NF3,NF4,NF5,NF6
      DOUBLE PRECISION NG1,NG2,NG3,NG4,NG5,NG6
      DOUBLE PRECISION NH1,NH2,NH3,XS06,XS2880
C
      INTEGER I1,I2,I3,I4,I5,I6,IELEM,II4,II5,II6,NPOU0
C
      DOUBLE PRECISION EPSILON
      DATA EPSILON/1.D-4/
C
C***********************************************************************
C
      XS06=XMUL/6.D0
      XS2880=XMUL/2880.D0
C
      IF(SF%ELM.NE.41) THEN
        IF (LNG.EQ.1) WRITE(LU,1000) SF%ELM
        IF (LNG.EQ.2) WRITE(LU,1001) SF%ELM
1000    FORMAT(1X,'MT02PP (BIEF) : TYPE DE F NON PREVU : ',I6)
1001    FORMAT(1X,'MT02PP (BIEF) : TYPE OF F NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(SG%ELM.NE.41) THEN
        IF (LNG.EQ.1) WRITE(LU,2000) SG%ELM
        IF (LNG.EQ.2) WRITE(LU,2001) SG%ELM
2000    FORMAT(1X,'MT02PP (BIEF) : TYPE DE G NON PREVU : ',I6)
2001    FORMAT(1X,'MT02PP (BIEF) : TYPE OF G NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(SH%ELM.NE.41) THEN
        IF (LNG.EQ.1) WRITE(LU,2000) SH%ELM
        IF (LNG.EQ.2) WRITE(LU,2001) SH%ELM
3000    FORMAT(1X,'MT02PP (BIEF) : TYPE DE H NON PREVU : ',I6)
3001    FORMAT(1X,'MT02PP (BIEF) : TYPE OF H NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     SEE VISCLM OF TELEMAC-3D
C     FOR THE TREATMENT OF P0 VERTICAL VISCOSITY ON THE VERTICAL
C
      IF(SH%DIMDISC.EQ.0) THEN
C       P1 VERTICAL VISCOSITY
        NPOU0=SH%DIM1/NPLAN
      ELSEIF(SH%DIMDISC.EQ.4111) THEN
C       P0 VERTICAL VISCOSITY ON THE VERTICAL (SEE II4,5,6)
        NPOU0=0
      ELSE
        IF (LNG.EQ.1) WRITE(LU,4000) SH%DIMDISC
        IF (LNG.EQ.2) WRITE(LU,4001) SH%DIMDISC
4000    FORMAT(1X,'MT02PP (BIEF) : DIMDISC DE H NON PREVU : ',I6)
4001    FORMAT(1X,'MT02PP (BIEF): DIMDISC OF H NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     VERSION WITH TREATMENT OF HYDROSTATIC INCONSISTENCIES
C
      IF(INCHYD) THEN
C
C-----------------------------------------------------------------------
C
C     DIFFUSION ALONG X
C
C-----------------------------------------------------------------------
C
C   LOOP ON THE ELEMENTS
C
      DO IELEM=1,NELEM
C
         I1=IKLE(IELEM,1)
         I2=IKLE(IELEM,2)
         I3=IKLE(IELEM,3)
         I4=IKLE(IELEM,4)
         I5=IKLE(IELEM,5)
         I6=IKLE(IELEM,6)
C        DEPENDING ON NPOU0, II4 WILL BE I4 OR I1, ETC
         II4=I1+NPOU0
         II5=I2+NPOU0
         II6=I3+NPOU0
C
         H1=Z(I4)-Z(I1)
         H2=Z(I5)-Z(I2)
         H3=Z(I6)-Z(I3)
C
         SH1=H1+H2+H3
         SHH=H1*H1+H2*H2+H3*H3
C
         D1=Y(I2)-Y(I3)
         D2=Y(I3)-Y(I1)
         D3=Y(I1)-Y(I2)
C
         D=XS2880/SURFAC(IELEM)
C
         RI=-(Z(I1)*D1+Z(I2)*D2+Z(I3)*D3)
         RS=-(Z(I4)*D1+Z(I5)*D2+Z(I6)*D3)
         R=RI+RS
         RRI=R+RI+RI
         RRS=R+RS+RS
         RRRI=R*R+2*RI*RI
         RRRS=R*R+2*RS*RS
C
         D12=D1*D2
         D13=D1*D3
         D23=D2*D3
C
         IF(MAX(Z(I1),Z(I2),Z(I3)).GT.MIN(Z(I4),Z(I5),Z(I6)).OR.
     &      H1.LT.EPSILON.OR.H2.LT.EPSILON.OR.H3.LT.EPSILON ) THEN
           NF1=0.D0
           NF2=0.D0
           NF3=0.D0
           NF4=0.D0
           NF5=0.D0
           NF6=0.D0
           NG1=0.D0
           NG2=0.D0
           NG3=0.D0
           NG4=0.D0
           NG5=0.D0
           NG6=0.D0
           NH1=0.D0
           NH2=0.D0
           NH3=0.D0
         ELSE
           NF1=F(I1)/H1
           NF2=F(I2)/H2
           NF3=F(I3)/H3
           NF4=F(I4)/H1
           NF5=F(I5)/H2
           NF6=F(I6)/H3
           NG1=G(I1)/H1
           NG2=G(I2)/H2
           NG3=G(I3)/H3
           NG4=G(I4)/H1
           NG5=G(I5)/H2
           NG6=G(I6)/H3
C          DEPENDING ON THE CASE (II4=I1 OR I4, ETC.)
C          ALTERNATIVE WITH VERTICAL LINEAR VISCOSITY (II4=I4)
C          ALTERNATIVE WITH P0 VERTICAL VISCOSITY ON THE VERTICAL (II4=I1)
           NH1=(H(I1)+H(II4))/H1
           NH2=(H(I2)+H(II5))/H2
           NH3=(H(I3)+H(II6))/H3
         ENDIF
C
         SNI=NF1+NF2+NF3
         SNS=NF4+NF5+NF6
         SNHI=NF1*H1+NF2*H2+NF3*H3
         SNHS=NF4*H1+NF5*H2+NF6*H3
         SNHHI=(SNI*SH1+SNHI+SNHI)*SH1+SNI*SHH
     &        +2*(NF1*H1*H1+NF2*H2*H2+NF3*H3*H3)
         SNHHS=(SNS*SH1+SNHS+SNHS)*SH1+SNS*SHH
     &        +2*(NF4*H1*H1+NF5*H2*H2+NF6*H3*H3)
         SNHH=SNHHI+SNHHS
         SNHHI=SNHH+SNHHI+SNHHI
         SNHHS=SNHH+SNHHS+SNHHS
C
         HHI12=SNHHI*D12
         HHI13=SNHHI*D13
         HHI23=SNHHI*D23
         HHS12=SNHHS*D12
         HHS13=SNHHS*D13
         HHS23=SNHHS*D23
         HH12=SNHH*D12
         HH13=SNHH*D13
         HH23=SNHH*D23
C
         SNHI1=(SNI+NF1)*(SH1+H1)+SNHI+NF1*H1
         SNHS1=(SNS+NF4)*(SH1+H1)+SNHS+NF4*H1
         SNHI2=(SNI+NF2)*(SH1+H2)+SNHI+NF2*H2
         SNHS2=(SNS+NF5)*(SH1+H2)+SNHS+NF5*H2
         SNHI3=(SNI+NF3)*(SH1+H3)+SNHI+NF3*H3
         SNHS3=(SNS+NF6)*(SH1+H3)+SNHS+NF6*H3
C
         HRI1=RRI*SNHI1+R*SNHS1
         HRS1=RRS*SNHS1+R*SNHI1
         HRI2=RRI*SNHI2+R*SNHS2
         HRS2=RRS*SNHS2+R*SNHI2
         HRI3=RRI*SNHI3+R*SNHS3
         HRS3=RRS*SNHS3+R*SNHI3
C
         RR11=2*(RRRI*(SNI+NF1+NF1)+RRRS*(SNS+NF4+NF4))
         RR22=2*(RRRI*(SNI+NF2+NF2)+RRRS*(SNS+NF5+NF5))
         RR33=2*(RRRI*(SNI+NF3+NF3)+RRRS*(SNS+NF6+NF6))
         RR12=   RRRI*(SNI+NF1+NF2)+RRRS*(SNS+NF4+NF5)
         RR13=   RRRI*(SNI+NF1+NF3)+RRRS*(SNS+NF4+NF6)
         RR23=   RRRI*(SNI+NF2+NF3)+RRRS*(SNS+NF5+NF6)
C
C        EXTRA-DIAGONAL TERMS
C
         XM(IELEM, 1)=D*( HHI12  -D1*HRI2-D2*HRI1 +RR12)
         XM(IELEM, 2)=D*( HHI13  -D1*HRI3-D3*HRI1 +RR13)
         XM(IELEM, 3)=D*(-HH12-HH13+D1*(HRI1-HRS1)-RR11)
         XM(IELEM, 4)=D*( HH12   +D1*HRI2-D2*HRS1 -RR12)
         XM(IELEM, 5)=D*( HH13   +D1*HRI3-D3*HRS1 -RR13)
         XM(IELEM, 6)=D*( HHI23  -D2*HRI3-D3*HRI2 +RR23)
         XM(IELEM, 7)=D*( HH12   +D2*HRI1-D1*HRS2 -RR12)
         XM(IELEM, 8)=D*(-HH12-HH23+D2*(HRI2-HRS2)-RR22)
         XM(IELEM, 9)=D*( HH23   +D2*HRI3-D3*HRS2 -RR23)
         XM(IELEM,10)=D*( HH13   +D3*HRI1-D1*HRS3 -RR13)
         XM(IELEM,11)=D*( HH23   +D3*HRI2-D2*HRS3 -RR23)
         XM(IELEM,12)=D*(-HH13-HH23+D3*(HRI3-HRS3)-RR33)
         XM(IELEM,13)=D*( HHS12  +D1*HRS2+D2*HRS1 +RR12)
         XM(IELEM,14)=D*( HHS13  +D1*HRS3+D3*HRS1 +RR13)
         XM(IELEM,15)=D*( HHS23  +D2*HRS3+D3*HRS2 +RR23)
C
C-----------------------------------------------------------------------
C
C        DIFFUSION ALONG Y
C
C-----------------------------------------------------------------------
C
         D1=X(I3)-X(I2)
         D2=X(I1)-X(I3)
         D3=X(I2)-X(I1)
C
         RI=-(Z(I1)*D1+Z(I2)*D2+Z(I3)*D3)
         RS=-(Z(I4)*D1+Z(I5)*D2+Z(I6)*D3)
         R=RI+RS
         RRI=R+RI+RI
         RRS=R+RS+RS
         RRRI=R*R+2*RI*RI
         RRRS=R*R+2*RS*RS
C
         D12=D1*D2
         D13=D1*D3
         D23=D2*D3
C
         SNI=NG1+NG2+NG3
         SNS=NG4+NG5+NG6
         SNHI=NG1*H1+NG2*H2+NG3*H3
         SNHS=NG4*H1+NG5*H2+NG6*H3
         SNHHI=(SNI*SH1+SNHI+SNHI)*SH1+SNI*SHH
     &        +2*(NG1*H1*H1+NG2*H2*H2+NG3*H3*H3)
         SNHHS=(SNS*SH1+SNHS+SNHS)*SH1+SNS*SHH
     &        +2*(NG4*H1*H1+NG5*H2*H2+NG6*H3*H3)
         SNHH=SNHHI+SNHHS
         SNHHI=SNHH+SNHHI+SNHHI
         SNHHS=SNHH+SNHHS+SNHHS
C
         HHI12=SNHHI*D12
         HHI13=SNHHI*D13
         HHI23=SNHHI*D23
         HHS12=SNHHS*D12
         HHS13=SNHHS*D13
         HHS23=SNHHS*D23
         HH12=SNHH*D12
         HH13=SNHH*D13
         HH23=SNHH*D23
C
         SNHI1=(SNI+NG1)*(SH1+H1)+SNHI+NG1*H1
         SNHS1=(SNS+NG4)*(SH1+H1)+SNHS+NG4*H1
         SNHI2=(SNI+NG2)*(SH1+H2)+SNHI+NG2*H2
         SNHS2=(SNS+NG5)*(SH1+H2)+SNHS+NG5*H2
         SNHI3=(SNI+NG3)*(SH1+H3)+SNHI+NG3*H3
         SNHS3=(SNS+NG6)*(SH1+H3)+SNHS+NG6*H3
C
         HRI1=RRI*SNHI1+R*SNHS1
         HRS1=RRS*SNHS1+R*SNHI1
         HRI2=RRI*SNHI2+R*SNHS2
         HRS2=RRS*SNHS2+R*SNHI2
         HRI3=RRI*SNHI3+R*SNHS3
         HRS3=RRS*SNHS3+R*SNHI3
C
         RR11=2*(RRRI*(SNI+NG1+NG1)+RRRS*(SNS+NG4+NG4))
         RR22=2*(RRRI*(SNI+NG2+NG2)+RRRS*(SNS+NG5+NG5))
         RR33=2*(RRRI*(SNI+NG3+NG3)+RRRS*(SNS+NG6+NG6))
         RR12=   RRRI*(SNI+NG1+NG2)+RRRS*(SNS+NG4+NG5)
         RR13=   RRRI*(SNI+NG1+NG3)+RRRS*(SNS+NG4+NG6)
         RR23=   RRRI*(SNI+NG2+NG3)+RRRS*(SNS+NG5+NG6)
C
C        EXTRA-DIAGONAL TERMS
C
         XM(IELEM, 1)=XM(IELEM, 1)+D*( HHI12  -D1*HRI2-D2*HRI1 +RR12)
         XM(IELEM, 2)=XM(IELEM, 2)+D*( HHI13  -D1*HRI3-D3*HRI1 +RR13)
         XM(IELEM, 3)=XM(IELEM, 3)+D*(-HH12-HH13+D1*(HRI1-HRS1)-RR11)
         XM(IELEM, 4)=XM(IELEM, 4)+D*( HH12   +D1*HRI2-D2*HRS1 -RR12)
         XM(IELEM, 5)=XM(IELEM, 5)+D*( HH13   +D1*HRI3-D3*HRS1 -RR13)
         XM(IELEM, 6)=XM(IELEM, 6)+D*( HHI23  -D2*HRI3-D3*HRI2 +RR23)
         XM(IELEM, 7)=XM(IELEM, 7)+D*( HH12   +D2*HRI1-D1*HRS2 -RR12)
         XM(IELEM, 8)=XM(IELEM, 8)+D*(-HH12-HH23+D2*(HRI2-HRS2)-RR22)
         XM(IELEM, 9)=XM(IELEM, 9)+D*( HH23   +D2*HRI3-D3*HRS2 -RR23)
         XM(IELEM,10)=XM(IELEM,10)+D*( HH13   +D3*HRI1-D1*HRS3 -RR13)
         XM(IELEM,11)=XM(IELEM,11)+D*( HH23   +D3*HRI2-D2*HRS3 -RR23)
         XM(IELEM,12)=XM(IELEM,12)+D*(-HH13-HH23+D3*(HRI3-HRS3)-RR33)
         XM(IELEM,13)=XM(IELEM,13)+D*( HHS12  +D1*HRS2+D2*HRS1 +RR12)
         XM(IELEM,14)=XM(IELEM,14)+D*( HHS13  +D1*HRS3+D3*HRS1 +RR13)
         XM(IELEM,15)=XM(IELEM,15)+D*( HHS23  +D2*HRS3+D3*HRS2 +RR23)
C
C-----------------------------------------------------------------------
C
C        DIFFUSION ALONG Z
C
C-----------------------------------------------------------------------
C
C        VERSION WITH SIMPLIFICATIONS TO ACHIEVE MONOTONY OF THE MATRIX
C
         D=SURFAC(IELEM)*XS06
C
C        EXTRA-DIAGONAL TERMS
C
         XM(IELEM, 3)=XM(IELEM, 3)-D*NH1
         XM(IELEM, 8)=XM(IELEM, 8)-D*NH2
         XM(IELEM,12)=XM(IELEM,12)-D*NH3
C
C-----------------------------------------------------------------------
C
C        OLD VERSION OF DIFFUSION ALONG Z
C
C        R=NH1+NH2+NH3+NH4+NH5+NH6
C        D=((X(I2)-X(I1))*(Y(I3)-Y(I1))-(X(I3)-X(I1))*(Y(I2)-Y(I1)))
C    *    *XMUL/240.D0
C
C        RR11=(R+NH1+NH1+NH4+NH4)*(D+D)
C        RR22=(R+NH2+NH2+NH5+NH5)*(D+D)
C        RR33=(R+NH3+NH3+NH6+NH6)*(D+D)
C        RR12=(R+NH1+NH2+NH4+NH5)*D
C        RR13=(R+NH1+NH3+NH4+NH6)*D
C        RR23=(R+NH2+NH3+NH5+NH6)*D
C
C        EXTRA-DIAGONAL TERMS
C
C        XM(IELEM, 1)=XM(IELEM, 1)+RR12
C        XM(IELEM, 2)=XM(IELEM, 2)+RR13
C        XM(IELEM, 3)=XM(IELEM, 3)-RR11
C        XM(IELEM, 4)=XM(IELEM, 4)-RR12
C        XM(IELEM, 5)=XM(IELEM, 5)-RR13
C        XM(IELEM, 6)=XM(IELEM, 6)+RR23
C        XM(IELEM, 7)=XM(IELEM, 7)-RR12
C        XM(IELEM, 8)=XM(IELEM, 8)-RR22
C        XM(IELEM, 9)=XM(IELEM, 9)-RR23
C        XM(IELEM,10)=XM(IELEM,10)-RR13
C        XM(IELEM,11)=XM(IELEM,11)-RR23
C        XM(IELEM,12)=XM(IELEM,12)-RR33
C        XM(IELEM,13)=XM(IELEM,13)+RR12
C        XM(IELEM,14)=XM(IELEM,14)+RR13
C        XM(IELEM,15)=XM(IELEM,15)+RR23
C
C
C-----------------------------------------------------------------------
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      ELSE
C
C     VERSION WITHOUT TREATMENT OF HYDROSTATIC INCONSISTENCIES
C
C-----------------------------------------------------------------------
C
C     DIFFUSION ALONG X
C
C-----------------------------------------------------------------------
C
C   LOOP ON THE ELEMENTS
C
      DO IELEM=1,NELEM
C
         I1=IKLE(IELEM,1)
         I2=IKLE(IELEM,2)
         I3=IKLE(IELEM,3)
         I4=IKLE(IELEM,4)
         I5=IKLE(IELEM,5)
         I6=IKLE(IELEM,6)
C        DEPENDING ON NPOU0, II4 WILL BE I4 OR I1, ETC
         II4=I1+NPOU0
         II5=I2+NPOU0
         II6=I3+NPOU0
C
         H1=Z(I4)-Z(I1)
         H2=Z(I5)-Z(I2)
         H3=Z(I6)-Z(I3)
C
         SH1=H1+H2+H3
         SHH=H1*H1+H2*H2+H3*H3
C
         D1=Y(I2)-Y(I3)
         D2=Y(I3)-Y(I1)
         D3=Y(I1)-Y(I2)
C
         D=XS2880/SURFAC(IELEM)
C
         RI=-(Z(I1)*D1+Z(I2)*D2+Z(I3)*D3)
         RS=-(Z(I4)*D1+Z(I5)*D2+Z(I6)*D3)
         R=RI+RS
         RRI=R+RI+RI
         RRS=R+RS+RS
         RRRI=R*R+2*RI*RI
         RRRS=R*R+2*RS*RS
C
         D12=D1*D2
         D13=D1*D3
         D23=D2*D3
C
         IF(H1.LT.EPSILON.OR.H2.LT.EPSILON.OR.H3.LT.EPSILON) THEN
           NF1=0.D0
           NF2=0.D0
           NF3=0.D0
           NF4=0.D0
           NF5=0.D0
           NF6=0.D0
           NG1=0.D0
           NG2=0.D0
           NG3=0.D0
           NG4=0.D0
           NG5=0.D0
           NG6=0.D0
           NH1=0.D0
           NH2=0.D0
           NH3=0.D0
         ELSE
           NF1=F(I1)/H1
           NF2=F(I2)/H2
           NF3=F(I3)/H3
           NF4=F(I4)/H1
           NF5=F(I5)/H2
           NF6=F(I6)/H3
           NG1=G(I1)/H1
           NG2=G(I2)/H2
           NG3=G(I3)/H3
           NG4=G(I4)/H1
           NG5=G(I5)/H2
           NG6=G(I6)/H3
C          DEPENDING ON THE CASE (II4=I1 OR I4, ETC.)
C          ALTERNATIVE WITH VERTICAL LINEAR VISCOSITY (II4=I4)
C          ALTERNATIVE WITH P0 VERTICAL VISCOSITY ON THE VERTICAL (II4=I1)
           NH1=(H(I1)+H(II4))/H1
           NH2=(H(I2)+H(II5))/H2
           NH3=(H(I3)+H(II6))/H3
         ENDIF
C
         SNI=NF1+NF2+NF3
         SNS=NF4+NF5+NF6
         SNHI=NF1*H1+NF2*H2+NF3*H3
         SNHS=NF4*H1+NF5*H2+NF6*H3
         SNHHI=(SNI*SH1+SNHI+SNHI)*SH1+SNI*SHH
     &        +2*(NF1*H1*H1+NF2*H2*H2+NF3*H3*H3)
         SNHHS=(SNS*SH1+SNHS+SNHS)*SH1+SNS*SHH
     &        +2*(NF4*H1*H1+NF5*H2*H2+NF6*H3*H3)
         SNHH=SNHHI+SNHHS
         SNHHI=SNHH+SNHHI+SNHHI
         SNHHS=SNHH+SNHHS+SNHHS
C
         HHI12=SNHHI*D12
         HHI13=SNHHI*D13
         HHI23=SNHHI*D23
         HHS12=SNHHS*D12
         HHS13=SNHHS*D13
         HHS23=SNHHS*D23
         HH12=SNHH*D12
         HH13=SNHH*D13
         HH23=SNHH*D23
C
         SNHI1=(SNI+NF1)*(SH1+H1)+SNHI+NF1*H1
         SNHS1=(SNS+NF4)*(SH1+H1)+SNHS+NF4*H1
         SNHI2=(SNI+NF2)*(SH1+H2)+SNHI+NF2*H2
         SNHS2=(SNS+NF5)*(SH1+H2)+SNHS+NF5*H2
         SNHI3=(SNI+NF3)*(SH1+H3)+SNHI+NF3*H3
         SNHS3=(SNS+NF6)*(SH1+H3)+SNHS+NF6*H3
C
         HRI1=RRI*SNHI1+R*SNHS1
         HRS1=RRS*SNHS1+R*SNHI1
         HRI2=RRI*SNHI2+R*SNHS2
         HRS2=RRS*SNHS2+R*SNHI2
         HRI3=RRI*SNHI3+R*SNHS3
         HRS3=RRS*SNHS3+R*SNHI3
C
         RR11=2*(RRRI*(SNI+NF1+NF1)+RRRS*(SNS+NF4+NF4))
         RR22=2*(RRRI*(SNI+NF2+NF2)+RRRS*(SNS+NF5+NF5))
         RR33=2*(RRRI*(SNI+NF3+NF3)+RRRS*(SNS+NF6+NF6))
         RR12=   RRRI*(SNI+NF1+NF2)+RRRS*(SNS+NF4+NF5)
         RR13=   RRRI*(SNI+NF1+NF3)+RRRS*(SNS+NF4+NF6)
         RR23=   RRRI*(SNI+NF2+NF3)+RRRS*(SNS+NF5+NF6)
C
C        EXTRA-DIAGONAL TERMS
C
         XM(IELEM, 1)=D*( HHI12  -D1*HRI2-D2*HRI1 +RR12)
         XM(IELEM, 2)=D*( HHI13  -D1*HRI3-D3*HRI1 +RR13)
         XM(IELEM, 3)=D*(-HH12-HH13+D1*(HRI1-HRS1)-RR11)
         XM(IELEM, 4)=D*( HH12   +D1*HRI2-D2*HRS1 -RR12)
         XM(IELEM, 5)=D*( HH13   +D1*HRI3-D3*HRS1 -RR13)
         XM(IELEM, 6)=D*( HHI23  -D2*HRI3-D3*HRI2 +RR23)
         XM(IELEM, 7)=D*( HH12   +D2*HRI1-D1*HRS2 -RR12)
         XM(IELEM, 8)=D*(-HH12-HH23+D2*(HRI2-HRS2)-RR22)
         XM(IELEM, 9)=D*( HH23   +D2*HRI3-D3*HRS2 -RR23)
         XM(IELEM,10)=D*( HH13   +D3*HRI1-D1*HRS3 -RR13)
         XM(IELEM,11)=D*( HH23   +D3*HRI2-D2*HRS3 -RR23)
         XM(IELEM,12)=D*(-HH13-HH23+D3*(HRI3-HRS3)-RR33)
         XM(IELEM,13)=D*( HHS12  +D1*HRS2+D2*HRS1 +RR12)
         XM(IELEM,14)=D*( HHS13  +D1*HRS3+D3*HRS1 +RR13)
         XM(IELEM,15)=D*( HHS23  +D2*HRS3+D3*HRS2 +RR23)
C
C-----------------------------------------------------------------------
C
C        DIFFUSION ALONG Y
C
C-----------------------------------------------------------------------
C
         D1=X(I3)-X(I2)
         D2=X(I1)-X(I3)
         D3=X(I2)-X(I1)
C
         RI=-(Z(I1)*D1+Z(I2)*D2+Z(I3)*D3)
         RS=-(Z(I4)*D1+Z(I5)*D2+Z(I6)*D3)
         R=RI+RS
         RRI=R+RI+RI
         RRS=R+RS+RS
         RRRI=R*R+2*RI*RI
         RRRS=R*R+2*RS*RS
C
         D12=D1*D2
         D13=D1*D3
         D23=D2*D3
C
         SNI=NG1+NG2+NG3
         SNS=NG4+NG5+NG6
         SNHI=NG1*H1+NG2*H2+NG3*H3
         SNHS=NG4*H1+NG5*H2+NG6*H3
         SNHHI=(SNI*SH1+SNHI+SNHI)*SH1+SNI*SHH
     &        +2*(NG1*H1*H1+NG2*H2*H2+NG3*H3*H3)
         SNHHS=(SNS*SH1+SNHS+SNHS)*SH1+SNS*SHH
     &        +2*(NG4*H1*H1+NG5*H2*H2+NG6*H3*H3)
         SNHH=SNHHI+SNHHS
         SNHHI=SNHH+SNHHI+SNHHI
         SNHHS=SNHH+SNHHS+SNHHS
C
         HHI12=SNHHI*D12
         HHI13=SNHHI*D13
         HHI23=SNHHI*D23
         HHS12=SNHHS*D12
         HHS13=SNHHS*D13
         HHS23=SNHHS*D23
         HH12=SNHH*D12
         HH13=SNHH*D13
         HH23=SNHH*D23
C
         SNHI1=(SNI+NG1)*(SH1+H1)+SNHI+NG1*H1
         SNHS1=(SNS+NG4)*(SH1+H1)+SNHS+NG4*H1
         SNHI2=(SNI+NG2)*(SH1+H2)+SNHI+NG2*H2
         SNHS2=(SNS+NG5)*(SH1+H2)+SNHS+NG5*H2
         SNHI3=(SNI+NG3)*(SH1+H3)+SNHI+NG3*H3
         SNHS3=(SNS+NG6)*(SH1+H3)+SNHS+NG6*H3
C
         HRI1=RRI*SNHI1+R*SNHS1
         HRS1=RRS*SNHS1+R*SNHI1
         HRI2=RRI*SNHI2+R*SNHS2
         HRS2=RRS*SNHS2+R*SNHI2
         HRI3=RRI*SNHI3+R*SNHS3
         HRS3=RRS*SNHS3+R*SNHI3
C
         RR11=2*(RRRI*(SNI+NG1+NG1)+RRRS*(SNS+NG4+NG4))
         RR22=2*(RRRI*(SNI+NG2+NG2)+RRRS*(SNS+NG5+NG5))
         RR33=2*(RRRI*(SNI+NG3+NG3)+RRRS*(SNS+NG6+NG6))
         RR12=   RRRI*(SNI+NG1+NG2)+RRRS*(SNS+NG4+NG5)
         RR13=   RRRI*(SNI+NG1+NG3)+RRRS*(SNS+NG4+NG6)
         RR23=   RRRI*(SNI+NG2+NG3)+RRRS*(SNS+NG5+NG6)
C
C        EXTRA-DIAGONAL TERMS
C
         XM(IELEM, 1)=XM(IELEM, 1)+D*( HHI12  -D1*HRI2-D2*HRI1 +RR12)
         XM(IELEM, 2)=XM(IELEM, 2)+D*( HHI13  -D1*HRI3-D3*HRI1 +RR13)
         XM(IELEM, 3)=XM(IELEM, 3)+D*(-HH12-HH13+D1*(HRI1-HRS1)-RR11)
         XM(IELEM, 4)=XM(IELEM, 4)+D*( HH12   +D1*HRI2-D2*HRS1 -RR12)
         XM(IELEM, 5)=XM(IELEM, 5)+D*( HH13   +D1*HRI3-D3*HRS1 -RR13)
         XM(IELEM, 6)=XM(IELEM, 6)+D*( HHI23  -D2*HRI3-D3*HRI2 +RR23)
         XM(IELEM, 7)=XM(IELEM, 7)+D*( HH12   +D2*HRI1-D1*HRS2 -RR12)
         XM(IELEM, 8)=XM(IELEM, 8)+D*(-HH12-HH23+D2*(HRI2-HRS2)-RR22)
         XM(IELEM, 9)=XM(IELEM, 9)+D*( HH23   +D2*HRI3-D3*HRS2 -RR23)
         XM(IELEM,10)=XM(IELEM,10)+D*( HH13   +D3*HRI1-D1*HRS3 -RR13)
         XM(IELEM,11)=XM(IELEM,11)+D*( HH23   +D3*HRI2-D2*HRS3 -RR23)
         XM(IELEM,12)=XM(IELEM,12)+D*(-HH13-HH23+D3*(HRI3-HRS3)-RR33)
         XM(IELEM,13)=XM(IELEM,13)+D*( HHS12  +D1*HRS2+D2*HRS1 +RR12)
         XM(IELEM,14)=XM(IELEM,14)+D*( HHS13  +D1*HRS3+D3*HRS1 +RR13)
         XM(IELEM,15)=XM(IELEM,15)+D*( HHS23  +D2*HRS3+D3*HRS2 +RR23)
C
C-----------------------------------------------------------------------
C
C        DIFFUSION ALONG Z
C
C-----------------------------------------------------------------------
C
C        VERSION WITH SIMPLIFICATIONS TO ACHIEVE MONOTONY OF THE MATRIX
C
         D=SURFAC(IELEM)*XS06
C
C        EXTRA-DIAGONAL TERMS
C
         XM(IELEM, 3)=XM(IELEM, 3)-D*NH1
         XM(IELEM, 8)=XM(IELEM, 8)-D*NH2
         XM(IELEM,12)=XM(IELEM,12)-D*NH3
C
C-----------------------------------------------------------------------
C
      ENDDO
C
C     IF(INCHYD) THEN
      ENDIF
C
C-----------------------------------------------------------------------
C
C     POSITIVE EXTRA-DIAGONAL TERMS REMOVED
C     TO ENSURE MONOTONY
C
      IF(FORMUL(14:16).EQ.'MON') THEN
C
        IF(XMUL.GT.0.D0) THEN
          DO IELEM=1,NELEM
            XM(IELEM, 1)=MIN(XM(IELEM, 1),0.D0)
            XM(IELEM, 2)=MIN(XM(IELEM, 2),0.D0)
            XM(IELEM, 3)=MIN(XM(IELEM, 3),0.D0)
            XM(IELEM, 4)=MIN(XM(IELEM, 4),0.D0)
            XM(IELEM, 5)=MIN(XM(IELEM, 5),0.D0)
            XM(IELEM, 6)=MIN(XM(IELEM, 6),0.D0)
            XM(IELEM, 7)=MIN(XM(IELEM, 7),0.D0)
            XM(IELEM, 8)=MIN(XM(IELEM, 8),0.D0)
            XM(IELEM, 9)=MIN(XM(IELEM, 9),0.D0)
            XM(IELEM,10)=MIN(XM(IELEM,10),0.D0)
            XM(IELEM,11)=MIN(XM(IELEM,11),0.D0)
            XM(IELEM,12)=MIN(XM(IELEM,12),0.D0)
            XM(IELEM,13)=MIN(XM(IELEM,13),0.D0)
            XM(IELEM,14)=MIN(XM(IELEM,14),0.D0)
            XM(IELEM,15)=MIN(XM(IELEM,15),0.D0)
          ENDDO
        ELSE
          DO IELEM=1,NELEM
            XM(IELEM, 1)=MAX(XM(IELEM, 1),0.D0)
            XM(IELEM, 2)=MAX(XM(IELEM, 2),0.D0)
            XM(IELEM, 3)=MAX(XM(IELEM, 3),0.D0)
            XM(IELEM, 4)=MAX(XM(IELEM, 4),0.D0)
            XM(IELEM, 5)=MAX(XM(IELEM, 5),0.D0)
            XM(IELEM, 6)=MAX(XM(IELEM, 6),0.D0)
            XM(IELEM, 7)=MAX(XM(IELEM, 7),0.D0)
            XM(IELEM, 8)=MAX(XM(IELEM, 8),0.D0)
            XM(IELEM, 9)=MAX(XM(IELEM, 9),0.D0)
            XM(IELEM,10)=MAX(XM(IELEM,10),0.D0)
            XM(IELEM,11)=MAX(XM(IELEM,11),0.D0)
            XM(IELEM,12)=MAX(XM(IELEM,12),0.D0)
            XM(IELEM,13)=MAX(XM(IELEM,13),0.D0)
            XM(IELEM,14)=MAX(XM(IELEM,14),0.D0)
            XM(IELEM,15)=MAX(XM(IELEM,15),0.D0)
          ENDDO
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
C     DIAGONAL TERMS OBTAINED GIVEN THAT :
C     SUM OF THE TERMS IN A ROW =0
C
C-----------------------------------------------------------------------
C
      DO IELEM=1,NELEM
C
         T(IELEM,1)= -XM(IELEM,01)
     &               -XM(IELEM,02)
     &               -XM(IELEM,03)
     &               -XM(IELEM,04)
     &               -XM(IELEM,05)
         T(IELEM,2)= -XM(IELEM,01)
     &               -XM(IELEM,06)
     &               -XM(IELEM,07)
     &               -XM(IELEM,08)
     &               -XM(IELEM,09)
         T(IELEM,3)= -XM(IELEM,02)
     &               -XM(IELEM,06)
     &               -XM(IELEM,10)
     &               -XM(IELEM,11)
     &               -XM(IELEM,12)
         T(IELEM,4)= -XM(IELEM,03)
     &               -XM(IELEM,07)
     &               -XM(IELEM,10)
     &               -XM(IELEM,13)
     &               -XM(IELEM,14)
         T(IELEM,5)= -XM(IELEM,04)
     &               -XM(IELEM,08)
     &               -XM(IELEM,11)
     &               -XM(IELEM,13)
     &               -XM(IELEM,15)
         T(IELEM,6)= -XM(IELEM,05)
     &               -XM(IELEM,09)
     &               -XM(IELEM,12)
     &               -XM(IELEM,14)
     &               -XM(IELEM,15)
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
