C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE DIFFUSION MATRIX AS IN MT02PP BUT HERE
!>               WITH A PRIOR DECOMPOSITION OF GRADIENTS ALOG PLANES
!>               AND ON THE VERTICAL, THIS GIVES 4 TERMS WHICH ARE
!>               SUCCESSIVELY COMPUTED HERE, AND MAY BE COMPUTED
!>               SEPARATELY. AS THERE ARE APPROXIMATIONS WHICH ARE
!>               DIFFERENT, THE RESULT IS SLIGHTLY DIFFERENT FROM MT02PP
!>               THIS IS USED E.G. FOR CORRECTING HORIZONTAL AND VERTICAL 
!>               TO GET DIVERGENCE FREE FLUXES.
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
!>    </th><td> D, D1, D12, D13, D2, D23, D3, EPSILON, H1, H2, H3, HH12, HH13, HH23, HHI12, HHI13, HHI23, HHS12, HHS13, HHS23, HRI1, HRI2, HRI3, HRS1, HRS2, HRS3, I1, I2, I3, I4, I5, I6, IELEM, II4, II5, II6, NETAGE, NF1, NF2, NF3, NF4, NF5, NF6, NG1, NG2, NG3, NG4, NG5, NG6, NH1, NH2, NH3, NPOU0, R, RI, RR11, RR12, RR13, RR22, RR23, RR33, RRI, RRRI, RRRS, RRS, RS, SH1, SHH, SNHH, SNHHI, SNHHS, SNHI, SNHI1, SNHI2, SNHI3, SNHS, SNHS1, SNHS2, SNHS3, SNI, SNS, XS06, XS2880
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
                        SUBROUTINE MT02PP_STAR
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
C| T              |---| 
C| X,Y,Z          |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XM             |---| 
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
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
C                                                              15 OR 30
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6),XM(NELMAX,*)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
C
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
      DOUBLE PRECISION, INTENT(IN)    :: F(*),G(*),H(*)
C
C     STRUCTURES DE F,G,H 
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
C     DECLARATIONS SPECIFIQUES A CE SOUS-PROGRAMME
C
      DOUBLE PRECISION H1,H2,H3
      DOUBLE PRECISION NF1,NF2,NF3,NG1,NG2,NG3,NH1,NH2,NH3
      DOUBLE PRECISION XS06,XS24,NPX,NPY,NPXL,NPXU,XS03
      DOUBLE PRECISION NPYL,NPYU,GX(3),GY(3),NUXMOY,NUYMOY
C
      DOUBLE PRECISION X2,X3,Y2,Y3,SOMVX,X2X3,X2AUX,X3AUX,NPX2,NPY2
      DOUBLE PRECISION SOMVY,Y2Y3,Y2AUX,Y3AUX,AUX
C
      INTEGER I1,I2,I3,I4,I5,I6,IELEM,II4,II5,II6,NPOU0
C
      DOUBLE PRECISION EPSILON
      DATA EPSILON/1.D-4/
C
C***********************************************************************
C
      XS03  =XMUL/3.D0
      XS06  =XMUL/6.D0
      XS24  =XMUL/24.D0
C
      IF(SF%ELM.NE.41) THEN
        IF (LNG.EQ.1) WRITE(LU,1000) SF%ELM
        IF (LNG.EQ.2) WRITE(LU,1001) SF%ELM
1000    FORMAT(1X,'MT02PP_STAR (BIEF) : TYPE DE F NON PREVU : ',I6)
1001    FORMAT(1X,'MT02PP_STAR (BIEF) : TYPE OF F NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(SG%ELM.NE.41) THEN
        IF (LNG.EQ.1) WRITE(LU,2000) SG%ELM
        IF (LNG.EQ.2) WRITE(LU,2001) SG%ELM
2000    FORMAT(1X,'MT02PP_STAR (BIEF) : TYPE DE G NON PREVU : ',I6)
2001    FORMAT(1X,'MT02PP_STAR (BIEF) : TYPE OF G NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(SH%ELM.NE.41) THEN
        IF (LNG.EQ.1) WRITE(LU,2000) SH%ELM
        IF (LNG.EQ.2) WRITE(LU,2001) SH%ELM
3000    FORMAT(1X,'MT02PP_STAR (BIEF) : TYPE DE H NON PREVU : ',I6)
3001    FORMAT(1X,'MT02PP_STAR (BIEF) : TYPE OF H NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     POUR LE TRAITEMENT DE LA VISCOSITE VERTICALE P0 SUR LA VERTICALE
C     VOIR VISCLM DE TELEMAC-3D
C
      IF(SH%DIMDISC.EQ.0) THEN
C       VISCOSITE VERTICALE P1
        NPOU0=SH%DIM1/NPLAN
      ELSEIF(SH%DIMDISC.EQ.4111) THEN
C       VISCOSITE VERTICALE P0 SUR LA VERTICALE (VOIR II4,5,6)
        NPOU0=0
      ELSE
        IF (LNG.EQ.1) WRITE(LU,4000) SH%DIMDISC
        IF (LNG.EQ.2) WRITE(LU,4001) SH%DIMDISC
4000    FORMAT(1X,'MT02PP_STAR (BIEF) : DIMDISC NON PREVU : ',I6)
4001    FORMAT(1X,'MT02PP_STAR (BIEF): DIMDISC NOT IMPLEMENTED: ',I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
      IF(FORMUL(11:13).EQ.'MON') THEN
C
        IF(XMUL.LT.0.D0) THEN
          IF (LNG.EQ.1) WRITE(LU,4002) XMUL
          IF (LNG.EQ.2) WRITE(LU,4003) XMUL
4002      FORMAT(1X,'MT02PP (BIEF) : XMUL NEGATIF NON PREVU : ',G16.7)
4003      FORMAT(1X,'MT02PP (BIEF): NEGATIVE XMUL EXCLUDED: ',G16.7)
          CALL PLANTE(1)
          STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
C     VERSION WITH MONOTONICITY
C
C-----------------------------------------------------------------------
C
      DO IELEM=1,NELEM
C
         I1=IKLE(IELEM,1)
         I2=IKLE(IELEM,2)
         I3=IKLE(IELEM,3)
         I4=IKLE(IELEM,4)
         I5=IKLE(IELEM,5)
         I6=IKLE(IELEM,6)
C        SUIVANT NPOU0, II4 SERA I4 OU I1, ETC.
         II4=I1+NPOU0
         II5=I2+NPOU0
         II6=I3+NPOU0
C
         H1=Z(I4)-Z(I1)
         H2=Z(I5)-Z(I2)
         H3=Z(I6)-Z(I3)
C
         IF((INCHYD.AND.MAX(Z(I1),Z(I2),Z(I3)).GT.
     *                  MIN(Z(I4),Z(I5),Z(I6)))    .OR.
     *       H1.LT.EPSILON.OR.H2.LT.EPSILON.OR.H3.LT.EPSILON ) THEN
           NH1=0.D0
           NH2=0.D0
           NH3=0.D0
         ELSE
C          SUIVANT LES CAS (II4=I1 OU I4, ETC.)
C          VARIANTE AVEC VISCOSITE VERTICALE LINEAIRE (II4=I4)
C          VARIANTE AVEC VISCOSITE VERTICALE P0 SUR LA VERTICALE (II4=I1)
           NH1=0.5D0*(H(I1)+H(II4))/H1
           NH2=0.5D0*(H(I2)+H(II5))/H2
           NH3=0.5D0*(H(I3)+H(II6))/H3
         ENDIF
C
C        DIFFUSION ALONG PLANES
C
         X2 = X(I2)-X(I1)
         X3 = X(I3)-X(I1)
         Y2 = Y(I2)-Y(I1)
         Y3 = Y(I3)-Y(I1)
C
         X2X3  = X2*X3
         Y2Y3  = Y2*Y3
         X2AUX = X2X3-X2**2
         X3AUX = X3**2-X2X3
         Y2AUX = Y2Y3-Y2**2
         Y3AUX = Y3**2-Y2Y3 
C
         AUX = XS24 / SURFAC(IELEM)
C
C        2D DIFFUSION, LOWER LEVEL (SEE MT02AA)
C        LIKE IN 2D BUT MULTIPLIED BY DELTA(Z)/2
         SOMVX = ( F(I1)*H1+F(I2)*H2+F(I3)*H3 ) * AUX
         SOMVY = ( G(I1)*H1+G(I2)*H2+G(I3)*H3 ) * AUX
C               
C        OFF-DIAGONAL TERMS FOR POINTS OF LOWER LEVEL
C        WITH MONOTONICITY ENSURED
C
         XM(IELEM, 1) = MIN(0.D0,- SOMVY * X3AUX - SOMVX * Y3AUX)
         XM(IELEM, 2) = MIN(0.D0,  SOMVY * X2AUX + SOMVX * Y2AUX)
         XM(IELEM, 6) = MIN(0.D0,- SOMVY * X2X3  - SOMVX * Y2Y3 )     
C
C        2D DIFFUSION, UPPER LEVEL 
         SOMVX = ( F(I4)*H1+F(I5)*H2+F(I6)*H3 ) * AUX
         SOMVY = ( G(I4)*H1+G(I5)*H2+G(I6)*H3 ) * AUX
C               
C        OFF-DIAGONAL TERMS FOR POINTS OF UPPER LEVEL
C        WITH MONOTONICITY ENSURED
C
         XM(IELEM,13) = MIN(0.D0,- SOMVY * X3AUX - SOMVX * Y3AUX) 
         XM(IELEM,14) = MIN(0.D0,  SOMVY * X2AUX + SOMVX * Y2AUX)
         XM(IELEM,15) = MIN(0.D0,- SOMVY * X2X3  - SOMVX * Y2Y3 ) 
C
C        HERE TERMS 2 AND 3 HAVE BEEN REMOVED FOR MONOTONICITY
C        CROSSED TERMS CANCELLED
C
         XM(IELEM,04)=0.D0
         XM(IELEM,05)=0.D0
         XM(IELEM,07)=0.D0
         XM(IELEM,09)=0.D0
         XM(IELEM,10)=0.D0
         XM(IELEM,11)=0.D0 
C
C        TERM 4 
C
C        HERE SOME HORIZONTAL TERMS HAVE BEEN REMOVED BECAUSE
C        HORIZONTAL FLUXES THROUGH BOTTOM AND TOP OF PRISM
C        ARE NEGLECTED FOR MONOTONICITY
C        SEE THE WHOLE TERM IN THE OPTION WITHOUT MONOTONICITY
C
         XM(IELEM,03)=-NH1*SURFAC(IELEM)*XS03 
         XM(IELEM,08)=-NH2*SURFAC(IELEM)*XS03
         XM(IELEM,12)=-NH3*SURFAC(IELEM)*XS03        
C                       
C-----------------------------------------------------------------------
C
      ENDDO
C                       
C-----------------------------------------------------------------------
C
      ELSEIF(FORMUL(10:13).EQ.'1234') THEN
C
C-----------------------------------------------------------------------
C
C     VERSION WITHOUT MONOTONICITY AND ALL TERMS
C
C-----------------------------------------------------------------------
C
      DO IELEM=1,NELEM
C
         I1=IKLE(IELEM,1)
         I2=IKLE(IELEM,2)
         I3=IKLE(IELEM,3)
         I4=IKLE(IELEM,4)
         I5=IKLE(IELEM,5)
         I6=IKLE(IELEM,6)
C        SUIVANT NPOU0, II4 SERA I4 OU I1, ETC.
         II4=I1+NPOU0
         II5=I2+NPOU0
         II6=I3+NPOU0
C
         H1=Z(I4)-Z(I1)
         H2=Z(I5)-Z(I2)
         H3=Z(I6)-Z(I3)
C
         IF((INCHYD.AND.MAX(Z(I1),Z(I2),Z(I3)).GT.
     *                  MIN(Z(I4),Z(I5),Z(I6)))    .OR.
     *       H1.LT.EPSILON.OR.H2.LT.EPSILON.OR.H3.LT.EPSILON ) THEN
           NF1=0.D0
           NF2=0.D0
           NF3=0.D0
           NG1=0.D0
           NG2=0.D0
           NG3=0.D0
           NH1=0.D0
           NH2=0.D0
           NH3=0.D0
           AUX=0.D0
           NUXMOY=0.D0
           NUYMOY=0.D0
         ELSE
C          SUIVANT LES CAS (II4=I1 OU I4, ETC.)
C          VARIANTE AVEC VISCOSITE VERTICALE LINEAIRE (II4=I4)
C          VARIANTE AVEC VISCOSITE VERTICALE P0 SUR LA VERTICALE (II4=I1)
           NF1=0.5D0*(F(I1)+F(II4))/H1
           NF2=0.5D0*(F(I2)+F(II5))/H2
           NF3=0.5D0*(F(I3)+F(II6))/H3
           NG1=0.5D0*(G(I1)+G(II4))/H1
           NG2=0.5D0*(G(I2)+G(II5))/H2
           NG3=0.5D0*(G(I3)+G(II6))/H3
           NH1=0.5D0*(H(I1)+H(II4))/H1
           NH2=0.5D0*(H(I2)+H(II5))/H2
           NH3=0.5D0*(H(I3)+H(II6))/H3
           AUX = XS24 / SURFAC(IELEM)
C          TAKING INTO ACCOUNT AN AVERAGE DIFFUSION ON THE PRISM
           NUXMOY=(F(I1)+F(I2)+F(I3)+F(I4)+F(I5)+F(I6))/6.D0
           NUYMOY=(G(I1)+G(I2)+G(I3)+G(I4)+G(I5)+G(I6))/6.D0
         ENDIF
C
         X2 = X(I2)-X(I1)
         X3 = X(I3)-X(I1)
         Y2 = Y(I2)-Y(I1)
         Y3 = Y(I3)-Y(I1)
C
         X2X3  = X2*X3
         Y2Y3  = Y2*Y3
         X2AUX = X2X3-X2**2
         X3AUX = X3**2-X2X3
         Y2AUX = Y2Y3-Y2**2
         Y3AUX = Y3**2-Y2Y3 
C
C        2D DIFFUSION, LOWER LEVEL (SEE MT02AA)
C        LIKE IN 2D BUT MULTIPLIED BY DELTA(Z)/2
         SOMVX = ( F(I1)*H1+F(I2)*H2+F(I3)*H3 ) * AUX
         SOMVY = ( G(I1)*H1+G(I2)*H2+G(I3)*H3 ) * AUX
C               
C        OFF-DIAGONAL TERMS FOR POINTS OF LOWER LEVEL
C        WITH MONOTONICITY ENSURED
C
C        TERM 1
C
         XM(IELEM, 1) = - SOMVY * X3AUX - SOMVX * Y3AUX
         XM(IELEM, 2) =   SOMVY * X2AUX + SOMVX * Y2AUX
         XM(IELEM, 6) = - SOMVY * X2X3  - SOMVX * Y2Y3     
C
C        2D DIFFUSION, UPPER LEVEL 
         SOMVX = ( F(I4)*H1+F(I5)*H2+F(I6)*H3 ) * AUX
         SOMVY = ( G(I4)*H1+G(I5)*H2+G(I6)*H3 ) * AUX
C               
C        OFF-DIAGONAL TERMS FOR POINTS OF UPPER LEVEL
C        WITH MONOTONICITY ENSURED
C
C        TERM 1
C
         XM(IELEM,13) = - SOMVY * X3AUX - SOMVX * Y3AUX 
         XM(IELEM,14) =   SOMVY * X2AUX + SOMVX * Y2AUX
         XM(IELEM,15) = - SOMVY * X2X3  - SOMVX * Y2Y3 
C
C        AVERAGE OF NORMAL VECTOR TO PLANES (NOT NORMED)
C        ONE CAN CHECK THAT WE GET -1 0 OR 0 -1 WITH Z=X OR Z=Y
C        NPX=-DZ/DX   NPY=-DZ/DY
C
         NPXL=-0.5D0*(Y2*(Z(I1)-Z(I3))+Y3*(Z(I2)-Z(I1)))
         NPXU=-0.5D0*(Y2*(Z(I4)-Z(I6))+Y3*(Z(I5)-Z(I4)))
         NPYL=-0.5D0*(X2*(Z(I3)-Z(I1))+X3*(Z(I1)-Z(I2)))
         NPYU=-0.5D0*(X2*(Z(I6)-Z(I4))+X3*(Z(I4)-Z(I5)))
         NPX=0.5D0*(NPXL+NPXU)/SURFAC(IELEM)
         NPY=0.5D0*(NPYL+NPYU)/SURFAC(IELEM)
         NPX2=NPX**2
         NPY2=NPY**2
C
C        TERMS 2 AND 3
C
         NPX=NPX*NUXMOY
         NPY=NPY*NUYMOY
C
C        2D GRADIENT MATRIX  GX(3,3) AND GY(3,3) 
C        BUT GX(1,J)=GX(2,J)=GX(3,J) 
C        AND GY(1,J)=GY(2,J)=GY(3,J), HENCE ONLY GX(3) AND GY(3)               
C
         GX(2) =   Y3*XS06
         GX(3) = - Y2*XS06
         GX(1) = - GX(2) - GX(3)
C
         GY(2) = - X3 * XS06
         GY(3) =   X2 * XS06
         GY(1) = - GY(2) - GY(3)
C
C        OFF-DIAGONAL TERMS, SOME (UP AND DOWN) ALREADY INITIALISED, 
C                            SOME (CROSSED TERMS) NOT
C
C        TERMS 2 
C
C        TERM 1-2 (01)
         XM(IELEM,01)=XM(IELEM,01)+0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )   
C        TERM 1-3 (02)
         XM(IELEM,02)=XM(IELEM,02)+0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) ) 
C        TERM 1-4 (03)
         XM(IELEM,03)=            +0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) ) 
C        TERM 1-5 (04) 
         XM(IELEM,04)=            +0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 1-6 (05)
         XM(IELEM,05)=            +0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
C        TERM 2-3 (06)  
         XM(IELEM,06)=XM(IELEM,06)+0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )    
C        TERM 2-4 (07)
         XM(IELEM,07)=            +0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
C        TERM 2-5 (08)
         XM(IELEM,08)=            +0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 2-6 (09)
         XM(IELEM,09)=            +0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
C        TERM 3-4 (10)
         XM(IELEM,10)=            +0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
C        TERM 3-5 (11)
         XM(IELEM,11)=            +0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 3-6 (12)
         XM(IELEM,12)=            +0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
C        TERME 4-5 (13)
         XM(IELEM,13)=XM(IELEM,13)+0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )      
C        TERM 4-6 (14)
         XM(IELEM,14)=XM(IELEM,14)+0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )   
C        TERM 5-6 (15)
         XM(IELEM,15)=XM(IELEM,15)+0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
C
C        TERMS 3
C
C        TERM 1-2 (01)
         XM(IELEM,01)=XM(IELEM,01)+0.5D0*( - ( NPX*GX(1)+NPY*GY(1)) )   
C        TERM 1-3 (02)
         XM(IELEM,02)=XM(IELEM,02)+0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
C        TERM 1-4 (03)
         XM(IELEM,03)=XM(IELEM,03)+0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) ) 
C        TERM 1-5 (04) 
         XM(IELEM,04)=XM(IELEM,04)+0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
C        TERM 1-6 (05)
         XM(IELEM,05)=XM(IELEM,05)+0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
C        TERM 2-3 (06)  
         XM(IELEM,06)=XM(IELEM,06)+0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )    
C        TERM 2-4 (07)
         XM(IELEM,07)=XM(IELEM,07)+0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 2-5 (08)
         XM(IELEM,08)=XM(IELEM,08)+0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 2-6 (09)
         XM(IELEM,09)=XM(IELEM,09)+0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 3-4 (10)
         XM(IELEM,10)=XM(IELEM,10)+0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
C        TERM 3-5 (11)
         XM(IELEM,11)=XM(IELEM,11)+0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
C        TERM 3-6 (12)
         XM(IELEM,12)=XM(IELEM,12)+0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
C        TERME 4-5 (13)
         XM(IELEM,13)=XM(IELEM,13)+0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )      
C        TERM 4-6 (14)
         XM(IELEM,14)=XM(IELEM,14)+0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )   
C        TERM 5-6 (15)
         XM(IELEM,15)=XM(IELEM,15)+0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) ) 
C
C  TERM 4 
C      
         XM(IELEM,03)=XM(IELEM,03)
     *               -(NPX2*NF1+NPY2*NG1+NH1)*SURFAC(IELEM)*XS03 
         XM(IELEM,08)=XM(IELEM,08)
     *               -(NPX2*NF2+NPY2*NG2+NH2)*SURFAC(IELEM)*XS03
         XM(IELEM,12)=XM(IELEM,12)
     *               -(NPX2*NF3+NPY2*NG3+NH3)*SURFAC(IELEM)*XS03    
C                       
C-----------------------------------------------------------------------
C
      ENDDO
C                       
C-----------------------------------------------------------------------
C
      ELSEIF(FORMUL(10:13).EQ.'1 3 ') THEN
C
C-----------------------------------------------------------------------
C
C     VERSION WITHOUT MONOTONICITY AND ONLY TERMS 1 AND 3
C
C-----------------------------------------------------------------------
C
      DO IELEM=1,NELEM
C
         I1=IKLE(IELEM,1)
         I2=IKLE(IELEM,2)
         I3=IKLE(IELEM,3)
         I4=IKLE(IELEM,4)
         I5=IKLE(IELEM,5)
         I6=IKLE(IELEM,6)
C        SUIVANT NPOU0, II4 SERA I4 OU I1, ETC.
         II4=I1+NPOU0
         II5=I2+NPOU0
         II6=I3+NPOU0
C
         H1=Z(I4)-Z(I1)
         H2=Z(I5)-Z(I2)
         H3=Z(I6)-Z(I3)
C
         IF((INCHYD.AND.MAX(Z(I1),Z(I2),Z(I3)).GT.
     *                  MIN(Z(I4),Z(I5),Z(I6)))    .OR.
     *       H1.LT.EPSILON.OR.H2.LT.EPSILON.OR.H3.LT.EPSILON ) THEN
           NF1=0.D0
           NF2=0.D0
           NF3=0.D0
           NG1=0.D0
           NG2=0.D0
           NG3=0.D0
           NH1=0.D0
           NH2=0.D0
           NH3=0.D0
           AUX=0.D0
           NUXMOY=0.D0
           NUYMOY=0.D0
         ELSE
C          SUIVANT LES CAS (II4=I1 OU I4, ETC.)
C          VARIANTE AVEC VISCOSITE VERTICALE LINEAIRE (II4=I4)
C          VARIANTE AVEC VISCOSITE VERTICALE P0 SUR LA VERTICALE (II4=I1)
           NF1=0.5D0*(F(I1)+F(II4))/H1
           NF2=0.5D0*(F(I2)+F(II5))/H2
           NF3=0.5D0*(F(I3)+F(II6))/H3
           NG1=0.5D0*(G(I1)+G(II4))/H1
           NG2=0.5D0*(G(I2)+G(II5))/H2
           NG3=0.5D0*(G(I3)+G(II6))/H3
           NH1=0.5D0*(H(I1)+H(II4))/H1
           NH2=0.5D0*(H(I2)+H(II5))/H2
           NH3=0.5D0*(H(I3)+H(II6))/H3
           AUX = XS24 / SURFAC(IELEM)
C          TAKING INTO ACCOUNT AN AVERAGE DIFFUSION ON THE PRISM
           NUXMOY=(F(I1)+F(I2)+F(I3)+F(I4)+F(I5)+F(I6))/6.D0
           NUYMOY=(G(I1)+G(I2)+G(I3)+G(I4)+G(I5)+G(I6))/6.D0
         ENDIF
C
         X2 = X(I2)-X(I1)
         X3 = X(I3)-X(I1)
         Y2 = Y(I2)-Y(I1)
         Y3 = Y(I3)-Y(I1)
C
         X2X3  = X2*X3
         Y2Y3  = Y2*Y3
         X2AUX = X2X3-X2**2
         X3AUX = X3**2-X2X3
         Y2AUX = Y2Y3-Y2**2
         Y3AUX = Y3**2-Y2Y3 
C
C        2D DIFFUSION, LOWER LEVEL (SEE MT02AA)
C        LIKE IN 2D BUT MULTIPLIED BY DELTA(Z)/2
         SOMVX = ( F(I1)*H1+F(I2)*H2+F(I3)*H3 ) * AUX
         SOMVY = ( G(I1)*H1+G(I2)*H2+G(I3)*H3 ) * AUX
C               
C        OFF-DIAGONAL TERMS FOR POINTS OF LOWER LEVEL
C        WITH MONOTONICITY ENSURED
C
C        TERM 1
C
         XM(IELEM, 1) = - SOMVY * X3AUX - SOMVX * Y3AUX
         XM(IELEM, 2) =   SOMVY * X2AUX + SOMVX * Y2AUX
         XM(IELEM, 6) = - SOMVY * X2X3  - SOMVX * Y2Y3 
         XM(IELEM,16) = XM(IELEM, 1)
         XM(IELEM,17) = XM(IELEM, 2)
         XM(IELEM,21) = XM(IELEM, 6)     
C
C        2D DIFFUSION, UPPER LEVEL 
         SOMVX = ( F(I4)*H1+F(I5)*H2+F(I6)*H3 ) * AUX
         SOMVY = ( G(I4)*H1+G(I5)*H2+G(I6)*H3 ) * AUX
C               
C        OFF-DIAGONAL TERMS FOR POINTS OF UPPER LEVEL
C        WITH MONOTONICITY ENSURED
C
C        TERM 1
C
         XM(IELEM,13) = - SOMVY * X3AUX - SOMVX * Y3AUX 
         XM(IELEM,14) =   SOMVY * X2AUX + SOMVX * Y2AUX
         XM(IELEM,15) = - SOMVY * X2X3  - SOMVX * Y2Y3
         XM(IELEM,28) = XM(IELEM,13) 
         XM(IELEM,29) = XM(IELEM,14)
         XM(IELEM,30) = XM(IELEM,15) 
C
C        AVERAGE OF NORMAL VECTOR TO PLANES (NOT NORMED)
C        ONE CAN CHECK THAT WE GET -1 0 OR 0 -1 WITH Z=X OR Z=Y
C        NPX=-DZ/DX   NPY=-DZ/DY
C
         NPXL=-0.5D0*(Y2*(Z(I1)-Z(I3))+Y3*(Z(I2)-Z(I1)))
         NPXU=-0.5D0*(Y2*(Z(I4)-Z(I6))+Y3*(Z(I5)-Z(I4)))
         NPYL=-0.5D0*(X2*(Z(I3)-Z(I1))+X3*(Z(I1)-Z(I2)))
         NPYU=-0.5D0*(X2*(Z(I6)-Z(I4))+X3*(Z(I4)-Z(I5)))
         NPX=0.5D0*(NPXL+NPXU)/SURFAC(IELEM)
         NPY=0.5D0*(NPYL+NPYU)/SURFAC(IELEM)
         NPX2=NPX**2
         NPY2=NPY**2
C
C        TERM 3
C
         NPX=NPX*NUXMOY
         NPY=NPY*NUYMOY
C
C        2D GRADIENT MATRIX  GX(3,3) AND GY(3,3) 
C        BUT GX(1,J)=GX(2,J)=GX(3,J) 
C        AND GY(1,J)=GY(2,J)=GY(3,J), HENCE ONLY GX(3) AND GY(3)               
C
         GX(2) =   Y3*XS06
         GX(3) = - Y2*XS06
         GX(1) = - GX(2) - GX(3)
C
         GY(2) = - X3 * XS06
         GY(3) =   X2 * XS06
         GY(1) = - GY(2) - GY(3)
C
C        OFF-DIAGONAL TERMS, SOME (UP AND DOWN) ALREADY INITIALISED, 
C                            SOME (CROSSED TERMS) NOT
C
C        TERMS 3
C
C        TERM 1-2 (01)
         XM(IELEM,01)=XM(IELEM,01)+0.5D0*( - ( NPX*GX(1)+NPY*GY(1)) )   
C        TERM 1-3 (02)
         XM(IELEM,02)=XM(IELEM,02)+0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
C        TERM 1-4 (03)
         XM(IELEM,03)=            +0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) ) 
C        TERM 1-5 (04) 
         XM(IELEM,04)=            +0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
C        TERM 1-6 (05)
         XM(IELEM,05)=            +0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
C        TERM 2-3 (06)  
         XM(IELEM,06)=XM(IELEM,06)+0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )    
C        TERM 2-4 (07)
         XM(IELEM,07)=            +0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 2-5 (08)
         XM(IELEM,08)=            +0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 2-6 (09)
         XM(IELEM,09)=            +0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 3-4 (10)
         XM(IELEM,10)=            +0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
C        TERM 3-5 (11)
         XM(IELEM,11)=            +0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
C        TERM 3-6 (12)
         XM(IELEM,12)=            +0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
C        TERME 4-5 (13)
         XM(IELEM,13)=XM(IELEM,13)+0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )      
C        TERM 4-6 (14)
         XM(IELEM,14)=XM(IELEM,14)+0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )   
C        TERM 5-6 (15)
         XM(IELEM,15)=XM(IELEM,15)+0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
C
C        TERM 2-1 (16)
         XM(IELEM,16)=XM(IELEM,16)+0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) ) 
C        TERM 3-1 (17)
         XM(IELEM,17)=XM(IELEM,17)+0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) ) 
C        TERM 4-1 (18)
         XM(IELEM,18)=             0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) ) 
C        TERM 5-1 (19) 
         XM(IELEM,19)=             0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 6-1 (20)
         XM(IELEM,20)=             0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
C        TERM 3-2 (21)  
         XM(IELEM,21)=XM(IELEM,21)+0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )    
C        TERM 4-2 (22)
         XM(IELEM,22)=             0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
C        TERM 5-2 (23)
         XM(IELEM,23)=             0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 6-2 (24)
         XM(IELEM,24)=             0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
C        TERM 4-3 (25)
         XM(IELEM,25)=             0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
C        TERM 5-3 (26)
         XM(IELEM,26)=             0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 5-4 (28)
         XM(IELEM,28)=XM(IELEM,28)+0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) ) 
C        TERM 6-3 (27)
         XM(IELEM,27)=             0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )     
C        TERM 6-4 (29)
         XM(IELEM,29)=XM(IELEM,29)+0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )   
C        TERM 6-5 (30)
         XM(IELEM,30)=XM(IELEM,30)+0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )      
C                       
C-----------------------------------------------------------------------
C
      ENDDO
C                       
C-----------------------------------------------------------------------
C
      ELSEIF(FORMUL(10:13).EQ.' 2 4') THEN
C
C-----------------------------------------------------------------------
C
C     VERSION WITHOUT MONOTONICITY AND ONLY TERMS 2 AND 4
C
C-----------------------------------------------------------------------
C
      DO IELEM=1,NELEM
C
         I1=IKLE(IELEM,1)
         I2=IKLE(IELEM,2)
         I3=IKLE(IELEM,3)
         I4=IKLE(IELEM,4)
         I5=IKLE(IELEM,5)
         I6=IKLE(IELEM,6)
C        SUIVANT NPOU0, II4 SERA I4 OU I1, ETC.
         II4=I1+NPOU0
         II5=I2+NPOU0
         II6=I3+NPOU0
C
         H1=Z(I4)-Z(I1)
         H2=Z(I5)-Z(I2)
         H3=Z(I6)-Z(I3)
C
         IF((INCHYD.AND.MAX(Z(I1),Z(I2),Z(I3)).GT.
     *                  MIN(Z(I4),Z(I5),Z(I6)))    .OR.
     *       H1.LT.EPSILON.OR.H2.LT.EPSILON.OR.H3.LT.EPSILON ) THEN
           NF1=0.D0
           NF2=0.D0
           NF3=0.D0
           NG1=0.D0
           NG2=0.D0
           NG3=0.D0
           NH1=0.D0
           NH2=0.D0
           NH3=0.D0
           NUXMOY=0.D0
           NUYMOY=0.D0
         ELSE
C          SUIVANT LES CAS (II4=I1 OU I4, ETC.)
C          VARIANTE AVEC VISCOSITE VERTICALE LINEAIRE (II4=I4)
C          VARIANTE AVEC VISCOSITE VERTICALE P0 SUR LA VERTICALE (II4=I1)
           NF1=0.5D0*(F(I1)+F(II4))/H1
           NF2=0.5D0*(F(I2)+F(II5))/H2
           NF3=0.5D0*(F(I3)+F(II6))/H3
           NG1=0.5D0*(G(I1)+G(II4))/H1
           NG2=0.5D0*(G(I2)+G(II5))/H2
           NG3=0.5D0*(G(I3)+G(II6))/H3
           NH1=0.5D0*(H(I1)+H(II4))/H1
           NH2=0.5D0*(H(I2)+H(II5))/H2
           NH3=0.5D0*(H(I3)+H(II6))/H3
C          TAKING INTO ACCOUNT AN AVERAGE DIFFUSION ON THE PRISM
           NUXMOY=(F(I1)+F(I2)+F(I3)+F(I4)+F(I5)+F(I6))/6.D0
           NUYMOY=(G(I1)+G(I2)+G(I3)+G(I4)+G(I5)+G(I6))/6.D0
         ENDIF
C
         X2 = X(I2)-X(I1)
         X3 = X(I3)-X(I1)
         Y2 = Y(I2)-Y(I1)
         Y3 = Y(I3)-Y(I1)
C
C        AVERAGE OF NORMAL VECTOR TO PLANES (NOT NORMED)
C        ONE CAN CHECK THAT WE GET -1 0 OR 0 -1 WITH Z=X OR Z=Y
C        NPX=-DZ/DX   NPY=-DZ/DY
C
         NPXL=-0.5D0*(Y2*(Z(I1)-Z(I3))+Y3*(Z(I2)-Z(I1)))
         NPXU=-0.5D0*(Y2*(Z(I4)-Z(I6))+Y3*(Z(I5)-Z(I4)))
         NPYL=-0.5D0*(X2*(Z(I3)-Z(I1))+X3*(Z(I1)-Z(I2)))
         NPYU=-0.5D0*(X2*(Z(I6)-Z(I4))+X3*(Z(I4)-Z(I5)))
         NPX=0.5D0*(NPXL+NPXU)/SURFAC(IELEM)
         NPY=0.5D0*(NPYL+NPYU)/SURFAC(IELEM)
         NPX2=NPX**2
         NPY2=NPY**2
C
C        TERMS 2 
C
         NPX=NPX*NUXMOY
         NPY=NPY*NUYMOY
C
C        2D GRADIENT MATRIX  GX(3,3) AND GY(3,3) 
C        BUT GX(1,J)=GX(2,J)=GX(3,J) 
C        AND GY(1,J)=GY(2,J)=GY(3,J), HENCE ONLY GX(3) AND GY(3)               
C
         GX(2) =   Y3*XS06
         GX(3) = - Y2*XS06
         GX(1) = - GX(2) - GX(3)
C
         GY(2) = - X3 * XS06
         GY(3) =   X2 * XS06
         GY(1) = - GY(2) - GY(3)
C
C        OFF-DIAGONAL TERMS
C
C        TERMS 2 
C
C        TERM 1-2 (01)
         XM(IELEM,01)=0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) ) 
C        TERM 1-3 (02)
         XM(IELEM,02)=0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) ) 
C        TERM 1-4 (03)
         XM(IELEM,03)=0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) ) 
C        TERM 1-5 (04) 
         XM(IELEM,04)=0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 1-6 (05)
         XM(IELEM,05)=0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
C        TERM 2-3 (06)  
         XM(IELEM,06)=0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )    
C        TERM 2-4 (07)
         XM(IELEM,07)=0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
C        TERM 2-5 (08)
         XM(IELEM,08)=0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 2-6 (09)
         XM(IELEM,09)=0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
C        TERM 3-4 (10)
         XM(IELEM,10)=0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
C        TERM 3-5 (11)
         XM(IELEM,11)=0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 3-6 (12)
         XM(IELEM,12)=0.5D0*( - ( NPX*GX(3)+NPY*GY(3) ) )
C        TERM 4-5 (13)
         XM(IELEM,13)=0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )      
C        TERM 4-6 (14)
         XM(IELEM,14)=0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )   
C        TERM 5-6 (15)
         XM(IELEM,15)=0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
C
C        TERM 2-1 (16)
         XM(IELEM,16)=0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) ) 
C        TERM 3-1 (17)
         XM(IELEM,17)=0.5D0*( - ( NPX*GX(1)+NPY*GY(1) ) )
C        TERM 4-1 (18)
         XM(IELEM,18)=0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )  
C        TERM 5-1 (19) 
         XM(IELEM,19)=0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
C        TERM 6-1 (20)
         XM(IELEM,20)=0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )
C        TERM 3-2 (21)  
         XM(IELEM,21)=0.5D0*( - ( NPX*GX(2)+NPY*GY(2) ) )    
C        TERM 4-2 (22)
         XM(IELEM,22)=0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 5-2 (23)
         XM(IELEM,23)=0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 6-2 (24)
         XM(IELEM,24)=0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
C        TERM 4-3 (25)
         XM(IELEM,25)=0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
C        TERM 5-3 (26)
         XM(IELEM,26)=0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
C        TERM 6-3 (27)
         XM(IELEM,27)=0.5D0*( + ( NPX*GX(3)+NPY*GY(3) ) )
C        TERM 5-4 (28)
         XM(IELEM,28)=0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )      
C        TERM 6-4 (29)
         XM(IELEM,29)=0.5D0*( + ( NPX*GX(1)+NPY*GY(1) ) )   
C        TERM 6-5 (30)
         XM(IELEM,30)=0.5D0*( + ( NPX*GX(2)+NPY*GY(2) ) )
C
C  TERM 4 
C     
         XM(IELEM,03)=XM(IELEM,03)
     *               -(NPX2*NF1+NPY2*NG1+NH1)*SURFAC(IELEM)*XS03 
         XM(IELEM,08)=XM(IELEM,08)
     *               -(NPX2*NF2+NPY2*NG2+NH2)*SURFAC(IELEM)*XS03
         XM(IELEM,12)=XM(IELEM,12)
     *               -(NPX2*NF3+NPY2*NG3+NH3)*SURFAC(IELEM)*XS03 
         XM(IELEM,18)=XM(IELEM,18)
     *               -(NPX2*NF1+NPY2*NG1+NH1)*SURFAC(IELEM)*XS03
         XM(IELEM,23)=XM(IELEM,23)
     *               -(NPX2*NF2+NPY2*NG2+NH2)*SURFAC(IELEM)*XS03
         XM(IELEM,27)=XM(IELEM,27)
     *               -(NPX2*NF3+NPY2*NG3+NH3)*SURFAC(IELEM)*XS03             
C                       
C-----------------------------------------------------------------------
C
      ENDDO
C                       
C-----------------------------------------------------------------------
C
      ELSE
       IF(LNG.EQ.1) WRITE(LU,*) 'MT02PP_STAR (BIEF): FORMULE INCONNUE'
       IF(LNG.EQ.2) WRITE(LU,*) 'MT02PP_STAR (BIEF): UNKNOWN FORMULA'
       WRITE(LU,*) FORMUL
       CALL PLANTE(1)
       STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C     DIAGONAL TERMS OBTAINED BY THE FACT THAT THE SUM OF TERMS IN A
C     LINE IS 0
C
C-----------------------------------------------------------------------
C
C     IN SYMMETRIC MODE
C
      IF(FORMUL(11:13).EQ.'MON'.OR.FORMUL(10:13).EQ.'1234') THEN
C
      DO IELEM=1,NELEM
C
         T(IELEM,1)= -XM(IELEM,01)
     *               -XM(IELEM,02)
     *               -XM(IELEM,03)
     *               -XM(IELEM,04)
     *               -XM(IELEM,05)
         T(IELEM,2)= -XM(IELEM,01)
     *               -XM(IELEM,06)
     *               -XM(IELEM,07)
     *               -XM(IELEM,08)
     *               -XM(IELEM,09)
         T(IELEM,3)= -XM(IELEM,02)
     *               -XM(IELEM,06)
     *               -XM(IELEM,10)
     *               -XM(IELEM,11)
     *               -XM(IELEM,12)
         T(IELEM,4)= -XM(IELEM,03)
     *               -XM(IELEM,07)
     *               -XM(IELEM,10)
     *               -XM(IELEM,13)
     *               -XM(IELEM,14)
         T(IELEM,5)= -XM(IELEM,04)
     *               -XM(IELEM,08)
     *               -XM(IELEM,11)
     *               -XM(IELEM,13)
     *               -XM(IELEM,15)
         T(IELEM,6)= -XM(IELEM,05)
     *               -XM(IELEM,09)
     *               -XM(IELEM,12)
     *               -XM(IELEM,14)
     *               -XM(IELEM,15)
C
      ENDDO
C
C     IN NON SYMMETRIC MODE
C
      ELSEIF(FORMUL(10:13).EQ.'1 3 '.OR.FORMUL(10:13).EQ.' 2 4') THEN
C
      DO IELEM=1,NELEM
C
         T(IELEM,1)= -XM(IELEM,01)
     *               -XM(IELEM,02)
     *               -XM(IELEM,03)
     *               -XM(IELEM,04)
     *               -XM(IELEM,05)
         T(IELEM,2)= -XM(IELEM,16)
     *               -XM(IELEM,06)
     *               -XM(IELEM,07)
     *               -XM(IELEM,08)
     *               -XM(IELEM,09)
         T(IELEM,3)= -XM(IELEM,17)
     *               -XM(IELEM,21)
     *               -XM(IELEM,10)
     *               -XM(IELEM,11)
     *               -XM(IELEM,12)
         T(IELEM,4)= -XM(IELEM,18)
     *               -XM(IELEM,22)
     *               -XM(IELEM,25)
     *               -XM(IELEM,13)
     *               -XM(IELEM,14)
         T(IELEM,5)= -XM(IELEM,19)
     *               -XM(IELEM,23)
     *               -XM(IELEM,26)
     *               -XM(IELEM,28)
     *               -XM(IELEM,15)
         T(IELEM,6)= -XM(IELEM,20)
     *               -XM(IELEM,24)
     *               -XM(IELEM,27)
     *               -XM(IELEM,29)
     *               -XM(IELEM,30)
C
      ENDDO
C
      ELSE
       IF(LNG.EQ.1) WRITE(LU,*) 'MT02PP_STAR (BIEF): FORMULE INCONNUE'
       IF(LNG.EQ.2) WRITE(LU,*) 'MT02PP_STAR (BIEF): UNKNOWN FORMULA'
       WRITE(LU,*) FORMUL
       CALL PLANTE(1)
       STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
