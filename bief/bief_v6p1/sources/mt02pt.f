C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE DIFFUSION MATRIX.
!><br>            THE FUNCTION DIFFUSION COEFFICIENT IS HERE A P1
!>                DIAGONAL TENSOR.
!><br>            CASE WHERE THE PRISM IS DIVIDED INTO 3 TETRAHEDRONS.
!>                OPTIMISED COEFFICIENT COMPUTATION.
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

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, G, H, IKLE, INCHYD, NELEM, NELMAX, SF, SG, SH, T, X, XM, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> COEF, DIAG1, DIAG2, DIAG3, DIAG4, EXTR12, EXTR13, EXTR14, EXTR23, EXTR24, EXTR34, HTOT, I, I1, I2, I3, I4, I5, I6, IELEM, IGLOB, IT1, IT2, IT3, IT4, NUM1, NUM2, NUM3, NUM4, SENS, STO, SUR24, SURF, T1, T11, T13, T15, T17, T19, T21, T23, T28, T3, T35, T42, T49, T5, T51, T54, T7, T9, TETRA, VTOT, WTOT, X2, X3, X4, Y2, Y3, Y4, Z2, Z3, Z4
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MT02PT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

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
!>          <tr><td>A11,A12
!></td><td><--</td><td>ELEMENTS DE LA MATRICE
!>    </td></tr>
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
!>          <tr><td>SW
!></td><td>--></td><td>SWITCH 1:CARRE MAGIQUE, 2:CLASSIC COEFF
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
                        SUBROUTINE MT02PT
     &( T,XM,XMUL,SF,SG,SH,F,G,H,
     &  X,Y,Z,IKLE,NELEM,NELMAX,INCHYD)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A11,A12        |<--| ELEMENTS DE LA MATRICE
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LE CALCUL DE LA
C|                |   | MATRICE.
C| IKLE           |---| 
C| IKLE1          |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| INCHYD         |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SF,SG,SH       |-->| STRUCTURES DE F,G ET H.
C| SW             |-->| SWITCH 1:CARRE MAGIQUE, 2:CLASSIC COEFF
C| T             |---| 
C| X,Y,Z          |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XM             |---| 
C| XMUL           |-->| FACTEUR MULTIPLICATIF
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MT02PT => MT02PT
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NELEM,NELMAX
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,6)
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,6),XM(NELMAX,15)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL
      DOUBLE PRECISION, INTENT(IN)    :: F(*),G(*),H(*)
      TYPE(BIEF_OBJ), INTENT(IN)      :: SF,SG,SH
      DOUBLE PRECISION, INTENT(IN)    :: X(*),Y(*),Z(*)
      LOGICAL, INTENT(IN)             :: INCHYD
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     SPECIFIC DECLARATIONS
C
      DOUBLE PRECISION X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4
      DOUBLE PRECISION DIAG1,DIAG2,DIAG3,DIAG4
      DOUBLE PRECISION EXTR12,EXTR13,EXTR14,EXTR23,EXTR24,EXTR34

      INTEGER IT1,IT2,IT3,IT4,I,I1,I2,I3,I4,I5,I6,NUM1,NUM2,NUM3,NUM4
      INTEGER IGLOB(6),SENS(3),STO(6,6),IELEM
C
      DOUBLE PRECISION SUR24,HTOT,VTOT,WTOT,COEF
      DOUBLE PRECISION T1,T3,T5,T7,T9,T11,T13,T15,T17,T19,T21,T23
      DOUBLE PRECISION T35,T49,T28,T42,T51,T54,SURF
C
C     PRISM SPLITTING (SEE FORTRAN EQUIVALENT IN COMMENT BELOW)
C
      INTEGER TETRA(2,2,2,3,4)
      DATA TETRA / 0,1,1,1,1,1,1,0,0,4,4,4,4,4,4,0,0,6,4,5,5,4,6,0,
     &             0,2,2,2,2,2,2,0,0,6,6,6,6,6,6,0,0,3,1,2,2,1,3,0,
     &             0,3,3,3,3,3,3,0,0,5,5,5,5,5,5,0,0,2,3,4,1,6,5,0,
     &             0,4,5,4,6,6,5,0,0,2,3,3,1,2,1,0,0,4,5,3,6,2,1,0 /
C
C     STORAGE CONVENTION FOR THE PRISM EXTRADIAGONAL TERMS
C
      DATA STO / 00 , 01 , 02 , 03 , 04 , 05 ,
     &           01 , 00 , 06 , 07 , 08 , 09 ,
     &           02 , 06 , 00 , 10 , 11 , 12 ,
     &           03 , 07 , 10 , 00 , 13 , 14 ,
     &           04 , 08 , 11 , 13 , 00 , 15 ,
     &           05 , 09 , 12 , 14 , 15 , 00 /
C
C***********************************************************************
C
C     DIFFERENT WAYS TO SPLIT PRISMS (TO ENSURE MATCHING OF TETRAHEDRONS)
C
C     TETRA(2,2,2,3,4)
C
C     FIRST 3 DIMENSIONS : TYPE OF FACE
C                      1 : CUT RECTANGLE BETWEEN  LOW-LEFT AND HIGH-RIGHT
C                      2 : CUT RECTANGLE BETWEEN  HIGH-LEFT AND LOW-RIGHT
C
C     4TH DIMENSION : NUMBER OF TETRAHEDRON
C     5TH DIMENSION : 4 POINTS OF THE TETRAHEDRON (IN LOCAL PRISM NUMBERING)
C
C     1 1 2 CUTTING
C
C     TETRA(1,1,2,1,1)= 1
C     TETRA(1,1,2,1,2)= 2
C     TETRA(1,1,2,1,3)= 3
C     TETRA(1,1,2,1,4)= 6
C
C     TETRA(1,1,2,2,1)= 4
C     TETRA(1,1,2,2,2)= 6
C     TETRA(1,1,2,2,3)= 5
C     TETRA(1,1,2,2,4)= 1
C
C     TETRA(1,1,2,3,1)= 5
C     TETRA(1,1,2,3,2)= 2
C     TETRA(1,1,2,3,3)= 1
C     TETRA(1,1,2,3,4)= 6
C
C     2 1 1 CUTTING
C
C     TETRA(2,1,1,1,1)= 1
C     TETRA(2,1,1,1,2)= 2
C     TETRA(2,1,1,1,3)= 3
C     TETRA(2,1,1,1,4)= 4
C
C     TETRA(2,1,1,2,1)= 4
C     TETRA(2,1,1,2,2)= 6
C     TETRA(2,1,1,2,3)= 5
C     TETRA(2,1,1,2,4)= 2
C
C     TETRA(2,1,1,3,1)= 6
C     TETRA(2,1,1,3,2)= 3
C     TETRA(2,1,1,3,3)= 2
C     TETRA(2,1,1,3,4)= 4
C
C     1 2 1 CUTTING
C
C     TETRA(1,2,1,1,1)= 1
C     TETRA(1,2,1,1,2)= 2
C     TETRA(1,2,1,1,3)= 3
C     TETRA(1,2,1,1,4)= 5
C
C     TETRA(1,2,1,2,1)= 4
C     TETRA(1,2,1,2,2)= 6
C     TETRA(1,2,1,2,3)= 5
C     TETRA(1,2,1,2,4)= 3
C
C     TETRA(1,2,1,3,1)= 4
C     TETRA(1,2,1,3,2)= 1
C     TETRA(1,2,1,3,3)= 3
C     TETRA(1,2,1,3,4)= 5
C
C     2 2 1 CUTTING
C
C     TETRA(2,2,1,1,1)= 1
C     TETRA(2,2,1,1,2)= 2
C     TETRA(2,2,1,1,3)= 3
C     TETRA(2,2,1,1,4)= 4
C
C     TETRA(2,2,1,2,1)= 4
C     TETRA(2,2,1,2,2)= 6
C     TETRA(2,2,1,2,3)= 5
C     TETRA(2,2,1,2,4)= 3
C
C     TETRA(2,2,1,3,1)= 5
C     TETRA(2,2,1,3,2)= 2
C     TETRA(2,2,1,3,3)= 4
C     TETRA(2,2,1,3,4)= 3
C
C     1 2 2 CUTTING
C
C     TETRA(1,2,2,1,1)= 1
C     TETRA(1,2,2,1,2)= 2
C     TETRA(1,2,2,1,3)= 3
C     TETRA(1,2,2,1,4)= 5
C
C     TETRA(1,2,2,2,1)= 4
C     TETRA(1,2,2,2,2)= 6
C     TETRA(1,2,2,2,3)= 5
C     TETRA(1,2,2,2,4)= 1
C
C     TETRA(1,2,2,3,1)= 6
C     TETRA(1,2,2,3,2)= 3
C     TETRA(1,2,2,3,3)= 5
C     TETRA(1,2,2,3,4)= 1
C
C     2 1 2 CUTTING
C
C     TETRA(2,1,2,1,1)= 1
C     TETRA(2,1,2,1,2)= 2
C     TETRA(2,1,2,1,3)= 3
C     TETRA(2,1,2,1,4)= 6
C
C     TETRA(2,1,2,2,1)= 4
C     TETRA(2,1,2,2,2)= 6
C     TETRA(2,1,2,2,3)= 5
C     TETRA(2,1,2,2,4)= 2
C
C     TETRA(2,1,2,3,1)= 4
C     TETRA(2,1,2,3,2)= 1
C     TETRA(2,1,2,3,3)= 6
C     TETRA(2,1,2,3,4)= 2
C
C-----------------------------------------------------------------------
C
      SUR24=1.D0/24.D0
C
      IF(SF%ELM.EQ.41.AND.SG%ELM.EQ.41.AND.SH%ELM.EQ.41) THEN
C
C-----------------------------------------------------------------------
C
C   LINEAR DISCRETISATION OF DIFFUSION COEFFICIENTS
C
C   LOOP ON THE PRISMS
C
      DO 20 IELEM=1,NELEM
C
      IGLOB(1)=IKLE(IELEM,1)
      IGLOB(2)=IKLE(IELEM,2)
      IGLOB(3)=IKLE(IELEM,3)
      IGLOB(4)=IKLE(IELEM,4)
      IGLOB(5)=IKLE(IELEM,5)
      IGLOB(6)=IKLE(IELEM,6)
C
      IF(IGLOB(1).GT.IGLOB(2)) THEN
        SENS(1)=1
      ELSE
        SENS(1)=2
      ENDIF
      IF(IGLOB(2).GT.IGLOB(3)) THEN
        SENS(2)=1
      ELSE
        SENS(2)=2
      ENDIF
      IF(IGLOB(3).GT.IGLOB(1)) THEN
        SENS(3)=1
      ELSE
        SENS(3)=2
      ENDIF
C
C     FOOTPRINT OF THE PRISM
C
      SURF=0.5D0*
     &    ((X(IGLOB(2))-X(IGLOB(1)))*(Y(IGLOB(3))-Y(IGLOB(1)))
     &    -(X(IGLOB(3))-X(IGLOB(1)))*(Y(IGLOB(2))-Y(IGLOB(1))))
C
C INITIALISES TO 0.D0
C
      T(IELEM,1)=0.D0
      T(IELEM,2)=0.D0
      T(IELEM,3)=0.D0
      T(IELEM,4)=0.D0
      T(IELEM,5)=0.D0
      T(IELEM,6)=0.D0
C
      XM(IELEM, 1)= 0.D0
      XM(IELEM, 2)= 0.D0
      XM(IELEM, 3)= 0.D0
      XM(IELEM, 4)= 0.D0
      XM(IELEM, 5)= 0.D0
C
      XM(IELEM, 6)= 0.D0
      XM(IELEM, 7)= 0.D0
      XM(IELEM, 8)= 0.D0
      XM(IELEM, 9)= 0.D0
C
      XM(IELEM,10)= 0.D0
      XM(IELEM,11)= 0.D0
      XM(IELEM,12)= 0.D0
C
      XM(IELEM,13)= 0.D0
      XM(IELEM,14)= 0.D0
      XM(IELEM,15)= 0.D0
C
C-----------------------------------------------------------------------
C     LOOP OVER  TI
C
      DO 40 I=1,3
C
C     TETRAHEDRON POINTS NUMBERS IN THE PRISM NUMBERING
C
      NUM1=TETRA(SENS(1),SENS(2),SENS(3),I,1)
      NUM2=TETRA(SENS(1),SENS(2),SENS(3),I,2)
      NUM3=TETRA(SENS(1),SENS(2),SENS(3),I,3)
      NUM4=TETRA(SENS(1),SENS(2),SENS(3),I,4)
C
C     GLOBAL NUMBERS OF THE TETRAHEDRON POINTS
C
      IT1=IGLOB(NUM1)
      IT2=IGLOB(NUM2)
      IT3=IGLOB(NUM3)
      IT4=IGLOB(NUM4)
C
C VISCOSITY ALONG X Y AND Z
C
      HTOT=F(IT1)+F(IT2)+F(IT3)+F(IT4)
      VTOT=G(IT1)+G(IT2)+G(IT3)+G(IT4)
      WTOT=H(IT1)+H(IT2)+H(IT3)+H(IT4)
C
      X2=X(IT2)-X(IT1)
      Y2=Y(IT2)-Y(IT1)
      Z2=Z(IT2)-Z(IT1)
      X3=X(IT3)-X(IT1)
      Y3=Y(IT3)-Y(IT1)
      Z3=Z(IT3)-Z(IT1)
      X4=X(IT4)-X(IT1)
      Y4=Y(IT4)-Y(IT1)
      Z4=Z(IT4)-Z(IT1)
C
C-----------------------------------------------------------------------
C    COEF:  THANKS MAPLE...
C-----------------------------------------------------------------------
C
      T1  = X2*Y3
      T3  = X2*Y4
      T5  = X3*Y2
      T7  = X4*Y2
      T9  = X3*Z2
      T11 = X4*Z2
C     T13 = 4 TIMES THE TETRAHEDRON VOLUME ?
      T13 = T1*Z4-T3*Z3-T5*Z4+T7*Z3+T9*Y4-T11*Y3
C
      T15 = -Y2*Z3+Y3*Z2
      T17 =  X2*Z4-T11
      T19 = -Y3*Z4+Y4*Z3
      T21 =  X2*Z3-T9
      T23 = -Y2*Z4+Y4*Z2
      T35 =  X3*Z4-X4*Z3
      T49 =  X3*Y4-X4*Y3
C
C     IF WIDTH MORE THAN 0.01 M
C
      COEF=XMUL*SUR24/MAX(T13,0.01D0*SURF)
C
      T28 = -T19+T23-T15
      T42 = -T35+T17-T21
      T51 = T3-T7
      T54 = T49-T3+T7+T1-T5
C
      DIAG1  =COEF*( HTOT*T28**2 +VTOT*T42**2 +WTOT*T54**2)
      DIAG2  =COEF*( HTOT*T19**2 +VTOT*T35**2 +WTOT*T49**2)
      DIAG3  =COEF*( HTOT*T23**2 +VTOT*T17**2 +WTOT*T51**2)
      EXTR12 =COEF*( HTOT*T28*T19+VTOT*T42*T35-WTOT*T54*T49)
      EXTR13 =COEF*(-HTOT*T28*T23-VTOT*T42*T17+WTOT*T54*T51)
      EXTR23 =COEF*(-HTOT*T19*T23-VTOT*T35*T17-WTOT*T49*T51)
C
C     DEDUCED FROM PROPERTIES OF THE DIFFUSION MATRIX
C
      EXTR14 = -(EXTR13+EXTR12+DIAG1)
      EXTR24 = -(EXTR23+DIAG2+EXTR12)
      EXTR34 = -(DIAG3+EXTR23+EXTR13)
      DIAG4  = -(EXTR14+EXTR24+EXTR34)
C
C ASSEMBLES ON PRISM
C
      T(IELEM,NUM1) = T(IELEM,NUM1)+ DIAG1
      T(IELEM,NUM2) = T(IELEM,NUM2)+ DIAG2
      T(IELEM,NUM3) = T(IELEM,NUM3)+ DIAG3
      T(IELEM,NUM4) = T(IELEM,NUM4)+ DIAG4
C
      XM(IELEM,STO(NUM1,NUM2))=XM(IELEM,STO(NUM1,NUM2))+EXTR12
      XM(IELEM,STO(NUM1,NUM3))=XM(IELEM,STO(NUM1,NUM3))+EXTR13
      XM(IELEM,STO(NUM1,NUM4))=XM(IELEM,STO(NUM1,NUM4))+EXTR14
      XM(IELEM,STO(NUM2,NUM3))=XM(IELEM,STO(NUM2,NUM3))+EXTR23
      XM(IELEM,STO(NUM2,NUM4))=XM(IELEM,STO(NUM2,NUM4))+EXTR24
      XM(IELEM,STO(NUM3,NUM4))=XM(IELEM,STO(NUM3,NUM4))+EXTR34
C
40    CONTINUE
C
C---------------------------------------------------------------
C
20    CONTINUE
C
      ELSEIF(SF%ELM.EQ.40.AND.SG%ELM.EQ.40.AND.SH%ELM.EQ.40) THEN
C
C
C-----------------------------------------------------------------------
C
C   P0 DISCRETISATION OF DIFFUSION COEFFICIENTS (CONSTANT ON A PRISM)
C
C   LOOP ON THE PRISMS
C
      DO 21 IELEM=1,NELEM
C
      IGLOB(1)=IKLE(IELEM,1)
      IGLOB(2)=IKLE(IELEM,2)
      IGLOB(3)=IKLE(IELEM,3)
      IGLOB(4)=IKLE(IELEM,4)
      IGLOB(5)=IKLE(IELEM,5)
      IGLOB(6)=IKLE(IELEM,6)
C
      IF(IGLOB(1).GT.IGLOB(2)) THEN
        SENS(1)=1
      ELSE
        SENS(1)=2
      ENDIF
      IF(IGLOB(2).GT.IGLOB(3)) THEN
        SENS(2)=1
      ELSE
        SENS(2)=2
      ENDIF
      IF(IGLOB(3).GT.IGLOB(1)) THEN
        SENS(3)=1
      ELSE
        SENS(3)=2
      ENDIF
C
C     FOOTPRINT OF THE PRISM
C
      SURF=0.5D0*
     &    ((X(IGLOB(2))-X(IGLOB(1)))*(Y(IGLOB(3))-Y(IGLOB(1)))
     &    -(X(IGLOB(3))-X(IGLOB(1)))*(Y(IGLOB(2))-Y(IGLOB(1))))
C
C INITIALISES TO 0.D0
C
      T(IELEM,1)=0.D0
      T(IELEM,2)=0.D0
      T(IELEM,3)=0.D0
      T(IELEM,4)=0.D0
      T(IELEM,5)=0.D0
      T(IELEM,6)=0.D0
C
      XM(IELEM, 1)= 0.D0
      XM(IELEM, 2)= 0.D0
      XM(IELEM, 3)= 0.D0
      XM(IELEM, 4)= 0.D0
      XM(IELEM, 5)= 0.D0
C
      XM(IELEM, 6)= 0.D0
      XM(IELEM, 7)= 0.D0
      XM(IELEM, 8)= 0.D0
      XM(IELEM, 9)= 0.D0
C
      XM(IELEM,10)= 0.D0
      XM(IELEM,11)= 0.D0
      XM(IELEM,12)= 0.D0
C
      XM(IELEM,13)= 0.D0
      XM(IELEM,14)= 0.D0
      XM(IELEM,15)= 0.D0
C
C-----------------------------------------------------------------------
C     LOOP OVER  TI
C
      DO 41 I=1,3
C
C     TETRAHEDRON POINTS NUMBERS IN THE PRISM NUMBERING
C
      NUM1=TETRA(SENS(1),SENS(2),SENS(3),I,1)
      NUM2=TETRA(SENS(1),SENS(2),SENS(3),I,2)
      NUM3=TETRA(SENS(1),SENS(2),SENS(3),I,3)
      NUM4=TETRA(SENS(1),SENS(2),SENS(3),I,4)
C
C     GLOBAL NUMBERS OF THE TETRAHEDRON POINTS
C
      IT1=IGLOB(NUM1)
      IT2=IGLOB(NUM2)
      IT3=IGLOB(NUM3)
      IT4=IGLOB(NUM4)
C
C VISCOSITY ALONG X Y AND Z
C
      HTOT=4*F(IELEM)
      VTOT=4*G(IELEM)
      WTOT=4*H(IELEM)
C
      X2=X(IT2)-X(IT1)
      Y2=Y(IT2)-Y(IT1)
      Z2=Z(IT2)-Z(IT1)
      X3=X(IT3)-X(IT1)
      Y3=Y(IT3)-Y(IT1)
      Z3=Z(IT3)-Z(IT1)
      X4=X(IT4)-X(IT1)
      Y4=Y(IT4)-Y(IT1)
      Z4=Z(IT4)-Z(IT1)
C
C-----------------------------------------------------------------------
C    COEF:  THANKS MAPLE...
C-----------------------------------------------------------------------
C
      T1  = X2*Y3
      T3  = X2*Y4
      T5  = X3*Y2
      T7  = X4*Y2
      T9  = X3*Z2
      T11 = X4*Z2
C     T13 = 4 TIMES THE TETRAHEDRON VOLUME ?
      T13 = T1*Z4-T3*Z3-T5*Z4+T7*Z3+T9*Y4-T11*Y3
C
      T15 = -Y2*Z3+Y3*Z2
      T17 =  X2*Z4-T11
      T19 = -Y3*Z4+Y4*Z3
      T21 =  X2*Z3-T9
      T23 = -Y2*Z4+Y4*Z2
      T35 =  X3*Z4-X4*Z3
      T49 =  X3*Y4-X4*Y3
C
C     IF WIDTH MORE THAN 0.01 M
C
      COEF=XMUL*SUR24/MAX(T13,0.01D0*SURF)
C
      T28 = -T19+T23-T15
      T42 = -T35+T17-T21
      T51 = T3-T7
      T54 = T49-T3+T7+T1-T5
C
      DIAG1  =COEF*( HTOT*T28**2 +VTOT*T42**2 +WTOT*T54**2)
      DIAG2  =COEF*( HTOT*T19**2 +VTOT*T35**2 +WTOT*T49**2)
      DIAG3  =COEF*( HTOT*T23**2 +VTOT*T17**2 +WTOT*T51**2)
      EXTR12 =COEF*( HTOT*T28*T19+VTOT*T42*T35-WTOT*T54*T49)
      EXTR13 =COEF*(-HTOT*T28*T23-VTOT*T42*T17+WTOT*T54*T51)
      EXTR23 =COEF*(-HTOT*T19*T23-VTOT*T35*T17-WTOT*T49*T51)
C
C     DEDUCED FROM PROPERTIES OF THE DIFFUSION MATRIX
C
      EXTR14 = -(EXTR13+EXTR12+DIAG1)
      EXTR24 = -(EXTR23+DIAG2+EXTR12)
      EXTR34 = -(DIAG3+EXTR23+EXTR13)
      DIAG4  = -(EXTR14+EXTR24+EXTR34)
C
C ASSEMBLES ON PRISM
C
      T(IELEM,NUM1) = T(IELEM,NUM1)+ DIAG1
      T(IELEM,NUM2) = T(IELEM,NUM2)+ DIAG2
      T(IELEM,NUM3) = T(IELEM,NUM3)+ DIAG3
      T(IELEM,NUM4) = T(IELEM,NUM4)+ DIAG4
C
      XM(IELEM,STO(NUM1,NUM2))=XM(IELEM,STO(NUM1,NUM2))+EXTR12
      XM(IELEM,STO(NUM1,NUM3))=XM(IELEM,STO(NUM1,NUM3))+EXTR13
      XM(IELEM,STO(NUM1,NUM4))=XM(IELEM,STO(NUM1,NUM4))+EXTR14
      XM(IELEM,STO(NUM2,NUM3))=XM(IELEM,STO(NUM2,NUM3))+EXTR23
      XM(IELEM,STO(NUM2,NUM4))=XM(IELEM,STO(NUM2,NUM4))+EXTR24
      XM(IELEM,STO(NUM3,NUM4))=XM(IELEM,STO(NUM3,NUM4))+EXTR34
C
41    CONTINUE
C
C---------------------------------------------------------------
C
21    CONTINUE
C
C-----------------------------------------------------------------------
C
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,1000) SF%ELM,SG%ELM,SH%ELM
        IF (LNG.EQ.2) WRITE(LU,1001) SF%ELM,SG%ELM,SH%ELM
1000    FORMAT(1X,'MT02PT (BIEF) : MAUVAIS TYPE DE F,G OU H : ',
     &  I6,1X,I6,1X,I6)
1001    FORMAT(1X,'MT02PT (BIEF) : WRONG TYPE OF F,G OR H: ',
     &  I6,1X,I6,1X,I6)
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  TREATMENT OF HYDROSTATIC INCONSISTENCIES
C
      IF(INCHYD) THEN
C
      DO 22 IELEM=1,NELEM
C
         I1=IKLE(IELEM,1)
         I2=IKLE(IELEM,2)
         I3=IKLE(IELEM,3)
         I4=IKLE(IELEM,4)
         I5=IKLE(IELEM,5)
         I6=IKLE(IELEM,6)
C
         IF(MAX(Z(I1),Z(I2),Z(I3)).GT.MIN(Z(I4),Z(I5),Z(I6))) THEN
C
           T(IELEM,1)  =0.D0
           T(IELEM,2)  =0.D0
           T(IELEM,3)  =0.D0
           T(IELEM,4)  =0.D0
           T(IELEM,5)  =0.D0
           T(IELEM,6)  =0.D0
           XM(IELEM, 1)=0.D0
           XM(IELEM, 2)=0.D0
           XM(IELEM, 3)=0.D0
           XM(IELEM, 4)=0.D0
           XM(IELEM, 5)=0.D0
           XM(IELEM, 6)=0.D0
           XM(IELEM, 7)=0.D0
           XM(IELEM, 8)=0.D0
           XM(IELEM, 9)=0.D0
           XM(IELEM,10)=0.D0
           XM(IELEM,11)=0.D0
           XM(IELEM,12)=0.D0
           XM(IELEM,13)=0.D0
           XM(IELEM,14)=0.D0
           XM(IELEM,15)=0.D0
C
         ENDIF
C
22    CONTINUE
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