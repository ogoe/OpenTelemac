C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MATRIX VECTOR OPERATIONS FOR P1 TRIANGLES.
!>  @code
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON VECTORS X,Y AND MATRIX M.<br>
!>   THE RESULT IS VECTOR X.<br>
!>   THESE OPERATIONS ARE DIFFERENT DEPENDING ON THE DIAGONAL TYPE
!>   AND THE TYPE OF EXTRADIAGONAL TERMS.<br>
!>   IMPLEMENTED OPERATIONS:<br>
!>      OP = 'X=AY    '  : X = AY
!>      OP = 'X=-AY   '  : X = - AY
!>      OP = 'X=X+AY  '  : X = X + AY
!>      OP = 'X=X-AY  '  : X = X - AY
!>      OP = 'X=X+CAY '  : X = X + C AY
!>      OP = 'X=TAY   '  : X = TA Y (TRANSPOSE OF A)
!>      OP = 'X=-TAY  '  : X = - TA Y (- TRANSPOSE OF A)
!>      OP = 'X=X+TAY '  : X = X + TA Y
!>      OP = 'X=X-TAY '  : X = X - TA Y
!>      OP = 'X=X+CTAY'  : X = X + C TA Y
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, DA, IKLE1, IKLE2, IKLE3, IKLE4, IKLE5, IKLE6, NELEM, NELMAX, NPOIN, OP, TYPDIA, TYPEXT, W1, W2, W3, W4, W5, W6, X, XA, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I1, I2, I3, I4, I5, I6, IELEM, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MV0606
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MATVCT()

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
!> </td><td> 05/02/91
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C
!></td><td>--></td><td>CONSTANTE DONNEE
!>    </td></tr>
!>          <tr><td>DA
!></td><td>--></td><td>DIAGONALE DE LA MATRICE
!>    </td></tr>
!>          <tr><td>IKLE1,
!></td><td>--></td><td>CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
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
!></td><td>--></td><td>NOMBRE D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS.
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>OPERATION A EFFECTUER
!>    </td></tr>
!>          <tr><td>TYPDIA
!></td><td>--></td><td>TYPE DE LA DIAGONALE (CHAINE DE CARACTERES)
!>                  TYPDIA = 'Q' : DIAGONALE QUELCONQUE
!>                  TYPDIA = 'I' : DIAGONALE IDENTITE.
!>                  TYPDIA = '0' : DIAGONALE NULLE.
!>    </td></tr>
!>          <tr><td>TYPEXT
!></td><td>--></td><td>TYPEXT = 'Q' : QUELCONQUES.
!>                  TYPEXT = 'S' : SYMETRIQUES.
!>                  TYPEXT = '0' : NULS.
!>    </td></tr>
!>          <tr><td>W1,
!></td><td><--</td><td>TABLEAUX DE TRAVAIL DE DIMENSION NELEM
!>                  QUI CONTIENDRONT UNE PARTIE DU RESULTAT SOUS
!>                  FORME NON ASSEMBLEE.
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
!>          <tr><td>W5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VECTEUR IMAGE
!>    </td></tr>
!>          <tr><td>XA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA12,
!></td><td>--></td><td>TERMES EXTRADIAGONAUX ELEMENTAIRES
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>VECTEUR OPERANDE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MV0606
     &(OP, X , DA,TYPDIA,XA,TYPEXT, Y,C,
     & IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,
     & NPOIN,NELEM,NELMAX,W1,W2,W3,W4,W5,W6)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
C| DA             |-->| DIAGONALE DE LA MATRICE
C| IKLE1,         |-->| CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| IKLE5          |---| 
C| IKLE6          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS.
C| NELMAX         |---| 
C| NPOIN          |-->| NOMBRE DE POINTS.
C| OP             |-->| OPERATION A EFFECTUER
C| TYPDIA         |-->| TYPE DE LA DIAGONALE (CHAINE DE CARACTERES)
C|                |   | TYPDIA = 'Q' : DIAGONALE QUELCONQUE
C|                |   | TYPDIA = 'I' : DIAGONALE IDENTITE.
C|                |   | TYPDIA = '0' : DIAGONALE NULLE.
C| TYPEXT         |-->| TYPEXT = 'Q' : QUELCONQUES.
C|                |   | TYPEXT = 'S' : SYMETRIQUES.
C|                |   | TYPEXT = '0' : NULS.
C| W1,            |<--| TABLEAUX DE TRAVAIL DE DIMENSION NELEM
C|                |   | QUI CONTIENDRONT UNE PARTIE DU RESULTAT SOUS
C|                |   | FORME NON ASSEMBLEE.
C| W2             |---| 
C| W3             |---| 
C| W4             |---| 
C| W5             |---| 
C| W6             |---| 
C| X             |<--| VECTEUR IMAGE
C| XA             |---| 
C| XA12,          |-->| TERMES EXTRADIAGONAUX ELEMENTAIRES
C| Y             |-->| VECTEUR OPERANDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MV0606 => MV0606
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN
      INTEGER, INTENT(IN) :: IKLE1(*),IKLE2(*),IKLE3(*)
      INTEGER, INTENT(IN) :: IKLE4(*),IKLE5(*),IKLE6(*)
C
      DOUBLE PRECISION, INTENT(INOUT) :: W1(*),W2(*),W3(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W4(*),W5(*),W6(*)
      DOUBLE PRECISION, INTENT(IN) :: Y(*),DA(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN) :: XA(NELMAX,*)
      DOUBLE PRECISION, INTENT(IN) :: C
C
      CHARACTER(LEN=8), INTENT(IN) :: OP
      CHARACTER(LEN=1), INTENT(IN) :: TYPDIA,TYPEXT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,I1,I2,I3,I4,I5,I6
      DOUBLE PRECISION Z(1)
C
C-----------------------------------------------------------------------
C
      IF(OP(1:8).EQ.'X=AY    ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 10 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) =
     &                   + XA(IELEM,1) * Y(I2)
     &                   + XA(IELEM,2) * Y(I3)
     &                   + XA(IELEM,3) * Y(I4)
     &                   + XA(IELEM,4) * Y(I5)
     &                   + XA(IELEM,5) * Y(I6)
C
           W2(IELEM) =
     &                   + XA(IELEM,1) * Y(I1)
     &                   + XA(IELEM,6) * Y(I3)
     &                   + XA(IELEM,7) * Y(I4)
     &                   + XA(IELEM,8) * Y(I5)
     &                   + XA(IELEM,9) * Y(I6)
C
           W3(IELEM) =
     &                   + XA(IELEM,2)  * Y(I1)
     &                   + XA(IELEM,6)  * Y(I2)
     &                   + XA(IELEM,10) * Y(I4)
     &                   + XA(IELEM,11) * Y(I5)
     &                   + XA(IELEM,12) * Y(I6)
C
           W4(IELEM) =
     &                   + XA(IELEM,3)  * Y(I1)
     &                   + XA(IELEM,7)  * Y(I2)
     &                   + XA(IELEM,10) * Y(I3)
     &                   + XA(IELEM,13) * Y(I5)
     &                   + XA(IELEM,14) * Y(I6)
C
           W5(IELEM) =
     &                   + XA(IELEM,4)  * Y(I1)
     &                   + XA(IELEM,8)  * Y(I2)
     &                   + XA(IELEM,11) * Y(I3)
     &                   + XA(IELEM,13) * Y(I4)
     &                   + XA(IELEM,15) * Y(I6)
C
           W6(IELEM) =
     &                   + XA(IELEM,5)  * Y(I1)
     &                   + XA(IELEM,9)  * Y(I2)
     &                   + XA(IELEM,12) * Y(I3)
     &                   + XA(IELEM,14) * Y(I4)
     &                   + XA(IELEM,15) * Y(I5)
C
10         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 20 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) =
     &                   + XA(IELEM, 1) * Y(I2)
     &                   + XA(IELEM, 2) * Y(I3)
     &                   + XA(IELEM, 3) * Y(I4)
     &                   + XA(IELEM, 4) * Y(I5)
     &                   + XA(IELEM, 5) * Y(I6)
C
           W2(IELEM) =
     &                   + XA(IELEM,16) * Y(I1)
     &                   + XA(IELEM, 6) * Y(I3)
     &                   + XA(IELEM, 7) * Y(I4)
     &                   + XA(IELEM, 8) * Y(I5)
     &                   + XA(IELEM, 9) * Y(I6)
C
           W3(IELEM) =
     &                   + XA(IELEM,17) * Y(I1)
     &                   + XA(IELEM,21) * Y(I2)
     &                   + XA(IELEM,10) * Y(I4)
     &                   + XA(IELEM,11) * Y(I5)
     &                   + XA(IELEM,12) * Y(I6)
C
           W4(IELEM) =
     &                   + XA(IELEM,18) * Y(I1)
     &                   + XA(IELEM,22) * Y(I2)
     &                   + XA(IELEM,25) * Y(I3)
     &                   + XA(IELEM,13) * Y(I5)
     &                   + XA(IELEM,14) * Y(I6)
C
           W5(IELEM) =
     &                   + XA(IELEM,19) * Y(I1)
     &                   + XA(IELEM,23) * Y(I2)
     &                   + XA(IELEM,26) * Y(I3)
     &                   + XA(IELEM,28) * Y(I4)
     &                   + XA(IELEM,15) * Y(I6)
C
           W6(IELEM) =
     &                   + XA(IELEM,20) * Y(I1)
     &                   + XA(IELEM,24) * Y(I2)
     &                   + XA(IELEM,27) * Y(I3)
     &                   + XA(IELEM,29) * Y(I4)
     &                   + XA(IELEM,30) * Y(I5)
C
20         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'0') THEN
C
           CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W4 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W5 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W6 , Y , Z , 0.D0 , NELEM )
C
         ELSE
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
C
         ENDIF
C
C   CONTRIBUTION OF THE DIAGONAL:
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=YZ    ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=Y     ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=-AY   ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 11 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) =
     &                   - XA(IELEM,1) * Y(I2)
     &                   - XA(IELEM,2) * Y(I3)
     &                   - XA(IELEM,3) * Y(I4)
     &                   - XA(IELEM,4) * Y(I5)
     &                   - XA(IELEM,5) * Y(I6)
C
           W2(IELEM) =
     &                   - XA(IELEM,1) * Y(I1)
     &                   - XA(IELEM,6) * Y(I3)
     &                   - XA(IELEM,7) * Y(I4)
     &                   - XA(IELEM,8) * Y(I5)
     &                   - XA(IELEM,9) * Y(I6)
C
           W3(IELEM) =
     &                   - XA(IELEM,2)  * Y(I1)
     &                   - XA(IELEM,6)  * Y(I2)
     &                   - XA(IELEM,10) * Y(I4)
     &                   - XA(IELEM,11) * Y(I5)
     &                   - XA(IELEM,12) * Y(I6)
C
           W4(IELEM) =
     &                   - XA(IELEM,3)  * Y(I1)
     &                   - XA(IELEM,7)  * Y(I2)
     &                   - XA(IELEM,10) * Y(I3)
     &                   - XA(IELEM,13) * Y(I5)
     &                   - XA(IELEM,14) * Y(I6)
C
           W5(IELEM) =
     &                   - XA(IELEM,4)  * Y(I1)
     &                   - XA(IELEM,8)  * Y(I2)
     &                   - XA(IELEM,11) * Y(I3)
     &                   - XA(IELEM,13) * Y(I4)
     &                   - XA(IELEM,15) * Y(I6)
C
           W6(IELEM) =
     &                   - XA(IELEM,5)  * Y(I1)
     &                   - XA(IELEM,9)  * Y(I2)
     &                   - XA(IELEM,12) * Y(I3)
     &                   - XA(IELEM,14) * Y(I4)
     &                   - XA(IELEM,15) * Y(I5)
C
11         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 21 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) =
     &                   - XA(IELEM, 1) * Y(I2)
     &                   - XA(IELEM, 2) * Y(I3)
     &                   - XA(IELEM, 3) * Y(I4)
     &                   - XA(IELEM, 4) * Y(I5)
     &                   - XA(IELEM, 5) * Y(I6)
C
           W2(IELEM) =
     &                   - XA(IELEM,16) * Y(I1)
     &                   - XA(IELEM, 6) * Y(I3)
     &                   - XA(IELEM, 7) * Y(I4)
     &                   - XA(IELEM, 8) * Y(I5)
     &                   - XA(IELEM, 9) * Y(I6)
C
           W3(IELEM) =
     &                   - XA(IELEM,17) * Y(I1)
     &                   - XA(IELEM,21) * Y(I2)
     &                   - XA(IELEM,10) * Y(I4)
     &                   - XA(IELEM,11) * Y(I5)
     &                   - XA(IELEM,12) * Y(I6)
C
           W4(IELEM) =
     &                   - XA(IELEM,18) * Y(I1)
     &                   - XA(IELEM,22) * Y(I2)
     &                   - XA(IELEM,25) * Y(I3)
     &                   - XA(IELEM,13) * Y(I5)
     &                   - XA(IELEM,14) * Y(I6)
C
           W5(IELEM) =
     &                   - XA(IELEM,19) * Y(I1)
     &                   - XA(IELEM,23) * Y(I2)
     &                   - XA(IELEM,26) * Y(I3)
     &                   - XA(IELEM,28) * Y(I4)
     &                   - XA(IELEM,15) * Y(I6)
C
           W6(IELEM) =
     &                   - XA(IELEM,20) * Y(I1)
     &                   - XA(IELEM,24) * Y(I2)
     &                   - XA(IELEM,27) * Y(I3)
     &                   - XA(IELEM,29) * Y(I4)
     &                   - XA(IELEM,30) * Y(I5)
C
21         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'0') THEN
C
           CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W4 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W5 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W6 , Y , Z , 0.D0 , NELEM )
C
         ELSE
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
C
         ENDIF
C
C   CONTRIBUTION OF THE DIAGONAL:
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=-YZ   ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=-Y    ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+AY  ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 30 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) = W1(IELEM)
     &                   + XA(IELEM,1) * Y(I2)
     &                   + XA(IELEM,2) * Y(I3)
     &                   + XA(IELEM,3) * Y(I4)
     &                   + XA(IELEM,4) * Y(I5)
     &                   + XA(IELEM,5) * Y(I6)
C
           W2(IELEM) = W2(IELEM)
     &                   + XA(IELEM,1) * Y(I1)
     &                   + XA(IELEM,6) * Y(I3)
     &                   + XA(IELEM,7) * Y(I4)
     &                   + XA(IELEM,8) * Y(I5)
     &                   + XA(IELEM,9) * Y(I6)
C
           W3(IELEM) = W3(IELEM)
     &                   + XA(IELEM,2)  * Y(I1)
     &                   + XA(IELEM,6)  * Y(I2)
     &                   + XA(IELEM,10) * Y(I4)
     &                   + XA(IELEM,11) * Y(I5)
     &                   + XA(IELEM,12) * Y(I6)
C
           W4(IELEM) = W4(IELEM)
     &                   + XA(IELEM,3)  * Y(I1)
     &                   + XA(IELEM,7)  * Y(I2)
     &                   + XA(IELEM,10) * Y(I3)
     &                   + XA(IELEM,13) * Y(I5)
     &                   + XA(IELEM,14) * Y(I6)
C
           W5(IELEM) = W5(IELEM)
     &                   + XA(IELEM,4)  * Y(I1)
     &                   + XA(IELEM,8)  * Y(I2)
     &                   + XA(IELEM,11) * Y(I3)
     &                   + XA(IELEM,13) * Y(I4)
     &                   + XA(IELEM,15) * Y(I6)
C
           W6(IELEM) = W6(IELEM)
     &                   + XA(IELEM,5)  * Y(I1)
     &                   + XA(IELEM,9)  * Y(I2)
     &                   + XA(IELEM,12) * Y(I3)
     &                   + XA(IELEM,14) * Y(I4)
     &                   + XA(IELEM,15) * Y(I5)
C
30         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 40  IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) = W1(IELEM)
     &                   + XA(IELEM, 1) * Y(I2)
     &                   + XA(IELEM, 2) * Y(I3)
     &                   + XA(IELEM, 3) * Y(I4)
     &                   + XA(IELEM, 4) * Y(I5)
     &                   + XA(IELEM, 5) * Y(I6)
C
           W2(IELEM) = W2(IELEM)
     &                   + XA(IELEM,16) * Y(I1)
     &                   + XA(IELEM, 6) * Y(I3)
     &                   + XA(IELEM, 7) * Y(I4)
     &                   + XA(IELEM, 8) * Y(I5)
     &                   + XA(IELEM, 9) * Y(I6)
C
           W3(IELEM) = W3(IELEM)
     &                   + XA(IELEM,17) * Y(I1)
     &                   + XA(IELEM,21) * Y(I2)
     &                   + XA(IELEM,10) * Y(I4)
     &                   + XA(IELEM,11) * Y(I5)
     &                   + XA(IELEM,12) * Y(I6)
C
           W4(IELEM) = W4(IELEM)
     &                   + XA(IELEM,18) * Y(I1)
     &                   + XA(IELEM,22) * Y(I2)
     &                   + XA(IELEM,25) * Y(I3)
     &                   + XA(IELEM,13) * Y(I5)
     &                   + XA(IELEM,14) * Y(I6)
C
           W5(IELEM) = W5(IELEM)
     &                   + XA(IELEM,19) * Y(I1)
     &                   + XA(IELEM,23) * Y(I2)
     &                   + XA(IELEM,26) * Y(I3)
     &                   + XA(IELEM,28) * Y(I4)
     &                   + XA(IELEM,15) * Y(I6)
C
           W6(IELEM) = W6(IELEM)
     &                   + XA(IELEM,20) * Y(I1)
     &                   + XA(IELEM,24) * Y(I2)
     &                   + XA(IELEM,27) * Y(I3)
     &                   + XA(IELEM,29) * Y(I4)
     &                   + XA(IELEM,30) * Y(I5)
C
40         CONTINUE
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
C
         ENDIF
C
C   CONTRIBUTION OF THE DIAGONAL:
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X+YZ  ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X+Y   ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X-AY  ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 50 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) = W1(IELEM)
     &                   - XA(IELEM,1) * Y(I2)
     &                   - XA(IELEM,2) * Y(I3)
     &                   - XA(IELEM,3) * Y(I4)
     &                   - XA(IELEM,4) * Y(I5)
     &                   - XA(IELEM,5) * Y(I6)
C
           W2(IELEM) = W2(IELEM)
     &                   - XA(IELEM,1) * Y(I1)
     &                   - XA(IELEM,6) * Y(I3)
     &                   - XA(IELEM,7) * Y(I4)
     &                   - XA(IELEM,8) * Y(I5)
     &                   - XA(IELEM,9) * Y(I6)
C
           W3(IELEM) = W3(IELEM)
     &                   - XA(IELEM,2)  * Y(I1)
     &                   - XA(IELEM,6)  * Y(I2)
     &                   - XA(IELEM,10) * Y(I4)
     &                   - XA(IELEM,11) * Y(I5)
     &                   - XA(IELEM,12) * Y(I6)
C
           W4(IELEM) = W4(IELEM)
     &                   - XA(IELEM,3)  * Y(I1)
     &                   - XA(IELEM,7)  * Y(I2)
     &                   - XA(IELEM,10) * Y(I3)
     &                   - XA(IELEM,13) * Y(I5)
     &                   - XA(IELEM,14) * Y(I6)
C
           W5(IELEM) = W5(IELEM)
     &                   - XA(IELEM,4)  * Y(I1)
     &                   - XA(IELEM,8)  * Y(I2)
     &                   - XA(IELEM,11) * Y(I3)
     &                   - XA(IELEM,13) * Y(I4)
     &                   - XA(IELEM,15) * Y(I6)
C
           W6(IELEM) = W6(IELEM)
     &                   - XA(IELEM,5)  * Y(I1)
     &                   - XA(IELEM,9)  * Y(I2)
     &                   - XA(IELEM,12) * Y(I3)
     &                   - XA(IELEM,14) * Y(I4)
     &                   - XA(IELEM,15) * Y(I5)
C
50         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 60 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) = W1(IELEM)
     &                   - XA(IELEM, 1) * Y(I2)
     &                   - XA(IELEM, 2) * Y(I3)
     &                   - XA(IELEM, 3) * Y(I4)
     &                   - XA(IELEM, 4) * Y(I5)
     &                   - XA(IELEM, 5) * Y(I6)
C
           W2(IELEM) = W2(IELEM)
     &                   - XA(IELEM,16) * Y(I1)
     &                   - XA(IELEM, 6) * Y(I3)
     &                   - XA(IELEM, 7) * Y(I4)
     &                   - XA(IELEM, 8) * Y(I5)
     &                   - XA(IELEM, 9) * Y(I6)
C
           W3(IELEM) = W3(IELEM)
     &                   - XA(IELEM,17) * Y(I1)
     &                   - XA(IELEM,21) * Y(I2)
     &                   - XA(IELEM,10) * Y(I4)
     &                   - XA(IELEM,11) * Y(I5)
     &                   - XA(IELEM,12) * Y(I6)
C
           W4(IELEM) = W4(IELEM)
     &                   - XA(IELEM,18) * Y(I1)
     &                   - XA(IELEM,22) * Y(I2)
     &                   - XA(IELEM,25) * Y(I3)
     &                   - XA(IELEM,13) * Y(I5)
     &                   - XA(IELEM,14) * Y(I6)
C
           W5(IELEM) = W5(IELEM)
     &                   - XA(IELEM,19) * Y(I1)
     &                   - XA(IELEM,23) * Y(I2)
     &                   - XA(IELEM,26) * Y(I3)
     &                   - XA(IELEM,28) * Y(I4)
     &                   - XA(IELEM,15) * Y(I6)
C
           W6(IELEM) = W6(IELEM)
     &                   - XA(IELEM,20) * Y(I1)
     &                   - XA(IELEM,24) * Y(I2)
     &                   - XA(IELEM,27) * Y(I3)
     &                   - XA(IELEM,29) * Y(I4)
     &                   - XA(IELEM,30) * Y(I5)
C
60         CONTINUE
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
C
         ENDIF
C
C   CONTRIBUTION OF THE DIAGONAL:
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X-YZ  ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X-Y   ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+CAY ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 70 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) = W1(IELEM) + C * (
     &                   + XA(IELEM,1) * Y(I2)
     &                   + XA(IELEM,2) * Y(I3)
     &                   + XA(IELEM,3) * Y(I4)
     &                   + XA(IELEM,4) * Y(I5)
     &                   + XA(IELEM,5) * Y(I6)  )
C
           W2(IELEM) = W2(IELEM) + C * (
     &                   + XA(IELEM,1) * Y(I1)
     &                   + XA(IELEM,6) * Y(I3)
     &                   + XA(IELEM,7) * Y(I4)
     &                   + XA(IELEM,8) * Y(I5)
     &                   + XA(IELEM,9) * Y(I6)  )
C
           W3(IELEM) = W3(IELEM) + C * (
     &                   + XA(IELEM,2)  * Y(I1)
     &                   + XA(IELEM,6)  * Y(I2)
     &                   + XA(IELEM,10) * Y(I4)
     &                   + XA(IELEM,11) * Y(I5)
     &                   + XA(IELEM,12)*  Y(I6) )
C
           W4(IELEM) = W4(IELEM) + C * (
     &                   + XA(IELEM,3)  * Y(I1)
     &                   + XA(IELEM,7)  * Y(I2)
     &                   + XA(IELEM,10) * Y(I3)
     &                   + XA(IELEM,13) * Y(I5)
     &                   + XA(IELEM,14) * Y(I6) )
C
           W5(IELEM) = W5(IELEM) + C * (
     &                   + XA(IELEM,4)  * Y(I1)
     &                   + XA(IELEM,8)  * Y(I2)
     &                   + XA(IELEM,11) * Y(I3)
     &                   + XA(IELEM,13) * Y(I4)
     &                   + XA(IELEM,15) * Y(I6) )
C
           W6(IELEM) = W6(IELEM) + C * (
     &                   + XA(IELEM,5)  * Y(I1)
     &                   + XA(IELEM,9)  * Y(I2)
     &                   + XA(IELEM,12) * Y(I3)
     &                   + XA(IELEM,14) * Y(I4)
     &                   + XA(IELEM,15) * Y(I5) )
C
70         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 80 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) = W1(IELEM) + C * (
     &                   + XA(IELEM, 1) * Y(I2)
     &                   + XA(IELEM, 2) * Y(I3)
     &                   + XA(IELEM, 3) * Y(I4)
     &                   + XA(IELEM, 4) * Y(I5)
     &                   + XA(IELEM, 5) * Y(I6) )
C
           W2(IELEM) = W2(IELEM) + C * (
     &                   + XA(IELEM,16) * Y(I1)
     &                   + XA(IELEM, 6) * Y(I3)
     &                   + XA(IELEM, 7) * Y(I4)
     &                   + XA(IELEM, 8) * Y(I5)
     &                   + XA(IELEM, 9) * Y(I6) )
C
           W3(IELEM) = W3(IELEM) + C * (
     &                   + XA(IELEM,17) * Y(I1)
     &                   + XA(IELEM,21) * Y(I2)
     &                   + XA(IELEM,10) * Y(I4)
     &                   + XA(IELEM,11) * Y(I5)
     &                   + XA(IELEM,12) * Y(I6) )
C
           W4(IELEM) = W4(IELEM) + C * (
     &                   + XA(IELEM,18) * Y(I1)
     &                   + XA(IELEM,22) * Y(I2)
     &                   + XA(IELEM,25) * Y(I3)
     &                   + XA(IELEM,13) * Y(I5)
     &                   + XA(IELEM,14) * Y(I6) )
C
           W5(IELEM) = W5(IELEM) + C * (
     &                   + XA(IELEM,19) * Y(I1)
     &                   + XA(IELEM,23) * Y(I2)
     &                   + XA(IELEM,26) * Y(I3)
     &                   + XA(IELEM,28) * Y(I4)
     &                   + XA(IELEM,15) * Y(I6) )
C
           W6(IELEM) = W6(IELEM) + C * (
     &                   + XA(IELEM,20) * Y(I1)
     &                   + XA(IELEM,24) * Y(I2)
     &                   + XA(IELEM,27) * Y(I3)
     &                   + XA(IELEM,29) * Y(I4)
     &                   + XA(IELEM,30) * Y(I5) )
C
80         CONTINUE
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
C
         ENDIF
C
C   CONTRIBUTION OF THE DIAGONAL:
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X+CYZ ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X+CY  ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=TAY   ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 90  IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) =
     &                   + XA(IELEM,1) * Y(I2)
     &                   + XA(IELEM,2) * Y(I3)
     &                   + XA(IELEM,3) * Y(I4)
     &                   + XA(IELEM,4) * Y(I5)
     &                   + XA(IELEM,5) * Y(I6)
C
           W2(IELEM) =
     &                   + XA(IELEM,1) * Y(I1)
     &                   + XA(IELEM,6) * Y(I3)
     &                   + XA(IELEM,7) * Y(I4)
     &                   + XA(IELEM,8) * Y(I5)
     &                   + XA(IELEM,9) * Y(I6)
C
           W3(IELEM) =
     &                   + XA(IELEM,2)  * Y(I1)
     &                   + XA(IELEM,6)  * Y(I2)
     &                   + XA(IELEM,10) * Y(I4)
     &                   + XA(IELEM,11) * Y(I5)
     &                   + XA(IELEM,12) * Y(I6)
C
           W4(IELEM) =
     &                   + XA(IELEM,3)  * Y(I1)
     &                   + XA(IELEM,7)  * Y(I2)
     &                   + XA(IELEM,10) * Y(I3)
     &                   + XA(IELEM,13) * Y(I5)
     &                   + XA(IELEM,14) * Y(I6)
C
           W5(IELEM) =
     &                   + XA(IELEM,4)  * Y(I1)
     &                   + XA(IELEM,8)  * Y(I2)
     &                   + XA(IELEM,11) * Y(I3)
     &                   + XA(IELEM,13) * Y(I4)
     &                   + XA(IELEM,15) * Y(I6)
C
           W6(IELEM) =
     &                   + XA(IELEM,5)  * Y(I1)
     &                   + XA(IELEM,9)  * Y(I2)
     &                   + XA(IELEM,12) * Y(I3)
     &                   + XA(IELEM,14) * Y(I4)
     &                   + XA(IELEM,15) * Y(I5)
C
90         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 100 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) =
     &                   + XA(IELEM,16) * Y(I2)
     &                   + XA(IELEM,17) * Y(I3)
     &                   + XA(IELEM,18) * Y(I4)
     &                   + XA(IELEM,19) * Y(I5)
     &                   + XA(IELEM,20) * Y(I6)
C
           W2(IELEM) =
     &                   + XA(IELEM, 1) * Y(I1)
     &                   + XA(IELEM,21) * Y(I3)
     &                   + XA(IELEM,22) * Y(I4)
     &                   + XA(IELEM,23) * Y(I5)
     &                   + XA(IELEM,24) * Y(I6)
C
           W3(IELEM) =
     &                   + XA(IELEM, 2) * Y(I1)
     &                   + XA(IELEM, 6) * Y(I2)
     &                   + XA(IELEM,25) * Y(I4)
     &                   + XA(IELEM,26) * Y(I5)
     &                   + XA(IELEM,27) * Y(I6)
C
           W4(IELEM) =
     &                   + XA(IELEM, 3) * Y(I1)
     &                   + XA(IELEM, 7) * Y(I2)
     &                   + XA(IELEM,10) * Y(I3)
     &                   + XA(IELEM,28) * Y(I5)
     &                   + XA(IELEM,29) * Y(I6)
C
           W5(IELEM) =
     &                   + XA(IELEM, 4) * Y(I1)
     &                   + XA(IELEM, 8) * Y(I2)
     &                   + XA(IELEM,11) * Y(I3)
     &                   + XA(IELEM,13) * Y(I4)
     &                   + XA(IELEM,30) * Y(I6)
C
           W6(IELEM) =
     &                   + XA(IELEM, 5) * Y(I1)
     &                   + XA(IELEM, 9) * Y(I2)
     &                   + XA(IELEM,12) * Y(I3)
     &                   + XA(IELEM,14) * Y(I4)
     &                   + XA(IELEM,15) * Y(I5)
C
100        CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'0') THEN
C
           CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W4 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W5 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W6 , Y , Z , 0.D0 , NELEM )
C
         ELSE
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
C
         ENDIF
C
C   CONTRIBUTION OF THE DIAGONAL
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=YZ    ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=Y     ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=-TAY  ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 91  IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) =
     &                   - XA(IELEM,1) * Y(I2)
     &                   - XA(IELEM,2) * Y(I3)
     &                   - XA(IELEM,3) * Y(I4)
     &                   - XA(IELEM,4) * Y(I5)
     &                   - XA(IELEM,5) * Y(I6)
C
           W2(IELEM) =
     &                   - XA(IELEM,1) * Y(I1)
     &                   - XA(IELEM,6) * Y(I3)
     &                   - XA(IELEM,7) * Y(I4)
     &                   - XA(IELEM,8) * Y(I5)
     &                   - XA(IELEM,9) * Y(I6)
C
           W3(IELEM) =
     &                   - XA(IELEM,2)  * Y(I1)
     &                   - XA(IELEM,6)  * Y(I2)
     &                   - XA(IELEM,10) * Y(I4)
     &                   - XA(IELEM,11) * Y(I5)
     &                   - XA(IELEM,12) * Y(I6)
C
           W4(IELEM) =
     &                   - XA(IELEM,3)  * Y(I1)
     &                   - XA(IELEM,7)  * Y(I2)
     &                   - XA(IELEM,10) * Y(I3)
     &                   - XA(IELEM,13) * Y(I5)
     &                   - XA(IELEM,14) * Y(I6)
C
           W5(IELEM) =
     &                   - XA(IELEM,4)  * Y(I1)
     &                   - XA(IELEM,8)  * Y(I2)
     &                   - XA(IELEM,11) * Y(I3)
     &                   - XA(IELEM,13) * Y(I4)
     &                   - XA(IELEM,15) * Y(I6)
C
           W6(IELEM) =
     &                   - XA(IELEM,5)  * Y(I1)
     &                   - XA(IELEM,9)  * Y(I2)
     &                   - XA(IELEM,12) * Y(I3)
     &                   - XA(IELEM,14) * Y(I4)
     &                   - XA(IELEM,15) * Y(I5)
C
91         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 101 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) =
     &                   - XA(IELEM,16) * Y(I2)
     &                   - XA(IELEM,17) * Y(I3)
     &                   - XA(IELEM,18) * Y(I4)
     &                   - XA(IELEM,19) * Y(I5)
     &                   - XA(IELEM,20) * Y(I6)
C
           W2(IELEM) =
     &                   - XA(IELEM, 1) * Y(I1)
     &                   - XA(IELEM,21) * Y(I3)
     &                   - XA(IELEM,22) * Y(I4)
     &                   - XA(IELEM,23) * Y(I5)
     &                   - XA(IELEM,24) * Y(I6)
C
           W3(IELEM) =
     &                   - XA(IELEM, 2) * Y(I1)
     &                   - XA(IELEM, 6) * Y(I2)
     &                   - XA(IELEM,25) * Y(I4)
     &                   - XA(IELEM,26) * Y(I5)
     &                   - XA(IELEM,27) * Y(I6)
C
           W4(IELEM) =
     &                   - XA(IELEM, 3) * Y(I1)
     &                   - XA(IELEM, 7) * Y(I2)
     &                   - XA(IELEM,10) * Y(I3)
     &                   - XA(IELEM,28) * Y(I5)
     &                   - XA(IELEM,29) * Y(I6)
C
           W5(IELEM) =
     &                   - XA(IELEM, 4) * Y(I1)
     &                   - XA(IELEM, 8) * Y(I2)
     &                   - XA(IELEM,11) * Y(I3)
     &                   - XA(IELEM,13) * Y(I4)
     &                   - XA(IELEM,30) * Y(I6)
C
           W6(IELEM) =
     &                   - XA(IELEM, 5) * Y(I1)
     &                   - XA(IELEM, 9) * Y(I2)
     &                   - XA(IELEM,12) * Y(I3)
     &                   - XA(IELEM,14) * Y(I4)
     &                   - XA(IELEM,15) * Y(I5)
C
101        CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'0') THEN
C
           CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W4 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W5 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W6 , Y , Z , 0.D0 , NELEM )
C
         ELSE
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
C
         ENDIF
C
C   CONTRIBUTION OF THE DIAGONAL
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=-YZ   ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=-Y    ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , DA , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+TAY ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 110 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) = W1(IELEM)
     &                   + XA(IELEM,1) * Y(I2)
     &                   + XA(IELEM,2) * Y(I3)
     &                   + XA(IELEM,3) * Y(I4)
     &                   + XA(IELEM,4) * Y(I5)
     &                   + XA(IELEM,5) * Y(I6)
C
           W2(IELEM) = W2(IELEM)
     &                   + XA(IELEM,1) * Y(I1)
     &                   + XA(IELEM,6) * Y(I3)
     &                   + XA(IELEM,7) * Y(I4)
     &                   + XA(IELEM,8) * Y(I5)
     &                   + XA(IELEM,9) * Y(I6)
C
           W3(IELEM) = W3(IELEM)
     &                   + XA(IELEM,2)  * Y(I1)
     &                   + XA(IELEM,6)  * Y(I2)
     &                   + XA(IELEM,10) * Y(I4)
     &                   + XA(IELEM,11) * Y(I5)
     &                   + XA(IELEM,12) * Y(I6)
C
           W4(IELEM) = W4(IELEM)
     &                   + XA(IELEM,3)  * Y(I1)
     &                   + XA(IELEM,7)  * Y(I2)
     &                   + XA(IELEM,10) * Y(I3)
     &                   + XA(IELEM,13) * Y(I5)
     &                   + XA(IELEM,14) * Y(I6)
C
           W5(IELEM) = W5(IELEM)
     &                   + XA(IELEM,4)  * Y(I1)
     &                   + XA(IELEM,8)  * Y(I2)
     &                   + XA(IELEM,11) * Y(I3)
     &                   + XA(IELEM,13) * Y(I4)
     &                   + XA(IELEM,15) * Y(I6)
C
           W6(IELEM) = W6(IELEM)
     &                   + XA(IELEM,5)  * Y(I1)
     &                   + XA(IELEM,9)  * Y(I2)
     &                   + XA(IELEM,12) * Y(I3)
     &                   + XA(IELEM,14) * Y(I4)
     &                   + XA(IELEM,15) * Y(I5)
C
110      CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 120 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) = W1(IELEM)
     &                   + XA(IELEM,16) * Y(I2)
     &                   + XA(IELEM,17) * Y(I3)
     &                   + XA(IELEM,18) * Y(I4)
     &                   + XA(IELEM,19) * Y(I5)
     &                   + XA(IELEM,20) * Y(I6)
C
           W2(IELEM) = W2(IELEM)
     &                   + XA(IELEM, 1) * Y(I1)
     &                   + XA(IELEM,21) * Y(I3)
     &                   + XA(IELEM,22) * Y(I4)
     &                   + XA(IELEM,23) * Y(I5)
     &                   + XA(IELEM,24) * Y(I6)
C
           W3(IELEM) = W3(IELEM)
     &                   + XA(IELEM, 2) * Y(I1)
     &                   + XA(IELEM, 6) * Y(I2)
     &                   + XA(IELEM,25) * Y(I4)
     &                   + XA(IELEM,26) * Y(I5)
     &                   + XA(IELEM,27) * Y(I6)
C
           W4(IELEM) = W4(IELEM)
     &                   + XA(IELEM, 3) * Y(I1)
     &                   + XA(IELEM, 7) * Y(I2)
     &                   + XA(IELEM,10) * Y(I3)
     &                   + XA(IELEM,28) * Y(I5)
     &                   + XA(IELEM,29) * Y(I6)
C
           W5(IELEM) = W5(IELEM)
     &                   + XA(IELEM, 4) * Y(I1)
     &                   + XA(IELEM, 8) * Y(I2)
     &                   + XA(IELEM,11) * Y(I3)
     &                   + XA(IELEM,13) * Y(I4)
     &                   + XA(IELEM,30) * Y(I6)
C
           W6(IELEM) = W6(IELEM)
     &                   + XA(IELEM, 5) * Y(I1)
     &                   + XA(IELEM, 9) * Y(I2)
     &                   + XA(IELEM,12) * Y(I3)
     &                   + XA(IELEM,14) * Y(I4)
     &                   + XA(IELEM,15) * Y(I5)
C
120        CONTINUE
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
C
         ENDIF
C
C   CONTRIBUTION OF THE DIAGONAL
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X+YZ  ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X+Y   ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X-TAY ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 130 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) = W1(IELEM)
     &                   - XA(IELEM,1) * Y(I2)
     &                   - XA(IELEM,2) * Y(I3)
     &                   - XA(IELEM,3) * Y(I4)
     &                   - XA(IELEM,4) * Y(I5)
     &                   - XA(IELEM,5) * Y(I6)
C
           W2(IELEM) = W2(IELEM)
     &                   - XA(IELEM,1) * Y(I1)
     &                   - XA(IELEM,6) * Y(I3)
     &                   - XA(IELEM,7) * Y(I4)
     &                   - XA(IELEM,8) * Y(I5)
     &                   - XA(IELEM,9) * Y(I6)
C
           W3(IELEM) = W3(IELEM)
     &                   - XA(IELEM,2)  * Y(I1)
     &                   - XA(IELEM,6)  * Y(I2)
     &                   - XA(IELEM,10) * Y(I4)
     &                   - XA(IELEM,11) * Y(I5)
     &                   - XA(IELEM,12) * Y(I6)
C
           W4(IELEM) = W4(IELEM)
     &                   - XA(IELEM,3)  * Y(I1)
     &                   - XA(IELEM,7)  * Y(I2)
     &                   - XA(IELEM,10) * Y(I3)
     &                   - XA(IELEM,13) * Y(I5)
     &                   - XA(IELEM,14) * Y(I6)
C
           W5(IELEM) = W5(IELEM)
     &                   - XA(IELEM,4)  * Y(I1)
     &                   - XA(IELEM,8)  * Y(I2)
     &                   - XA(IELEM,11) * Y(I3)
     &                   - XA(IELEM,13) * Y(I4)
     &                   - XA(IELEM,15) * Y(I6)
C
           W6(IELEM) = W6(IELEM)
     &                   - XA(IELEM,5)  * Y(I1)
     &                   - XA(IELEM,9)  * Y(I2)
     &                   - XA(IELEM,12) * Y(I3)
     &                   - XA(IELEM,14) * Y(I4)
     &                   - XA(IELEM,15) * Y(I5)
C
130      CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 140 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) = W1(IELEM)
     &                   - XA(IELEM,16) * Y(I2)
     &                   - XA(IELEM,17) * Y(I3)
     &                   - XA(IELEM,18) * Y(I4)
     &                   - XA(IELEM,19) * Y(I5)
     &                   - XA(IELEM,20) * Y(I6)
C
           W2(IELEM) = W2(IELEM)
     &                   - XA(IELEM, 1) * Y(I1)
     &                   - XA(IELEM,21) * Y(I3)
     &                   - XA(IELEM,22) * Y(I4)
     &                   - XA(IELEM,23) * Y(I5)
     &                   - XA(IELEM,24) * Y(I6)
C
           W3(IELEM) = W3(IELEM)
     &                   - XA(IELEM, 2) * Y(I1)
     &                   - XA(IELEM, 6) * Y(I2)
     &                   - XA(IELEM,25) * Y(I4)
     &                   - XA(IELEM,26) * Y(I5)
     &                   - XA(IELEM,27) * Y(I6)
C
           W4(IELEM) = W4(IELEM)
     &                   - XA(IELEM, 3) * Y(I1)
     &                   - XA(IELEM, 7) * Y(I2)
     &                   - XA(IELEM,10) * Y(I3)
     &                   - XA(IELEM,28) * Y(I5)
     &                   - XA(IELEM,29) * Y(I6)
C
           W5(IELEM) = W5(IELEM)
     &                   - XA(IELEM, 4) * Y(I1)
     &                   - XA(IELEM, 8) * Y(I2)
     &                   - XA(IELEM,11) * Y(I3)
     &                   - XA(IELEM,13) * Y(I4)
     &                   - XA(IELEM,30) * Y(I6)
C
           W6(IELEM) = W6(IELEM)
     &                   - XA(IELEM, 5) * Y(I1)
     &                   - XA(IELEM, 9) * Y(I2)
     &                   - XA(IELEM,12) * Y(I3)
     &                   - XA(IELEM,14) * Y(I4)
     &                   - XA(IELEM,15) * Y(I5)
C
140        CONTINUE
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
C
         ENDIF
C
C   CONTRIBUTION OF THE DIAGONAL
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X-YZ  ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X-Y   ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+CTAY') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 150 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) = W1(IELEM) + C * (
     &                   + XA(IELEM,1) * Y(I2)
     &                   + XA(IELEM,2) * Y(I3)
     &                   + XA(IELEM,3) * Y(I4)
     &                   + XA(IELEM,4) * Y(I5)
     &                   + XA(IELEM,5) * Y(I6)  )
C
           W2(IELEM) = W2(IELEM) + C * (
     &                   + XA(IELEM,1) * Y(I1)
     &                   + XA(IELEM,6) * Y(I3)
     &                   + XA(IELEM,7) * Y(I4)
     &                   + XA(IELEM,8) * Y(I5)
     &                   + XA(IELEM,9) * Y(I6)  )
C
           W3(IELEM) = W3(IELEM) + C * (
     &                   + XA(IELEM,2)  * Y(I1)
     &                   + XA(IELEM,6)  * Y(I2)
     &                   + XA(IELEM,10) * Y(I4)
     &                   + XA(IELEM,11) * Y(I5)
     &                   + XA(IELEM,12)*  Y(I6) )
C
           W4(IELEM) = W4(IELEM) + C * (
     &                   + XA(IELEM,3)  * Y(I1)
     &                   + XA(IELEM,7)  * Y(I2)
     &                   + XA(IELEM,10) * Y(I3)
     &                   + XA(IELEM,13) * Y(I5)
     &                   + XA(IELEM,14) * Y(I6) )
C
           W5(IELEM) = W5(IELEM) + C * (
     &                   + XA(IELEM,4)  * Y(I1)
     &                   + XA(IELEM,8)  * Y(I2)
     &                   + XA(IELEM,11) * Y(I3)
     &                   + XA(IELEM,13) * Y(I4)
     &                   + XA(IELEM,15) * Y(I6) )
C
           W6(IELEM) = W6(IELEM) + C * (
     &                   + XA(IELEM,5)  * Y(I1)
     &                   + XA(IELEM,9)  * Y(I2)
     &                   + XA(IELEM,12) * Y(I3)
     &                   + XA(IELEM,14) * Y(I4)
     &                   + XA(IELEM,15) * Y(I5) )
C
150      CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 160 IELEM = 1 , NELEM
C
           I1 = IKLE1(IELEM)
           I2 = IKLE2(IELEM)
           I3 = IKLE3(IELEM)
           I4 = IKLE4(IELEM)
           I5 = IKLE5(IELEM)
           I6 = IKLE6(IELEM)
C
           W1(IELEM) = W1(IELEM) + C * (
     &                   + XA(IELEM,16) * Y(I2)
     &                   + XA(IELEM,17) * Y(I3)
     &                   + XA(IELEM,18) * Y(I4)
     &                   + XA(IELEM,19) * Y(I5)
     &                   + XA(IELEM,20) * Y(I6) )
C
           W2(IELEM) = W2(IELEM) + C * (
     &                   + XA(IELEM, 1) * Y(I1)
     &                   + XA(IELEM,21) * Y(I3)
     &                   + XA(IELEM,22) * Y(I4)
     &                   + XA(IELEM,23) * Y(I5)
     &                   + XA(IELEM,24) * Y(I6) )
C
           W3(IELEM) = W3(IELEM) + C * (
     &                   + XA(IELEM, 2) * Y(I1)
     &                   + XA(IELEM, 6) * Y(I2)
     &                   + XA(IELEM,25) * Y(I4)
     &                   + XA(IELEM,26) * Y(I5)
     &                   + XA(IELEM,27) * Y(I6) )
C
           W4(IELEM) = W4(IELEM) + C * (
     &                   + XA(IELEM, 3) * Y(I1)
     &                   + XA(IELEM, 7) * Y(I2)
     &                   + XA(IELEM,10) * Y(I3)
     &                   + XA(IELEM,28) * Y(I5)
     &                   + XA(IELEM,29) * Y(I6) )
C
           W5(IELEM) = W5(IELEM) + C * (
     &                   + XA(IELEM, 4) * Y(I1)
     &                   + XA(IELEM, 8) * Y(I2)
     &                   + XA(IELEM,11) * Y(I3)
     &                   + XA(IELEM,13) * Y(I4)
     &                   + XA(IELEM,30) * Y(I6) )
C
           W6(IELEM) = W6(IELEM) + C * (
     &                   + XA(IELEM, 5) * Y(I1)
     &                   + XA(IELEM, 9) * Y(I2)
     &                   + XA(IELEM,12) * Y(I3)
     &                   + XA(IELEM,14) * Y(I4)
     &                   + XA(IELEM,15) * Y(I5) )
C
160        CONTINUE
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(0)
           STOP
C
         ENDIF
C
C   CONTRIBUTION OF THE DIAGONAL
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X+CYZ ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X+CY  ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(0)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,3000) OP
        IF (LNG.EQ.2) WRITE(LU,3001) OP
        CALL PLANTE(0)
C
C-----------------------------------------------------------------------
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
C
1000  FORMAT(1X,'MV0606 (BIEF) : TERMES EXTRADIAG. TYPE INCONNU: ',A1)
1001  FORMAT(1X,'MV0606 (BIEF) : EXTRADIAG. TERMS  UNKNOWN TYPE : ',A1)
2000  FORMAT(1X,'MV0606 (BIEF) : DIAGONALE : TYPE INCONNU: ',A1)
2001  FORMAT(1X,'MV0606 (BIEF) : DIAGONAL : UNKNOWN TYPE : ',A1)
3000  FORMAT(1X,'MV0606 (BIEF) : OPERATION INCONNUE : ',A8)
3001  FORMAT(1X,'MV0606 (BIEF) : UNKNOWN OPERATION : ',A8)
C
      END

C
C#######################################################################
C