C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MATRIX VECTOR OPERATIONS FOR P1 AND P2 TRIANGLES.
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
!>    </th><td> C, DA, IKLE1, IKLE2, IKLE3, IKLE4, IKLE5, IKLE6, NELEM, NPOIN, NPT2, OP, TYPDIA, TYPEXT, W1, W2, W3, W4, W5, W6, X, XA12, XA13, XA21, XA23, XA31, XA32, XA41, XA42, XA43, XA51, XA52, XA53, XA61, XA62, XA63, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, Z
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
!>      <td><center> 5.9                                       </center>
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
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS.
!>    </td></tr>
!>          <tr><td>NPT2
!></td><td>---</td><td>
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
!>          <tr><td>XA12,
!></td><td>--></td><td>TERMES EXTRADIAGONAUX ELEMENTAIRES
!>    </td></tr>
!>          <tr><td>XA13
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA21
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA23
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA31
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA32
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA41
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA42
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA43
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA51
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA52
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA53
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA61
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA62
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XA63
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>VECTEUR OPERANDE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MV0603
     &(OP, X , DA,TYPDIA,
     & XA12,XA13,XA21,XA23,XA31,XA32,XA41,XA42,XA43,
     & XA51,XA52,XA53,XA61,XA62,XA63,
     & TYPEXT, Y,C,
     & IKLE1,IKLE2,IKLE3,IKLE4,IKLE5,IKLE6,
     & NPOIN,NPT2,NELEM,W1,W2,W3,W4,W5,W6)
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
C| NPOIN          |-->| NOMBRE DE POINTS.
C| NPT2           |---| 
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
C| XA12,          |-->| TERMES EXTRADIAGONAUX ELEMENTAIRES
C| XA13           |---| 
C| XA21           |---| 
C| XA23           |---| 
C| XA31           |---| 
C| XA32           |---| 
C| XA41           |---| 
C| XA42           |---| 
C| XA43           |---| 
C| XA51           |---| 
C| XA52           |---| 
C| XA53           |---| 
C| XA61           |---| 
C| XA62           |---| 
C| XA63           |---| 
C| Y             |-->| VECTEUR OPERANDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF!, EX_MV0603 => MV0603
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NPOIN,NPT2
C
      INTEGER, INTENT(IN) :: IKLE1(*),IKLE2(*),IKLE3(*)
      INTEGER, INTENT(IN) :: IKLE4(*),IKLE5(*),IKLE6(*)
C
      DOUBLE PRECISION, INTENT(INOUT) :: W1(*),W2(*),W3(*)
      DOUBLE PRECISION, INTENT(INOUT) :: W4(*),W5(*),W6(*)
      DOUBLE PRECISION, INTENT(IN) :: Y(*),DA(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN) :: XA12(*),XA13(*),XA21(*)
      DOUBLE PRECISION, INTENT(IN) :: XA23(*),XA31(*),XA32(*)
      DOUBLE PRECISION, INTENT(IN) :: XA41(*),XA42(*),XA43(*)
      DOUBLE PRECISION, INTENT(IN) :: XA51(*),XA52(*),XA53(*)
      DOUBLE PRECISION, INTENT(IN) :: XA61(*),XA62(*),XA63(*)
      DOUBLE PRECISION, INTENT(IN) :: C
C
      CHARACTER(LEN=8), INTENT(IN) :: OP
      CHARACTER(LEN=1), INTENT(IN) :: TYPDIA,TYPEXT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM
      DOUBLE PRECISION Z(1)
C
C-----------------------------------------------------------------------
C
      IF(OP(1:8).EQ.'X=AY    ') THEN
C
C   CONTRIBUTION OF THE DIAGONAL:
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=YZ    ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=Y     ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , Z  , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(1)
           STOP
         ENDIF
C
C        THE DIAGONAL REACHES ONLY LINEAR POINTS, OTHERS NOT INITIALISED
C        THEY ARE SET TO 0 HERE
         CALL OV ('X=C     ',X(NPOIN+1:NPT2),Y,Z,0.D0,NPT2-NPOIN)
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 20 IELEM = 1 , NELEM
             W1(IELEM) =     XA12(IELEM) * Y(IKLE2(IELEM))
     &                     + XA13(IELEM) * Y(IKLE3(IELEM))
             W2(IELEM) =     XA21(IELEM) * Y(IKLE1(IELEM))
     &                     + XA23(IELEM) * Y(IKLE3(IELEM))
             W3(IELEM) =     XA31(IELEM) * Y(IKLE1(IELEM))
     &                     + XA32(IELEM) * Y(IKLE2(IELEM))
             W4(IELEM) =     XA41(IELEM) * Y(IKLE1(IELEM))
     &                     + XA42(IELEM) * Y(IKLE2(IELEM))
     &                     + XA43(IELEM) * Y(IKLE3(IELEM))
             W5(IELEM) =     XA51(IELEM) * Y(IKLE1(IELEM))
     &                     + XA52(IELEM) * Y(IKLE2(IELEM))
     &                     + XA53(IELEM) * Y(IKLE3(IELEM))
             W6(IELEM) =     XA61(IELEM) * Y(IKLE1(IELEM))
     &                     + XA62(IELEM) * Y(IKLE2(IELEM))
     &                     + XA63(IELEM) * Y(IKLE3(IELEM))
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
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=-AY   ') THEN
C
C   CONTRIBUTION OF THE DIAGONAL:
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=-YZ   ', X , Y , DA , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=-Y    ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', X , Y , Z  , 0.D0 , NPOIN )
         ELSE
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(1)
           STOP
         ENDIF
C
C        THE DIAGONAL REACHES ONLY LINEAR POINTS, OTHERS NOT INITIALISED
C        THEY ARE SET TO 0 HERE
         CALL OV ('X=C     ',X(NPOIN+1:NPT2),Y,Z,0.D0,NPT2-NPOIN)
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 21 IELEM = 1 , NELEM
             W1(IELEM) =   - XA12(IELEM) * Y(IKLE2(IELEM))
     &                     - XA13(IELEM) * Y(IKLE3(IELEM))
             W2(IELEM) =   - XA21(IELEM) * Y(IKLE1(IELEM))
     &                     - XA23(IELEM) * Y(IKLE3(IELEM))
             W3(IELEM) =   - XA31(IELEM) * Y(IKLE1(IELEM))
     &                     - XA32(IELEM) * Y(IKLE2(IELEM))
             W4(IELEM) =   - XA41(IELEM) * Y(IKLE1(IELEM))
     &                     - XA42(IELEM) * Y(IKLE2(IELEM))
     &                     - XA43(IELEM) * Y(IKLE3(IELEM))
             W5(IELEM) =   - XA51(IELEM) * Y(IKLE1(IELEM))
     &                     - XA52(IELEM) * Y(IKLE2(IELEM))
     &                     - XA53(IELEM) * Y(IKLE3(IELEM))
             W6(IELEM) =   - XA61(IELEM) * Y(IKLE1(IELEM))
     &                     - XA62(IELEM) * Y(IKLE2(IELEM))
     &                     - XA63(IELEM) * Y(IKLE3(IELEM))
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
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+AY  ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 40  IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM) + XA12(IELEM) * Y(IKLE2(IELEM))
     &                             + XA13(IELEM) * Y(IKLE3(IELEM))
             W2(IELEM) = W2(IELEM) + XA21(IELEM) * Y(IKLE1(IELEM))
     &                             + XA23(IELEM) * Y(IKLE3(IELEM))
             W3(IELEM) = W3(IELEM) + XA31(IELEM) * Y(IKLE1(IELEM))
     &                             + XA32(IELEM) * Y(IKLE2(IELEM))
             W4(IELEM) = W4(IELEM) + XA41(IELEM) * Y(IKLE1(IELEM))
     &                             + XA42(IELEM) * Y(IKLE2(IELEM))
     &                             + XA43(IELEM) * Y(IKLE3(IELEM))
             W5(IELEM) = W5(IELEM) + XA51(IELEM) * Y(IKLE1(IELEM))
     &                             + XA52(IELEM) * Y(IKLE2(IELEM))
     &                             + XA53(IELEM) * Y(IKLE3(IELEM))
             W6(IELEM) = W6(IELEM) + XA61(IELEM) * Y(IKLE1(IELEM))
     &                             + XA62(IELEM) * Y(IKLE2(IELEM))
     &                             + XA63(IELEM) * Y(IKLE3(IELEM))
40         CONTINUE
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
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
           CALL PLANTE(1)
           STOP
         ENDIF
C
C        THE DIAGONAL REACHES ONLY LINEAR POINTS, OTHERS NOT INITIALISED
C        THEY ARE SET TO 0 HERE
         CALL OV ('X=C     ',X(NPOIN+1:NPT2),Y,Z,0.D0,NPT2-NPOIN)
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X-AY  ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 60 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM) - XA12(IELEM) * Y(IKLE2(IELEM))
     &                             - XA13(IELEM) * Y(IKLE3(IELEM))
             W2(IELEM) = W2(IELEM) - XA21(IELEM) * Y(IKLE1(IELEM))
     &                             - XA23(IELEM) * Y(IKLE3(IELEM))
             W3(IELEM) = W3(IELEM) - XA31(IELEM) * Y(IKLE1(IELEM))
     &                             - XA32(IELEM) * Y(IKLE2(IELEM))
             W4(IELEM) = W4(IELEM) - XA41(IELEM) * Y(IKLE1(IELEM))
     &                             - XA42(IELEM) * Y(IKLE2(IELEM))
     &                             - XA43(IELEM) * Y(IKLE3(IELEM))
             W5(IELEM) = W5(IELEM) - XA51(IELEM) * Y(IKLE1(IELEM))
     &                             - XA52(IELEM) * Y(IKLE2(IELEM))
     &                             - XA53(IELEM) * Y(IKLE3(IELEM))
             W6(IELEM) = W6(IELEM) - XA61(IELEM) * Y(IKLE1(IELEM))
     &                             - XA62(IELEM) * Y(IKLE2(IELEM))
     &                             - XA63(IELEM) * Y(IKLE3(IELEM))
60         CONTINUE
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
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
           CALL PLANTE(1)
           STOP
         ENDIF
C
C        THE DIAGONAL REACHES ONLY LINEAR POINTS, OTHERS NOT INITIALISED
C        THEY ARE SET TO 0 HERE
         CALL OV ('X=C     ',X(NPOIN+1:NPT2),Y,Z,0.D0,NPT2-NPOIN)
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+CAY ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 80 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM)
     &               + C * (      XA12(IELEM) * Y(IKLE2(IELEM))
     &                          + XA13(IELEM) * Y(IKLE3(IELEM)) )
             W2(IELEM) = W2(IELEM)
     &               + C * (      XA21(IELEM) * Y(IKLE1(IELEM))
     &                          + XA23(IELEM) * Y(IKLE3(IELEM)) )
             W3(IELEM) = W3(IELEM)
     &               + C * (      XA31(IELEM) * Y(IKLE1(IELEM))
     &                          + XA32(IELEM) * Y(IKLE2(IELEM)) )
             W4(IELEM) = W4(IELEM)
     &               + C * (      XA41(IELEM) * Y(IKLE1(IELEM))
     &                          + XA42(IELEM) * Y(IKLE2(IELEM))
     &                          + XA43(IELEM) * Y(IKLE3(IELEM)) )
             W5(IELEM) = W5(IELEM)
     &               + C * (      XA51(IELEM) * Y(IKLE1(IELEM))
     &                          + XA52(IELEM) * Y(IKLE2(IELEM))
     &                          + XA53(IELEM) * Y(IKLE3(IELEM)) )
             W6(IELEM) = W6(IELEM)
     &               + C * (      XA61(IELEM) * Y(IKLE1(IELEM))
     &                          + XA62(IELEM) * Y(IKLE2(IELEM))
     &                          + XA63(IELEM) * Y(IKLE3(IELEM)) )
80         CONTINUE
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C   CONTRIBUTION OF THE DIAGONAL:
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=X+CYZ  ', X , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=X+CY   ', X , Y , Z  , C  , NPOIN )
         ELSEIF(TYPDIA(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
           IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
           CALL PLANTE(1)
           STOP
         ENDIF
C
C        THE DIAGONAL REACHES ONLY LINEAR POINTS, OTHERS NOT INITIALISED
C        THEY ARE SET TO 0 HERE
         CALL OV ('X=C     ',X(NPOIN+1:NPT2),Y,Z,0.D0,NPT2-NPOIN)
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=TAY   ') THEN
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
           CALL PLANTE(1)
           STOP
         ENDIF
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 100 IELEM = 1 , NELEM
             W1(IELEM) =   + XA21(IELEM) * Y(IKLE2(IELEM))
     &                     + XA31(IELEM) * Y(IKLE3(IELEM))
     &                     + XA41(IELEM) * Y(IKLE4(IELEM))
     &                     + XA51(IELEM) * Y(IKLE5(IELEM))
     &                     + XA61(IELEM) * Y(IKLE6(IELEM))
             W2(IELEM) =   + XA12(IELEM) * Y(IKLE1(IELEM))
     &                     + XA32(IELEM) * Y(IKLE3(IELEM))
     &                     + XA42(IELEM) * Y(IKLE4(IELEM))
     &                     + XA52(IELEM) * Y(IKLE5(IELEM))
     &                     + XA62(IELEM) * Y(IKLE6(IELEM))
             W3(IELEM) =   + XA13(IELEM) * Y(IKLE1(IELEM))
     &                     + XA23(IELEM) * Y(IKLE2(IELEM))
     &                     + XA43(IELEM) * Y(IKLE4(IELEM))
     &                     + XA53(IELEM) * Y(IKLE5(IELEM))
     &                     + XA63(IELEM) * Y(IKLE6(IELEM))
100        CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'0') THEN
C
           CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
C
         ELSE
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=-TAY  ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 101 IELEM = 1 , NELEM
             W1(IELEM) =   - XA21(IELEM) * Y(IKLE2(IELEM))
     &                     - XA31(IELEM) * Y(IKLE3(IELEM))
     &                     - XA41(IELEM) * Y(IKLE4(IELEM))
     &                     - XA51(IELEM) * Y(IKLE5(IELEM))
     &                     - XA61(IELEM) * Y(IKLE6(IELEM))
             W2(IELEM) =   - XA12(IELEM) * Y(IKLE1(IELEM))
     &                     - XA32(IELEM) * Y(IKLE3(IELEM))
     &                     - XA42(IELEM) * Y(IKLE4(IELEM))
     &                     - XA52(IELEM) * Y(IKLE5(IELEM))
     &                     - XA62(IELEM) * Y(IKLE6(IELEM))
             W3(IELEM) =   - XA13(IELEM) * Y(IKLE1(IELEM))
     &                     - XA23(IELEM) * Y(IKLE2(IELEM))
     &                     - XA43(IELEM) * Y(IKLE4(IELEM))
     &                     - XA53(IELEM) * Y(IKLE5(IELEM))
     &                     - XA63(IELEM) * Y(IKLE6(IELEM))
101        CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'0') THEN
C
           CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W3 , Y , Z , 0.D0 , NELEM )
C
         ELSE
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
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
           CALL PLANTE(1)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+TAY ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 120 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM) + XA21(IELEM) * Y(IKLE2(IELEM))
     &                             + XA31(IELEM) * Y(IKLE3(IELEM))
     &                             + XA41(IELEM) * Y(IKLE4(IELEM))
     &                             + XA51(IELEM) * Y(IKLE5(IELEM))
     &                             + XA61(IELEM) * Y(IKLE6(IELEM))
             W2(IELEM) = W2(IELEM) + XA12(IELEM) * Y(IKLE1(IELEM))
     &                             + XA32(IELEM) * Y(IKLE3(IELEM))
     &                             + XA42(IELEM) * Y(IKLE4(IELEM))
     &                             + XA52(IELEM) * Y(IKLE5(IELEM))
     &                             + XA62(IELEM) * Y(IKLE6(IELEM))
             W3(IELEM) = W3(IELEM) + XA13(IELEM) * Y(IKLE1(IELEM))
     &                             + XA23(IELEM) * Y(IKLE2(IELEM))
     &                             + XA43(IELEM) * Y(IKLE4(IELEM))
     &                             + XA53(IELEM) * Y(IKLE5(IELEM))
     &                             + XA63(IELEM) * Y(IKLE6(IELEM))
120        CONTINUE
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
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
           CALL PLANTE(1)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X-TAY ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 140 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM) - XA21(IELEM) * Y(IKLE2(IELEM))
     &                             - XA31(IELEM) * Y(IKLE3(IELEM))
     &                             - XA41(IELEM) * Y(IKLE4(IELEM))
     &                             - XA51(IELEM) * Y(IKLE5(IELEM))
     &                             - XA61(IELEM) * Y(IKLE6(IELEM))
             W2(IELEM) = W2(IELEM) - XA12(IELEM) * Y(IKLE1(IELEM))
     &                             - XA32(IELEM) * Y(IKLE3(IELEM))
     &                             - XA42(IELEM) * Y(IKLE4(IELEM))
     &                             - XA52(IELEM) * Y(IKLE5(IELEM))
     &                             - XA62(IELEM) * Y(IKLE6(IELEM))
             W3(IELEM) = W3(IELEM) - XA13(IELEM) * Y(IKLE1(IELEM))
     &                             - XA23(IELEM) * Y(IKLE2(IELEM))
     &                             - XA43(IELEM) * Y(IKLE4(IELEM))
     &                             - XA53(IELEM) * Y(IKLE5(IELEM))
     &                             - XA63(IELEM) * Y(IKLE6(IELEM))
140        CONTINUE
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
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
           CALL PLANTE(1)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+CTAY') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 160 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM)
     &                 + C * (    + XA21(IELEM) * Y(IKLE2(IELEM))
     &                            + XA31(IELEM) * Y(IKLE3(IELEM))
     &                            + XA41(IELEM) * Y(IKLE4(IELEM))
     &                            + XA51(IELEM) * Y(IKLE5(IELEM))
     &                            + XA61(IELEM) * Y(IKLE6(IELEM)) )
             W2(IELEM) = W2(IELEM)
     &                 + C * (    + XA12(IELEM) * Y(IKLE1(IELEM))
     &                            + XA32(IELEM) * Y(IKLE3(IELEM))
     &                            + XA42(IELEM) * Y(IKLE4(IELEM))
     &                            + XA52(IELEM) * Y(IKLE5(IELEM))
     &                            + XA62(IELEM) * Y(IKLE6(IELEM)) )
             W3(IELEM) = W3(IELEM)
     &                 + C * (    + XA13(IELEM) * Y(IKLE1(IELEM))
     &                            + XA23(IELEM) * Y(IKLE2(IELEM))
     &                            + XA43(IELEM) * Y(IKLE4(IELEM))
     &                            + XA53(IELEM) * Y(IKLE5(IELEM))
     &                            + XA63(IELEM) * Y(IKLE6(IELEM)))
160        CONTINUE
C
         ELSEIF(TYPEXT(1:1).NE.'0') THEN
C
           IF (LNG.EQ.1) WRITE(LU,1000) TYPEXT
           IF (LNG.EQ.2) WRITE(LU,1001) TYPEXT
           CALL PLANTE(1)
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
           CALL PLANTE(1)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,3000) OP
        IF (LNG.EQ.2) WRITE(LU,3001) OP
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
C
1000  FORMAT(1X,'MV0603 (BIEF) : TERMES EXTRADIAG. TYPE INCONNU: ',A1)
1001  FORMAT(1X,'MV0603 (BIEF) : EXTRADIAG. TERMS  UNKNOWN TYPE : ',A1)
2000  FORMAT(1X,'MV0603 (BIEF) : DIAGONALE : TYPE INCONNU: ',A1)
2001  FORMAT(1X,'MV0603 (BIEF) : DIAGONAL : UNKNOWN TYPE : ',A1)
3000  FORMAT(1X,'MV0603 (BIEF) : OPERATION INCONNUE : ',A8)
3001  FORMAT(1X,'MV0603 (BIEF) : UNKNOWN OPERATION : ',A8)
C
C-----------------------------------------------------------------------
C
      END
C
C#######################################################################
C