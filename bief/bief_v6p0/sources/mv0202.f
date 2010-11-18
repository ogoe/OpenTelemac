C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MATRIX VECTOR OPERATIONS FOR P1 SEGMENTS.
!>  @code
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON VECTORS X,Y AND MATRIX M.<br>
!>   THE RESULT IS VECTOR X.<br>
!>   THESE OPERATIONS ARE DIFFERENT DEPENDING ON THE DIAGONAL TYPE
!>   AND THE TYPE OF EXTRADIAGONAL TERMS.<br>
!>   IMPLEMENTED OPERATIONS:<br>
!>      OP = 'X=AY    '  : X = AY
!>      OP = 'X=-AY   '  : X = -AY
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
!>    </th><td> C, DA, IKLE1, IKLE2, NELEM, NPOIN, OP, TYPDIA, TYPEXT, W1, W2, X, XA12, XA21, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MV0202
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
!>          <tr><td>IKLE1,2
!></td><td>--></td><td>CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS.
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
!>          <tr><td>W1,2
!></td><td><--</td><td>TABLEAUX DE TRAVAIL DE DIMENSION NELEM
!>                  QUI CONTIENDRONT UNE PARTIE DU RESULTAT SOUS
!>                  FORME NON ASSEMBLEE.
!>    </td></tr>
!>          <tr><td>W2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VECTEUR IMAGE
!>    </td></tr>
!>          <tr><td>XA12,
!></td><td>--></td><td>TERMES EXTRADIAGONAUX ELEMENTAIRES
!>    </td></tr>
!>          <tr><td>XA21
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>VECTEUR OPERANDE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MV0202
     &(OP, X , DA,TYPDIA,XA12,XA21,TYPEXT, Y,C,
     & IKLE1,IKLE2,
     & NPOIN,NELEM,W1,W2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
C| DA             |-->| DIAGONALE DE LA MATRICE
C| IKLE1,2        |-->| CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
C| IKLE2          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS.
C| NPOIN          |-->| NOMBRE DE POINTS.
C| OP             |-->| OPERATION A EFFECTUER
C| TYPDIA         |-->| TYPE DE LA DIAGONALE (CHAINE DE CARACTERES)
C|                |   | TYPDIA = 'Q' : DIAGONALE QUELCONQUE
C|                |   | TYPDIA = 'I' : DIAGONALE IDENTITE.
C|                |   | TYPDIA = '0' : DIAGONALE NULLE.
C| TYPEXT         |-->| TYPEXT = 'Q' : QUELCONQUES.
C|                |   | TYPEXT = 'S' : SYMETRIQUES.
C|                |   | TYPEXT = '0' : NULS.
C| W1,2           |<--| TABLEAUX DE TRAVAIL DE DIMENSION NELEM
C|                |   | QUI CONTIENDRONT UNE PARTIE DU RESULTAT SOUS
C|                |   | FORME NON ASSEMBLEE.
C| W2             |---| 
C| X             |<--| VECTEUR IMAGE
C| XA12,          |-->| TERMES EXTRADIAGONAUX ELEMENTAIRES
C| XA21           |---| 
C| Y             |-->| VECTEUR OPERANDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MV0202 => MV0202
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NPOIN
      INTEGER, INTENT(IN) :: IKLE1(*),IKLE2(*)
C
      DOUBLE PRECISION, INTENT(INOUT) :: W1(*),W2(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(*)
      DOUBLE PRECISION, INTENT(IN) :: Y(*),DA(*)
      DOUBLE PRECISION, INTENT(IN) :: XA12(*),XA21(*)
      DOUBLE PRECISION, INTENT(IN) :: C
C
      CHARACTER*(*), INTENT(IN) :: OP,TYPDIA,TYPEXT
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
C   CONTRIBUTION OF EXTRADIAGONAL TERMS:
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 10 IELEM = 1 , NELEM
             W1(IELEM) =     XA12(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) =     XA12(IELEM) * Y(IKLE1(IELEM))
10         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 20 IELEM = 1 , NELEM
             W1(IELEM) =     XA12(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) =     XA21(IELEM) * Y(IKLE1(IELEM))
20         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'0') THEN
C
           CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
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
             W1(IELEM) =   - XA12(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) =   - XA12(IELEM) * Y(IKLE1(IELEM))
11         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 21 IELEM = 1 , NELEM
             W1(IELEM) =   - XA12(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) =   - XA21(IELEM) * Y(IKLE1(IELEM))
21         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'0') THEN
C
           CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
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
C   CONTRIBUTION OF EXTRADIAGONAL TERMS
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 30 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM)  + XA12(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) = W2(IELEM)  + XA12(IELEM) * Y(IKLE1(IELEM))
30         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 40  IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM)   + XA12(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) = W2(IELEM)   + XA21(IELEM) * Y(IKLE1(IELEM))
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
C   CONTRIBUTION OF EXTRADIAGONAL TERMS
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 50 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM)  - XA12(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) = W2(IELEM)  - XA12(IELEM) * Y(IKLE1(IELEM))
50         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 60 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM)   - XA12(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) = W2(IELEM)   - XA21(IELEM) * Y(IKLE1(IELEM))
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
           CALL PLANTE(1)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+CAY ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 70 IELEM = 1 , NELEM
             W1(IELEM)=W1(IELEM) + C * (XA12(IELEM) * Y(IKLE2(IELEM)))
             W2(IELEM)=W2(IELEM) + C * (XA12(IELEM) * Y(IKLE1(IELEM)))
70         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 80 IELEM = 1 , NELEM
             W1(IELEM)=W1(IELEM) + C * (XA12(IELEM) * Y(IKLE2(IELEM)))
             W2(IELEM)=W2(IELEM) + C * (XA21(IELEM) * Y(IKLE1(IELEM)))
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
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=TAY   ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 90  IELEM = 1 , NELEM
             W1(IELEM) =     XA12(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) =     XA12(IELEM) * Y(IKLE1(IELEM))
90         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 100 IELEM = 1 , NELEM
             W1(IELEM) =   + XA21(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) =   + XA12(IELEM) * Y(IKLE1(IELEM))
100        CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'0') THEN
C
           CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
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
      ELSEIF(OP(1:8).EQ.'X=-TAY   ') THEN
C
C   CONTRIBUTION OF EXTRADIAGONAL TERMS
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 91  IELEM = 1 , NELEM
             W1(IELEM) =   - XA12(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) =   - XA12(IELEM) * Y(IKLE1(IELEM))
91         CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 101 IELEM = 1 , NELEM
             W1(IELEM) =   - XA21(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) =   - XA12(IELEM) * Y(IKLE1(IELEM))
101        CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'0') THEN
C
           CALL OV ('X=C     ', W1 , Y , Z , 0.D0 , NELEM )
           CALL OV ('X=C     ', W2 , Y , Z , 0.D0 , NELEM )
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
C   CONTRIBUTION OF EXTRADIAGONAL TERMS
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 110 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM) + XA12(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) = W2(IELEM) + XA12(IELEM) * Y(IKLE1(IELEM))
110      CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 120 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM) + XA21(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) = W2(IELEM) + XA12(IELEM) * Y(IKLE1(IELEM))
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
C   CONTRIBUTION OF EXTRADIAGONAL TERMS
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 130 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM) - XA12(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) = W2(IELEM) - XA12(IELEM) * Y(IKLE1(IELEM))
130      CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 140 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM) - XA21(IELEM) * Y(IKLE2(IELEM))
             W2(IELEM) = W2(IELEM) - XA12(IELEM) * Y(IKLE1(IELEM))
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
C   CONTRIBUTION OF EXTRADIAGONAL TERMS
C
         IF(TYPEXT(1:1).EQ.'S') THEN
C
           DO 150 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM) + C*(XA12(IELEM) * Y(IKLE2(IELEM)))
             W2(IELEM) = W2(IELEM) + C*(XA12(IELEM) * Y(IKLE1(IELEM)))
150      CONTINUE
C
         ELSEIF(TYPEXT(1:1).EQ.'Q') THEN
C
           DO 160 IELEM = 1 , NELEM
             W1(IELEM) = W1(IELEM) + C*(XA21(IELEM) * Y(IKLE2(IELEM)))
             W2(IELEM) = W2(IELEM) + C*(XA12(IELEM) * Y(IKLE1(IELEM)))
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
1000  FORMAT(1X,'MV0202 (BIEF) : TERMES EXTRADIAG. TYPE INCONNU: ',A1)
1001  FORMAT(1X,'MV0202 (BIEF) : EXTRADIAG. TERMS  UNKNOWN TYPE : ',A1)
2000  FORMAT(1X,'MV0202 (BIEF) : DIAGONALE : TYPE INCONNU: ',A1)
2001  FORMAT(1X,'MV0202 (BIEF) : DIAGONAL : UNKNOWN TYPE : ',A1)
3000  FORMAT(1X,'MV0202 (BIEF) : OPERATION INCONNUE : ',A8)
3001  FORMAT(1X,'MV0202 (BIEF) : UNKNOWN OPERATION : ',A8)
C
      END

C
C#######################################################################
C