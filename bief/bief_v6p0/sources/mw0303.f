C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MATRIX VECTOR PRODUCT FOR P1 TRIANGLES.
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
!>    </th><td> C, DA, DIMIKM, IKLEM1, LIMVOI, MXPTVS, NPMAX, NPOIN, OP, TRAV, TYPDIA, TYPEXT, X, XAS, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MW0303
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OPASS(), OV(), PLANTE()
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
!>          <tr><td>DIMIKM
!></td><td>--></td><td>PREMIERE DIMENSION DE IKLEM1
!>    </td></tr>
!>          <tr><td>IKLEM1
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>LIMVOI
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>MXPTVS
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>NPMAX
!></td><td>--></td><td>NOMBRE MAXIMUM DE POINTS.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS.
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>OPERATION A EFFECTUER
!>    </td></tr>
!>          <tr><td>TRAV
!></td><td>--></td><td>TABLEAU DE TRAVAIL.
!>    </td></tr>
!>          <tr><td>TYPDIA
!></td><td>--></td><td>TYPE DE LA DIAGONALE (CHAINE DE CARACTERES)
!>                  TYPDIA = 'Q' : DIAGONALE QUELCONQUE
!>                  TYPDIA = 'I' : DIAGONALE IDENTITE.
!>                  TYPDIA = '0' : DIAGONALE NULLE.
!>    </td></tr>
!>          <tr><td>TYPEXT
!></td><td>--></td><td>TYPE DES TERMES EXTRADIAGONAUX
!>                  TYPEXT = 'Q' : QUELCONQUES.
!>                  TYPEXT = 'S' : SYMETRIQUES.
!>                  TYPEXT = '0' : NULS.
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VECTEUR IMAGE
!>    </td></tr>
!>          <tr><td>XAS
!></td><td>--></td><td>TERMES EXTRA-DIAGONAUX DE LA MATRICE
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>VECTEUR OPERANDE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MW0303
     &(OP, X , DA,TYPDIA,XAS,TYPEXT, Y,C,
     & IKLEM1,DIMIKM,LIMVOI,MXPTVS,NPMAX,NPOIN,TRAV)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
C| DA             |-->| DIAGONALE DE LA MATRICE
C| DIMIKM         |-->| PREMIERE DIMENSION DE IKLEM1
C| IKLEM1         |-->| 
C| LIMVOI         |-->| 
C| MXPTVS         |-->| 
C| NPMAX          |-->| NOMBRE MAXIMUM DE POINTS.
C| NPOIN          |-->| NOMBRE DE POINTS.
C| OP             |-->| OPERATION A EFFECTUER
C| TRAV           |-->| TABLEAU DE TRAVAIL.
C| TYPDIA         |-->| TYPE DE LA DIAGONALE (CHAINE DE CARACTERES)
C|                |   | TYPDIA = 'Q' : DIAGONALE QUELCONQUE
C|                |   | TYPDIA = 'I' : DIAGONALE IDENTITE.
C|                |   | TYPDIA = '0' : DIAGONALE NULLE.
C| TYPEXT         |-->| TYPE DES TERMES EXTRADIAGONAUX
C|                |   | TYPEXT = 'Q' : QUELCONQUES.
C|                |   | TYPEXT = 'S' : SYMETRIQUES.
C|                |   | TYPEXT = '0' : NULS.
C| X             |<--| VECTEUR IMAGE
C| XAS            |-->| TERMES EXTRA-DIAGONAUX DE LA MATRICE
C| Y             |-->| VECTEUR OPERANDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MW0303 => MW0303
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: DIMIKM,MXPTVS,NPMAX,NPOIN
      INTEGER, INTENT(IN) :: IKLEM1(DIMIKM,4,2),LIMVOI(MXPTVS,2)
C
      DOUBLE PRECISION, INTENT(INOUT) :: X(*),TRAV(*)
      DOUBLE PRECISION, INTENT(IN)    :: DA(*),Y(*)
      DOUBLE PRECISION, INTENT(IN)    :: XAS(*),C
C
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      CHARACTER(LEN=1), INTENT(IN)    :: TYPDIA,TYPEXT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
      DOUBLE PRECISION Z(1)
C
C-----------------------------------------------------------------------
C
C   TREATMENT SPECIFIC TO THE TRANSPOSITION:
C
      I = 1
      IF(OP(3:3).EQ.'T'.OR.OP(4:4).EQ.'T'.OR.
     &   OP(5:5).EQ.'T'.OR.OP(6:6).EQ.'T') I = 3
C
C-----------------------------------------------------------------------
C
C   MATRIX VECTOR PRODUCT, SIMPLE FUNCTION OF THE SHAPE OF THE MATRIX:
C
      IF(TYPEXT(1:1).EQ.'S'.OR.TYPEXT(1:1).EQ.'Q') THEN
C
        IF(TYPEXT(1:1).EQ.'Q') THEN
        CALL OPASS('X=WY    ',TRAV,XAS,IKLEM1(1,I,1),
     &             Y,IKLEM1(1,I+1,1),LIMVOI,MXPTVS,NPMAX)
        ELSEIF(TYPEXT(1:1).EQ.'S') THEN
        CALL OPASS('X=WY    ',TRAV,XAS,IKLEM1(1,I,2),
     &             Y,IKLEM1(1,I+1,2),LIMVOI,MXPTVS,NPMAX)
        ENDIF
C
        IF(TYPDIA(1:1).EQ.'Q') THEN
          CALL OV ('X=X+YZ  ', TRAV , Y , DA , C , NPOIN )
        ELSEIF(TYPDIA(1:1).EQ.'I') THEN
          CALL OV ('X=X+Y   ', TRAV , Y , Z , C , NPOIN )
        ELSEIF(TYPDIA(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
          IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
          CALL PLANTE(0)
          STOP
        ENDIF
C
      ELSEIF(TYPEXT(1:1).EQ.'0') THEN
C
         IF(TYPDIA(1:1).EQ.'Q') THEN
           CALL OV ('X=YZ    ', TRAV , Y , DA , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'I') THEN
           CALL OV ('X=Y     ', TRAV , Y , Z , C , NPOIN )
         ELSEIF(TYPDIA(1:1).EQ.'0') THEN
           CALL OV ('X=C     ', TRAV , Y , Z , 0.D0 , NPOIN )
         ELSE
            IF (LNG.EQ.1) WRITE(LU,2000) TYPDIA
            IF (LNG.EQ.2) WRITE(LU,2001) TYPDIA
            CALL PLANTE(0)
            STOP
         ENDIF
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
C-----------------------------------------------------------------------
C
C   IMPLEMENTED OPERATIONS:
C
      IF(OP(1:8).EQ.'X=AY    '.OR.OP(1:8).EQ.'X=TAY   ') THEN
         CALL OV ('X=Y     ', X , TRAV , Z , C , NPOIN )
      ELSEIF(OP(1:8).EQ.'X=-AY   '.OR.OP(1:8).EQ.'X=-TAY  ') THEN
         CALL OV ('X=-Y    ', X , TRAV , Z , C , NPOIN )
      ELSEIF(OP(1:8).EQ.'X=X+AY  '.OR.OP(1:8).EQ.'X=X+TAY ') THEN
         CALL OV ('X=X+Y   ', X , TRAV , Z , C , NPOIN )
      ELSEIF(OP(1:8).EQ.'X=X-AY  '.OR.OP(1:8).EQ.'X=X-TAY ') THEN
         CALL OV ('X=X-Y   ', X , TRAV , Z , C , NPOIN )
      ELSEIF(OP(1:8).EQ.'X=X+CAY '.OR.OP(1:8).EQ.'X=X+CTAY') THEN
         CALL OV ('X=X+CY  ', X , TRAV , Z , C , NPOIN )
      ELSEIF(OP(1:8).EQ.'X=CAY   ') THEN
         CALL OV ('X=CY    ', X , TRAV , Z , C , NPOIN )
      ELSE
         IF (LNG.EQ.1) WRITE(LU,3000) OP
         IF (LNG.EQ.2) WRITE(LU,3001) OP
         CALL PLANTE(0)
         STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
C
1000  FORMAT(1X,'MW0303 (BIEF) : TERMES EXTRADIAG. TYPE INCONNU: ',A1)
1001  FORMAT(1X,'MW0303 (BIEF) : EXTRADIAG. TERMS  UNKNOWN TYPE : ',A1)
2000  FORMAT(1X,'MW0303 (BIEF) : DIAGONALE : TYPE INCONNU: ',A1)
2001  FORMAT(1X,'MW0303 (BIEF) : DIAGONAL : UNKNOWN TYPE : ',A1)
3000  FORMAT(1X,'MW0303 (BIEF) : OPERATION INCONNUE : ',A8)
3001  FORMAT(1X,'MW0303 (BIEF) : UNKNOWN OPERATION : ',A8)
C
      END
C
C#######################################################################
C