C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MATRIX VECTOR OPERATIONS.
!>  @code
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON VECTORS X,Y AND MATRIX M.<br>
!>   THE RESULT IS VECTOR X (A NON-ASSEMBLED PART OF WHICH CAN BE IN
!>   ARRAY W IF LEGO = .FALSE.)<br>
!>   THESE OPERATIONS ARE DIFFERENTS DEPENDING ON THE DIAGONAL TYPE
!>   AND THE OFF-DIAGONAL TERMS TYPE.<br>
!>   IMPLEMENTED OPERATIONS :<br>
!>      OP = 'X=AY    '  : X = AY
!>      OP = 'X=X+AY  '  : X = X + AY
!>      OP = 'X=X-AY  '  : X = X - AY
!>      OP = 'X=X+CAY '  : X = X + C AY
!>      OP = 'X=TAY   '  : X = TA Y (TA: TRANSPOSE OF A)
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
!>    </th><td> A, C, MESH, OP, X, Y
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> S
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MATRBL
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> MATVEC(), PARCOM(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CGSQUA(), CGSTAB(), EQUNOR(), ERRMIN(), GMRES(), GRACJG(), RESCJG()

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
!> </td><td> 24/04/97
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>C
!></td><td>--></td><td>CONSTANTE DONNEE
!>    </td></tr>
!>          <tr><td>DA
!></td><td>--></td><td>DIAGONALE DE LA MATRICE.
!>    </td></tr>
!>          <tr><td>MESH
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
!></td><td>--></td><td>TYPE DES TERMES EXTRADIAGONAUX
!>                  TYPEXT = 'Q' : QUELCONQUES.
!>                  TYPEXT = 'S' : SYMETRIQUES.
!>                  TYPEXT = '0' : NULS.
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VECTEUR IMAGE
!>    </td></tr>
!>          <tr><td>XA
!></td><td>--></td><td>TERMES EXTRA-DIAGONAUX DE LA MATRICE
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>VECTEUR OPERANDE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MATRBL
     &( OP , X , A , Y , C , MESH )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |---| 
C| C             |-->| CONSTANTE DONNEE
C| DA             |-->| DIAGONALE DE LA MATRICE.
C| MESH           |---| 
C| OP             |-->| OPERATION A EFFECTUER
C| TYPDIA         |-->| TYPE DE LA DIAGONALE (CHAINE DE CARACTERES)
C|                |   | TYPDIA = 'Q' : DIAGONALE QUELCONQUE
C|                |   | TYPDIA = 'I' : DIAGONALE IDENTITE.
C|                |   | TYPDIA = '0' : DIAGONALE NULLE.
C| TYPEXT         |-->| TYPE DES TERMES EXTRADIAGONAUX
C|                |   | TYPEXT = 'Q' : QUELCONQUES.
C|                |   | TYPEXT = 'S' : SYMETRIQUES.
C|                |   | TYPEXT = '0' : NULS.
C| X             |<--| VECTEUR IMAGE
C| XA             |-->| TERMES EXTRA-DIAGONAUX DE LA MATRICE
C| Y             |-->| VECTEUR OPERANDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MATRBL => MATRBL
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=8), INTENT(IN)   :: OP
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: X
      TYPE(BIEF_OBJ), INTENT(IN)     :: A,Y
      DOUBLE PRECISION, INTENT(IN)   :: C
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER S
C
C-----------------------------------------------------------------------
C
C     CASE WHERE THE STRUCTURES ARE BLOCKS
C
      IF(A%TYPE.EQ.4) THEN
C
        S = X%N
C
        IF(S.EQ.1) THEN
C
         CALL MATVEC( OP,X%ADR(1)%P,A%ADR(1)%P,Y%ADR(1)%P,C,MESH)
C
        ELSEIF(S.EQ.2) THEN
C
          IF(OP(1:8).EQ.'X=AY    ') THEN
            CALL MATVEC('X=AY    ',
     &      X%ADR(1)%P,A%ADR(1)%P,Y%ADR(1)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+AY  ',
     &      X%ADR(1)%P,A%ADR(2)%P,Y%ADR(2)%P,C,MESH,LEGO=.TRUE.)
            CALL MATVEC('X=AY    ',
     &      X%ADR(2)%P,A%ADR(3)%P,Y%ADR(1)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+AY  ',
     &      X%ADR(2)%P,A%ADR(4)%P,Y%ADR(2)%P,C,MESH,LEGO=.TRUE.)
          ELSEIF(OP(1:8).EQ.'X=TAY   ') THEN
            CALL MATVEC('X=TAY   ',
     &      X%ADR(1)%P,A%ADR(1)%P,Y%ADR(1)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+TAY ',
     &      X%ADR(1)%P,A%ADR(3)%P,Y%ADR(2)%P,C,MESH,LEGO=.TRUE.)
            CALL MATVEC('X=TAY   ',
     &      X%ADR(2)%P,A%ADR(2)%P,Y%ADR(1)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+TAY ',
     &      X%ADR(2)%P,A%ADR(4)%P,Y%ADR(2)%P,C,MESH,LEGO=.TRUE.)
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,10) OP
            IF (LNG.EQ.2) WRITE(LU,11) OP
            CALL PLANTE(1)
            STOP
          ENDIF
C
        ELSEIF(S.EQ.3) THEN
C
          IF(OP(1:8).EQ.'X=AY    ') THEN
            CALL MATVEC('X=AY    ',
     &      X%ADR(1)%P,A%ADR(1)%P,Y%ADR(1)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+AY  ',
     &      X%ADR(1)%P,A%ADR(2)%P,Y%ADR(2)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+AY  ',
     &      X%ADR(1)%P,A%ADR(3)%P,Y%ADR(3)%P,C,MESH,LEGO=.TRUE.)
            CALL MATVEC('X=AY    ',
     &      X%ADR(2)%P,A%ADR(4)%P,Y%ADR(1)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+AY  ',
     &      X%ADR(2)%P,A%ADR(5)%P,Y%ADR(2)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+AY  ',
     &      X%ADR(2)%P,A%ADR(6)%P,Y%ADR(3)%P,C,MESH,LEGO=.TRUE. )
            CALL MATVEC('X=AY    ',
     &      X%ADR(3)%P,A%ADR(7)%P,Y%ADR(1)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+AY  ',
     &      X%ADR(3)%P,A%ADR(8)%P,Y%ADR(2)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+AY  ',
     &      X%ADR(3)%P,A%ADR(9)%P,Y%ADR(3)%P,C,MESH,LEGO=.TRUE.)
          ELSEIF(OP(1:8).EQ.'X=TAY   ') THEN
            CALL MATVEC('X=TAY   ',
     &      X%ADR(1)%P,A%ADR(1)%P,Y%ADR(1)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+TAY ',
     &      X%ADR(1)%P,A%ADR(4)%P,Y%ADR(2)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+TAY ',
     &      X%ADR(1)%P,A%ADR(7)%P,Y%ADR(3)%P,C,MESH,LEGO=.TRUE.)
            CALL MATVEC('X=TAY   ',
     &      X%ADR(2)%P,A%ADR(2)%P,Y%ADR(1)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+TAY ',
     &      X%ADR(2)%P,A%ADR(5)%P,Y%ADR(2)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+TAY ',
     &      X%ADR(2)%P,A%ADR(8)%P,Y%ADR(3)%P,C,MESH,LEGO=.TRUE.)
            CALL MATVEC('X=TAY   ',
     &      X%ADR(3)%P,A%ADR(3)%P,Y%ADR(1)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+TAY ',
     &      X%ADR(3)%P,A%ADR(6)%P,Y%ADR(2)%P,C,MESH,LEGO=.FALSE.)
            CALL MATVEC('X=X+TAY ',
     &      X%ADR(3)%P,A%ADR(9)%P,Y%ADR(3)%P,C,MESH,LEGO=.TRUE.)
C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,10) OP
            IF (LNG.EQ.2) WRITE(LU,11) OP
10          FORMAT(1X,'MATRBL (BIEF) : OPERATION INCONNUE : ',A8)
11          FORMAT(1X,'MATRBL (BIEF) : UNKNOWN OPERATION  : ',A8)
            CALL PLANTE(0)
            STOP
          ENDIF
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,150) S
          IF (LNG.EQ.2) WRITE(LU,151) S
          IF (LNG.EQ.1) WRITE(LU,50) X%NAME,X%TYPE
          IF (LNG.EQ.1) WRITE(LU,51) Y%NAME,Y%TYPE
          IF (LNG.EQ.1) WRITE(LU,52) A%NAME,A%TYPE
          IF (LNG.EQ.1) WRITE(LU,53)
          IF (LNG.EQ.2) WRITE(LU,60) X%NAME,X%TYPE
          IF (LNG.EQ.2) WRITE(LU,61) Y%NAME,Y%TYPE
          IF (LNG.EQ.2) WRITE(LU,62) A%NAME,A%TYPE
          IF (LNG.EQ.2) WRITE(LU,63)
150       FORMAT(1X,'MATRBL (BIEF) : TROP DE VECTEURS INCONNUS :',1I6)
151       FORMAT(1X,'MATRBL (BIEF) : TOO MANY VECTORS          :',1I6)
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C-----------------------------------------------------------------------
C
C  CASE WHERE THE STRUCTURES ARE NOT BLOCKS
C
      ELSEIF(A%TYPE.EQ.3.AND.X%TYPE.EQ.4.AND.Y%TYPE.EQ.4) THEN
C
        CALL MATVEC( OP , X%ADR(1)%P , A , Y%ADR(1)%P , C , MESH )
C
C-----------------------------------------------------------------------
C
      ELSEIF(A%TYPE.EQ.3.AND.X%TYPE.EQ.2.AND.Y%TYPE.EQ.2) THEN
C
        CALL MATVEC( OP , X          , A , Y          , C , MESH )
C
C-----------------------------------------------------------------------
C
C  ERROR
C
      ELSE
C
         IF (LNG.EQ.1) WRITE(LU,50) X%NAME,X%TYPE
         IF (LNG.EQ.1) WRITE(LU,51) Y%NAME,Y%TYPE
         IF (LNG.EQ.1) WRITE(LU,52) A%NAME,A%TYPE
         IF (LNG.EQ.1) WRITE(LU,53)
         IF (LNG.EQ.2) WRITE(LU,60) X%NAME,X%TYPE
         IF (LNG.EQ.2) WRITE(LU,61) Y%NAME,Y%TYPE
         IF (LNG.EQ.2) WRITE(LU,62) A%NAME,A%TYPE
         IF (LNG.EQ.2) WRITE(LU,63)
50       FORMAT(1X,'MATRBL (BIEF) : NOM DE X : ',A6,'  TYPE : ',1I6)
51       FORMAT(1X,'                NOM DE Y : ',A6,'  TYPE : ',1I6)
52       FORMAT(1X,'                NOM DE A : ',A6,'  TYPE : ',1I6)
53       FORMAT(1X,'                CAS NON PREVU')
60       FORMAT(1X,'MATRBL (BIEF) : NAME OF X : ',A6,'  TYPE : ',1I6)
61       FORMAT(1X,'                NAME OF Y : ',A6,'  TYPE : ',1I6)
62       FORMAT(1X,'                NAME OF A : ',A6,'  TYPE : ',1I6)
63       FORMAT(1X,'                NOT IMPLEMENTED')
         CALL PLANTE(1)
         STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  COMPLEMENTS THE VECTOR (PARALLEL MODE)
C
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(X,2,MESH)
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C