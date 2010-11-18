C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE SYSTEM L X = B (ELEMENT BY ELEMENT).
!>  @code
!>            MATRIX L IS HERE THE RESULT OF THE FACTORISATION
!>            PERFORMED IN SUBROUTINE DECLDU.<br>
!>            EACH ELEMENTARY MATRIX IS DECOMPOSED IN THE FORM:<br>
!>            LE X DE X UE<br>
!>            LE : LOWER TRIANGULAR WITH 1S ON THE DIAGONAL
!>            DE : DIAGONAL
!>            UE : UPPER TRIANGULAR WITH 1S ON THE DIAGONAL<br>
!>                                                T
!>            IF THE MATRIX IS SYMMETRICAL : LE =  UE
!>  @endcode
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @code
!>-----------------------------------------------------------------------
!>  MEANING OF IELM :
!>
!>  TYPE OF ELEMENT      NUMBER OF POINTS      CODED IN THIS SUBROUTINE
!>
!>  11 : P1 TRIANGLE            3                       YES
!>  12 : P2 TRIANGLE            6
!>  13 : P1-ISO P1 TRIANGLE     6
!>  14 : P2 TRIANGLE            7
!>  21 : Q1 QUADRILATERAL       4                       YES
!>  22 : Q2 QUADRILATERAL       8
!>  24 : Q2 QUADRILATERAL       9
!>  31 : P1 TETRAHEDRON         4
!>  32 : P2 TETRAHEDRON        10
!>  41 : TELEMAC-3D PRISMS      6                       YES
!>  41 : PRISMS SPLIT IN TETRAHEDRONS     4             YES
!>
!>-----------------------------------------------------------------------
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> B, COPY, DITR, IELM, IKLE, LV, NELEM, NELMAX, NPOIN, TYPEXA, X, XA
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DESCEN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DES11(), DES21(), DES41(), OV(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DWNUP1(), GODWN1()

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
!> </td><td> 18/08/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>B
!></td><td><--</td><td>SECOND MEMBRE DU SYSTEME A RESOUDRE.
!>    </td></tr>
!>          <tr><td>COPY
!></td><td>--></td><td>SI .TRUE. B EST RECOPIE DANS X AU PREALABLE
!>    </td></tr>
!>          <tr><td>DITR
!></td><td>--></td><td>CARACTERE  'D' : ON CALCULE AVEC A
!>                  'T' : ON CALCULE AVEC A TRANSPOSEE
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!>    </td></tr>
!>          <tr><td>LV
!></td><td>--></td><td>LONGUEUR DU VECTEUR POUR LA VECTORISATION
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>DIMENSION DES TABLEAUX
!>    </td></tr>
!>          <tr><td>TYPEXA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>SOLUTION DU SYSTEME AX = B
!>    </td></tr>
!>          <tr><td>XA
!></td><td><--</td><td>TERMES EXTRADIAGONAUX DE LA MATRICE A
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DESCEN
     &(X, XA,TYPEXA,B,IKLE,NELEM,NELMAX,NPOIN,IELM,DITR,COPY,LV)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| B             |<--| SECOND MEMBRE DU SYSTEME A RESOUDRE.
C| COPY           |-->| SI .TRUE. B EST RECOPIE DANS X AU PREALABLE
C| DITR           |-->| CARACTERE  'D' : ON CALCULE AVEC A
C|                |   | 'T' : ON CALCULE AVEC A TRANSPOSEE
C| IELM           |-->| TYPE D'ELEMENT
C| IKLE           |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NPOIN          |-->| DIMENSION DES TABLEAUX
C| TYPEXA         |---| 
C| X             |<--| SOLUTION DU SYSTEME AX = B
C| XA             |<--| TERMES EXTRADIAGONAUX DE LA MATRICE A
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_DESCEN => DESCEN
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN)    :: IELM,NPOIN,NELEM,NELMAX,LV
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XA(NELMAX,*),B(NPOIN)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXA,DITR
      LOGICAL         , INTENT(IN)    :: COPY
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C 1) INITIALISES : X = SECOND MEMBER
C
      IF(COPY) CALL OV( 'X=Y     ' , X , B , B , 0.D0 , NPOIN )
C
C-----------------------------------------------------------------------
C
C 2) INVERTS THE LOWER TRIANGULAR MATRICES (DESCENT)
C
C     2.1) TRANSPOSE CASE
C
      IF(TYPEXA(1:1).EQ.'S' .OR.
     &  (TYPEXA(1:1).EQ.'Q'.AND.DITR(1:1).EQ.'T')) THEN
C
      IF(IELM.EQ.11) THEN
C
        CALL DES11(X,XA(1,1),XA(1,2),XA(1,3),
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,NPOIN,LV)
C
      ELSEIF(IELM.EQ.21.OR.IELM.EQ.12.OR.IELM.EQ.31.OR.IELM.EQ.51) THEN
C
        CALL DES21(X,XA(1,1),XA(1,2),XA(1,3),XA(1,4),XA(1,5),XA(1,6),
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &             NELEM,NELMAX,NPOIN,LV)
C
      ELSEIF(IELM.EQ.41) THEN
C
        CALL DES41(X,XA(1,1),XA(1,2),XA(1,3),XA(1,4),XA(1,5),XA(1,6),
     &             XA(1,7),XA(1,8),XA(1,9),XA(1,10),XA(1,11),XA(1,12),
     &             XA(1,13),XA(1,14),XA(1,15),
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &             IKLE(1,4),IKLE(1,5),IKLE(1,6),NELEM,NELMAX,NPOIN,LV)
C
C  IELM NOT IMPLEMENTED: ERROR
C
      ELSE
C
       IF (LNG.EQ.1) WRITE(LU,100) IELM
       IF (LNG.EQ.2) WRITE(LU,101) IELM
100    FORMAT(1X,'DESCEN (BIEF) : IELM = ',1I6,' ELEMENT NON PREVU')
101    FORMAT(1X,'DESCEN (BIEF) : IELM = ',1I6,' ELEMENT NOT AVAILABLE')
       CALL PLANTE(1)
       STOP
C
      ENDIF
C
C     2.2) DIRECT CASE
C
      ELSEIF(TYPEXA(1:1).EQ.'Q'.AND.DITR(1:1).EQ.'D') THEN
C
      IF(IELM.EQ.11) THEN
C
        CALL DES11(X,XA(1,4),XA(1,5),XA(1,6),
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,NPOIN,LV)
C
      ELSEIF(IELM.EQ.21.OR.IELM.EQ.12.OR.IELM.EQ.31.OR.IELM.EQ.51) THEN
C
        CALL DES21(X,XA(1,7),XA(1,8),XA(1,9),XA(1,10),XA(1,11),XA(1,12),
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &             NELEM,NELMAX,NPOIN,LV)
C
      ELSEIF(IELM.EQ.41) THEN
C
        CALL DES41(X,
     &            XA(1,16),XA(1,17),XA(1,18),XA(1,19),XA(1,20),XA(1,21),
     &            XA(1,22),XA(1,23),XA(1,24),XA(1,25),XA(1,26),XA(1,27),
     &            XA(1,28),XA(1,29),XA(1,30),
     &            IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &            IKLE(1,4),IKLE(1,5),IKLE(1,6),NELEM,NELMAX,NPOIN,LV)
C
C  IELM NOT IMPLEMENTED: ERROR
C
      ELSE
C
       IF (LNG.EQ.1) WRITE(LU,100) IELM
       IF (LNG.EQ.2) WRITE(LU,101) IELM
       CALL PLANTE(1)
       STOP
C
      ENDIF
C
C     2.3) CASE NOT IMPLEMENTED
C
      ELSE
         IF (LNG.EQ.1) WRITE(LU,200) TYPEXA(1:1),DITR(1:1)
         IF (LNG.EQ.2) WRITE(LU,201) TYPEXA(1:1),DITR(1:1)
200      FORMAT(1X,'DESCEN (BIEF) : TYPE DE MATRICE NON PREVU :',A1,/,
     &          1X,'AVEC DITR=',A1)
201      FORMAT(1X,'DESCEN (BIEF) : UNEXPECTED TYPE OF MATRIX :',A1,/,
     &          1X,'WITH DITR=',A1)
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