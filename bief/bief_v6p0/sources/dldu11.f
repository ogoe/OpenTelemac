C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       L D U FACTORISATION OF THE ELEMENTARY MATRICES
!>                IN MATRIX A
!>                FOR P1 TRIANGLES.
!><br>            REQUIRES THAT THE DIAGONAL OF A BE THE IDENTITY.
!>  @code
!>            EACH ELEMENTARY MATRIX IS DECOMPOSED IN THE FORM:
!>
!>            LE X DE X UE
!>
!>            LE : LOWER TRIANGULAR WITH 1S ON THE DIAGONAL
!>            DE : DIAGONAL
!>            UE : UPPER TRIANGULAR WITH 1S ON THE DIAGONAL
!>
!>                                                T
!>            IF THE MATRIX IS SYMMETRICAL : LE =  UE
!>
!>            "DE" MATRICES ARE CONSIDERED LIKE DIAGONALS OF SIZE
!>            NPOIN X NPOIN, WHICH ARE FILLED WITH 1S FOR THE POINTS
!>            WHICH DO NOT BELONG TO THE CONSIDERED ELEMENT
!>
!>            THEN PERFORMS THE PRODUCT OF ALL THESE DIAGONALS
!>            YIELDING DIAGONAL DB
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> COPY, DB, IKLE, LV, NELEM, NELMAX, NPOIN, TYPDIA, TYPEXA, W, XA, XB
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
!>    </th><td> C, IELEM, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DLDU11
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ASMVEC(), OV(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DECLDU()

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
!>          <tr><td>COPY
!></td><td>--></td><td>SI .TRUE. A EST COPIEE DANS B
!>                  SINON ON CONSIDERE QUE B EST DEJA REMPLIE
!>    </td></tr>
!>          <tr><td>DB
!></td><td><--</td><td>DIAGONALE DE LA MATRICE RESULTAT
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
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES TRIANGLES.
!>    </td></tr>
!>          <tr><td>TYPDIA
!></td><td><--</td><td>TYPE DE DIAGONALE ( 'Q', 'I' , OU '0' )
!>    </td></tr>
!>          <tr><td>TYPEXA
!></td><td><--</td><td>TYPE DE TERMES EXTRADIAGONAUX ('Q','S',OU'0')
!>    </td></tr>
!>          <tr><td>W
!></td><td>--></td><td>TABLEAU DE TRAVAIL DE DIMENSION (NELMAX,3)
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>XA
!></td><td><--</td><td>TERMES EXTRADIAGONAUX DE LA MATRICE A
!>    </td></tr>
!>          <tr><td>XB
!></td><td><--</td><td>TERMES EXTRADIAGONAUX DE LA MATRICE RESULTAT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DLDU11
     &(DB,XB,TYPDIA,XA,TYPEXA,
     & IKLE,NELEM,NELMAX,NPOIN,W,COPY,LV)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| COPY           |-->| SI .TRUE. A EST COPIEE DANS B
C|                |   | SINON ON CONSIDERE QUE B EST DEJA REMPLIE
C| DB             |<--| DIAGONALE DE LA MATRICE RESULTAT
C| IKLE           |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NPOIN          |-->| DIMENSION DES TABLEAUX
C| SURFAC         |-->| SURFACE DES TRIANGLES.
C| TYPDIA         |<--| TYPE DE DIAGONALE ( 'Q', 'I' , OU '0' )
C| TYPEXA         |<--| TYPE DE TERMES EXTRADIAGONAUX ('Q','S',OU'0')
C| W             |-->| TABLEAU DE TRAVAIL DE DIMENSION (NELMAX,3)
C| X,Y,Z          |-->| COORDONNEES DU MAILLAGE.
C| XA             |<--| TERMES EXTRADIAGONAUX DE LA MATRICE A
C| XB             |<--| TERMES EXTRADIAGONAUX DE LA MATRICE RESULTAT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_DLDU11 => DLDU11
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)           :: NELEM,NELMAX,LV,NPOIN
      DOUBLE PRECISION, INTENT(OUT) :: DB(NPOIN),XB(NELMAX,*)
      DOUBLE PRECISION, INTENT(IN)  :: XA(NELMAX,*)
      CHARACTER(LEN=1), INTENT(IN)  :: TYPDIA,TYPEXA
      INTEGER, INTENT(IN)           :: IKLE(NELMAX,*)
      DOUBLE PRECISION, INTENT(OUT) :: W(NELMAX,3)
      LOGICAL, INTENT(IN)           :: COPY
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM
C
      DOUBLE PRECISION Z(1),C
C
C-----------------------------------------------------------------------
C
C REQUIRES THAT THE DIAGONAL OF A BE THE IDENTITY (EXCEPT IN PARALLEL MODE)
C
      IF(TYPDIA(1:1).NE.'I'.AND.NCSIZE.LE.1) THEN
         IF (LNG.EQ.1) WRITE(LU,100) TYPDIA(1:1)
         IF (LNG.EQ.2) WRITE(LU,101) TYPDIA(1:1)
100      FORMAT(1X,'DLDU11 (BIEF) : DIAGONALE DE A NON EGALE A I :',A1)
101      FORMAT(1X,'DLDU11 (BIEF) : DIAGONAL OF A NOT EQUAL TO I :',A1)
         CALL PLANTE(0)
         STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(TYPEXA(1:1).EQ.'S') THEN
C
        IF(COPY) CALL OV('X=Y     ' , XB , XA , Z , C  , NELMAX*3 )
C
        DO 10 IELEM = 1 , NELEM
         W(IELEM,2) = 1.D0 - XB(IELEM,1)**2
         XB(IELEM,3) = (XB(IELEM,3)-XB(IELEM,1)*XB(IELEM,2))/W(IELEM,2)
         W(IELEM,3) = 1.D0 - XB(IELEM,2)**2 -XB(IELEM,3)**2
10      CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(TYPEXA(1:1).EQ.'Q') THEN
C
        IF(COPY) CALL OV('X=Y     ' , XB , XA , Z , C  , NELMAX*6 )
C
        DO 20 IELEM = 1 , NELEM
C L U FACTORISATION
         W(IELEM,2)=1.D0 - XB(IELEM,1)*XB(IELEM,4)
         XB(IELEM,6) = (XB(IELEM,6)-XB(IELEM,1)*XB(IELEM,5))/W(IELEM,2)
         XB(IELEM,3) =  XB(IELEM,3)-XB(IELEM,4)*XB(IELEM,2)
         W(IELEM,3)=1.D0-XB(IELEM,2)*XB(IELEM,5)-XB(IELEM,3)*XB(IELEM,6)
C L D U FACTORISATION
         XB(IELEM,3) = XB(IELEM,3) / W(IELEM,2)
20      CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSE
         IF (LNG.EQ.1) WRITE(LU,200) TYPEXA(1:1)
         IF (LNG.EQ.2) WRITE(LU,201) TYPEXA(1:1)
200      FORMAT(1X,'DLDU11 (BIEF) : TYPE DE MATRICE NON PREVU :',A1)
201      FORMAT(1X,'DLDU11 (BIEF) : TYPE OF MATRIX NOT AVAILABLE :',A1)
         CALL PLANTE(0)
         STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  MULTIPLICATIVE ASSEMBLY OF THE DIAGONAL WITH INITIALISATION OF DB TO 1
C  SKIPS IKLE1 BECAUSE W1 = 1
C
      CALL ASMVEC(DB,IKLE(1,2),NPOIN,NELEM,NELMAX,2,W(1,2),.TRUE.,LV)
C
C  INVERTS DB
C
      CALL OV( 'X=1/Y   ' , DB , DB , Z , C , NPOIN )
C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C