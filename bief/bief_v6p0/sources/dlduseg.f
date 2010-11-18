C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       L D U FACTORISATION OF THE ELEMENTARY MATRICES BY SEGMENT
!>                FOR SEGMENTS.
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
!>
!>
!>
!>      (  1   X12 )   (  1   0 ) (  1       0     ) (  1   X12 )
!>      (          ) = (        ) (                ) (          )
!>      ( X21   1  )   ( X21  1 ) (  0   1-X12*X21 ) (  0     1 )
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> COPY, DB, GLOSEG, NPOIN, NSEG, TYPDIA, TYPEXA, XA, XB
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
!>    </th><td> C, ISEG, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DLDUSEG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV(), PLANTE()
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
!>      <td><center> 5.5                                       </center>
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
!>          <tr><td>GLOSEG
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION SEGMENT A GLOBALE
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>DIMENSION DES TABLEAUX
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NOMBRE DE SEGMENTS
!>    </td></tr>
!>          <tr><td>TYPDIA
!></td><td><--</td><td>TYPE DE DIAGONALE ( 'Q', 'I' , OU '0' )
!>    </td></tr>
!>          <tr><td>TYPEXA
!></td><td><--</td><td>TYPE DE TERMES EXTRADIAGONAUX ('Q','S',OU'0')
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
                        SUBROUTINE DLDUSEG
     &(DB,XB,TYPDIA,XA,TYPEXA,GLOSEG,NSEG,NPOIN,COPY)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| COPY           |-->| SI .TRUE. A EST COPIEE DANS B
C|                |   | SINON ON CONSIDERE QUE B EST DEJA REMPLIE
C| DB             |<--| DIAGONALE DE LA MATRICE RESULTAT
C| GLOSEG         |-->| PASSAGE DE LA NUMEROTATION SEGMENT A GLOBALE
C| NPOIN          |-->| DIMENSION DES TABLEAUX
C| NSEG           |-->| NOMBRE DE SEGMENTS
C| TYPDIA         |<--| TYPE DE DIAGONALE ( 'Q', 'I' , OU '0' )
C| TYPEXA         |<--| TYPE DE TERMES EXTRADIAGONAUX ('Q','S',OU'0')
C| XA             |<--| TERMES EXTRADIAGONAUX DE LA MATRICE A
C| XB             |<--| TERMES EXTRADIAGONAUX DE LA MATRICE RESULTAT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_DLDUSEG => DLDUSEG
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)           :: NSEG,NPOIN
      DOUBLE PRECISION, INTENT(OUT) :: DB(NPOIN),XB(NSEG,*)
      DOUBLE PRECISION, INTENT(IN)  :: XA(NSEG,*)
      CHARACTER(LEN=1), INTENT(IN)  :: TYPDIA,TYPEXA
      INTEGER, INTENT(IN)           :: GLOSEG(NSEG,2)
      LOGICAL, INTENT(IN)           :: COPY
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ISEG
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
100      FORMAT(1X,'DLDUSEG (BIEF) : DIAGONALE DE A NON IDENTITE :',A1)
101      FORMAT(1X,'DLDUSEG (BIEF) : DIAGONAL OF A NOT IDENTITY :',A1)
         CALL PLANTE(1)
         STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(TYPEXA(1:1).EQ.'S') THEN
C
        IF(COPY) THEN
          CALL OV('X=Y     ' , XB , XA , Z , C , NSEG )
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(TYPEXA(1:1).EQ.'Q') THEN
C
        IF(COPY) THEN
          CALL OV('X=Y     ' , XB , XA , Z , C , 2*NSEG )
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
        IF (LNG.EQ.1) WRITE(LU,200) TYPEXA(1:1)
        IF (LNG.EQ.2) WRITE(LU,201) TYPEXA(1:1)
200     FORMAT(1X,'DLDUSEG (BIEF) : TYPE DE MATRICE NON PREVU :',A1)
201     FORMAT(1X,'DLDUSEG (BIEF) : TYPE OF MATRIX NOT TREATED:',A1)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  MULTIPLICATIVE ASSEMBLY OF THE DIAGONAL WITH INITIALISATION
C  OF DB TO 1
C
      CALL OV('X=C     ' , DB , DB , DB , 1.D0 , NPOIN )
C
      IF(TYPEXA(1:1).EQ.'S') THEN
C
      DO ISEG=1,NSEG
        DB(GLOSEG(ISEG,2))=DB(GLOSEG(ISEG,2))*(1.D0-XB(ISEG,1)**2)
      ENDDO
C
      ELSE
C
      DO ISEG=1,NSEG
        DB(GLOSEG(ISEG,2))=
     &  DB(GLOSEG(ISEG,2))*(1.D0-XB(ISEG,1)*XB(ISEG,2))
      ENDDO
C
      ENDIF
C
C  INVERTS DB (COULD DIVIDE BY 0)
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