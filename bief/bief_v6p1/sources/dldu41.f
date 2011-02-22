C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       L D U FACTORISATION OF THE ELEMENTARY MATRICES
!>                IN MATRIX A
!>                FOR P1 PRISMS.
!><br>            REQUIRES THAT THE DIAGONAL OF A BE THE IDENTITY.
!>  @code
!>            EACH ELEMENTARY MATRIX IS DECOMPOSED IN THE FORM:
!>
!>            LE * DE * UE
!>
!>            LE : LOWER TRIANGULAR WITH 1S ON THE DIAGONAL
!>            DE : DIAGONAL
!>            UE : UPPER TRIANGULAR WITH 1S ON THE DIAGONAL
!>
!>                                                   T
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

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  FOR THE NONSYMMETRICAL MATRICES: UE  LE
!><br>  UE (THE BETAS) IS STORED IN XB (. , 1  TO 15)
!><br>  LE (THE ALFAS) IS STORED IN XB (. , 16 TO 30)

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
!>    </th><td> A12, A13, A14, A15, A16, A21, A22, A23, A24, A25, A26, A31, A32, A33, A34, A35, A36, A41, A42, A43, A44, A45, A46, A51, A52, A53, A54, A55, A56, A61, A62, A63, A64, A65, A66, ALFA21, ALFA31, ALFA32, ALFA41, ALFA42, ALFA43, ALFA51, ALFA52, ALFA53, ALFA54, ALFA61, ALFA62, ALFA63, ALFA64, ALFA65, BETA12, BETA13, BETA14, BETA15, BETA16, BETA22, BETA23, BETA24, BETA25, BETA26, BETA33, BETA34, BETA35, BETA36, BETA44, BETA45, BETA46, BETA55, BETA56, BETA66, C, IELEM, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DLDU41
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
!></td><td>--></td><td>TYPE DE DIAGONALE ( 'Q', 'I' , OU '0' )
!>    </td></tr>
!>          <tr><td>TYPEXA
!></td><td>--></td><td>TYPE DE TERMES EXTRADIAGONAUX ('Q','S',OU'0')
!>    </td></tr>
!>          <tr><td>W
!></td><td><-></td><td>TABLEAUX CONTENANT DB NON ASSEMBLEE
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>XA
!></td><td>--></td><td>TERMES EXTRADIAGONAUX DE LA MATRICE A
!>    </td></tr>
!>          <tr><td>XB
!></td><td><--</td><td>TERMES EXTRADIAGONAUX DE LA MATRICE RESULTAT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DLDU41
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
C| TYPDIA         |-->| TYPE DE DIAGONALE ( 'Q', 'I' , OU '0' )
C| TYPEXA         |-->| TYPE DE TERMES EXTRADIAGONAUX ('Q','S',OU'0')
C| W             |<->| TABLEAUX CONTENANT DB NON ASSEMBLEE
C| X,Y,Z          |-->| COORDONNEES DU MAILLAGE.
C| XA             |-->| TERMES EXTRADIAGONAUX DE LA MATRICE A
C| XB             |<--| TERMES EXTRADIAGONAUX DE LA MATRICE RESULTAT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_DLDU41 => DLDU41
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
      DOUBLE PRECISION, INTENT(OUT) :: W(NELMAX,6)
      LOGICAL, INTENT(IN)           :: COPY
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM
C
      DOUBLE PRECISION Z(1),C
C
      DOUBLE PRECISION     A12,A13,A14,A15,A16
      DOUBLE PRECISION A21,A22,A23,A24,A25,A26
      DOUBLE PRECISION A31,A32,A33,A34,A35,A36
      DOUBLE PRECISION A41,A42,A43,A44,A45,A46
      DOUBLE PRECISION A51,A52,A53,A54,A55,A56
      DOUBLE PRECISION A61,A62,A63,A64,A65,A66
      DOUBLE PRECISION        BETA12,BETA13,BETA14,BETA15,BETA16
      DOUBLE PRECISION ALFA21,BETA22,BETA23,BETA24,BETA25,BETA26
      DOUBLE PRECISION ALFA31,ALFA32,BETA33,BETA34,BETA35,BETA36
      DOUBLE PRECISION ALFA41,ALFA42,ALFA43,BETA44,BETA45,BETA46
      DOUBLE PRECISION ALFA51,ALFA52,ALFA53,ALFA54,BETA55,BETA56
      DOUBLE PRECISION ALFA61,ALFA62,ALFA63,ALFA64,ALFA65,BETA66
C
C-----------------------------------------------------------------------
C
C REQUIRES THAT THE DIAGONAL OF A BE THE IDENTITY
C
      IF(TYPDIA(1:1).NE.'I'.AND.NCSIZE.LE.1) THEN
         IF (LNG.EQ.1) WRITE(LU,1000) TYPDIA(1:1)
         IF (LNG.EQ.2) WRITE(LU,1001) TYPDIA(1:1)
1000     FORMAT(1X,'DLDU41 (BIEF) : DIAGONALE DE A NON EGALE A I :',A1)
1001     FORMAT(1X,'DLDU41 (BIEF) : DIAGONAL OF A NOT EQUAL TO I :',A1)
         CALL PLANTE(0)
         STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(TYPEXA(1:1).EQ.'S') THEN
C
        IF(COPY) CALL OV('X=Y     ' , XB , XA , Z , C  , NELMAX*15 )
C
        DO 100 IELEM=1,NELEM
C
C MATRIX TO FACTORISE (SYMMETRICAL WITH 1S ON THE DIAGONAL)
C
C LINE 1
C          A11 = 1.D0
           A12 = XA(IELEM,1 )
           A13 = XA(IELEM,2 )
           A14 = XA(IELEM,3 )
           A15 = XA(IELEM,4 )
           A16 = XA(IELEM,5 )
C LINE 2
           A22 = 1.D0
           A23 = XA(IELEM,6 )
           A24 = XA(IELEM,7 )
           A25 = XA(IELEM,8 )
           A26 = XA(IELEM,9 )
C LINE 3
           A33 = 1.D0
           A34 = XA(IELEM,10)
           A35 = XA(IELEM,11)
           A36 = XA(IELEM,12)
C LINE 4
           A44 = 1.D0
           A45 = XA(IELEM,13)
           A46 = XA(IELEM,14)
C LINE 5
           A55 = 1.D0
           A56 = XA(IELEM,15)
C LINE 6
           A66 = 1.D0
C
C CROUT L*U FACTORISATION
C
C COLUMN 1
           ALFA21 = A12
           ALFA31 = A13
           ALFA41 = A14
           ALFA51 = A15
           ALFA61 = A16
C
C COLUMN 2
           BETA12 =  A12
           BETA22 =  A22 - ALFA21*BETA12
           ALFA32 = (A23 - ALFA31*BETA12)/BETA22
           ALFA42 = (A24 - ALFA41*BETA12)/BETA22
           ALFA52 = (A25 - ALFA51*BETA12)/BETA22
           ALFA62 = (A26 - ALFA61*BETA12)/BETA22
C
C COLUMN 3
           BETA13 =  A13
           BETA23 =  A23 - ALFA21*BETA13
           BETA33 =  A33 - ALFA31*BETA13 - ALFA32*BETA23
           ALFA43 = (A34 - ALFA41*BETA13 - ALFA42*BETA23)/BETA33
           ALFA53 = (A35 - ALFA51*BETA13 - ALFA52*BETA23)/BETA33
           ALFA63 = (A36 - ALFA61*BETA13 - ALFA62*BETA23)/BETA33
C
C COLUMN 4
           BETA14 =  A14
           BETA24 =  A24 - ALFA21*BETA14
           BETA34 =  A34 - ALFA31*BETA14 - ALFA32*BETA24
           BETA44 =  A44 - ALFA41*BETA14 - ALFA42*BETA24 - ALFA43*BETA34
           ALFA54 = (A45 - ALFA51*BETA14 - ALFA52*BETA24 - ALFA53*BETA34
     &     )/BETA44
           ALFA64 = (A46 - ALFA61*BETA14 - ALFA62*BETA24 - ALFA63*BETA34
     &     )/BETA44
C
C COLUMN 5
           BETA15 =  A15
           BETA25 =  A25 - ALFA21*BETA15
           BETA35 =  A35 - ALFA31*BETA15 - ALFA32*BETA25
           BETA45 =  A45 - ALFA41*BETA15 - ALFA42*BETA25 - ALFA43*BETA35
           BETA55 =  A55 - ALFA51*BETA15 - ALFA52*BETA25 - ALFA53*BETA35
     &                   - ALFA54*BETA45
           ALFA65 = (A56 - ALFA61*BETA15 - ALFA62*BETA25 - ALFA63*BETA35
     &                   - ALFA64*BETA45
     &     )/BETA55
C
C COLUMN 6
           BETA16 =  A16
           BETA26 =  A26 - ALFA21*BETA16
           BETA36 =  A36 - ALFA31*BETA16 - ALFA32*BETA26
           BETA46 =  A46 - ALFA41*BETA16 - ALFA42*BETA26 - ALFA43*BETA36
           BETA56 =  A56 - ALFA51*BETA16 - ALFA52*BETA26 - ALFA53*BETA36
     &                   - ALFA54*BETA46
           BETA66 =  A66 - ALFA61*BETA16 - ALFA62*BETA26 - ALFA63*BETA36
     &                   - ALFA64*BETA46 - ALFA65*BETA56
C
C STORES IN XB AND W2,...,W6
C
           XB(IELEM,1 ) = ALFA21
           XB(IELEM,2 ) = ALFA31
           XB(IELEM,3 ) = ALFA41
           XB(IELEM,4 ) = ALFA51
           XB(IELEM,5 ) = ALFA61
C
           XB(IELEM,6 ) = ALFA32
           XB(IELEM,7 ) = ALFA42
           XB(IELEM,8 ) = ALFA52
           XB(IELEM,9 ) = ALFA62
C
           XB(IELEM,10) = ALFA43
           XB(IELEM,11) = ALFA53
           XB(IELEM,12) = ALFA63
C
           XB(IELEM,13) = ALFA54
           XB(IELEM,14) = ALFA64
C
           XB(IELEM,15) = ALFA65
C
           W(IELEM,2)    = BETA22
           W(IELEM,3)    = BETA33
           W(IELEM,4)    = BETA44
           W(IELEM,5)    = BETA55
           W(IELEM,6)    = BETA66
C
100     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSEIF(TYPEXA(1:1).EQ.'Q') THEN
C
        IF(COPY) CALL OV('X=Y     ' , XB , XA , Z , C  , NELMAX*30 )
C
        DO 200 IELEM=1,NELEM
C
C MATRIX TO FACTORISE (WITH 1S ON THE DIAGONAL)
C
C LINE 1
C          A11 = 1.D0
           A12 = XA(IELEM,1 )
           A13 = XA(IELEM,2 )
           A14 = XA(IELEM,3 )
           A15 = XA(IELEM,4 )
           A16 = XA(IELEM,5 )
C LINE 2
           A21 = XA(IELEM,16)
           A22 = 1.D0
           A23 = XA(IELEM,6 )
           A24 = XA(IELEM,7 )
           A25 = XA(IELEM,8 )
           A26 = XA(IELEM,9 )
C LINE 3
           A31 = XA(IELEM,17)
           A32 = XA(IELEM,21)
           A33 = 1.D0
           A34 = XA(IELEM,10)
           A35 = XA(IELEM,11)
           A36 = XA(IELEM,12)
C LINE 4
           A41 = XA(IELEM,18)
           A42 = XA(IELEM,22)
           A43 = XA(IELEM,25)
           A44 = 1.D0
           A45 = XA(IELEM,13)
           A46 = XA(IELEM,14)
C LINE 5
           A51 = XA(IELEM,19)
           A52 = XA(IELEM,23)
           A53 = XA(IELEM,26)
           A54 = XA(IELEM,28)
           A55 = 1.D0
           A56 = XA(IELEM,15)
C LINE 6
           A61 = XA(IELEM,20)
           A62 = XA(IELEM,24)
           A63 = XA(IELEM,27)
           A64 = XA(IELEM,29)
           A65 = XA(IELEM,30)
           A66 = 1.D0
C
C CROUT L*U FACTORISATION
C
C COLUMN 1
           ALFA21 = A21
           ALFA31 = A31
           ALFA41 = A41
           ALFA51 = A51
           ALFA61 = A61
C
C COLUMN 2
           BETA12 =  A12
           BETA22 =  A22 - ALFA21*BETA12
           ALFA32 = (A32 - ALFA31*BETA12)/BETA22
           ALFA42 = (A42 - ALFA41*BETA12)/BETA22
           ALFA52 = (A52 - ALFA51*BETA12)/BETA22
           ALFA62 = (A62 - ALFA61*BETA12)/BETA22
C
C COLUMN 3
           BETA13 =  A13
           BETA23 =  A23 - ALFA21*BETA13
           BETA33 =  A33 - ALFA31*BETA13 - ALFA32*BETA23
           ALFA43 = (A43 - ALFA41*BETA13 - ALFA42*BETA23)/BETA33
           ALFA53 = (A53 - ALFA51*BETA13 - ALFA52*BETA23)/BETA33
           ALFA63 = (A63 - ALFA61*BETA13 - ALFA62*BETA23)/BETA33
C
C COLUMN 4
           BETA14 =  A14
           BETA24 =  A24 - ALFA21*BETA14
           BETA34 =  A34 - ALFA31*BETA14 - ALFA32*BETA24
           BETA44 =  A44 - ALFA41*BETA14 - ALFA42*BETA24 - ALFA43*BETA34
           ALFA54 = (A54 - ALFA51*BETA14 - ALFA52*BETA24 - ALFA53*BETA34
     &     )/BETA44
           ALFA64 = (A64 - ALFA61*BETA14 - ALFA62*BETA24 - ALFA63*BETA34
     &     )/BETA44
C
C COLUMN 5
           BETA15 =  A15
           BETA25 =  A25 - ALFA21*BETA15
           BETA35 =  A35 - ALFA31*BETA15 - ALFA32*BETA25
           BETA45 =  A45 - ALFA41*BETA15 - ALFA42*BETA25 - ALFA43*BETA35
           BETA55 =  A55 - ALFA51*BETA15 - ALFA52*BETA25 - ALFA53*BETA35
     &                   - ALFA54*BETA45
           ALFA65 = (A65 - ALFA61*BETA15 - ALFA62*BETA25 - ALFA63*BETA35
     &                   - ALFA64*BETA45
     &     )/BETA55
C
C COLUMN 6
           BETA16 =  A16
           BETA26 =  A26 - ALFA21*BETA16
           BETA36 =  A36 - ALFA31*BETA16 - ALFA32*BETA26
           BETA46 =  A46 - ALFA41*BETA16 - ALFA42*BETA26 - ALFA43*BETA36
           BETA56 =  A56 - ALFA51*BETA16 - ALFA52*BETA26 - ALFA53*BETA36
     &                   - ALFA54*BETA46
           BETA66 =  A66 - ALFA61*BETA16 - ALFA62*BETA26 - ALFA63*BETA36
     &                   - ALFA64*BETA46 - ALFA65*BETA56
C
C STORES IN XB AND W2,...,W6
C L D U FACTORISATION AT THE SAME TIME
C
           XB(IELEM,1 ) = BETA12
           XB(IELEM,2 ) = BETA13
           XB(IELEM,3 ) = BETA14
           XB(IELEM,4 ) = BETA15
           XB(IELEM,5 ) = BETA16
C
           XB(IELEM,6 ) = BETA23/BETA22
           XB(IELEM,7 ) = BETA24/BETA22
           XB(IELEM,8 ) = BETA25/BETA22
           XB(IELEM,9 ) = BETA26/BETA22
C
           XB(IELEM,10) = BETA34/BETA33
           XB(IELEM,11) = BETA35/BETA33
           XB(IELEM,12) = BETA36/BETA33
C
           XB(IELEM,13) = BETA45/BETA44
           XB(IELEM,14) = BETA46/BETA44
C
           XB(IELEM,15) = BETA56/BETA55
C
           XB(IELEM,16) = ALFA21
           XB(IELEM,17) = ALFA31
           XB(IELEM,18) = ALFA41
           XB(IELEM,19) = ALFA51
           XB(IELEM,20) = ALFA61
C
           XB(IELEM,21) = ALFA32
           XB(IELEM,22) = ALFA42
           XB(IELEM,23) = ALFA52
           XB(IELEM,24) = ALFA62
C
           XB(IELEM,25) = ALFA43
           XB(IELEM,26) = ALFA53
           XB(IELEM,27) = ALFA63
C
           XB(IELEM,28) = ALFA54
           XB(IELEM,29) = ALFA64
C
           XB(IELEM,30) = ALFA65
C
           W(IELEM,2)    = BETA22
           W(IELEM,3)    = BETA33
           W(IELEM,4)    = BETA44
           W(IELEM,5)    = BETA55
           W(IELEM,6)    = BETA66
C
200     CONTINUE
C
C-----------------------------------------------------------------------
C
      ELSE
         IF (LNG.EQ.1) WRITE(LU,2000) TYPEXA(1:1)
         IF (LNG.EQ.2) WRITE(LU,2001) TYPEXA(1:1)
2000     FORMAT(1X,'DLDU41 (BIEF) : TYPE DE MATRICE NON PREVU :',A1)
2001     FORMAT(1X,'DLDU41 (BIEF) : TYPE OF MATRIX NOT AVAILABLE :',A1)
         CALL PLANTE(0)
         STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  MULTIPLICATIVE ASSEMBLY OF THE DIAGONAL WITH INITIALISATION OF DB TO 1
C  SKIPS IKLE1 BECAUSE W1 = 1
C
      CALL ASMVEC(DB,IKLE(1,2),NPOIN,NELEM,NELMAX,5,W(1,2),.TRUE.,LV)
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