C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       L D U FACTORISATION OF THE ELEMENTARY MATRICES
!>                IN MATRIX A.
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
!>   ||||||   FINALLY: DB IS INVERTED BECAUSE THAT'S HOW IT WILL BE
!>                     USED IN DESREM
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
!>  21 : Q1 QUADRILATERAL       4                       YES
!>  41 : TELEMAC-3D PRISMS      6                       YES
!>
!>-----------------------------------------------------------------------
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, B, COPY, LV, MESH
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELM, NELEM, NELMAX, NPOIN, TYPDA, TYPEA
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DECLDU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DLDU11(), DLDU21(), DLDU41(), DLDUSEG(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DCPLDU()

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
!> </td><td> 26/02/04
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A
!></td><td>--></td><td>MATRICE A
!>    </td></tr>
!>          <tr><td>B
!></td><td><--</td><td>RESULTAT
!>    </td></tr>
!>          <tr><td>COPY
!></td><td>--></td><td>SI .TRUE. A EST COPIEE DANS B
!>                  SINON ON CONSIDERE QUE B EST DEJA REMPLIE
!>    </td></tr>
!>          <tr><td>LV
!></td><td>--></td><td>LONGUEUR DU VECTEUR POUR LA VECTORISATION
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DECLDU
     &(B,A,MESH,COPY,LV)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |-->| MATRICE A
C| B             |<--| RESULTAT
C| COPY           |-->| SI .TRUE. A EST COPIEE DANS B
C|                |   | SINON ON CONSIDERE QUE B EST DEJA REMPLIE
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| MESH           |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF   !, EX_DECLDU => DECLDU
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: B
      TYPE(BIEF_OBJ) , INTENT(IN)    :: A
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      LOGICAL        , INTENT(IN)    :: COPY
      INTEGER        , INTENT(IN)    :: LV
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NELMAX,IELM,NPOIN,NELEM
C
      CHARACTER*1 TYPDA,TYPEA
C
C-----------------------------------------------------------------------
C
      IELM   = A%ELMLIN
      NELEM  = MESH%NELEM
      NELMAX = MESH%NELMAX
C
      TYPDA = A%TYPDIA
      TYPEA = A%TYPEXT
C
      NPOIN = A%D%DIM1
C
C-----------------------------------------------------------------------
C
      IF(A%STO.EQ.1) THEN
C
      IF(IELM.EQ.11) THEN
C
        CALL DLDU11(B%D%R,B%X%R,TYPDA,A%X%R,TYPEA,
     &              MESH%IKLE%I,NELEM,NELMAX,NPOIN,MESH%W%R,COPY,LV)
C
      ELSEIF(IELM.EQ.21.OR.IELM.EQ.12.OR.IELM.EQ.31.OR.IELM.EQ.51) THEN
C
        CALL DLDU21(B%D%R,B%X%R,TYPDA,A%X%R,TYPEA,
     &              MESH%IKLE%I,NELEM,NELMAX,NPOIN,MESH%W%R,COPY,LV)
C
      ELSEIF(IELM.EQ.41) THEN
C
        CALL DLDU41(B%D%R,B%X%R,TYPDA,A%X%R,TYPEA,
     &              MESH%IKLE%I,NELEM,NELMAX,NPOIN,MESH%W%R,COPY,LV)
C
C  IELM NOT IMPLEMENTED: ERROR
C
      ELSE
       IF (LNG.EQ.1) WRITE(LU,100) IELM
       IF (LNG.EQ.2) WRITE(LU,101) IELM
100    FORMAT(1X,'DECLDU (BIEF) : IELM = ',1I6,' ELEMENT NON PREVU')
101    FORMAT(1X,'DECLDU (BIEF) : IELM = ',1I6,' ELEMENT NOT AVAILABLE')
       CALL PLANTE(1)
       STOP
      ENDIF
C
      ELSEIF(A%STO.EQ.3) THEN
        CALL DLDUSEG(B%D%R,B%X%R,TYPDA,A%X%R,TYPEA,
     &               MESH%GLOSEG%I,MESH%NSEG,NPOIN,COPY)
      ELSE
        WRITE(LU,*) 'UNKNOWN MATRIX STORAGE IN DECLDU'
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  DESCRIPTION OF B
C
      B%TYPDIA='Q'
      B%TYPEXT=TYPEA
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
