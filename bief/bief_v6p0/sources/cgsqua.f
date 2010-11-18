C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE LINEAR SYSTEM A X = B
!>                USING THE SQUARED CONJUGATE GRADIENT METHOD.
!>  @code
!>     ALGORITHM:
!>
!>        |
!>        |   INITIALISATION
!>        |   ---------------
!>        |
!>        |    0            N
!>        |   X  VECTOR IN R , APPROXIMATION OF THE SOLUTION
!>        |
!>        |      0      0
!>        |     G  = A X  - B
!>        |
!>        |     K0 = P0 = G0
!>        |
!>        |
!>        |   ITERATIONS
!>        |   ----------
!>        |
!>        |              M     0
!>        |       M   ( K  ,  G  )
!>        |     RO =  ------------
!>        |               M    0
!>        |           (A P ,  G  )
!>        |
!>        |      M     M    M     M
!>        |     H   = K - RO * A P
!>        |
!>        |      M+1   M    M       M    M
!>        |     G   = G - RO * A ( H  + K  )
!>        |
!>        |      M+1   M      M     M    M
!>        |     X   = X   - RO * ( H  + K  )
!>        |
!>        |                 0     M+1
!>        |              ( G  , G   )
!>        |     BETA =   ------------
!>        |                 0   M
!>        |              ( G , G    )
!>        |
!>        |      M+1   M+1        M    M     M       M
!>        |     P  =  G   + 2*BETA  * H + BETA**2 * P
!>        |
!>        |      M+1   M+1        M    M
!>        |     K  =  G   +   BETA  * H
!>        |
!>  @endcode
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @code
!>-----------------------------------------------------------------------
!>                        PRECONDITIONING
!>                        (ALSO SEE SOLV01)
!>-----------------------------------------------------------------------
!>    PRECON VALUE     I                  MEANING
!>-----------------------------------------------------------------------
!>        0            I  NO PRECONDITIONING
!>        2            I  DIAGONAL PRECONDITIONING WITH THE MATRIX
!>                     I  DIAGONAL
!>        3            I  DIAGONAL PRECONDITIONING WITH THE CONDENSED
!>                     I  MATRIX
!>        5            I  OTHER
!>-----------------------------------------------------------------------
!>  @endcode
!>  @code
!>  MEANING OF IELM :
!>
!>  TYPE OF ELEMENT      NUMBER OF POINTS      CODED IN THIS SUBROUTINE
!>
!>  11 : P1 TRIANGLE            3                       YES
!>  12 : P2 TRIANGLE            6                       YES
!>  13 : P1-ISO P1 TRIANGLE     6                       YES
!>  14 : P2 TRIANGLE            7
!>  21 : Q1 QUADRILATERAL       4                       YES
!>  22 : Q2 QUADRILATERAL       8
!>  24 : Q2 QUADRILATERAL       9
!>  31 : P1 TETRAHEDRON         4                       YES
!>  32 : P2 TETRAHEDRON        10
!>  41 : MITHRIDATE PRISMS      6                       YES
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, AHPK, B, CFG, G, G0, H, INFOGR, K, MESH, P, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BETA, C, GMG0, GMP1G0, M, RELAT, RL, RO, TESTL, XL
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CGSQUA
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> MATRBL(), OS(), P_DOTS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SOLVE()

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
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A
!></td><td>--></td><td>MATRICE DU SYSTEME
!>    </td></tr>
!>          <tr><td>AD
!></td><td><-></td><td>MATRICE A MULTIPLIEE PAR D.
!>    </td></tr>
!>          <tr><td>AG
!></td><td><-></td><td>A X (GRADIENT DE DESCENTE).
!>    </td></tr>
!>          <tr><td>AHPK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>B
!></td><td>--></td><td>SECOND MEMBRE DU SYSTEME.
!>    </td></tr>
!>          <tr><td>CFG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>D
!></td><td><-></td><td>DIRECTION DE DESCENTE.
!>    </td></tr>
!>          <tr><td>G
!></td><td><-></td><td>GRADIENT DE DESCENTE.
!>    </td></tr>
!>          <tr><td>G0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>H
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INFOGR
!></td><td>--></td><td>SI OUI, ON IMPRIME UN COMPTE-RENDU
!>    </td></tr>
!>          <tr><td>K
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>P
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VALEUR INITIALE, PUIS SOLUTION
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CGSQUA
     &(X,A,B,MESH, G,G0,P,K,H,AHPK,CFG,INFOGR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |-->| MATRICE DU SYSTEME
C| AD             |<->| MATRICE A MULTIPLIEE PAR D.
C| AG             |<->| A X (GRADIENT DE DESCENTE).
C| AHPK           |---| 
C| B             |-->| SECOND MEMBRE DU SYSTEME.
C| CFG            |---| 
C| D             |<->| DIRECTION DE DESCENTE.
C| G             |<->| GRADIENT DE DESCENTE.
C| G0             |---| 
C| H             |---| 
C| INFOGR         |-->| SI OUI, ON IMPRIME UN COMPTE-RENDU
C| K             |---| 
C| MESH           |---| 
C| P             |---| 
C| X             |<--| VALEUR INITIALE, PUIS SOLUTION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CGSQUA => CGSQUA
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: X,G,G0,P,K,H,AHPK
      TYPE(BIEF_OBJ)  , INTENT(IN   ) :: B
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: A
      TYPE(SLVCFG)    , INTENT(INOUT) :: CFG
      LOGICAL         , INTENT(IN)    :: INFOGR
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION XL,RO,TESTL,RL,GMP1G0,BETA,GMG0,C
C
      INTEGER M
C
      LOGICAL RELAT
C
      INTRINSIC SQRT
C
C-----------------------------------------------------------------------
C
C COMPUTES THE NORM OF THE SECOND MEMBER
C
      XL = P_DOTS(B,B,MESH)
C
      IF( XL.LT.1.D0) THEN
            XL = 1.D0
            RELAT = .FALSE.
        ELSE
            RELAT = .TRUE.
      ENDIF
C
      M = 0
C
C INITIALISES G  : A X0 - B
C
      CALL MATRBL( 'X=AY    ',G,A,X,C,  MESH)
C
      CALL OS( 'X=X-Y   ' , G , B , B , C )
C
C CHECKS THAT THE ACCURACY HAS NOT ALREADY BEEN REACHED
C
      RL   = P_DOTS(G,G,MESH)
C
      IF(RL.LT.CFG%EPS**2*XL) THEN
        TESTL = SQRT(RL/XL)
        IF (INFOGR) THEN
          IF(RELAT) THEN
            IF (LNG.EQ.1) WRITE(LU,100) M,TESTL
            IF (LNG.EQ.2) WRITE(LU,101) M,TESTL
          ELSE
            IF (LNG.EQ.1) WRITE(LU,200) M,TESTL
            IF (LNG.EQ.2) WRITE(LU,201) M,TESTL
          ENDIF
        ENDIF
        GOTO 1000
      ENDIF
C
C INITIALISES G0 , P , AND K
C
      CALL OS('X=Y     ' , G0 , G , G , C )
      CALL OS('X=Y     ' , P  , G , G , C )
      CALL OS('X=Y     ' , K  , G , G , C )
C
      M = 1
20    CONTINUE
C
C COMPUTES AP (IN H, RECOMPUTED LATER)
C
      CALL MATRBL( 'X=AY    ',H,A,P,C,  MESH)
C
C COMPUTES RO
C
      RO = P_DOTS(K,G0,MESH) / P_DOTS(H,G0,MESH)
C
C COMPUTES H+K (IN H, AP ALREADY BEING IN H)
C
      CALL OS( 'X=CX    ' , H , H , H , -RO   )
      CALL OS( 'X=X+CY  ' , H , K , K , 2.D0  )
C
C            M+1   M       M       M
C COMPUTES  X   = X  -   RO * (H+K)     (H+K IN H)
C
      CALL OS( 'X=X+CY  ' , X , H , H , -RO )
C
C COMPUTES A(H+K)  (HERE H+K IN H, A(H+K) IN AHPK)
C
      CALL MATRBL( 'X=AY    ',AHPK,A,H,C,  MESH)
C
C             M     0
C COMPUTES ( G   , G  )
C
      GMG0 = P_DOTS(G,G0,MESH)
C
C COMPUTES GM
C
      CALL OS( 'X=X+CY  ' , G , AHPK , AHPK , -RO )
C
      RL   = P_DOTS(G,G,MESH)
      IF (RL.GT.CFG%EPS**2*XL) THEN
         IF (M.GE.CFG%NITMAX) THEN
C          IF(INFOGR) THEN
                TESTL=SQRT(RL/XL)
                IF(RELAT) THEN
                  IF (LNG.EQ.1) WRITE(LU,102) M,TESTL
                  IF (LNG.EQ.2) WRITE(LU,103) M,TESTL
                ELSE
                  IF (LNG.EQ.1) WRITE(LU,202) M,TESTL
                  IF (LNG.EQ.2) WRITE(LU,203) M,TESTL
                ENDIF
C          ENDIF
           GOTO 1000
         ELSE
           M = M + 1
         ENDIF
      ELSE
         IF(INFOGR) THEN
           TESTL=SQRT(RL/XL)
           IF(RELAT) THEN
             IF (LNG.EQ.1) WRITE(LU,100) M,TESTL
             IF (LNG.EQ.2) WRITE(LU,101) M,TESTL
           ELSE
             IF (LNG.EQ.1) WRITE(LU,200) M,TESTL
             IF (LNG.EQ.2) WRITE(LU,201) M,TESTL
           ENDIF
         ENDIF
         GOTO 1000
      ENDIF
C
C             M+1   0
C COMPUTES ( G   , G  )
C
      GMP1G0 = P_DOTS(G,G0,MESH)
C
C COMPUTES BETA
C
      BETA = GMP1G0 / GMG0
C
C           M
C COMPUTES H   (H+K IN H)
C
      CALL OS('X=X-Y   ' , H , K , K , C )
C
C           M+1
C COMPUTES P
C
      CALL OS('X=CX    ' , P , P , P , BETA**2 )
      CALL OS('X=X+Y   ' , P , G , G , C       )
      CALL OS('X=X+CY  ' , P , H , H , 2*BETA  )
C
C           M+1
C COMPUTES K
C
      CALL OS('X=Y     ' , K , G , G , C    )
      CALL OS('X=X+CY  ' , K , H , H , BETA )
C
      GOTO 20
C
1000  RETURN
C
C-----------------------------------------------------------------------
C
C   FORMATS
C
100   FORMAT(1X,'CGSQUA (BIEF) : ',1I8,' ITERATIONS',
     &          ' PRECISION RELATIVE: ',G16.7)
101   FORMAT(1X,'CGSQUA (BIEF) : ',1I8,' ITERATIONS',
     &          ' RELATIVE PRECISION: ',G16.7)
200   FORMAT(1X,'CGSQUA (BIEF) : ',1I8,' ITERATIONS',
     &          ' PRECISION ABSOLUE: ',G16.7)
201   FORMAT(1X,'CGSQUA (BIEF) : ',1I8,' ITERATIONS',
     &          ' ABSOLUTE PRECISION: ',G16.7)
102   FORMAT(1X,'CGSQUA (BIEF) : MAX D''ITERATIONS ATTEINT ',1I8,
     &          ' PRECISION RELATIVE: ',G16.7)
103   FORMAT(1X,'CGSQUA (BIEF) : EXCEEDING MAXIMUM ITERATIONS ',1I8,
     &          ' RELATIVE PRECISION: ',G16.7)
202   FORMAT(1X,'CGSQUA (BIEF) : MAX D''ITERATIONS ATTEINT ',1I8,
     &          ' PRECISION ABSOLUE: ',G16.7)
203   FORMAT(1X,'CGSQUA (BIEF) : EXCEEDING MAXIMUM ITERATIONS ',1I8,
     &          ' ABSOLUTE PRECISION:',G16.7)
C
C-----------------------------------------------------------------------
C
      END


C
C#######################################################################
C