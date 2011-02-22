C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE LINEAR SYSTEM A X = B
!>                USING THE SQUARED CONJUGATE GRADIENT METHOD.
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
!>        5            I  DIAGONAL PRECONDITIONING BUT ACCEPTS 0 OR
!>                     I  NEGATIVE VALUES ON THE DIAGONAL
!>        7            I  CROUT
!>-----------------------------------------------------------------------
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, AUX, B, CFG, INFOGR, MESH, P, Q, R, S, T, V, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ALFA, ALFA1, BETA, BETA1, C, CROUT, M, OMEG, OMEG1, OMEG2, RELAT, RMRM, TESTL, XL
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CGSTAB
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DOWNUP(), MATRBL(), OS(), PARMOY(), P_DOTS()
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
!> </td><td> R RATKE (HANNOVER); A MALCHEREK (HANNOVER); J-M HERVOUET (LNH) 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
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
!>          <tr><td>AUX
!></td><td>--></td><td>MATRICE DE PRECONDITIONNEMENT.
!>    </td></tr>
!>          <tr><td>B
!></td><td>--></td><td>SECOND MEMBRE DU SYSTEME.
!>    </td></tr>
!>          <tr><td>CFG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INFOGR
!></td><td>--></td><td>SI OUI, ON IMPRIME UN COMPTE-RENDU
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>P,Q,R,S,T,V
!></td><td><-></td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VALEUR INITIALE, PUIS SOLUTION
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CGSTAB
     &(X, A,B , MESH, P,Q,R,S,T,V, CFG,INFOGR,AUX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |-->| MATRICE DU SYSTEME
C| AUX            |-->| MATRICE DE PRECONDITIONNEMENT.
C| B             |-->| SECOND MEMBRE DU SYSTEME.
C| CFG            |---| 
C| INFOGR         |-->| SI OUI, ON IMPRIME UN COMPTE-RENDU
C| MESH           |---| 
C| P,Q,R,S,T,V    |<->| TABLEAUX DE TRAVAIL
C| X             |<--| VALEUR INITIALE, PUIS SOLUTION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_CGSTAB => CGSTAB
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: X,P,Q,R,S,T,V
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: AUX,A,B
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      TYPE(SLVCFG)    , INTENT(INOUT) :: CFG
      LOGICAL         , INTENT(IN)    :: INFOGR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION ALFA,ALFA1,BETA,BETA1,OMEG,OMEG1,OMEG2
      DOUBLE PRECISION XL,TESTL,RMRM,C
C
      INTEGER M
C
      LOGICAL RELAT,CROUT
C
      INTRINSIC SQRT
C
C-----------------------------------------------------------------------
C
C   INITIALISES
      CROUT=.FALSE.
      IF(7*(CFG%PRECON/7).EQ.CFG%PRECON) CROUT=.TRUE.
C
C-----------------------------------------------------------------------
C   INITIALISES
C-----------------------------------------------------------------------
C
      M   = 0
C
C  NORM OF THE SECOND MEMBER TO COMPUTE THE RELATIVE ACCURACY:
C
      XL = P_DOTS(B,B,MESH)
      IF (XL.LT.1.D0) THEN
         XL = 1.D0
         RELAT = .FALSE.
      ELSE
         RELAT = .TRUE.
      ENDIF
C
C IF THE SECOND MEMBER IS 0, X=0 AND IT'S THE END
C
      IF(XL.LT.CFG%ZERO**2) THEN
        RMRM = 0.D0
        CALL OS( 'X=C     ' , X , X , X , 0.D0 )
        GOTO 900
      ENDIF
C
C COMPUTES THE INITIAL RESIDUAL AND EXITS IF RESIDUAL IS SMALL:
C
      CALL MATRBL( 'X=AY    ',V,A,X,C,  MESH)
C
      CALL OS( 'X=Y-Z   ' , R , B , V , C )
      RMRM   = P_DOTS(R,R,MESH)
C
      IF (RMRM.LT.CFG%EPS**2*XL) GO TO 900
C
C-----------------------------------------------------------------------
C PRECONDITIONING :
C-----------------------------------------------------------------------
C
      IF(CROUT) THEN
C       COMPUTES C R  = B
        CALL DOWNUP(R, AUX , B , 'D' , MESH)
        IF(NCSIZE.GT.1) CALL PARMOY(R,MESH)
      ELSE
        CALL OS( 'X=Y     ' , R , B , B , C )
      ENDIF
C
C-----------------------------------------------------------------------
C RESUMES INITIALISATIONS
C-----------------------------------------------------------------------
C
      IF(CROUT) THEN
        CALL DOWNUP(V, AUX , V , 'D' , MESH)
        IF(NCSIZE.GT.1) CALL PARMOY(V,MESH)
      ENDIF
C
      CALL OS( 'X=X-Y   ' , R , V , V , C    )
      CALL OS( 'X=Y     ' , P , R , R , C    )
      CALL OS( 'X=C     ' , V , V , V , 0.D0 )
      CALL OS( 'X=C     ' , Q , Q , Q , 0.D0 )
C
      ALFA  = 1.D0
      BETA  = 1.D0
      OMEG1 = 1.D0
C
C-----------------------------------------------------------------------
C  ITERATIONS:
C-----------------------------------------------------------------------
C
2     M  = M  + 1
C
      BETA1 = P_DOTS(R,P,MESH)
      OMEG2 = OMEG1*BETA1/BETA
      OMEG  = OMEG2/ALFA
      BETA  = BETA1
C
      CALL OS( 'X=Y+CZ  ' , Q , R    , Q ,  OMEG )
      CALL OS( 'X=X+CY  ' , Q , V    , V , -OMEG2)
C
      CALL MATRBL( 'X=AY    ',V,A,Q,C,  MESH)
C
      IF(CROUT) THEN
        CALL DOWNUP(V, AUX , V , 'D' , MESH)
        IF(NCSIZE.GT.1) CALL PARMOY(V,MESH)
      ENDIF
C
      OMEG1 = P_DOTS(P,V,MESH)
      OMEG1 = BETA1/OMEG1
C
      CALL OS( 'X=Y+CZ  ' , S , R    , V , -OMEG1)
C
      CALL MATRBL( 'X=AY    ',T,A,S,C,  MESH)
C
      IF(CROUT) THEN
        CALL DOWNUP(T, AUX , T , 'D' , MESH)
        IF(NCSIZE.GT.1) CALL PARMOY(T,MESH)
      ENDIF
C
      ALFA  = P_DOTS(T,S,MESH)
      ALFA1 = P_DOTS(T,T,MESH)
      ALFA  = ALFA/ALFA1
C
      CALL OS( 'X=X+CY  ' , X , Q , Q ,  OMEG1)
      CALL OS( 'X=X+CY  ' , X , S , S ,  ALFA )
C
      CALL OS( 'X=Y+CZ  ' , R , S , T , -ALFA )
C
      RMRM   = P_DOTS(R,R,MESH)
C
C TESTS CONVERGENCE :
C
      IF (RMRM.LE.XL*CFG%EPS**2) GO TO 900
C
      IF(M.LT.CFG%NITMAX) GO TO 2
C
C-----------------------------------------------------------------------
C
C     IF(INFOGR) THEN
        TESTL = SQRT( RMRM / XL )
        IF(RELAT) THEN
          IF (LNG.EQ.1) WRITE(LU,102) M,TESTL
          IF (LNG.EQ.2) WRITE(LU,104) M,TESTL
        ELSE
          IF (LNG.EQ.1) WRITE(LU,202) M,TESTL
          IF (LNG.EQ.2) WRITE(LU,204) M,TESTL
        ENDIF
C     ENDIF
      GO TO 1000
C
C-----------------------------------------------------------------------
C
900   CONTINUE
C
      IF(INFOGR) THEN
        TESTL = SQRT( RMRM / XL )
        IF(RELAT) THEN
          IF (LNG.EQ.1) WRITE(LU,101) M,TESTL
          IF (LNG.EQ.2) WRITE(LU,103) M,TESTL
        ELSE
          IF (LNG.EQ.1) WRITE(LU,201) M,TESTL
          IF (LNG.EQ.2) WRITE(LU,203) M,TESTL
        ENDIF
      ENDIF
C
1000  RETURN
C
C-----------------------------------------------------------------------
C
C   FORMATS
C
101   FORMAT(1X,'CGSTAB (BIEF) : ',1I8,' ITERATIONS',
     &          ' PRECISION RELATIVE: ',G16.7)
103   FORMAT(1X,'CGSTAB (BIEF) : ',1I8,' ITERATIONS',
     &          ' RELATIVE PRECISION: ',G16.7)
201   FORMAT(1X,'CGSTAB (BIEF) : ',1I8,' ITERATIONS',
     &          ' PRECISION ABSOLUE: ',G16.7)
203   FORMAT(1X,'CGSTAB (BIEF) : ',1I8,' ITERATIONS',
     &          ' ABSOLUTE PRECISION: ',G16.7)
102   FORMAT(1X,'CGSTAB (BIEF) : MAX D''ITERATIONS ATTEINT ',1I8,
     &          ' PRECISION RELATIVE: ',G16.7)
104   FORMAT(1X,'CGSTAB (BIEF) : EXCEEDING MAXIMUM ITERATIONS ',1I8,
     &          ' RELATIVE PRECISION: ',G16.7)
202   FORMAT(1X,'CGSTAB (BIEF) : MAX D''ITERATIONS ATTEINT ',1I8,
     &          ' PRECISION ABSOLUE: ',G16.7)
204   FORMAT(1X,'CGSTAB (BIEF) : EXCEEDING MAXIMUM ITERATIONS',1I8,
     &          ' ABSOLUTE PRECISION:',G16.7)
C
C-----------------------------------------------------------------------
C
      END


C
C#######################################################################
C