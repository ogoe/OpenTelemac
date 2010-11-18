C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE LINEAR SYSTEM A X = B
!>                USING THE CONJUGATE GRADIENT METHOD.
!>  @code
!>-----------------------------------------------------------------------
!>                        PRECONDITIONING
!>-----------------------------------------------------------------------
!>   CFG%PRECON VALUE  I                  MEANING
!>-----------------------------------------------------------------------
!>                     I
!>        0 OR 1       I  NO PRECONDITIONING
!>                     I
!>        2            I  DIAGONAL PRECONDITIONING USING THE MATRIX
!>                     I  DIAGONAL
!>        3            I  BLOCK-DIAGONAL PRECONDITIONING
!>                     I
!>        5            I  DIAGONAL PRECONDITIONING USING THE ABSOLUTE
!>                     I  VALUE OF THE MATRIX DIAGONAL
!>                     I
!>        7            I  CROUT EBE PRECONDITIONING
!>                     I
!>       11            I  GAUSS-SEIDEL EBE PRECONDITIONING
!>                     I
!>-----------------------------------------------------------------------
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, AD, AUX, B, CFG, D, G, INFOGR, MESH, R, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BETA, C, CROUT, DAD, GSEB, M, PRE3D, PREBE, PREC, RELAT, RM1GM1, RMDM, RMGM, RMIN, RMRM, RO, STO1, TESTL, XL
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_GRACJG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), DOWNUP(), MATRBL(), NBPTS(), OS(), PARMOY(), P_DOTS(), TRID3D()
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
!>      <td><center> 5.9                                       </center>
!> </td><td> 06/10/08
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
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
!>          <tr><td>AUX
!></td><td>--></td><td>MATRICE POUR LE PRECONDITIONNEMENT.
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
!>          <tr><td>INFOGR
!></td><td>--></td><td>SI OUI, IMPRESSION D'UN COMPTE-RENDU.
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES ENTIERS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>R
!></td><td><-></td><td>RESIDU (CONFONDU AVEC LE GRADIENT SI IL N'Y A
!>                  PAS DE PRECONDITIONNEMENT DANS GRACJG)
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VALEUR INITIALE, PUIS SOLUTION
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE GRACJG
     &(X, A,B , MESH, D,AD,G,R, CFG,INFOGR,AUX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |-->| MATRICE DU SYSTEME
C| AD             |<->| MATRICE A MULTIPLIEE PAR D.
C| AUX            |-->| MATRICE POUR LE PRECONDITIONNEMENT.
C| B             |-->| SECOND MEMBRE DU SYSTEME.
C| CFG            |---| 
C| D             |<->| DIRECTION DE DESCENTE.
C| G             |<->| GRADIENT DE DESCENTE.
C| INFOGR         |-->| SI OUI, IMPRESSION D'UN COMPTE-RENDU.
C| MESH           |-->| BLOC DES ENTIERS DU MAILLAGE.
C| R             |<->| RESIDU (CONFONDU AVEC LE GRADIENT SI IL N'Y A
C|                |   | PAS DE PRECONDITIONNEMENT DANS GRACJG)
C| X             |<--| VALEUR INITIALE, PUIS SOLUTION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_GRACJG => GRACJG
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(SLVCFG), INTENT(INOUT)    :: CFG
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: B
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: D,AD,G,R,X
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)     :: A
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: AUX
      LOGICAL, INTENT(IN)            :: INFOGR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER M
C
      DOUBLE PRECISION XL,RMRM,RMDM,RMGM,TESTL
      DOUBLE PRECISION BETA,RO,DAD,RM1GM1,STO1,C
C
      LOGICAL RELAT,PREC,CROUT,GSEB,PRE3D,PREBE
C
      DOUBLE PRECISION RMIN
      DATA RMIN/1.D-15/
C
C-----------------------------------------------------------------------
C
      INTRINSIC SQRT
C
C-----------------------------------------------------------------------
C
C   INITIALISES
C
      STO1 = 0.D0
      CROUT =.FALSE.
      IF(07*(CFG%PRECON/07).EQ.CFG%PRECON) CROUT=.TRUE.
      GSEB=.FALSE.
      IF(11*(CFG%PRECON/11).EQ.CFG%PRECON) GSEB=.TRUE.
      PREBE=.FALSE.
      IF(13*(CFG%PRECON/13).EQ.CFG%PRECON) PREBE=.TRUE.
      PRE3D=.FALSE.
      IF(17*(CFG%PRECON/17).EQ.CFG%PRECON) PRE3D=.TRUE.
      PREC=.FALSE.
      IF(CROUT.OR.GSEB.OR.PREBE.OR.PRE3D) PREC=.TRUE.
C
C-----------------------------------------------------------------------
C   INITIALISES
C-----------------------------------------------------------------------
C
      M   = 0
C
C  NORMALISES THE SECOND MEMBER TO COMPUTE THE RELATIVE PRECISION:
C
      XL = P_DOTS(B,B,MESH)
C
C     OLIVIER BOITEAU'S (SINETICS) TEST : SECOND MEMBER TOO SMALL
C                                         ==> UNKNOWN = 0
C
      TESTL=SQRT(XL)
      IF(TESTL.LT.RMIN) THEN
        CALL OS('X=0     ',X=X)
        IF(INFOGR) THEN
          IF(LNG.EQ.1) WRITE(LU,105) TESTL
          IF(LNG.EQ.2) WRITE(LU,106) TESTL
        ENDIF
        GOTO 1000
      ENDIF
C
      IF(XL.LT.1.D0) THEN
         XL = 1.D0
         RELAT = .FALSE.
      ELSE
         RELAT = .TRUE.
      ENDIF
C
C COMPUTES THE INITIAL RESIDUAL AND POSSIBLY EXITS:
C
      CALL MATRBL( 'X=AY    ',R,A,X,  C,MESH)
C
      CALL OS( 'X=X-Y   ' , R , B , B , C )
      RMRM   = P_DOTS(R,R,MESH)
      RMGM   = RMRM
C
      IF (RMRM.LT.CFG%EPS**2*XL) GO TO 900
C
C-----------------------------------------------------------------------
C PRECONDITIONING :
C-----------------------------------------------------------------------
C
      IF(PREC) THEN
C       COMPUTES C G0 = R0
        IF(CROUT.OR.GSEB.OR.PREBE) THEN
          CALL DOWNUP( G, AUX , R , 'D' , MESH )
          IF(NCSIZE.GT.1) CALL PARMOY(G,MESH)
        ELSEIF(PRE3D) THEN
          CALL CPSTVC(R%ADR(1)%P,G%ADR(1)%P)
          CALL TRID3D(AUX%X%R,G%ADR(1)%P%R,R%ADR(1)%P%R,
     &                MESH%NPOIN,NBPTS(11))
        ENDIF
C       COMPUTES RMGM AND STORES
        RMGM = P_DOTS(R,G,MESH)
        STO1 = RMGM
      ENDIF
C
C-----------------------------------------------------------------------
C COMPUTES THE DIRECTION OF INITIAL DESCENT:
C-----------------------------------------------------------------------
C
      CALL OS( 'X=Y     ' , D , G , G , C )
C
C-----------------------------------------------------------------------
C COMPUTES THE INITIAL PRODUCT A D :
C-----------------------------------------------------------------------
C
      CALL MATRBL( 'X=AY    ',AD,A,D,C,MESH)
C
C-----------------------------------------------------------------------
C COMPUTES INITIAL RO :
C-----------------------------------------------------------------------
C
      DAD = P_DOTS(D,AD,MESH)
      RO = RMGM / DAD
C     RO = RMDM / DAD  (USES RMGM BECAUSE HERE D0=G0)
C
C-----------------------------------------------------------------------
C
C COMPUTES X1 = X0 - RO  * D
C
      CALL OS('X=X+CY  ',X=X,Y=D,C=-RO)
C
C-----------------------------------------------------------------------
C  ITERATIONS LOOP:
C-----------------------------------------------------------------------
C
2     M  = M  + 1
C
C-----------------------------------------------------------------------
C COMPUTES THE RESIDUAL : R(M) = R(M-1) - RO(M-1) A D(M-1)
C-----------------------------------------------------------------------
C
      CALL OS('X=X+CY  ',X=R,Y=AD,C=-RO)
C
C  SOME VALUES WILL CHANGE IN CASE OF PRECONDITIONING
C
      RM1GM1 = RMGM
      RMRM   = P_DOTS(R,R,MESH)
      RMDM   = RMRM
      RMGM   = RMRM
C
C CHECKS END:
C
      IF(RMRM.LE.XL*CFG%EPS**2) GO TO 900
C
C-----------------------------------------------------------------------
C PRECONDITIONING : SOLVES C G = R
C-----------------------------------------------------------------------
C
      IF(PREC) THEN
C
C       SOLVES C G = R
        IF(CROUT.OR.GSEB.OR.PREBE) THEN
          CALL DOWNUP( G, AUX , R , 'D' , MESH )
          IF(NCSIZE.GT.1) CALL PARMOY(G,MESH)
        ELSEIF(PRE3D) THEN
          CALL CPSTVC(R%ADR(1)%P,G%ADR(1)%P)
          CALL TRID3D(AUX%X%R,G%ADR(1)%P%R,R%ADR(1)%P%R,
     &                MESH%NPOIN,NBPTS(11))
        ENDIF
C       COMPUTES RMGM AND RM1GM1
        RM1GM1 = STO1
        RMGM = P_DOTS(R,G,MESH)
        STO1=RMGM
C
      ENDIF
C
C-----------------------------------------------------------------------
C COMPUTES D BY RECURRENCE:
C-----------------------------------------------------------------------
C
      BETA = RMGM / RM1GM1
C     CALL OS( 'X=CX    ' , D , D , D , BETA )
C     CALL OS( 'X=X+Y   ' , D , G , G , C    )
C     OPTIMISED EQUIVALENT OF THE 2 OS CALLS:
      CALL OS( 'X=Y+CZ  ' , X=D , Y=G , Z=D , C=BETA )
C
C-----------------------------------------------------------------------
C PRECONDITIONING :
C-----------------------------------------------------------------------
C
      IF(PREC) THEN
C       COMPUTES RMDM
        RMDM=P_DOTS(R,D,MESH)
      ENDIF
C
C-----------------------------------------------------------------------
C COMPUTES A D :
C-----------------------------------------------------------------------
C
      CALL MATRBL( 'X=AY    ',AD,A,D,C,MESH)
C
C-----------------------------------------------------------------------
C COMPUTES RO
C-----------------------------------------------------------------------
C
      DAD = P_DOTS(D,AD,MESH)
      RO = RMDM/DAD
C
C COMPUTES X(M) = X(M-1) - RO * D
C
      CALL OS('X=X+CY  ',X=X,Y=D,C=-RO)
C
      IF(M.LT.CFG%NITMAX) GO TO 2
C
C-----------------------------------------------------------------------
C
C     IF(INFOGR) THEN
        TESTL = SQRT( RMRM / XL )
        IF (RELAT) THEN
           IF (LNG.EQ.1) WRITE(LU,103) M,TESTL
           IF (LNG.EQ.2) WRITE(LU,104) M,TESTL
        ELSE
           IF (LNG.EQ.1) WRITE(LU,203) M,TESTL
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
        IF (RELAT) THEN
           IF (LNG.EQ.1) WRITE(LU,101) M,TESTL
           IF (LNG.EQ.2) WRITE(LU,102) M,TESTL
        ELSE
           IF (LNG.EQ.1) WRITE(LU,201) M,TESTL
           IF (LNG.EQ.2) WRITE(LU,202) M,TESTL
        ENDIF
      ENDIF
C
1000  CONTINUE
      RETURN
C
C-----------------------------------------------------------------------
C
C   FORMATS
C
101   FORMAT(1X,'GRACJG (BIEF) : ',
     &                     1I8,' ITERATIONS, PRECISION RELATIVE:',G16.7)
102   FORMAT(1X,'GRACJG (BIEF) : ',
     &                     1I8,' ITERATIONS, RELATIVE PRECISION:',G16.7)
201   FORMAT(1X,'GRACJG (BIEF) : ',
     &                     1I8,' ITERATIONS, PRECISION ABSOLUE :',G16.7)
202   FORMAT(1X,'GRACJG (BIEF) : ',
     &                     1I8,' ITERATIONS, ABSOLUTE PRECISION:',G16.7)
103   FORMAT(1X,'GRACJG (BIEF) : MAX D''ITERATIONS ATTEINT:',
     &                     1I8,' PRECISION RELATIVE:',G16.7)
104   FORMAT(1X,'GRACJG (BIEF) : EXCEEDING MAXIMUM ITERATIONS:',
     &                     1I8,' RELATIVE PRECISION:',G16.7)
105   FORMAT(1X,'GRACJG (BIEF) : ',
     &         ' SOLUTION X=0 CAR NORME L2 DE B QUASI-NULLE:',G16.7)
106   FORMAT(1X,'GRACJG (BIEF) : ',
     &        ' SOLUTION X=0 BECAUSE L2-NORM OF B VERY SMALL:',G16.7)
203   FORMAT(1X,'GRACJG (BIEF) : MAX D''ITERATIONS ATTEINT:',
     &                     1I8,' PRECISION ABSOLUE :',G16.7)
204   FORMAT(1X,'GRACJG (BIEF) : EXCEEDING MAXIMUM ITERATIONS:',
     &                     1I8,' ABSOLUTE PRECISION:',G16.7)
C
C-----------------------------------------------------------------------
C
      END
C
C#######################################################################
C