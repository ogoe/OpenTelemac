C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE LINEAR SYSTEM A X = B
!>                USING THE CONJUGATE RESIDUAL METHOD.
!>  @code
!>-----------------------------------------------------------------------
!>                        PRECONDITIONING
!>-----------------------------------------------------------------------
!>    PRECON VALUE     I                  MEANING
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
!>    </th><td> A, AD, AG, AUX, B, CFG, D, G, INFOGR, MESH, R, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ADAD, AGAD, BETA, C, CROUT, DAD, GAD, GM1GM1, GMGM, GSEB, M, PRE3D, PREBE, PREC, RELAT, RM1GM1, RMDM, RMGM, RMRM, RO, STO1, STO2, TESTL, TGMTGM, XL
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_RESCJG
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
!>      <td><center> 5.6                                       </center>
!> </td><td> 27/02/06
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
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
!>                  PAS DE PRECONDITIONNEMENT DANS SOLGRA)
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VALEUR INITIALE, PUIS SOLUTION
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE RESCJG
     &(X, A,B , MESH,D,AD,AG,G,R, CFG,INFOGR,AUX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |-->| MATRICE DU SYSTEME
C| AD             |<->| MATRICE A MULTIPLIEE PAR D.
C| AG             |<->| A X (GRADIENT DE DESCENTE).
C| AUX            |-->| MATRICE POUR LE PRECONDITIONNEMENT.
C| B             |-->| SECOND MEMBRE DU SYSTEME.
C| CFG            |---| 
C| D             |<->| DIRECTION DE DESCENTE.
C| G             |<->| GRADIENT DE DESCENTE.
C| INFOGR         |-->| SI OUI, IMPRESSION D'UN COMPTE-RENDU.
C| MESH           |-->| BLOC DES ENTIERS DU MAILLAGE.
C| R             |<->| RESIDU (CONFONDU AVEC LE GRADIENT SI IL N'Y A
C|                |   | PAS DE PRECONDITIONNEMENT DANS SOLGRA)
C| X             |<--| VALEUR INITIALE, PUIS SOLUTION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_RESCJG => RESCJG
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL, INTENT(IN) :: INFOGR
C
C     STRUCTURES
C
      TYPE(BIEF_OBJ), INTENT(INOUT) :: D,AD,G,AG,R,X,B
      TYPE(SLVCFG)  , INTENT(INOUT) :: CFG
C
C     MESH STRUCTURE
C
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C     MATRIX STRUCTURE
C
      TYPE(BIEF_OBJ), INTENT(IN)    :: A
      TYPE(BIEF_OBJ), INTENT(INOUT) :: AUX
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER M
C
      DOUBLE PRECISION XL,RMRM,RMDM,RMGM,TESTL,GAD
      DOUBLE PRECISION AGAD,BETA,ADAD,RO,DAD,RM1GM1,GMGM,GM1GM1,STO1
      DOUBLE PRECISION STO2,TGMTGM,C
C
      LOGICAL RELAT,PREC,CROUT,GSEB,PREBE,PRE3D
C
C-----------------------------------------------------------------------
C
      INTRINSIC SQRT
C
C-----------------------------------------------------------------------
C
C   INITIALISES
C     STO1 AND STO2 AVOID A WARNING MESSAGE FROM CRAY COMPILER
      STO1  =0.D0
      STO2  =0.D0
      TGMTGM=0.D0
      CROUT =.FALSE.
      IF(7*(CFG%PRECON/7).EQ.CFG%PRECON) CROUT=.TRUE.
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
      IF (XL.LT.1.D0) THEN
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
      GMGM   = RMRM
C
      IF (RMRM.LT.CFG%EPS**2*XL) GO TO 900
C
C-----------------------------------------------------------------------
C PRECONDITIONING :
C-----------------------------------------------------------------------
C
      IF(PREC) THEN
C
C COMPUTES C G0 = R
C
        IF(CROUT.OR.GSEB.OR.PREBE) THEN
          CALL DOWNUP(G, AUX , R , 'D' , MESH)
          IF(NCSIZE.GT.1) CALL PARMOY(G,MESH)
        ELSEIF(PRE3D) THEN
          CALL CPSTVC(R%ADR(1)%P,G%ADR(1)%P)
          CALL TRID3D(AUX%X%R,G%ADR(1)%P%R,R%ADR(1)%P%R,
     &                MESH%NPOIN,BIEF_NBPTS(11,MESH))
        ENDIF
C
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
      CALL MATRBL( 'X=AY    ',AD,A,D,  C,MESH)
C
C-----------------------------------------------------------------------
C
      IF(PREC) THEN
C
C   COMPUTES  C DPRIM = AD  (DPRIM PUT IN B)
C
        IF(CROUT.OR.GSEB.OR.PREBE) THEN
          CALL DOWNUP(B, AUX , AD , 'D' , MESH)
          IF(NCSIZE.GT.1) CALL PARMOY(B,MESH)
        ELSEIF(PRE3D) THEN
          CALL CPSTVC(R%ADR(1)%P,G%ADR(1)%P)
          CALL TRID3D(AUX%X%R,B%ADR(1)%P%R,AD%ADR(1)%P%R,
     &                MESH%NPOIN,BIEF_NBPTS(11,MESH))
        ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C COMPUTES INITIAL RO :
C-----------------------------------------------------------------------
C
      DAD = P_DOTS(D,AD,MESH)
      IF(PREC) THEN
       ADAD = P_DOTS(AD,B,MESH)
      ELSE
       ADAD = P_DOTS(AD,AD,MESH)
      ENDIF
      RO = DAD/ADAD
C
C-----------------------------------------------------------------------
C
C COMPUTES X1 = X0 - RO  * D
C
      CALL OS( 'X=X+CY  ' , X , D , D , -RO )
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
      CALL OS( 'X=X+CY  ' , R , AD , AD , -RO )
C
C  SOME VALUES WILL CHANGE IN CASE OF PRECONDITIONING
C
      GM1GM1 = GMGM
      RM1GM1 = RMGM
      RMRM   = P_DOTS(R,R,MESH)
      RMDM   = RMRM
      RMGM   = RMRM
      GMGM   = RMRM
C
C CHECKS END:
C
      IF (RMRM.LE.XL*CFG%EPS**2) GO TO 900
C
C-----------------------------------------------------------------------
C PRECONDITIONING : SOLVES C G = R
C-----------------------------------------------------------------------
C
      IF(PREC) THEN
C       UPDATES G BY RECURRENCE (IN B: DPRIM)
        CALL OS( 'X=X+CY  ' , G , B , B , -RO )
      ENDIF
C
C-----------------------------------------------------------------------
C COMPUTES AG :
C-----------------------------------------------------------------------
C
      CALL MATRBL( 'X=AY    ',AG,A,G,  C,MESH)
C
C-----------------------------------------------------------------------
C COMPUTES D BY RECURRENCE:
C-----------------------------------------------------------------------
C
      IF(PREC) THEN
        AGAD = P_DOTS(AG,B,MESH)
      ELSE
        AGAD = P_DOTS(AG,AD,MESH)
      ENDIF
      BETA = - AGAD / ADAD
C
      CALL OS( 'X=CX    ' , D , D , D , BETA )
      CALL OS( 'X=X+Y   ' , D , G , G , C    )
C
C-----------------------------------------------------------------------
C COMPUTES A D :
C-----------------------------------------------------------------------
C
      CALL OS( 'X=CX    ' , AD , AD , AD , BETA )
      CALL OS( 'X=X+Y   ' , AD , AG , AG  , C   )
C
      IF(PREC) THEN
C
C   COMPUTES  C DPRIM = AD  (DPRIM PUT IN B)
C
        IF(CROUT.OR.GSEB.OR.PREBE) THEN
          CALL DOWNUP(B , AUX , AD , 'D' , MESH)
          IF(NCSIZE.GT.1) CALL PARMOY(B,MESH)
        ELSEIF(PRE3D) THEN
          CALL TRID3D(AUX%X%R,B%ADR(1)%P%R,AD%ADR(1)%P%R,
     &                MESH%NPOIN,BIEF_NBPTS(11,MESH))
        ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C COMPUTES RO
C-----------------------------------------------------------------------
C
      GAD = P_DOTS(G,AD,MESH)
      IF(PREC) THEN
        ADAD = P_DOTS(AD,B,MESH)
      ELSE
        ADAD = P_DOTS(AD,AD,MESH)
      ENDIF
      RO = GAD/ADAD
C
C COMPUTES X(M) = X(M-1) - RO * D
C
      CALL OS( 'X=X+CY  ' , X , D , D , -RO )
C
      IF(M.LT.CFG%NITMAX) GO TO 2
C
C-----------------------------------------------------------------------
C
      IF(INFOGR) THEN
        TESTL = SQRT( RMRM / XL )
        IF (RELAT) THEN
           IF (LNG.EQ.1) WRITE(LU,103) M,TESTL
           IF (LNG.EQ.2) WRITE(LU,104) M,TESTL
        ELSE
           IF (LNG.EQ.1) WRITE(LU,203) M,TESTL
           IF (LNG.EQ.2) WRITE(LU,204) M,TESTL
        ENDIF
      ENDIF
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
1000  RETURN
C
C-----------------------------------------------------------------------
C
C   FORMATS
C
101   FORMAT(1X,'RESCJG (BIEF) : ',
     &                     1I8,' ITERATIONS, PRECISION RELATIVE:',G16.7)
102   FORMAT(1X,'RESCJG (BIEF) : ',
     &                     1I8,' ITERATIONS, RELATIVE PRECISION:',G16.7)
201   FORMAT(1X,'RESCJG (BIEF) : ',
     &                     1I8,' ITERATIONS, PRECISION ABSOLUE :',G16.7)
202   FORMAT(1X,'RESCJG (BIEF) : ',
     &                     1I8,' ITERATIONS, ABSOLUTE PRECISION:',G16.7)
103   FORMAT(1X,'RESCJG (BIEF) : MAX D'' ITERATIONS ATTEINT:',
     &                     1I8,' PRECISION RELATIVE:',G16.7)
104   FORMAT(1X,'RESCJG (BIEF) : EXCEEDING MAXIMUM ITERATIONS:',
     &                     1I8,' RELATIVE PRECISION:',G16.7)
203   FORMAT(1X,'RESCJG (BIEF) : MAX D'' ITERATIONS ATTEINT:',
     &                     1I8,' PRECISION ABSOLUE :',G16.7)
204   FORMAT(1X,'RESCJG (BIEF) : EXCEEDING MAXIMUM ITERATIONS:',
     &                     1I8,' ABSOLUTE PRECISON:',G16.7)
C
C-----------------------------------------------------------------------
C
      END
C
C#######################################################################
C
