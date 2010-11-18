C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE LINEAR SYSTEM A X = B
!>                USING METHODS OF THE TYPE CONJUGATE GRADIENT.
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
!>    </th><td> ADAD, BETA, C, CROUT, GSEB, M, PREC, RELAT, RMRM, RO, TESTL, TG1TG1, TGMTGM, XL
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_EQUNOR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DOWNUP(), MATRBL(), OS(), P_DOTS()
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
!> </td><td> 24/04/97
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
                        SUBROUTINE EQUNOR
     &(X, A,B , MESH, D,AD,AG,G,R, CFG,INFOGR,AUX)
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
      USE BIEF, EX_EQUNOR => EQUNOR
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(SLVCFG), INTENT(INOUT)    :: CFG
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: B
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: D,AD,G,AG,R,X
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)     :: A
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: AUX
      LOGICAL, INTENT(IN)            :: INFOGR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER M
C
      DOUBLE PRECISION XL,RMRM,TESTL
      DOUBLE PRECISION BETA,ADAD,RO
      DOUBLE PRECISION TGMTGM,TG1TG1,C
C
      LOGICAL RELAT,PREC,CROUT,GSEB
C
C-----------------------------------------------------------------------
C
      INTRINSIC SQRT
C
C-----------------------------------------------------------------------
C
C   INITIALISES
C
      CROUT =.FALSE.
      IF(7*(CFG%PRECON/7).EQ.CFG%PRECON) CROUT=.TRUE.
      GSEB=.FALSE.
      IF(11*(CFG%PRECON/11).EQ.CFG%PRECON) GSEB=.TRUE.
      PREC=.FALSE.
      IF(CROUT.OR.GSEB.OR.13*(CFG%PRECON/13).EQ.CFG%PRECON) PREC=.TRUE.
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
C
      IF (RMRM.LT.CFG%EPS**2*XL) GO TO 900
C
C-----------------------------------------------------------------------
C PRECONDITIONING :
C-----------------------------------------------------------------------
C
      IF(PREC) THEN
C
C       COMPUTES C G0 = R
        CALL DOWNUP(G, AUX , R , 'D' , MESH)
C
C  C IS HERE CONSIDERED SYMMETRICAL,
C  SHOULD OTHERWISE SOLVE TC GPRIM = G
C
C        T -1
C         C   G   IS PUT IN B
C
        CALL DOWNUP(B , AUX , G , 'T' , MESH)
C
      ENDIF
C
C-----------------------------------------------------------------------
C COMPUTES THE DIRECTION OF INITIAL DESCENT:
C-----------------------------------------------------------------------
C
      IF(PREC) THEN
        CALL MATRBL( 'X=TAY   ',D,A,B,  C,MESH)
      ELSE
        CALL MATRBL( 'X=TAY   ',D,A,G,  C,MESH)
      ENDIF
C
      TGMTGM = P_DOTS(D,D,MESH)
C
C-----------------------------------------------------------------------
C COMPUTES THE INITIAL PRODUCT A D:
C-----------------------------------------------------------------------
C
      CALL MATRBL('X=AY    ',AD,A,D,C,MESH)
C
C-----------------------------------------------------------------------
C
      IF(PREC) THEN
C
C         COMPUTES  C DPRIM = AD  (DPRIM PUT IN AG)
          CALL DOWNUP(AG, AUX , AD , 'D' , MESH)
C
      ENDIF
C
C-----------------------------------------------------------------------
C COMPUTES INITIAL RO :
C-----------------------------------------------------------------------
C
      IF(PREC) THEN
        ADAD = P_DOTS(AG,AG,MESH)
      ELSE
        ADAD = P_DOTS(AD,AD,MESH)
      ENDIF
      RO = TGMTGM/ADAD
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
      RMRM   = P_DOTS(R,R,MESH)
C
C CHECKS END :
C
      IF (RMRM.LE.XL*CFG%EPS**2) GO TO 900
C
C-----------------------------------------------------------------------
C PRECONDITIONING : SOLVES C G = R
C-----------------------------------------------------------------------
C
      IF(PREC) THEN
C
C         UPDATES G BY RECURRENCE (IN AG: DPRIM)
          CALL OS( 'X=X+CY  ' , G , AG , AG , -RO )
C
          CALL DOWNUP(B , AUX , G , 'T' , MESH)
C
      ENDIF
C
C-----------------------------------------------------------------------
C COMPUTES D BY RECURRENCE:
C-----------------------------------------------------------------------
C
      IF(PREC) THEN
C                                          T  T -1          T -1
C                               AD IS HERE  A  C  G    B IS  C   G
        CALL MATRBL( 'X=TAY   ',AD,A,B,  C,MESH)
      ELSE
C                               AD IS HERE TAG
        CALL MATRBL( 'X=TAY   ',AD,A,G,  C,MESH)
      ENDIF
C
      TG1TG1 = TGMTGM
      TGMTGM = P_DOTS(AD,AD,MESH)
      BETA = TGMTGM / TG1TG1
C
      CALL OS( 'X=CX    ' , D , D , D , BETA )
C
C                               AD IS HERE TAG
      CALL OS( 'X=X+Y   ' , D , AD , AD , C   )
C
C-----------------------------------------------------------------------
C COMPUTES A D :
C-----------------------------------------------------------------------
C
      CALL MATRBL( 'X=AY    ',AD,A,D,  C,MESH)
C
      IF(PREC) THEN
C
C           COMPUTES  C DPRIM = AD  (DPRIM PUT IN AG)
            CALL DOWNUP(AG , AUX , AD , 'D' , MESH)
C
      ENDIF
C
C-----------------------------------------------------------------------
C COMPUTES RO
C-----------------------------------------------------------------------
C
      IF(PREC) THEN
        ADAD = P_DOTS(AG,AG,MESH)
      ELSE
        ADAD = P_DOTS(AD,AD,MESH)
      ENDIF
      RO = TGMTGM/ADAD
C
C COMPUTES X(M) = X(M-1) - RO * D
C
      CALL OS( 'X=X+CY  ' , X , D , D , -RO )
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
1000  RETURN
C
C-----------------------------------------------------------------------
C
C   FORMATS
C
101   FORMAT(1X,'EQUNOR (BIEF) : ',
     &                     1I8,' ITERATIONS, PRECISION RELATIVE:',G16.7)
102   FORMAT(1X,'EQUNOR (BIEF) : ',
     &                     1I8,' ITERATIONS, RELATIVE PRECISION:',G16.7)
201   FORMAT(1X,'EQUNOR (BIEF) : ',
     &                     1I8,' ITERATIONS, PRECISION ABSOLUE :',G16.7)
202   FORMAT(1X,'EQUNOR (BIEF) : ',
     &                     1I8,' ITERATIONS, ABSOLUTE PRECISION:',G16.7)
103   FORMAT(1X,'EQUNOR (BIEF) : MAX D'' ITERATIONS ATTEINT:',
     &                     1I8,' PRECISION RELATIVE:',G16.7)
104   FORMAT(1X,'EQUNOR (BIEF) : EXCEEDING MAXIMUM ITERATIONS:',
     &                     1I8,' RELATIVE PRECISION:',G16.7)
203   FORMAT(1X,'EQUNOR (BIEF) : MAX D'' ITERATIONS ATTEINT:',
     &                     1I8,' PRECISION ABSOLUE :',G16.7)
204   FORMAT(1X,'EQUNOR (BIEF) : EXCEEDING MAXIMUM ITERATIONS:',
     &                     1I8,' ABSOLUTE PRECISON:',G16.7)
C
C-----------------------------------------------------------------------
C
      END


C
C#######################################################################
C