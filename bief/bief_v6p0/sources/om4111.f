C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPERATIONS ON MATRICES.
!>  @code
!>   M: P1 TRIANGLE
!>   N: BOUNDARY MATRIX
!>   D: DIAGONAL MATRIX
!>   C: CONSTANT<br>
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON MATRICES M AND N, D AND C.<br>
!>   THE RESULT IS MATRIX M.<br>
!>      OP = 'M=M+N   '  : ADDS N TO M
!>      OP = 'M=M+TN  '  : ADDS TRANSPOSE(N) TO M
!>  @endcode
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @code
!>  CONVENTION FOR THE STORAGE OF EXTRA-DIAGONAL TERMS:
!>
!>      XM(     ,1)  ---->  M(1,2)
!>      XM(     ,2)  ---->  M(1,3)
!>      XM(     ,3)  ---->  M(2,3)
!>      XM(     ,4)  ---->  M(2,1)
!>      XM(     ,5)  ---->  M(3,1)
!>      XM(     ,6)  ---->  M(3,2)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, DM, DN, NELMAX3D, NETAGE, OP, SIZDN, SIZXN, SZMDN, SZMXN, TYPDIM, TYPDIN, TYPEXM, TYPEXN, XM, XN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> K, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_OM4111
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>OM()

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
!> </td><td> 06/12/94
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C
!></td><td>--></td><td>CONSTANTE DONNEE
!>    </td></tr>
!>          <tr><td>D
!></td><td>--></td><td>MATRICE DIAGONALE
!>    </td></tr>
!>          <tr><td>DM,TYPDIM
!></td><td><-></td><td>DIAGONALE ET TYPE DE DIAGONALE DE M
!>    </td></tr>
!>          <tr><td>DN,TYPDIN
!></td><td>--></td><td>DIAGONALE ET TYPE DE DIAGONALE DE N
!>    </td></tr>
!>          <tr><td>NELMAX3D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NETAGE
!></td><td>--></td><td>NOMBRE D'ETAGES DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>OPERATION A EFFECTUER
!>    </td></tr>
!>          <tr><td>SIZDN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>SIZXN
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>SZMDN
!></td><td>--></td><td>NOMBRE MAXIMUM DE POINTS DU MAILLAGE 2D
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>SZMXN
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE 2D
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>XM,TYPEXM
!></td><td>--></td><td>TERMES EXTRA-DIAG. ET TYPE POUR M
!>    </td></tr>
!>          <tr><td>XN,TYPEXN
!></td><td>--></td><td>TERMES EXTRA-DIAG. ET TYPE POUR N
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE OM4111
     &(OP ,  DM,TYPDIM,XM,TYPEXM,   DN,TYPDIN,XN,TYPEXN,   C,
     & SIZDN,SZMDN,SIZXN,SZMXN,NETAGE, NELMAX3D)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
C| D             |-->| MATRICE DIAGONALE
C| DM,TYPDIM      |<->| DIAGONALE ET TYPE DE DIAGONALE DE M
C| DN,TYPDIN      |-->| DIAGONALE ET TYPE DE DIAGONALE DE N
C| NELMAX3D       |---| 
C| NETAGE         |-->| NOMBRE D'ETAGES DU MAILLAGE 3D
C| OP             |-->| OPERATION A EFFECTUER
C| SIZDN          |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| SIZXN          |-->| NOMBRE D'ELEMENTS DU MAILLAGE 2D
C| SZMDN          |-->| NOMBRE MAXIMUM DE POINTS DU MAILLAGE 2D
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| SZMXN          |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE 2D
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| XM,TYPEXM      |-->| TERMES EXTRA-DIAG. ET TYPE POUR M
C| XN,TYPEXN      |-->| TERMES EXTRA-DIAG. ET TYPE POUR N
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_OM4111 => OM4111
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NETAGE,SIZDN,SZMXN,SZMDN,SIZXN
      INTEGER, INTENT(IN)             :: NELMAX3D
      CHARACTER(LEN=8), INTENT(IN)    :: OP
CCC      DOUBLE PRECISION, INTENT(IN)    :: DN(*),XN(SZMXN,*)
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),XN(NELMAX3D/NETAGE,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DM(SZMDN,*)
CCC      DOUBLE PRECISION, INTENT(INOUT) :: XM(SZMXN,NETAGE,*)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NELMAX3D/NETAGE,NETAGE,*)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIM,TYPEXM,TYPDIN,TYPEXN
      DOUBLE PRECISION, INTENT(IN)    :: C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K
C
      DOUBLE PRECISION Z(1)
C
C-----------------------------------------------------------------------
C
      IF(OP(1:8).EQ.'M=M+NF  ') THEN
C
        IF(TYPDIM.EQ.'Q'.AND.TYPDIN.EQ.'Q') THEN
          CALL OV( 'X=X+Y   ' , DM , DN , Z , C , SIZDN )
        ELSE
          IF (LNG.EQ.1) WRITE(LU,198) TYPDIM(1:1),OP(1:8),TYPDIN(1:1)
          IF (LNG.EQ.2) WRITE(LU,199) TYPDIM(1:1),OP(1:8),TYPDIN(1:1)
198       FORMAT(1X,'OM4111 (BIEF) : TYPDIM = ',A1,' NON PROGRAMME',
     &      /,1X,'POUR L''OPERATION : ',A8,' AVEC TYPDIN = ',A1)
199       FORMAT(1X,'OM4111 (BIEF) : TYPDIM = ',A1,' NOT IMPLEMENTED',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPDIN = ',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
C
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
C
C          CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
C
           DO 10 K = 1 , SIZXN
             XM(K,1, 1) = XM(K,1, 1) + XN(K,1)
             XM(K,1, 2) = XM(K,1, 2) + XN(K,2)
             XM(K,1, 6) = XM(K,1, 6) + XN(K,3)
             XM(K,1,16) = XM(K,1,16) + XN(K,4)
             XM(K,1,17) = XM(K,1,17) + XN(K,5)
             XM(K,1,21) = XM(K,1,21) + XN(K,6)
10         CONTINUE
C
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
C
           DO 20 K = 1 , SIZXN
             XM(K,1, 1) = XM(K,1, 1) + XN(K,1)
             XM(K,1, 2) = XM(K,1, 2) + XN(K,2)
             XM(K,1, 6) = XM(K,1, 6) + XN(K,3)
             XM(K,1,16) = XM(K,1,16) + XN(K,1)
             XM(K,1,17) = XM(K,1,17) + XN(K,2)
             XM(K,1,21) = XM(K,1,21) + XN(K,3)
20         CONTINUE
C
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE BOTH MATRICES ARE SYMMETRICAL
C
           DO 30 K = 1 , SIZXN
             XM(K,1, 1) = XM(K,1, 1) + XN(K,1)
             XM(K,1, 2) = XM(K,1, 2) + XN(K,2)
             XM(K,1, 6) = XM(K,1, 6) + XN(K,3)
30         CONTINUE
C
        ELSE
           IF (LNG.EQ.1) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
98         FORMAT(1X,'OM4111 (BIEF) : TYPEXM = ',A1,' NE CONVIENT PAS',
     &       /,1X,'POUR L''OPERATION : ',A8,' AVEC TYPEXN = ',A1)
99         FORMAT(1X,'OM4111 (BIEF) : TYPEXM = ',A1,' DOES NOT GO',
     &       /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'M=M+TNF ') THEN
C
        CALL OV( 'X=X+Y   ' , DM , DN , Z , C , SIZDN )
C
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
C
C          CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
C
           DO 40 K = 1 , SIZXN
             XM(K,1, 1) = XM(K,1, 1) + XN(K,4)
             XM(K,1, 2) = XM(K,1, 2) + XN(K,5)
             XM(K,1, 6) = XM(K,1, 6) + XN(K,6)
             XM(K,1,16) = XM(K,1,16) + XN(K,1)
             XM(K,1,17) = XM(K,1,17) + XN(K,2)
             XM(K,1,21) = XM(K,1,21) + XN(K,3)
40         CONTINUE
C
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
C
           DO 50 K = 1 , SIZXN
             XM(K,1, 1) = XM(K,1, 1) + XN(K,1)
             XM(K,1, 2) = XM(K,1, 2) + XN(K,2)
             XM(K,1, 6) = XM(K,1, 6) + XN(K,3)
             XM(K,1,16) = XM(K,1,16) + XN(K,1)
             XM(K,1,17) = XM(K,1,17) + XN(K,2)
             XM(K,1,21) = XM(K,1,21) + XN(K,3)
50         CONTINUE
C
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE BOTH MATRICES ARE SYMMETRICAL
C
           DO 60 K = 1 , SIZXN
             XM(K,1, 1) = XM(K,1, 1) + XN(K,1)
             XM(K,1, 2) = XM(K,1, 2) + XN(K,2)
             XM(K,1, 6) = XM(K,1, 6) + XN(K,3)
60         CONTINUE
C
        ELSE
           IF (LNG.EQ.1) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
           CALL PLANTE(1)
           STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'M=M+NS  ') THEN
C
        CALL OV( 'X=X+Y   ' , DM(1,NETAGE+1) , DN , Z , C , SIZDN )
C
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
C
C          CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
C
           DO 110 K = 1 , SIZXN
             XM(K,NETAGE,13) = XM(K,NETAGE,13) + XN(K,1)
             XM(K,NETAGE,14) = XM(K,NETAGE,14) + XN(K,2)
             XM(K,NETAGE,15) = XM(K,NETAGE,15) + XN(K,3)
             XM(K,NETAGE,28) = XM(K,NETAGE,28) + XN(K,4)
             XM(K,NETAGE,29) = XM(K,NETAGE,29) + XN(K,5)
             XM(K,NETAGE,30) = XM(K,NETAGE,30) + XN(K,6)
110        CONTINUE
C
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
C
           DO 120 K = 1 , SIZXN
             XM(K,NETAGE,13) = XM(K,NETAGE,13) + XN(K,1)
             XM(K,NETAGE,14) = XM(K,NETAGE,14) + XN(K,2)
             XM(K,NETAGE,15) = XM(K,NETAGE,15) + XN(K,3)
             XM(K,NETAGE,28) = XM(K,NETAGE,28) + XN(K,1)
             XM(K,NETAGE,29) = XM(K,NETAGE,29) + XN(K,2)
             XM(K,NETAGE,30) = XM(K,NETAGE,30) + XN(K,3)
120        CONTINUE
C
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE BOTH MATRICES ARE SYMMETRICAL
C
           DO 130 K = 1 , SIZXN
             XM(K,NETAGE,13) = XM(K,NETAGE,13) + XN(K,1)
             XM(K,NETAGE,14) = XM(K,NETAGE,14) + XN(K,2)
             XM(K,NETAGE,15) = XM(K,NETAGE,15) + XN(K,3)
130        CONTINUE
C
        ELSE
           IF (LNG.EQ.1) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
           CALL PLANTE(1)
           STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'M=M+TNS ') THEN
C
        CALL OV( 'X=X+Y   ' , DM(1,NETAGE+1) , DN , Z , C , SIZDN )
C
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
C
C          CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
C
           DO 140 K = 1 , SIZXN
             XM(K,NETAGE,13) = XM(K,NETAGE,13) + XN(K,4)
             XM(K,NETAGE,14) = XM(K,NETAGE,14) + XN(K,5)
             XM(K,NETAGE,15) = XM(K,NETAGE,15) + XN(K,6)
             XM(K,NETAGE,28) = XM(K,NETAGE,28) + XN(K,1)
             XM(K,NETAGE,29) = XM(K,NETAGE,29) + XN(K,2)
             XM(K,NETAGE,30) = XM(K,NETAGE,30) + XN(K,3)
140        CONTINUE
C
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
C
           DO 150 K = 1 , SIZXN
             XM(K,NETAGE,13) = XM(K,NETAGE,13) + XN(K,1)
             XM(K,NETAGE,14) = XM(K,NETAGE,14) + XN(K,2)
             XM(K,NETAGE,15) = XM(K,NETAGE,15) + XN(K,3)
             XM(K,NETAGE,28) = XM(K,NETAGE,28) + XN(K,1)
             XM(K,NETAGE,29) = XM(K,NETAGE,29) + XN(K,2)
             XM(K,NETAGE,30) = XM(K,NETAGE,30) + XN(K,3)
150        CONTINUE
C
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE BOTH MATRICES ARE SYMMETRICAL
C
           DO 160 K = 1 , SIZXN
             XM(K,NETAGE,13) = XM(K,NETAGE,13) + XN(K,1)
             XM(K,NETAGE,14) = XM(K,NETAGE,14) + XN(K,2)
             XM(K,NETAGE,15) = XM(K,NETAGE,15) + XN(K,3)
160        CONTINUE
C
        ELSE
           IF (LNG.EQ.1) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
           CALL PLANTE(1)
           STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,70) OP
        IF (LNG.EQ.2) WRITE(LU,71) OP
70      FORMAT(1X,'OM4111 (BIEF) : OPERATION INCONNUE : ',A8)
71      FORMAT(1X,'OM4111 (BIEF) : UNKNOWN OPERATION : ',A8)
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C