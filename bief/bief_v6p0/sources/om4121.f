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
!>    </th><td> C, DM, DN, NBOR, NELBOR, NELMAX, NULONE, OP, SIZDN, SIZXN, SZMXN, TYPDIM, TYPDIN, TYPEXM, TYPEXN, XM, XN
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
!>    </th><td> CORNSY, CORSYM, IEL, K, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_OM4121
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OVDB(), PLANTE()
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
!>      <td><center> 5.9                                       </center>
!> </td><td> 23/06/2008
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
!>          <tr><td>IKLE
!></td><td>--></td><td>CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROS GLOBAUX DES POINTS DE BORD.
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMEROS DES ELEMENTS DE BORD.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td>--></td><td>NUMEROS LOCAUX DES NOEUDS DE BORD.
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>OPERATION A EFFECTUER
!>    </td></tr>
!>          <tr><td>SIZDN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SIZXN
!></td><td>--></td><td>NOMBRE D'ELEMENTS DE BORD.
!>    </td></tr>
!>          <tr><td>SZMXN
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DE BORD.
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
                        SUBROUTINE OM4121
     &(OP ,  DM,TYPDIM,XM,TYPEXM,   DN,TYPDIN,XN,TYPEXN,   C,
     & NULONE,NELBOR,NBOR,NELMAX,SIZDN,SIZXN,SZMXN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
C| D             |-->| MATRICE DIAGONALE
C| DM,TYPDIM      |<->| DIAGONALE ET TYPE DE DIAGONALE DE M
C| DN,TYPDIN      |-->| DIAGONALE ET TYPE DE DIAGONALE DE N
C| IKLE           |-->| CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
C| NBOR           |-->| NUMEROS GLOBAUX DES POINTS DE BORD.
C| NELBOR         |-->| NUMEROS DES ELEMENTS DE BORD.
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NULONE         |-->| NUMEROS LOCAUX DES NOEUDS DE BORD.
C| OP             |-->| OPERATION A EFFECTUER
C| SIZDN          |---| 
C| SIZXN          |-->| NOMBRE D'ELEMENTS DE BORD.
C| SZMXN          |-->| NOMBRE MAXIMUM D'ELEMENTS DE BORD.
C| XM,TYPEXM      |-->| TERMES EXTRA-DIAG. ET TYPE POUR M
C| XN,TYPEXN      |-->| TERMES EXTRA-DIAG. ET TYPE POUR N
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_OM4121 => OM4121
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NELMAX,SIZDN,SIZXN,SZMXN
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      INTEGER, INTENT(IN)             :: NULONE(*),NELBOR(*),NBOR(*)
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),XN(SZMXN,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DM(*),XM(NELMAX,*)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIM,TYPEXM,TYPDIN,TYPEXN
      DOUBLE PRECISION, INTENT(IN)    :: C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,IEL
C
      DOUBLE PRECISION Z(1)
C
      INTEGER CORNSY(3,12),CORSYM(3,6)
C
C-----------------------------------------------------------------------
C
      DATA CORNSY/ 1, 6,17, 4, 9,10, 3, 8,12, 8,12, 3, 7,11, 5,28,30,14,
     &            16,21, 2,19,24,25,18,23,27,23,27,18,22,26,20,13,15,29/
      DATA CORSYM/ 1, 6, 2, 4, 9,10, 3, 8,12, 8,12, 3, 7,11, 5,13,15,14/
C
C***********************************************************************
C
      IF(OP(1:8).EQ.'M=M+N   ') THEN
C
        IF(TYPDIM.EQ.'Q'.AND.TYPDIN.EQ.'Q') THEN
          CALL OVDB( 'X=X+Y   ' , DM , DN , Z , C , NBOR , SIZDN )
        ELSE
          IF (LNG.EQ.1) WRITE(LU,198) TYPDIM(1:1),OP(1:8),TYPDIN(1:1)
          IF (LNG.EQ.2) WRITE(LU,199) TYPDIM(1:1),OP(1:8),TYPDIN(1:1)
198       FORMAT(1X,'OM4121 (BIEF) : TYPDIM = ',A1,' NON PROGRAMME',
     &      /,1X,'POUR L''OPERATION : ',A8,' AVEC TYPDIN = ',A1)
199       FORMAT(1X,'OM4121 (BIEF) : TYPDIM = ',A1,' NOT IMPLEMENTED',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPDIN = ',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
C
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
C
C          CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
C
           IF(NCSIZE.GT.1) THEN
           DO K = 1 , SIZXN
             IEL = NELBOR(K)
             IF(IEL.GT.0) THEN
             XM( IEL , CORNSY(NULONE(K), 1) ) =
     &       XM( IEL , CORNSY(NULONE(K), 1) ) + XN(K, 1)
             XM( IEL , CORNSY(NULONE(K), 2) ) =
     &       XM( IEL , CORNSY(NULONE(K), 2) ) + XN(K, 2)
             XM( IEL , CORNSY(NULONE(K), 3) ) =
     &       XM( IEL , CORNSY(NULONE(K), 3) ) + XN(K, 3)
             XM( IEL , CORNSY(NULONE(K), 4) ) =
     &       XM( IEL , CORNSY(NULONE(K), 4) ) + XN(K, 4)
             XM( IEL , CORNSY(NULONE(K), 5) ) =
     &       XM( IEL , CORNSY(NULONE(K), 5) ) + XN(K, 5)
             XM( IEL , CORNSY(NULONE(K), 6) ) =
     &       XM( IEL , CORNSY(NULONE(K), 6) ) + XN(K, 6)
             XM( IEL , CORNSY(NULONE(K), 7) ) =
     &       XM( IEL , CORNSY(NULONE(K), 7) ) + XN(K, 7)
             XM( IEL , CORNSY(NULONE(K), 8) ) =
     &       XM( IEL , CORNSY(NULONE(K), 8) ) + XN(K, 8)
             XM( IEL , CORNSY(NULONE(K), 9) ) =
     &       XM( IEL , CORNSY(NULONE(K), 9) ) + XN(K, 9)
             XM( IEL , CORNSY(NULONE(K),10) ) =
     &       XM( IEL , CORNSY(NULONE(K),10) ) + XN(K,10)
             XM( IEL , CORNSY(NULONE(K),11) ) =
     &       XM( IEL , CORNSY(NULONE(K),11) ) + XN(K,11)
             XM( IEL , CORNSY(NULONE(K),12) ) =
     &       XM( IEL , CORNSY(NULONE(K),12) ) + XN(K,12)
             ENDIF
           ENDDO
           ELSE
           DO K = 1 , SIZXN
             IEL = NELBOR(K)
             XM( IEL , CORNSY(NULONE(K), 1) ) =
     &       XM( IEL , CORNSY(NULONE(K), 1) ) + XN(K, 1)
             XM( IEL , CORNSY(NULONE(K), 2) ) =
     &       XM( IEL , CORNSY(NULONE(K), 2) ) + XN(K, 2)
             XM( IEL , CORNSY(NULONE(K), 3) ) =
     &       XM( IEL , CORNSY(NULONE(K), 3) ) + XN(K, 3)
             XM( IEL , CORNSY(NULONE(K), 4) ) =
     &       XM( IEL , CORNSY(NULONE(K), 4) ) + XN(K, 4)
             XM( IEL , CORNSY(NULONE(K), 5) ) =
     &       XM( IEL , CORNSY(NULONE(K), 5) ) + XN(K, 5)
             XM( IEL , CORNSY(NULONE(K), 6) ) =
     &       XM( IEL , CORNSY(NULONE(K), 6) ) + XN(K, 6)
             XM( IEL , CORNSY(NULONE(K), 7) ) =
     &       XM( IEL , CORNSY(NULONE(K), 7) ) + XN(K, 7)
             XM( IEL , CORNSY(NULONE(K), 8) ) =
     &       XM( IEL , CORNSY(NULONE(K), 8) ) + XN(K, 8)
             XM( IEL , CORNSY(NULONE(K), 9) ) =
     &       XM( IEL , CORNSY(NULONE(K), 9) ) + XN(K, 9)
             XM( IEL , CORNSY(NULONE(K),10) ) =
     &       XM( IEL , CORNSY(NULONE(K),10) ) + XN(K,10)
             XM( IEL , CORNSY(NULONE(K),11) ) =
     &       XM( IEL , CORNSY(NULONE(K),11) ) + XN(K,11)
             XM( IEL , CORNSY(NULONE(K),12) ) =
     &       XM( IEL , CORNSY(NULONE(K),12) ) + XN(K,12)
           ENDDO
           ENDIF
C
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
C
           IF(NCSIZE.GT.1) THEN
           DO K = 1 , SIZXN
             IEL = NELBOR(K)
             IF(IEL.GT.0) THEN
             XM( IEL , CORNSY(NULONE(K), 1) ) =
     &       XM( IEL , CORNSY(NULONE(K), 1) ) + XN(K, 1)
             XM( IEL , CORNSY(NULONE(K), 2) ) =
     &       XM( IEL , CORNSY(NULONE(K), 2) ) + XN(K, 2)
             XM( IEL , CORNSY(NULONE(K), 3) ) =
     &       XM( IEL , CORNSY(NULONE(K), 3) ) + XN(K, 3)
             XM( IEL , CORNSY(NULONE(K), 4) ) =
     &       XM( IEL , CORNSY(NULONE(K), 4) ) + XN(K, 4)
             XM( IEL , CORNSY(NULONE(K), 5) ) =
     &       XM( IEL , CORNSY(NULONE(K), 5) ) + XN(K, 5)
             XM( IEL , CORNSY(NULONE(K), 6) ) =
     &       XM( IEL , CORNSY(NULONE(K), 6) ) + XN(K, 6)
             XM( IEL , CORNSY(NULONE(K), 7) ) =
     &       XM( IEL , CORNSY(NULONE(K), 7) ) + XN(K, 1)
             XM( IEL , CORNSY(NULONE(K), 8) ) =
     &       XM( IEL , CORNSY(NULONE(K), 8) ) + XN(K, 2)
             XM( IEL , CORNSY(NULONE(K), 9) ) =
     &       XM( IEL , CORNSY(NULONE(K), 9) ) + XN(K, 3)
             XM( IEL , CORNSY(NULONE(K),10) ) =
     &       XM( IEL , CORNSY(NULONE(K),10) ) + XN(K, 4)
             XM( IEL , CORNSY(NULONE(K),11) ) =
     &       XM( IEL , CORNSY(NULONE(K),11) ) + XN(K, 5)
             XM( IEL , CORNSY(NULONE(K),12) ) =
     &       XM( IEL , CORNSY(NULONE(K),12) ) + XN(K, 6)
             ENDIF
           ENDDO
           ELSE
           DO K = 1 , SIZXN
             IEL = NELBOR(K)
             XM( IEL , CORNSY(NULONE(K), 1) ) =
     &       XM( IEL , CORNSY(NULONE(K), 1) ) + XN(K, 1)
             XM( IEL , CORNSY(NULONE(K), 2) ) =
     &       XM( IEL , CORNSY(NULONE(K), 2) ) + XN(K, 2)
             XM( IEL , CORNSY(NULONE(K), 3) ) =
     &       XM( IEL , CORNSY(NULONE(K), 3) ) + XN(K, 3)
             XM( IEL , CORNSY(NULONE(K), 4) ) =
     &       XM( IEL , CORNSY(NULONE(K), 4) ) + XN(K, 4)
             XM( IEL , CORNSY(NULONE(K), 5) ) =
     &       XM( IEL , CORNSY(NULONE(K), 5) ) + XN(K, 5)
             XM( IEL , CORNSY(NULONE(K), 6) ) =
     &       XM( IEL , CORNSY(NULONE(K), 6) ) + XN(K, 6)
             XM( IEL , CORNSY(NULONE(K), 7) ) =
     &       XM( IEL , CORNSY(NULONE(K), 7) ) + XN(K, 1)
             XM( IEL , CORNSY(NULONE(K), 8) ) =
     &       XM( IEL , CORNSY(NULONE(K), 8) ) + XN(K, 2)
             XM( IEL , CORNSY(NULONE(K), 9) ) =
     &       XM( IEL , CORNSY(NULONE(K), 9) ) + XN(K, 3)
             XM( IEL , CORNSY(NULONE(K),10) ) =
     &       XM( IEL , CORNSY(NULONE(K),10) ) + XN(K, 4)
             XM( IEL , CORNSY(NULONE(K),11) ) =
     &       XM( IEL , CORNSY(NULONE(K),11) ) + XN(K, 5)
             XM( IEL , CORNSY(NULONE(K),12) ) =
     &       XM( IEL , CORNSY(NULONE(K),12) ) + XN(K, 6)
           ENDDO
           ENDIF
C
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE BOTH MATRICES ARE SYMMETRICAL
C
           IF(NCSIZE.GT.1) THEN
           DO K = 1 , SIZXN
             IEL = NELBOR(K)
             IF(IEL.GT.0) THEN
             XM( IEL , CORSYM(NULONE(K), 1) ) =
     &       XM( IEL , CORSYM(NULONE(K), 1) ) + XN(K, 1)
             XM( IEL , CORSYM(NULONE(K), 2) ) =
     &       XM( IEL , CORSYM(NULONE(K), 2) ) + XN(K, 2)
             XM( IEL , CORSYM(NULONE(K), 3) ) =
     &       XM( IEL , CORSYM(NULONE(K), 3) ) + XN(K, 3)
             XM( IEL , CORSYM(NULONE(K), 4) ) =
     &       XM( IEL , CORSYM(NULONE(K), 4) ) + XN(K, 4)
             XM( IEL , CORSYM(NULONE(K), 5) ) =
     &       XM( IEL , CORSYM(NULONE(K), 5) ) + XN(K, 5)
             XM( IEL , CORSYM(NULONE(K), 6) ) =
     &       XM( IEL , CORSYM(NULONE(K), 6) ) + XN(K, 6)
             ENDIF
           ENDDO
           ELSE
           DO K = 1 , SIZXN
             IEL = NELBOR(K)
             XM( IEL , CORSYM(NULONE(K), 1) ) =
     &       XM( IEL , CORSYM(NULONE(K), 1) ) + XN(K, 1)
             XM( IEL , CORSYM(NULONE(K), 2) ) =
     &       XM( IEL , CORSYM(NULONE(K), 2) ) + XN(K, 2)
             XM( IEL , CORSYM(NULONE(K), 3) ) =
     &       XM( IEL , CORSYM(NULONE(K), 3) ) + XN(K, 3)
             XM( IEL , CORSYM(NULONE(K), 4) ) =
     &       XM( IEL , CORSYM(NULONE(K), 4) ) + XN(K, 4)
             XM( IEL , CORSYM(NULONE(K), 5) ) =
     &       XM( IEL , CORSYM(NULONE(K), 5) ) + XN(K, 5)
             XM( IEL , CORSYM(NULONE(K), 6) ) =
     &       XM( IEL , CORSYM(NULONE(K), 6) ) + XN(K, 6)
           ENDDO
           ENDIF
C
        ELSE
           IF (LNG.EQ.1) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
98         FORMAT(1X,'OM4121 (BIEF) : TYPEXM = ',A1,' NE CONVIENT PAS',
     &       /,1X,'POUR L''OPERATION : ',A8,' AVEC TYPEXN = ',A1)
99         FORMAT(1X,'OM4121 (BIEF) : TYPEXM = ',A1,' DOES NOT GO',
     &       /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'M=M+TN  ') THEN
C
        CALL OVDB( 'X=X+Y   ' , DM , DN , Z , C , NBOR , SIZXN )
C
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
C
C          CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
C
           IF(NCSIZE.GT.1) THEN
           DO K = 1 , SIZXN
             IEL = NELBOR(K)
             IF(IEL.GT.0) THEN
             XM( IEL , CORNSY(NULONE(K), 1) ) =
     &       XM( IEL , CORNSY(NULONE(K), 1) ) + XN(K, 7)
             XM( IEL , CORNSY(NULONE(K), 2) ) =
     &       XM( IEL , CORNSY(NULONE(K), 2) ) + XN(K, 8)
             XM( IEL , CORNSY(NULONE(K), 3) ) =
     &       XM( IEL , CORNSY(NULONE(K), 3) ) + XN(K, 9)
             XM( IEL , CORNSY(NULONE(K), 4) ) =
     &       XM( IEL , CORNSY(NULONE(K), 4) ) + XN(K,10)
             XM( IEL , CORNSY(NULONE(K), 5) ) =
     &       XM( IEL , CORNSY(NULONE(K), 5) ) + XN(K,11)
             XM( IEL , CORNSY(NULONE(K), 6) ) =
     &       XM( IEL , CORNSY(NULONE(K), 6) ) + XN(K,12)
             XM( IEL , CORNSY(NULONE(K), 7) ) =
     &       XM( IEL , CORNSY(NULONE(K), 7) ) + XN(K, 1)
             XM( IEL , CORNSY(NULONE(K), 8) ) =
     &       XM( IEL , CORNSY(NULONE(K), 8) ) + XN(K, 2)
             XM( IEL , CORNSY(NULONE(K), 9) ) =
     &       XM( IEL , CORNSY(NULONE(K), 9) ) + XN(K, 3)
             XM( IEL , CORNSY(NULONE(K),10) ) =
     &       XM( IEL , CORNSY(NULONE(K),10) ) + XN(K, 4)
             XM( IEL , CORNSY(NULONE(K),11) ) =
     &       XM( IEL , CORNSY(NULONE(K),11) ) + XN(K, 5)
             XM( IEL , CORNSY(NULONE(K),12) ) =
     &       XM( IEL , CORNSY(NULONE(K),12) ) + XN(K, 6)
             ENDIF
           ENDDO
           ELSE
           DO K = 1 , SIZXN
             IEL = NELBOR(K)
             XM( IEL , CORNSY(NULONE(K), 1) ) =
     &       XM( IEL , CORNSY(NULONE(K), 1) ) + XN(K, 7)
             XM( IEL , CORNSY(NULONE(K), 2) ) =
     &       XM( IEL , CORNSY(NULONE(K), 2) ) + XN(K, 8)
             XM( IEL , CORNSY(NULONE(K), 3) ) =
     &       XM( IEL , CORNSY(NULONE(K), 3) ) + XN(K, 9)
             XM( IEL , CORNSY(NULONE(K), 4) ) =
     &       XM( IEL , CORNSY(NULONE(K), 4) ) + XN(K,10)
             XM( IEL , CORNSY(NULONE(K), 5) ) =
     &       XM( IEL , CORNSY(NULONE(K), 5) ) + XN(K,11)
             XM( IEL , CORNSY(NULONE(K), 6) ) =
     &       XM( IEL , CORNSY(NULONE(K), 6) ) + XN(K,12)
             XM( IEL , CORNSY(NULONE(K), 7) ) =
     &       XM( IEL , CORNSY(NULONE(K), 7) ) + XN(K, 1)
             XM( IEL , CORNSY(NULONE(K), 8) ) =
     &       XM( IEL , CORNSY(NULONE(K), 8) ) + XN(K, 2)
             XM( IEL , CORNSY(NULONE(K), 9) ) =
     &       XM( IEL , CORNSY(NULONE(K), 9) ) + XN(K, 3)
             XM( IEL , CORNSY(NULONE(K),10) ) =
     &       XM( IEL , CORNSY(NULONE(K),10) ) + XN(K, 4)
             XM( IEL , CORNSY(NULONE(K),11) ) =
     &       XM( IEL , CORNSY(NULONE(K),11) ) + XN(K, 5)
             XM( IEL , CORNSY(NULONE(K),12) ) =
     &       XM( IEL , CORNSY(NULONE(K),12) ) + XN(K, 6)
           ENDDO
           ENDIF
C
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
C
           IF(NCSIZE.GT.1) THEN
           DO K = 1 , SIZXN
             IEL = NELBOR(K)
             IF(IEL.GT.0) THEN
             XM( IEL , CORNSY(NULONE(K), 1) ) =
     &       XM( IEL , CORNSY(NULONE(K), 1) ) + XN(K, 1)
             XM( IEL , CORNSY(NULONE(K), 2) ) =
     &       XM( IEL , CORNSY(NULONE(K), 2) ) + XN(K, 2)
             XM( IEL , CORNSY(NULONE(K), 3) ) =
     &       XM( IEL , CORNSY(NULONE(K), 3) ) + XN(K, 3)
             XM( IEL , CORNSY(NULONE(K), 4) ) =
     &       XM( IEL , CORNSY(NULONE(K), 4) ) + XN(K, 4)
             XM( IEL , CORNSY(NULONE(K), 5) ) =
     &       XM( IEL , CORNSY(NULONE(K), 5) ) + XN(K, 5)
             XM( IEL , CORNSY(NULONE(K), 6) ) =
     &       XM( IEL , CORNSY(NULONE(K), 6) ) + XN(K, 6)
             XM( IEL , CORNSY(NULONE(K), 7) ) =
     &       XM( IEL , CORNSY(NULONE(K), 7) ) + XN(K, 1)
             XM( IEL , CORNSY(NULONE(K), 8) ) =
     &       XM( IEL , CORNSY(NULONE(K), 8) ) + XN(K, 2)
             XM( IEL , CORNSY(NULONE(K), 9) ) =
     &       XM( IEL , CORNSY(NULONE(K), 9) ) + XN(K, 3)
             XM( IEL , CORNSY(NULONE(K),10) ) =
     &       XM( IEL , CORNSY(NULONE(K),10) ) + XN(K, 4)
             XM( IEL , CORNSY(NULONE(K),11) ) =
     &       XM( IEL , CORNSY(NULONE(K),11) ) + XN(K, 5)
             XM( IEL , CORNSY(NULONE(K),12) ) =
     &       XM( IEL , CORNSY(NULONE(K),12) ) + XN(K, 6)
             ENDIF
           ENDDO
           ELSE
           DO K = 1 , SIZXN
             IEL = NELBOR(K)
             XM( IEL , CORNSY(NULONE(K), 1) ) =
     &       XM( IEL , CORNSY(NULONE(K), 1) ) + XN(K, 1)
             XM( IEL , CORNSY(NULONE(K), 2) ) =
     &       XM( IEL , CORNSY(NULONE(K), 2) ) + XN(K, 2)
             XM( IEL , CORNSY(NULONE(K), 3) ) =
     &       XM( IEL , CORNSY(NULONE(K), 3) ) + XN(K, 3)
             XM( IEL , CORNSY(NULONE(K), 4) ) =
     &       XM( IEL , CORNSY(NULONE(K), 4) ) + XN(K, 4)
             XM( IEL , CORNSY(NULONE(K), 5) ) =
     &       XM( IEL , CORNSY(NULONE(K), 5) ) + XN(K, 5)
             XM( IEL , CORNSY(NULONE(K), 6) ) =
     &       XM( IEL , CORNSY(NULONE(K), 6) ) + XN(K, 6)
             XM( IEL , CORNSY(NULONE(K), 7) ) =
     &       XM( IEL , CORNSY(NULONE(K), 7) ) + XN(K, 1)
             XM( IEL , CORNSY(NULONE(K), 8) ) =
     &       XM( IEL , CORNSY(NULONE(K), 8) ) + XN(K, 2)
             XM( IEL , CORNSY(NULONE(K), 9) ) =
     &       XM( IEL , CORNSY(NULONE(K), 9) ) + XN(K, 3)
             XM( IEL , CORNSY(NULONE(K),10) ) =
     &       XM( IEL , CORNSY(NULONE(K),10) ) + XN(K, 4)
             XM( IEL , CORNSY(NULONE(K),11) ) =
     &       XM( IEL , CORNSY(NULONE(K),11) ) + XN(K, 5)
             XM( IEL , CORNSY(NULONE(K),12) ) =
     &       XM( IEL , CORNSY(NULONE(K),12) ) + XN(K, 6)
           ENDDO
           ENDIF
C
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE BOTH MATRICES ARE SYMMETRICAL
C
           IF(NCSIZE.GT.1) THEN
           DO K = 1 , SIZXN
             IEL = NELBOR(K)
             IF(IEL.GT.0) THEN
             XM( IEL , CORSYM(NULONE(K), 1) ) =
     &       XM( IEL , CORSYM(NULONE(K), 1) ) + XN(K, 1)
             XM( IEL , CORSYM(NULONE(K), 2) ) =
     &       XM( IEL , CORSYM(NULONE(K), 2) ) + XN(K, 2)
             XM( IEL , CORSYM(NULONE(K), 3) ) =
     &       XM( IEL , CORSYM(NULONE(K), 3) ) + XN(K, 3)
             XM( IEL , CORSYM(NULONE(K), 4) ) =
     &       XM( IEL , CORSYM(NULONE(K), 4) ) + XN(K, 4)
             XM( IEL , CORSYM(NULONE(K), 5) ) =
     &       XM( IEL , CORSYM(NULONE(K), 5) ) + XN(K, 5)
             XM( IEL , CORSYM(NULONE(K), 6) ) =
     &       XM( IEL , CORSYM(NULONE(K), 6) ) + XN(K, 6)
             ENDIF
           ENDDO
           ELSE
           DO K = 1 , SIZXN
             IEL = NELBOR(K)
             XM( IEL , CORSYM(NULONE(K), 1) ) =
     &       XM( IEL , CORSYM(NULONE(K), 1) ) + XN(K, 1)
             XM( IEL , CORSYM(NULONE(K), 2) ) =
     &       XM( IEL , CORSYM(NULONE(K), 2) ) + XN(K, 2)
             XM( IEL , CORSYM(NULONE(K), 3) ) =
     &       XM( IEL , CORSYM(NULONE(K), 3) ) + XN(K, 3)
             XM( IEL , CORSYM(NULONE(K), 4) ) =
     &       XM( IEL , CORSYM(NULONE(K), 4) ) + XN(K, 4)
             XM( IEL , CORSYM(NULONE(K), 5) ) =
     &       XM( IEL , CORSYM(NULONE(K), 5) ) + XN(K, 5)
             XM( IEL , CORSYM(NULONE(K), 6) ) =
     &       XM( IEL , CORSYM(NULONE(K), 6) ) + XN(K, 6)
           ENDDO
           ENDIF
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
70      FORMAT(1X,'OM4121 (BIEF) : OPERATION INCONNUE : ',A8)
71      FORMAT(1X,'OM4121 (BIEF) : UNKNOWN OPERATION : ',A8)
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