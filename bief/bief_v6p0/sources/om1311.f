C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPERATIONS ON RECTANGULAR MATRICES
!>                CONSISTING OF ONE ELEMENT WITH 3 POINTS AND
!>                ONE WITH 6 POINTS (LINEAR,QUASIBUBBLE FOR EXAMPLE).
!>  @code
!>   D: DIAGONAL MATRIX
!>   C: CONSTANT
!>
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON MATRICES M AND N, D AND C.
!>
!>   THE RESULT IS MATRIX M.
!>
!>      OP = 'M=N     '  : COPIES N IN M
!>      OP = 'M=CN    '  : MULTIPLIES N BY C
!>      OP = 'M=M+CN  '  : ADDS CN TO M
!>      OP = 'M=M+N   '  : ADDS N TO M
!>      OP = 'M=MD    '  : M X D
!>      OP = 'M=DM    '  : D X M
!>      OP = 'M=M+D   '  : ADDS D TO M
!>      OP = 'M=MSK(M)'  : MASKS M EXTRADIAGONAL TERMS
!>                         (OLD MASKEX)
!>                         THE MASK IS TAKEN FROM D
!>  @endcode
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @code
!>  CONVENTION FOR THE STORAGE OF EXTRA-DIAGONAL TERMS:
!>
!>     XM(IELEM, 1)  ---->  M(1,2)
!>     XM(IELEM, 2)  ---->  M(1,3)
!>     XM(IELEM, 3)  ---->  M(2,1)
!>     XM(IELEM, 4)  ---->  M(2,3)
!>     XM(IELEM, 5)  ---->  M(3,1)
!>     XM(IELEM, 6)  ---->  M(3,2)
!>     XM(IELEM, 7)  ---->  M(4,1)
!>     XM(IELEM, 8)  ---->  M(4,2)
!>     XM(IELEM, 9)  ---->  M(4,3)
!>     XM(IELEM, 10)  ---->  M(5,1)
!>     XM(IELEM, 11)  ---->  M(5,2)
!>     XM(IELEM, 12)  ---->  M(5,3)
!>     XM(IELEM, 13)  ---->  M(6,1)
!>     XM(IELEM, 14)  ---->  M(6,2)
!>     XM(IELEM, 15)  ---->  M(6,3)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, D, DM, DN, IKLE, NDIAG, NELEM, NELMAX, OP, TYPDIM, TYPDIN, TYPEXM, TYPEXN, XM, XN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IELEM, J, Y, Z
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
!>      <td><center> 5.9                                       </center>
!> </td><td> 13/02/2008
!> </td><td> ALGIANE FROEHLY (MATMECA); J-M HERVOUET (LNHE) 01 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
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
!>          <tr><td>NDIAG
!></td><td>--></td><td>NOMBRE DE VALEURS DE LA DIAGONALE.
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>OPERATION A EFFECTUER
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
                        SUBROUTINE OM1311
     &(OP ,  DM,TYPDIM,XM,TYPEXM,   DN,TYPDIN,XN,TYPEXN,   D,C,
     & IKLE,NELEM,NELMAX,NDIAG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
C| D             |-->| MATRICE DIAGONALE
C| DM,TYPDIM      |<->| DIAGONALE ET TYPE DE DIAGONALE DE M
C| DN,TYPDIN      |-->| DIAGONALE ET TYPE DE DIAGONALE DE N
C| IKLE           |-->| CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
C| NDIAG          |-->| NOMBRE DE VALEURS DE LA DIAGONALE.
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| OP             |-->| OPERATION A EFFECTUER
C| XM,TYPEXM      |-->| TERMES EXTRA-DIAG. ET TYPE POUR M
C| XN,TYPEXN      |-->| TERMES EXTRA-DIAG. ET TYPE POUR N
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF !, EX_OM1311 => OM1311
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NDIAG
      INTEGER, INTENT(IN) :: IKLE(NELMAX,6)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),D(*),XN(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DM(*),XM(NELMAX,*)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIM,TYPEXM,TYPDIN,TYPEXN
      DOUBLE PRECISION, INTENT(IN)    :: C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,J,IELEM
C
      DOUBLE PRECISION Y(1),Z(1)
C
C-----------------------------------------------------------------------
C
      IF(OP(1:8).EQ.'M=N     ') THEN
C
        IF(TYPDIN(1:1).EQ.'Q') THEN
          CALL OV( 'X=Y     ' , DM , DN , Z , C , NDIAG )
        ELSEIF(TYPDIN(1:1).EQ.'I'.OR.TYPDIN(1:1).EQ.'0') THEN
C         NOTHING TO DO, ONLY NEEDS TO COPY TYPDIN
        ELSE
           IF (LNG.EQ.1) WRITE(LU,5) TYPDIN(1:1)
           IF (LNG.EQ.2) WRITE(LU,6) TYPDIN(1:1)
5          FORMAT(1X,'OM1311 (BIEF) : TYPDIN INCONNU :',A1)
6          FORMAT(1X,'OM1311 (BIEF) : TYPDIN UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
        TYPDIM(1:1)=TYPDIN(1:1)
C
        IF(TYPEXN(1:1).EQ.'Q') THEN
          DO 111 I=1,15
            CALL OV( 'X=Y     ' , XM(1,I) , XN(1,I) , Z , C , NELEM )
111       CONTINUE
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
10        FORMAT(1X,'OM1311 (BIEF) : TYPEXN INCONNU :',A1)
11        FORMAT(1X,'OM1311 (BIEF) : TYPEXN UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
C
        TYPEXM(1:1)=TYPEXN(1:1)
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'M=CN    ') THEN
C
        CALL OV( 'X=CY    ' , DM      , DN      , Z , C , NDIAG )
C
        IF(TYPEXN(1:1).EQ.'Q') THEN
          DO 22 I=1,15
            CALL OV( 'X=CY    ' , XM(1,I) , XN(1,I) , Z , C , NELEM )
22        CONTINUE
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
C
        TYPDIM(1:1)=TYPDIN(1:1)
        TYPEXM(1:1)=TYPEXN(1:1)
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'M=M+CN  ') THEN
C
        IF(TYPDIN(1:1).EQ.'I') THEN
          CALL OV( 'X=X+C   ' , DM , DN , Z , C , NDIAG )
        ELSEIF(TYPDIN(1:1).NE.'0') THEN
          CALL OV( 'X=X+CY  ' , DM , DN , Z , C , NDIAG )
        ENDIF
C
        IF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
99          FORMAT(1X,'OM1211 (BIEF) : TYPEXM = ',A1,' NE CONVIENT PAS',
     &       /,1X,'POUR L''OPERATION : ',A8,' AVEC TYPEXN = ',A1)
98          FORMAT(1X,'OM1311 (BIEF) : TYPEXM = ',A1,' DOES NOT GO ',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO 33 I=1,15
            CALL OV( 'X=X+CY  ' , XM(1,I) , XN(1,I) , Z , C , NELEM )
33        CONTINUE
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'M=M+N   ') THEN
C
        CALL OV( 'X=X+Y   ' , DM      , DN      , Z , C , NDIAG )
C
        IF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO I=1,15
            CALL OV( 'X=X+Y   ' , XM(1,I) , XN(1,I) , Z , C , NELEM )
          ENDDO
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'M=MD    ') THEN
C
C   DIAGONAL TERMS (DM AND D CAN HAVE DIFFERENT TYPES)
C
C
         IF(TYPDIM(1:1).EQ.'Q') THEN
           CALL OV( 'X=XY    ' , DM , D , Z , C , NDIAG )
         ELSEIF(TYPDIM(1:1).EQ.'I') THEN
           CALL OV( 'X=Y     ' , DM , D , Z , C , NDIAG )
           TYPDIM(1:1)='Q'
         ELSEIF(TYPDIM(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,12) TYPDIM(1:1)
           IF (LNG.EQ.2) WRITE(LU,13) TYPDIM(1:1)
12         FORMAT(1X,'OM1311 (BIEF) : TYPDIM INCONNU :',A1)
13         FORMAT(1X,'OM1311 (BIEF) : TYPDIM UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
         ENDIF
C
C   EXTRADIAGONAL TERMS
C
C
         IF(TYPEXM(1:1).EQ.'Q') THEN
C
         DO IELEM = 1 , NELEM
C
           XM(IELEM,  1) = XM(IELEM,  1) * D(IKLE(IELEM,2))
           XM(IELEM,  2) = XM(IELEM,  2) * D(IKLE(IELEM,3))
           XM(IELEM,  3) = XM(IELEM,  3) * D(IKLE(IELEM,1))
           XM(IELEM,  4) = XM(IELEM,  4) * D(IKLE(IELEM,3))
           XM(IELEM,  5) = XM(IELEM,  5) * D(IKLE(IELEM,1))
           XM(IELEM,  6) = XM(IELEM,  6) * D(IKLE(IELEM,2))
           XM(IELEM,  7) = XM(IELEM,  7) * D(IKLE(IELEM,1))
           XM(IELEM,  8) = XM(IELEM,  8) * D(IKLE(IELEM,2))
           XM(IELEM,  9) = XM(IELEM,  9) * D(IKLE(IELEM,3))
           XM(IELEM, 10) = XM(IELEM, 10) * D(IKLE(IELEM,1))
           XM(IELEM, 11) = XM(IELEM, 11) * D(IKLE(IELEM,2))
           XM(IELEM, 12) = XM(IELEM, 12) * D(IKLE(IELEM,3))
           XM(IELEM, 13) = XM(IELEM, 13) * D(IKLE(IELEM,1))
           XM(IELEM, 14) = XM(IELEM, 14) * D(IKLE(IELEM,2))
           XM(IELEM, 15) = XM(IELEM, 15) * D(IKLE(IELEM,3))
C
         ENDDO
C
         ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,162)
          IF (LNG.EQ.2) WRITE(LU,163)
          CALL PLANTE(1)
          STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'M=TN    ') THEN
C
C       RECOPIES EXTRADIAGONAL TERMS
C
         IF(TYPDIN(1:1).EQ.'Q') THEN
           CALL OV( 'X=Y     ' , DM , DN , Z , C , NDIAG )
         ELSEIF(TYPDIN(1:1).EQ.'I'.OR.TYPDIN(1:1).EQ.'0') THEN
C          NOTHING TO DO, ONLY NEEDS TO COPY TYPDIN
         ELSE
           IF (LNG.EQ.1) WRITE(LU,5) TYPDIN(1:1)
           IF (LNG.EQ.2) WRITE(LU,6) TYPDIN(1:1)
           CALL PLANTE(1)
           STOP
         ENDIF
         TYPDIM(1:1)=TYPDIN(1:1)
C
C        TRANSPOSES EXTRADIAGONAL TERMS
C
         IF(TYPEXM(1:1).EQ.'Q') THEN
C
         DO IELEM = 1 , NELEM
C
           XM(IELEM,  1) = XN(IELEM,  6)
           XM(IELEM,  2) = XN(IELEM, 11)
           XM(IELEM,  3) = XN(IELEM,  1)
           XM(IELEM,  4) = XN(IELEM, 12)
           XM(IELEM,  5) = XN(IELEM,  2)
           XM(IELEM,  6) = XN(IELEM,  7)
           XM(IELEM,  7) = XN(IELEM,  3)
           XM(IELEM,  8) = XN(IELEM,  8)
           XM(IELEM,  9) = XN(IELEM, 13)
           XM(IELEM, 10) = XN(IELEM,  4)
           XM(IELEM, 11) = XN(IELEM,  9)
           XM(IELEM, 12) = XN(IELEM, 14)
           XM(IELEM, 13) = XN(IELEM,  5)
           XM(IELEM, 14) = XN(IELEM, 10)
           XM(IELEM, 15) = XN(IELEM, 15)
C
         ENDDO
C
         ELSEIF(TYPEXM(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,162)
           IF (LNG.EQ.2) WRITE(LU,163)
           CALL PLANTE(1)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'M=DM    ') THEN
C
C   DIAGONAL TERMS
C
         IF(TYPDIM(1:1).EQ.'Q') THEN
           CALL OV( 'X=XY    ' , DM , D , Z , C , NDIAG )
         ELSEIF(TYPDIM(1:1).EQ.'I') THEN
           CALL OV( 'X=Y     ' , DM , D , Z , C , NDIAG )
           TYPDIM(1:1)='Q'
         ELSEIF(TYPDIM(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,12) TYPDIM(1:1)
           IF (LNG.EQ.2) WRITE(LU,13) TYPDIM(1:1)
           CALL PLANTE(1)
           STOP
         ENDIF
C
C   EXTRADIAGONAL TERMS
C
         IF(TYPEXM(1:1).EQ.'Q') THEN
C
         DO IELEM = 1 , NELEM
C
           XM(IELEM,  1) = XM(IELEM,  1) * D(IKLE(IELEM,1))
           XM(IELEM,  2) = XM(IELEM,  2) * D(IKLE(IELEM,1))
           XM(IELEM,  3) = XM(IELEM,  3) * D(IKLE(IELEM,2))
           XM(IELEM,  4) = XM(IELEM,  4) * D(IKLE(IELEM,2))
           XM(IELEM,  5) = XM(IELEM,  5) * D(IKLE(IELEM,3))
           XM(IELEM,  6) = XM(IELEM,  6) * D(IKLE(IELEM,3))
           XM(IELEM,  7) = XM(IELEM,  7) * D(IKLE(IELEM,4))
           XM(IELEM,  8) = XM(IELEM,  8) * D(IKLE(IELEM,4))
           XM(IELEM,  9) = XM(IELEM,  9) * D(IKLE(IELEM,4))
           XM(IELEM, 10) = XM(IELEM, 10) * D(IKLE(IELEM,5))
           XM(IELEM, 11) = XM(IELEM, 11) * D(IKLE(IELEM,5))
           XM(IELEM, 12) = XM(IELEM, 12) * D(IKLE(IELEM,5))
           XM(IELEM, 13) = XM(IELEM, 13) * D(IKLE(IELEM,6))
           XM(IELEM, 14) = XM(IELEM, 14) * D(IKLE(IELEM,6))
           XM(IELEM, 15) = XM(IELEM, 15) * D(IKLE(IELEM,6))
C
         ENDDO
C
         ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,162)
          IF (LNG.EQ.2) WRITE(LU,163)
162       FORMAT(1X,'OM1311 (BIEF) : TYPEXM NON PREVU : ',A1)
163       FORMAT(1X,'OM1311 (BIEF) : TYPEXM NOT AVAILABLE : ',A1)
          CALL PLANTE(1)
          STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'M=M+D   ') THEN
C
        IF(TYPDIM(1:1).EQ.'Q') THEN
          CALL OV( 'X=X+Y   ' , DM , D , Z , C , NDIAG )
        ELSE
          IF (LNG.EQ.1) WRITE(LU,12) TYPDIM(1:1)
          IF (LNG.EQ.2) WRITE(LU,13) TYPDIM(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'M=0     ') THEN
C
        CALL OV( 'X=C     ' , DM , Y , Z , 0.D0 , NDIAG )
C
        IF(TYPEXM(1:1).EQ.'Q') THEN
          DO I=1,15
            CALL OV( 'X=C     ' , XM(1,I) , Y , Z , 0.D0 , NELEM )
          ENDDO
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,710) TYPEXM(1:1)
          IF (LNG.EQ.2) WRITE(LU,711) TYPEXM(1:1)
710       FORMAT(1X,'OM11311 (BIEF) : TYPEXM INCONNU :',A1)
711       FORMAT(1X,'OM11311 (BIEF) : TYPEXM UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
C
        TYPDIM(1:1)='0'
C       TYPEXM(1:1) IS NOT CHANGED
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'M=MSK(M)') THEN
C
      IF(TYPEXM(1:1).EQ.'Q') THEN
        J = 15
      ELSEIF(TYPEXM(1:1).EQ.'0') THEN
        J = 0
      ELSE
        IF(LNG.EQ.1) WRITE(LU,710) TYPEXM
        IF(LNG.EQ.2) WRITE(LU,711) TYPEXM
        J = 0
        CALL PLANTE(1)
        STOP
      ENDIF
C
      IF(J.GT.0) THEN
         DO I = 1,J
            CALL OV ( 'X=XY    ' , XM(1,I) , D , Z , C , NELEM )
         ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,40) OP
        IF (LNG.EQ.2) WRITE(LU,41) OP
40      FORMAT(1X,'OM1311 (BIEF) : OPERATION INCONNUE : ',A8)
41      FORMAT(1X,'OM1311 (BIEF) : UNKNOWN OPERATION : ',A8)
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