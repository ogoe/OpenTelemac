C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPERATIONS ON MATRICES WITH P1 PRISMS
!>                OR ANY OTHER ELEMENT WITH THE SAME NUMBER OF POINTS.
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
!>      OP = 'M=DMD   '  : D X M X D
!>      OP = 'M=0     '  : SETS M TO 0
!>      OP = 'M=X(M)  '  : NOT SYMMETRICAL FORM OF M
!>                         (OLD MATSNS)
!>      OP = 'M=MSK(M)'  : MASKS M EXTRADIAGONAL TERMS
!>                         (OLD MASKEX)
!>                         THE MASK IS TAKEN FROM D
!>      OP = 'M=M+D   '  : ADDS D TO M
!>  @endcode
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @code
!>  CONVENTION FOR THE STORAGE OF EXTRA-DIAGONAL TERMS:
!>
!>     XA(IELEM, 1)  ---->  A(1,2)
!>     XA(IELEM, 2)  ---->  A(1,3)
!>     XA(IELEM, 3)  ---->  A(1,4)
!>     XA(IELEM, 4)  ---->  A(1,5)
!>     XA(IELEM, 5)  ---->  A(1,6)
!>     XA(IELEM, 6)  ---->  A(2,3)
!>     XA(IELEM, 7)  ---->  A(2,4)
!>     XA(IELEM, 8)  ---->  A(2,5)
!>     XA(IELEM, 9)  ---->  A(2,6)
!>     XA(IELEM,10)  ---->  A(3,4)
!>     XA(IELEM,11)  ---->  A(3,5)
!>     XA(IELEM,12)  ---->  A(3,6)
!>     XA(IELEM,13)  ---->  A(4,5)
!>     XA(IELEM,14)  ---->  A(4,6)
!>     XA(IELEM,15)  ---->  A(5,6)
!>     XA(IELEM,16)  ---->  A(2,1)
!>     XA(IELEM,17)  ---->  A(3,1)
!>     XA(IELEM,18)  ---->  A(4,1)
!>     XA(IELEM,19)  ---->  A(5,1)
!>     XA(IELEM,20)  ---->  A(6,1)
!>     XA(IELEM,21)  ---->  A(3,2)
!>     XA(IELEM,22)  ---->  A(4,2)
!>     XA(IELEM,23)  ---->  A(5,2)
!>     XA(IELEM,24)  ---->  A(6,2)
!>     XA(IELEM,25)  ---->  A(4,3)
!>     XA(IELEM,26)  ---->  A(5,3)
!>     XA(IELEM,27)  ---->  A(6,3)
!>     XA(IELEM,28)  ---->  A(5,4)
!>     XA(IELEM,29)  ---->  A(6,4)
!>     XA(IELEM,30)  ---->  A(6,5)
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
!>    </th><td> I, I1, I2, I3, I4, I5, I6, IELEM, J, Y, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_OM4141
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
!> </td><td> 09/07/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
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
                        SUBROUTINE OM4141
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
      USE BIEF, EX_OM4141 => OM4141
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
      INTEGER I,J,IELEM,I1,I2,I3,I4,I5,I6
C
      DOUBLE PRECISION Z(1),Y(1)
C
C-----------------------------------------------------------------------
C
      IF(OP(3:8).EQ.'N     ') THEN
C
        IF(TYPDIN(1:1).EQ.'Q') THEN
          CALL OV( 'X=Y     ' , DM , DN , Z , C , NDIAG )
        ELSEIF(TYPDIN(1:1).EQ.'I'.OR.TYPDIN(1:1).EQ.'0') THEN
C         NOTHING TO DO, ONLY NEEDS TO COPY TYPDIN
        ELSE
           IF (LNG.EQ.1) WRITE(LU,5) TYPDIN(1:1)
           IF (LNG.EQ.2) WRITE(LU,6) TYPDIN(1:1)
5          FORMAT(1X,'OM4141 (BIEF) : TYPDIN INCONNU :',A1)
6          FORMAT(1X,'OM4141 (BIEF) : TYPDIN UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
        TYPDIM(1:1)=TYPDIN(1:1)
C
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO 1 I=1,15
            CALL OV( 'X=Y     ' , XM(1,I) , XN(1,I) , Z , C , NELEM )
1         CONTINUE
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          DO 111 I=1,30
            CALL OV( 'X=Y     ' , XM(1,I) , XN(1,I) , Z , C , NELEM )
111       CONTINUE
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
10        FORMAT(1X,'OM4141 (BIEF) : TYPEXN INCONNU :',A1)
11        FORMAT(1X,'OM4141 (BIEF) : TYPEXN UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
C
        TYPEXM(1:1)=TYPEXN(1:1)
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'TN    ') THEN
C
        CALL OV( 'X=Y     ' , DM       , DN       , Z , C , NDIAG )
C
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO I=1,15
            CALL OV( 'X=Y     ' , XM(1,I) , XN(1,I) , Z , C , NELEM )
          ENDDO
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          DO I=1,15
            CALL OV( 'X=Y     ' , XM(1,I) , XN(1,I+15) , Z , C , NELEM )
            CALL OV( 'X=Y     ' , XM(1,I+15) , XN(1,I) , Z , C , NELEM )
          ENDDO
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
      ELSEIF(OP(3:8).EQ.'CN    ') THEN
C
        CALL OV( 'X=CY    ' , DM       , DN       , Z , C , NDIAG )
C
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO 2 I=1,15
            CALL OV( 'X=CY    ' , XM(1,I) , XN(1,I) , Z , C , NELEM )
2         CONTINUE
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          DO 22 I=1,30
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
      ELSEIF(OP(3:8).EQ.'M+CN  ') THEN
C
        IF(TYPDIN(1:1).EQ.'I') THEN
          CALL OV( 'X=X+C   ' , DM , DN , Z , C , NDIAG )
        ELSEIF(TYPDIN(1:1).NE.'0') THEN
          CALL OV( 'X=X+CY  ' , DM , DN , Z , C , NDIAG )
        ENDIF
C
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO 3 I=1,15
            CALL OV( 'X=X+CY  ' , XM(1,I) , XN(1,I) , Z , C , NELEM )
3         CONTINUE
          IF(TYPEXM(1:1).EQ.'Q') THEN
          DO 334 I=1,15
            CALL OV( 'X=X+CY  ' , XM(1,I+15) , XN(1,I) , Z , C , NELEM )
334       CONTINUE
          ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
99          FORMAT(1X,'OM4141 (BIEF) : TYPEXM = ',A1,' NE CONVIENT PAS',
     &      /,1X,'POUR L''OPERATION : ',A8,' AVEC TYPEXN = ',A1)
98          FORMAT(1X,'OM4141 (BIEF) : TYPEXM = ',A1,' DOES NOT GO',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO 33 I=1,30
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
      ELSEIF(OP(3:8).EQ.'M+N   '.OR.
     &      (OP(3:8).EQ.'M+TN  ').AND.TYPEXN(1:1).NE.'Q') THEN
C
        CALL OV( 'X=X+Y   ' , DM       , DN       , Z , C , NDIAG )
C
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO 4 I=1,15
            CALL OV( 'X=X+Y   ' , XM(1,I) , XN(1,I) , Z , C , NELEM )
4         CONTINUE
          IF(TYPEXM(1:1).EQ.'Q') THEN
          DO 444 I=1,15
           CALL OV( 'X=X+Y   ' , XM(1,I+15) , XN(1,I) , Z , C , NELEM )
444       CONTINUE
          ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO 44 I=1,30
            CALL OV( 'X=X+Y   ' , XM(1,I) , XN(1,I) , Z , C , NELEM )
44        CONTINUE
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'MD    ') THEN
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
         DO 50 IELEM = 1 , NELEM
C
           XM(IELEM, 1) = XM(IELEM, 1) * D(IKLE(IELEM,2))
           XM(IELEM, 2) = XM(IELEM, 2) * D(IKLE(IELEM,3))
           XM(IELEM, 3) = XM(IELEM, 3) * D(IKLE(IELEM,4))
           XM(IELEM, 4) = XM(IELEM, 4) * D(IKLE(IELEM,5))
           XM(IELEM, 5) = XM(IELEM, 5) * D(IKLE(IELEM,6))
           XM(IELEM, 6) = XM(IELEM, 6) * D(IKLE(IELEM,3))
           XM(IELEM, 7) = XM(IELEM, 7) * D(IKLE(IELEM,4))
           XM(IELEM, 8) = XM(IELEM, 8) * D(IKLE(IELEM,5))
           XM(IELEM, 9) = XM(IELEM, 9) * D(IKLE(IELEM,6))
           XM(IELEM,10) = XM(IELEM,10) * D(IKLE(IELEM,4))
           XM(IELEM,11) = XM(IELEM,11) * D(IKLE(IELEM,5))
           XM(IELEM,12) = XM(IELEM,12) * D(IKLE(IELEM,6))
           XM(IELEM,13) = XM(IELEM,13) * D(IKLE(IELEM,5))
           XM(IELEM,14) = XM(IELEM,14) * D(IKLE(IELEM,6))
           XM(IELEM,15) = XM(IELEM,15) * D(IKLE(IELEM,6))
C
           XM(IELEM,16) = XM(IELEM,16) * D(IKLE(IELEM,1))
           XM(IELEM,17) = XM(IELEM,17) * D(IKLE(IELEM,1))
           XM(IELEM,18) = XM(IELEM,18) * D(IKLE(IELEM,1))
           XM(IELEM,19) = XM(IELEM,19) * D(IKLE(IELEM,1))
           XM(IELEM,20) = XM(IELEM,20) * D(IKLE(IELEM,1))
           XM(IELEM,21) = XM(IELEM,21) * D(IKLE(IELEM,2))
           XM(IELEM,22) = XM(IELEM,22) * D(IKLE(IELEM,2))
           XM(IELEM,23) = XM(IELEM,23) * D(IKLE(IELEM,2))
           XM(IELEM,24) = XM(IELEM,24) * D(IKLE(IELEM,2))
           XM(IELEM,25) = XM(IELEM,25) * D(IKLE(IELEM,3))
           XM(IELEM,26) = XM(IELEM,26) * D(IKLE(IELEM,3))
           XM(IELEM,27) = XM(IELEM,27) * D(IKLE(IELEM,3))
           XM(IELEM,28) = XM(IELEM,28) * D(IKLE(IELEM,4))
           XM(IELEM,29) = XM(IELEM,29) * D(IKLE(IELEM,4))
           XM(IELEM,30) = XM(IELEM,30) * D(IKLE(IELEM,5))
C
50       CONTINUE
C
         ELSEIF(TYPEXM(1:1).EQ.'S') THEN
          IF (LNG.EQ.1) WRITE(LU,150)
          IF (LNG.EQ.2) WRITE(LU,151)
150       FORMAT(1X,'OM4141 (BIEF) : M=MD A ECRIRE SI M SYMETRIQUE')
151       FORMAT(1X,
     &    'OM4141 (BIEF) : M=MD NOT AVAILABLE IF M SYMMETRIC')
          CALL PLANTE(1)
          STOP
         ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,190)
          IF (LNG.EQ.2) WRITE(LU,200)
          CALL PLANTE(1)
          STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'DM    ') THEN
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
         DO 60 IELEM = 1 , NELEM
C
           XM(IELEM, 1) = XM(IELEM, 1) * D(IKLE(IELEM,1))
           XM(IELEM, 2) = XM(IELEM, 2) * D(IKLE(IELEM,1))
           XM(IELEM, 3) = XM(IELEM, 3) * D(IKLE(IELEM,1))
           XM(IELEM, 4) = XM(IELEM, 4) * D(IKLE(IELEM,1))
           XM(IELEM, 5) = XM(IELEM, 5) * D(IKLE(IELEM,1))
           XM(IELEM, 6) = XM(IELEM, 6) * D(IKLE(IELEM,2))
           XM(IELEM, 7) = XM(IELEM, 7) * D(IKLE(IELEM,2))
           XM(IELEM, 8) = XM(IELEM, 8) * D(IKLE(IELEM,2))
           XM(IELEM, 9) = XM(IELEM, 9) * D(IKLE(IELEM,2))
           XM(IELEM,10) = XM(IELEM,10) * D(IKLE(IELEM,3))
           XM(IELEM,11) = XM(IELEM,11) * D(IKLE(IELEM,3))
           XM(IELEM,12) = XM(IELEM,12) * D(IKLE(IELEM,3))
           XM(IELEM,13) = XM(IELEM,13) * D(IKLE(IELEM,4))
           XM(IELEM,14) = XM(IELEM,14) * D(IKLE(IELEM,4))
           XM(IELEM,15) = XM(IELEM,15) * D(IKLE(IELEM,5))
C
           XM(IELEM,16) = XM(IELEM,16) * D(IKLE(IELEM,2))
           XM(IELEM,17) = XM(IELEM,17) * D(IKLE(IELEM,3))
           XM(IELEM,18) = XM(IELEM,18) * D(IKLE(IELEM,4))
           XM(IELEM,19) = XM(IELEM,19) * D(IKLE(IELEM,5))
           XM(IELEM,20) = XM(IELEM,20) * D(IKLE(IELEM,6))
           XM(IELEM,21) = XM(IELEM,21) * D(IKLE(IELEM,3))
           XM(IELEM,22) = XM(IELEM,22) * D(IKLE(IELEM,4))
           XM(IELEM,23) = XM(IELEM,23) * D(IKLE(IELEM,5))
           XM(IELEM,24) = XM(IELEM,24) * D(IKLE(IELEM,6))
           XM(IELEM,25) = XM(IELEM,25) * D(IKLE(IELEM,4))
           XM(IELEM,26) = XM(IELEM,26) * D(IKLE(IELEM,5))
           XM(IELEM,27) = XM(IELEM,27) * D(IKLE(IELEM,6))
           XM(IELEM,28) = XM(IELEM,28) * D(IKLE(IELEM,5))
           XM(IELEM,29) = XM(IELEM,29) * D(IKLE(IELEM,6))
           XM(IELEM,30) = XM(IELEM,30) * D(IKLE(IELEM,6))
C
60       CONTINUE
C
         ELSEIF(TYPEXM(1:1).EQ.'S') THEN
          IF (LNG.EQ.1) WRITE(LU,160)
          IF (LNG.EQ.2) WRITE(LU,161)
160       FORMAT(1X,'OM4141 (BIEF) : M=DM A ECRIRE SI M SYMETRIQUE')
161       FORMAT(1X,
     &    'OM4141 (BIEF) : M=MD NOT AVAILABLE IF M SYMMETRIC')
          CALL PLANTE(1)
          STOP
         ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,190)
          IF (LNG.EQ.2) WRITE(LU,200)
190       FORMAT(1X,'OM4141 (BIEF) : TYPEXM NON PREVU : ',A1)
200       FORMAT(1X,'OM4141 (BIEF) : TYPEXM NOT AVAILABLE : ',A1)
          CALL PLANTE(1)
          STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'DMD   ') THEN
C
C   DIAGONAL TERMS
C
        IF(TYPDIM(1:1).EQ.'Q') THEN
           CALL OV( 'X=XY    ' , DM , D , Z , C , NDIAG )
           CALL OV( 'X=XY    ' , DM , D , Z , C , NDIAG )
        ELSEIF(TYPDIM(1:1).EQ.'I') THEN
           CALL OV( 'X=YZ    ' , DM , D , D , C , NDIAG )
           TYPDIM(1:1)='Q'
        ELSEIF(TYPDIM(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,12) TYPDIM(1:1)
           IF (LNG.EQ.2) WRITE(LU,13) TYPDIM(1:1)
12         FORMAT(1X,'OM4141 (BIEF) : TYPDIM INCONNU :',A1)
13         FORMAT(1X,'OM4141 (BIEF) : TYPDIM UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
C
C   EXTRADIAGONAL TERMS
C
        IF(TYPEXM(1:1).EQ.'S') THEN
C
         DO 65 IELEM = 1 , NELEM
C
           I1 = IKLE(IELEM,1)
           I2 = IKLE(IELEM,2)
           I3 = IKLE(IELEM,3)
           I4 = IKLE(IELEM,4)
           I5 = IKLE(IELEM,5)
           I6 = IKLE(IELEM,6)
C
           XM(IELEM, 1) =  XM(IELEM, 1) * D(I2) * D(I1)
           XM(IELEM, 2) =  XM(IELEM, 2) * D(I3) * D(I1)
           XM(IELEM, 3) =  XM(IELEM, 3) * D(I4) * D(I1)
           XM(IELEM, 4) =  XM(IELEM, 4) * D(I5) * D(I1)
           XM(IELEM, 5) =  XM(IELEM, 5) * D(I6) * D(I1)
           XM(IELEM, 6) =  XM(IELEM, 6) * D(I3) * D(I2)
           XM(IELEM, 7) =  XM(IELEM, 7) * D(I4) * D(I2)
           XM(IELEM, 8) =  XM(IELEM, 8) * D(I5) * D(I2)
           XM(IELEM, 9) =  XM(IELEM, 9) * D(I6) * D(I2)
           XM(IELEM,10) =  XM(IELEM,10) * D(I4) * D(I3)
           XM(IELEM,11) =  XM(IELEM,11) * D(I5) * D(I3)
           XM(IELEM,12) =  XM(IELEM,12) * D(I6) * D(I3)
           XM(IELEM,13) =  XM(IELEM,13) * D(I5) * D(I4)
           XM(IELEM,14) =  XM(IELEM,14) * D(I6) * D(I4)
           XM(IELEM,15) =  XM(IELEM,15) * D(I6) * D(I5)
C
65       CONTINUE
C
        ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
C
         DO 66 IELEM = 1 , NELEM
C
           I1 = IKLE(IELEM,1)
           I2 = IKLE(IELEM,2)
           I3 = IKLE(IELEM,3)
           I4 = IKLE(IELEM,4)
           I5 = IKLE(IELEM,5)
           I6 = IKLE(IELEM,6)
C
           XM(IELEM, 1) =  XM(IELEM, 1) * D(I2) * D(I1)
           XM(IELEM, 2) =  XM(IELEM, 2) * D(I3) * D(I1)
           XM(IELEM, 3) =  XM(IELEM, 3) * D(I4) * D(I1)
           XM(IELEM, 4) =  XM(IELEM, 4) * D(I5) * D(I1)
           XM(IELEM, 5) =  XM(IELEM, 5) * D(I6) * D(I1)
           XM(IELEM, 6) =  XM(IELEM, 6) * D(I3) * D(I2)
           XM(IELEM, 7) =  XM(IELEM, 7) * D(I4) * D(I2)
           XM(IELEM, 8) =  XM(IELEM, 8) * D(I5) * D(I2)
           XM(IELEM, 9) =  XM(IELEM, 9) * D(I6) * D(I2)
           XM(IELEM,10) =  XM(IELEM,10) * D(I4) * D(I3)
           XM(IELEM,11) =  XM(IELEM,11) * D(I5) * D(I3)
           XM(IELEM,12) =  XM(IELEM,12) * D(I6) * D(I3)
           XM(IELEM,13) =  XM(IELEM,13) * D(I5) * D(I4)
           XM(IELEM,14) =  XM(IELEM,14) * D(I6) * D(I4)
           XM(IELEM,15) =  XM(IELEM,15) * D(I6) * D(I5)
           XM(IELEM,16) =  XM(IELEM,16) * D(I2) * D(I1)
           XM(IELEM,17) =  XM(IELEM,17) * D(I3) * D(I1)
           XM(IELEM,18) =  XM(IELEM,18) * D(I4) * D(I1)
           XM(IELEM,19) =  XM(IELEM,19) * D(I5) * D(I1)
           XM(IELEM,20) =  XM(IELEM,20) * D(I6) * D(I1)
           XM(IELEM,21) =  XM(IELEM,21) * D(I3) * D(I2)
           XM(IELEM,22) =  XM(IELEM,22) * D(I4) * D(I2)
           XM(IELEM,23) =  XM(IELEM,23) * D(I5) * D(I2)
           XM(IELEM,24) =  XM(IELEM,24) * D(I6) * D(I2)
           XM(IELEM,25) =  XM(IELEM,25) * D(I4) * D(I3)
           XM(IELEM,26) =  XM(IELEM,26) * D(I5) * D(I3)
           XM(IELEM,27) =  XM(IELEM,27) * D(I6) * D(I3)
           XM(IELEM,28) =  XM(IELEM,28) * D(I5) * D(I4)
           XM(IELEM,29) =  XM(IELEM,29) * D(I6) * D(I4)
           XM(IELEM,30) =  XM(IELEM,30) * D(I6) * D(I5)
C
66       CONTINUE
C
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,20) TYPEXM(1:1)
           IF (LNG.EQ.2) WRITE(LU,21) TYPEXM(1:1)
20         FORMAT(1X,'OM4141 (BIEF) : TYPEXM INCONNU :',A1)
21         FORMAT(1X,'OM4141 (BIEF) : TYPEXM UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
       ELSEIF(OP(3:8).EQ.'0     ') THEN
C
        PRINT*,'OM4141 M=0'
        CALL OV( 'X=C     ' , DM , Y , Z , 0.D0 , NDIAG )
C
        IF(TYPEXM(1:1).EQ.'S') THEN
           CALL OV( 'X=C     ' , XM(1,1 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,2 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,3 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,4 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,5 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,6 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,7 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,8 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,9 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,10) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,11) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,12) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,13) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,14) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,15) , Y , Z , 0.D0 , NELEM )
        ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
           CALL OV( 'X=C     ' , XM(1,1 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,2 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,3 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,4 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,5 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,6 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,7 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,8 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,9 ) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,10) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,11) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,12) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,13) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,14) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,15) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,16) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,17) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,18) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,19) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,20) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,21) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,22) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,23) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,24) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,25) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,26) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,27) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,28) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,29) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,30) , Y , Z , 0.D0 , NELEM )

        ELSEIF(TYPEXM(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,710) TYPEXM(1:1)
           IF (LNG.EQ.2) WRITE(LU,711) TYPEXM(1:1)
710        FORMAT(1X,'OM4141 (BIEF) : TYPEXM INCONNU :',A1)
711        FORMAT(1X,'OM4141 (BIEF) : TYPEXM UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
C       TYPDIM IS NOT CHANGED
C        TYPDIM(1:1)='0'
C       TYPEXM IS NOT CHANGED
C        TYPEXM(1:1)='0'
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'X(M)  ') THEN
C
        IF(TYPEXM(1:1).EQ.'S') THEN
          CALL OV( 'X=Y     ' , XM(1,16) , XM(1, 1) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,17) , XM(1, 2) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,18) , XM(1, 3) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,19) , XM(1, 4) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,20) , XM(1, 5) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,21) , XM(1, 6) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,22) , XM(1, 7) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,23) , XM(1, 8) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,24) , XM(1, 9) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,25) , XM(1,10) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,26) , XM(1,11) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,27) , XM(1,12) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,28) , XM(1,13) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,29) , XM(1,14) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,30) , XM(1,15) , Z , C , NELEM )
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,810) TYPEXM(1:1)
          IF (LNG.EQ.2) WRITE(LU,811) TYPEXM(1:1)
810       FORMAT(1X,'OM4141 (BIEF) : MATRICE DEJA NON SYMETRIQUE : ',A1)
811       FORMAT(1X,'OM4141 (BIEF) : MATRIX ALREADY NON SYMMETRICAL: ',
     &           A1)
          CALL PLANTE(1)
          STOP
        ENDIF
        TYPEXM(1:1)='Q'
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'MSK(M)') THEN
C
      IF(TYPEXM(1:1).EQ.'S') THEN
        J = 15
      ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
        J = 30
      ELSEIF(TYPEXM(1:1).EQ.'0') THEN
        J = 0
      ELSE
        IF(LNG.EQ.1) WRITE(LU,190) TYPEXM
        IF(LNG.EQ.2) WRITE(LU,200) TYPEXM
        J = 0
        CALL PLANTE(1)
        STOP
      ENDIF
C
      IF(J.GT.0) THEN
         DO 30 I = 1,J
            CALL OV ( 'X=XY    ' , XM(1,I) , D , Z , C , NELEM )
30       CONTINUE
      ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'M+D   ') THEN
C
        CALL OV( 'X=X+Y   ' , DM , D , Z , C , NDIAG )
C       HERE THERE IS A DOUBT ABOUT TYPDIM
        TYPDIM(1:1)='Q'
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,40) OP
        IF (LNG.EQ.2) WRITE(LU,41) OP
40      FORMAT(1X,'OM4141 (BIEF) : OPERATION INCONNUE : ',A8)
41      FORMAT(1X,'OM4141 (BIEF) : UNKNOWN OPERATION : ',A8)
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