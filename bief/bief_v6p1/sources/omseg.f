C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPERATIONS ON MATRICES WITH AN EDGE-BASED STORAGE.
!>  @code
!>   D: DIAGONAL MATRIX
!>   C: CONSTANT<br>
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON MATRICES M AND N, D AND C.<br>
!>   THE RESULT IS MATRIX M.<br>
!>      OP = 'M=N     '  : COPIES N IN M
!>      OP = 'M=CN    '  : MULTIPLIES N BY C
!>      OP = 'M=M+CN  '  : ADDS CN TO M
!>      OP = 'M=TN    '  : COPIES TRANSPOSE OF N IN M
!>      OP = 'M=M+TN  '  : ADDS TRANSPOSE(N) TO M
!>      OP = 'M=M+CTN '  : ADDS C TRANSPOSE(N) TO M
!>      OP = 'M=M+N   '  : ADDS N TO M
!>      OP = 'M=MD    '  : M X D
!>      OP = 'M=DM    '  : D X M
!>      OP = 'M=M-ND  '  : SUBTRACTS ND FROM M
!>      OP = 'M=M-DN  '  : SUBTRACTS DN FROM M
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

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, D, DM, DN, GLOSEG, NDIAG, NSEG1, NSEG2, OP, SIZGLO, TYPDIM, TYPDIN, TYPEXM, TYPEXN, XM, XN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DIMX, ISEG, Y, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_OMSEG
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
!>      <td><center> 5.6                                       </center>
!> </td><td> 29/12/05
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
!>          <tr><td>GLOSEG
!></td><td>---</td><td>
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
!>          <tr><td>NSEG1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>OPERATION A EFFECTUER
!>    </td></tr>
!>          <tr><td>SIZGLO
!></td><td>---</td><td>
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
                        SUBROUTINE OMSEG
     &(OP ,  DM,TYPDIM,XM,TYPEXM,   DN,TYPDIN,XN,TYPEXN,   D,C,
     & NDIAG,NSEG1,NSEG2,GLOSEG,SIZGLO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
C| D             |-->| MATRICE DIAGONALE
C| DM,TYPDIM      |<->| DIAGONALE ET TYPE DE DIAGONALE DE M
C| DN,TYPDIN      |-->| DIAGONALE ET TYPE DE DIAGONALE DE N
C| GLOSEG         |---| 
C| IKLE           |-->| CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
C| NDIAG          |-->| NOMBRE DE VALEURS DE LA DIAGONALE.
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NSEG1          |---| 
C| NSEG2          |---| 
C| OP             |-->| OPERATION A EFFECTUER
C| SIZGLO         |---| 
C| XM,TYPEXM      |-->| TERMES EXTRA-DIAG. ET TYPE POUR M
C| XN,TYPEXN      |-->| TERMES EXTRA-DIAG. ET TYPE POUR N
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_OMSEG => OMSEG
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NDIAG,NSEG1,NSEG2,SIZGLO
      INTEGER, INTENT(IN) :: GLOSEG(SIZGLO,2)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),D(*)
C     XM AND XN MAY ONLY BE OF SIZE NSEG1 IF THE MATRIX IS SYMMETRICAL
C     SIZE GIVEN HERE ONLY TO CHECK BOUNDS
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NSEG1+NSEG2)
      DOUBLE PRECISION, INTENT(IN)    :: XN(NSEG1+NSEG2)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIM,TYPEXM,TYPDIN,TYPEXN
      DOUBLE PRECISION, INTENT(INOUT) :: DM(*)
      DOUBLE PRECISION, INTENT(IN)    :: C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTRINSIC MIN
C
      INTEGER ISEG,DIMX
C
      DOUBLE PRECISION Y(1),Z(1)
C
C-----------------------------------------------------------------------
C
C     ARRAYS XM AND XN ARE BASICALLY OF SIZE XM(DIMX,1 OR 2)
C     BUT IN THE CASE OF RECTANGULAR MATRICES OTHER DATA ARE STORED
C     BEYOND XM(2*DIMX)
C
      DIMX=MIN(NSEG1,NSEG2)
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
5          FORMAT(1X,'OMSEG (BIEF) : TYPDIN INCONNU :',A1)
6          FORMAT(1X,'OMSEG (BIEF) : TYPDIN UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
        TYPDIM(1:1)=TYPDIN(1:1)
C
        IF(TYPEXN(1:1).EQ.'S') THEN
           CALL OV( 'X=Y     ' , XM , XN , Z , C , NSEG1 )
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
           CALL OV( 'X=Y     ' , XM , XN , Z , C , NSEG1+NSEG2 )
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
10         FORMAT(1X,'OMSEG (BIEF) : TYPEXN INCONNU :',A1)
11         FORMAT(1X,'OMSEG (BIEF) : TYPEXN UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
        TYPEXM(1:1)=TYPEXN(1:1)
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'CN    ') THEN
C
        CALL OV( 'X=CY    ' , DM , DN , Z , C , NDIAG )
C
        IF(TYPEXN(1:1).EQ.'S') THEN
           CALL OV( 'X=CY    ' , XM , XN , Z , C , NSEG1 )
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
           CALL OV( 'X=CY    ' , XM , XN , Z , C , NSEG1+NSEG2 )
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
      ELSEIF(OP(3:8).EQ.'M+CN  ' .OR.
     &      (OP(3:8).EQ.'M+CTN ').AND.TYPEXN(1:1).NE.'Q') THEN
C
        IF(TYPDIN(1:1).EQ.'I') THEN
          CALL OV( 'X=X+C   ' , DM , DN , Z , C , NDIAG )
        ELSEIF(TYPDIN(1:1).NE.'0') THEN
          CALL OV( 'X=X+CY  ' , DM , DN , Z , C , NDIAG )
        ENDIF
C
        IF(TYPEXN(1:1).EQ.'S') THEN
           CALL OV( 'X=X+CY  ' , XM , XN , Z , C , NSEG1 )
           IF(TYPEXM(1:1).EQ.'Q') THEN
             CALL OV( 'X=X+CY  ' , XM(DIMX+1:DIMX+NSEG1) ,XN,Z,C,NSEG1)
           ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
           IF(TYPEXM(1:1).NE.'Q') THEN
             IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
             IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
99          FORMAT(1X,'OMSEG (BIEF) : TYPEXM = ',A1,' NE CONVIENT PAS',
     &       /,1X,'POUR L''OPERATION : ',A8,' AVEC TYPEXN = ',A1)
98          FORMAT(1X,'OMSEG (BIEF) : TYPEXM = ',A1,' DOES NOT GO',
     &       /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
             CALL PLANTE(1)
             STOP
           ENDIF
           CALL OV( 'X=X+CY  ' , XM , XN , Z , C , NSEG1+NSEG2 )
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
           CALL PLANTE(1)
           STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'M+CTN ') THEN
C
C  THE CASES WHERE N IS SYMMETRICAL ARE TREATED WITH M=M+CN
C
        CALL OV( 'X=X+CY  ' , DM , DN , Z , C , NDIAG )
C
        IF(NSEG1.NE.NSEG2) THEN
          WRITE(LU,*) 'M+CTN : RECTANGULAR MATRIX NOT IMPLEMENTED'
          STOP
        ENDIF
        IF(TYPEXN(1:1).EQ.'Q') THEN
           IF(TYPEXM(1:1).NE.'Q') THEN
             IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
             IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
             CALL PLANTE(1)
             STOP
           ENDIF
           CALL OV( 'X=X+CY  ' , XM , XN(DIMX+1:DIMX+NSEG1) ,Z,C,NSEG1)
           CALL OV( 'X=X+CY  ' , XM(DIMX+1:DIMX+NSEG1) , XN ,Z,C,NSEG1)
        ELSE
           IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
           CALL PLANTE(1)
           STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'TN    ') THEN
C
        CALL OV( 'X=Y     ' , DM , DN , Z , C , NDIAG )
C
        IF(TYPEXN(1:1).EQ.'S') THEN
           CALL OV( 'X=Y     ' , XM , XN , Z , C , NSEG1 )
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
           IF(TYPEXM(1:1).NE.'S'.AND.NSEG1.NE.NSEG2) THEN
             WRITE(LU,*) 'TN : RECTANGULAR MATRIX NOT IMPLEMENTED'
             STOP
           ENDIF
           CALL OV( 'X=Y     ' , XM , XN(DIMX+1:DIMX+NSEG1) ,Z,C,NSEG1)
           CALL OV( 'X=Y     ' , XM(DIMX+1:DIMX+NSEG1) , XN ,Z,C,NSEG1)
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
           CALL PLANTE(1)
           STOP
        ENDIF
        TYPDIM(1:1)=TYPDIN(1:1)
        TYPEXM(1:1)=TYPEXN(1:1)
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'M+N   '.OR.
     &      (OP(3:8).EQ.'M+TN  ').AND.TYPEXN(1:1).NE.'Q') THEN
C
        CALL OV( 'X=X+Y   ' , DM , DN , Z , C , NDIAG )
C
        IF(TYPEXN(1:1).EQ.'S') THEN
           CALL OV( 'X=X+Y   ' , XM , XN , Z , C , NSEG1 )
           IF(TYPEXM(1:1).EQ.'Q') THEN
           CALL OV( 'X=X+Y   ' , XM(DIMX+1:DIMX+NSEG1) , XN ,Z,C,NSEG1)
           ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
           IF(TYPEXM(1:1).NE.'Q') THEN
             IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
             IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
             CALL PLANTE(1)
             STOP
           ENDIF
           CALL OV( 'X=X+Y   ' , XM , XN , Z , C , NSEG1+NSEG2 )
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
           CALL PLANTE(1)
           STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'M+TN  ') THEN
C
C     THE CASE WHERE N IS SYMMETRICAL HAS ALREADY BEEN TREATED
C
        CALL OV( 'X=X+Y   ' , DM , DN , Z , C , NDIAG )
C
        IF(NSEG1.NE.NSEG2) THEN
          WRITE(LU,*) 'M+TN : RECTANGULAR MATRIX NOT IMPLEMENTED'
          STOP
        ENDIF
        IF(TYPEXM(1:1).EQ.'Q') THEN
           CALL OV( 'X=X+Y   ' , XM , XN(DIMX+1:DIMX+NSEG1) ,Z,C,NSEG1)
           CALL OV( 'X=X+Y   ' , XM(DIMX+1:DIMX+NSEG1) , XN ,Z,C,NSEG1)
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
           CALL PLANTE(1)
           STOP
        ENDIF
        TYPDIM(1:1)=TYPDIN(1:1)
        TYPEXM(1:1)=TYPEXN(1:1)
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
         DO ISEG = 1 , MIN(NSEG1,NSEG2)
           XM(ISEG) = XM(ISEG) * D(GLOSEG(ISEG,2))
           XM(ISEG+DIMX) = XM(ISEG+DIMX) * D(GLOSEG(ISEG,1))
         ENDDO
         IF(NSEG1.GT.NSEG2) THEN
           DO ISEG = MIN(NSEG1,NSEG2)+1,MAX(NSEG1,NSEG2)
             XM(ISEG+DIMX)=XM(ISEG+DIMX)*D(GLOSEG(ISEG,1))
           ENDDO
         ELSEIF(NSEG2.GT.NSEG1) THEN
           DO ISEG = MIN(NSEG1,NSEG2)+1,MAX(NSEG1,NSEG2)
             XM(ISEG+DIMX)=XM(ISEG+DIMX)*D(GLOSEG(ISEG,2))
           ENDDO
         ENDIF
C
         ELSEIF(TYPEXM(1:1).EQ.'S') THEN
          IF (LNG.EQ.1) WRITE(LU,170)
          IF (LNG.EQ.2) WRITE(LU,171)
170       FORMAT(1X,'OMSEG (BIEF) : M=MD , M DOIT ETRE NON SYMETRIQUE')
171       FORMAT(1X,'OMSEG (BIEF) : M=MD , M MUST BE NON-SYMMETRIC')
          CALL PLANTE(1)
          STOP
         ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,172) TYPEXM(1:1)
          IF (LNG.EQ.2) WRITE(LU,173) TYPEXM(1:1)
172       FORMAT(1X,'OMSEG (BIEF) : TYPEXM NON PREVU : ',A1)
173       FORMAT(1X,'OMSEG (BIEF) : TYPEXM NOT AVAILABLE : ',A1)
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
         DO ISEG = 1 , MIN(NSEG1,NSEG2)
           XM(ISEG) = XM(ISEG) * D(GLOSEG(ISEG,1))
           XM(ISEG+DIMX) = XM(ISEG+DIMX) * D(GLOSEG(ISEG,2))
         ENDDO
         IF(NSEG1.GT.NSEG2) THEN
           DO ISEG = MIN(NSEG1,NSEG2)+1,MAX(NSEG1,NSEG2)
             XM(ISEG+DIMX)=XM(ISEG+DIMX)*D(GLOSEG(ISEG,2))
           ENDDO
         ELSEIF(NSEG2.GT.NSEG1) THEN
           DO ISEG = MIN(NSEG1,NSEG2)+1,MAX(NSEG1,NSEG2)
             XM(ISEG+DIMX)=XM(ISEG+DIMX)*D(GLOSEG(ISEG,1))
           ENDDO
         ENDIF
C
         ELSEIF(TYPEXM(1:1).EQ.'S') THEN
          IF (LNG.EQ.1) WRITE(LU,180)
          IF (LNG.EQ.2) WRITE(LU,181)
180       FORMAT(1X,'OMSEG (BIEF) : M=DM N''EST PAS SYMETRIQUE')
181       FORMAT(1X,'OMSEG (BIEF) : M=MD IS NOT SYMMETRIC')
          CALL PLANTE(1)
          STOP
         ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,172) TYPEXM(1:1)
          IF (LNG.EQ.2) WRITE(LU,173) TYPEXM(1:1)
          CALL PLANTE(1)
          STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'M-DN  ') THEN
C
C   DIAGONAL TERMS
C
         IF(TYPDIM(1:1).EQ.'Q') THEN
           CALL OV( 'X=X-YZ  ' , DM , DN , D , C , NDIAG )
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
           IF(TYPEXN(1:1).EQ.'Q') THEN
           DO ISEG = 1 , NSEG1
            XM(ISEG     )=XM(ISEG     )-XN(ISEG     )*D(GLOSEG(ISEG,1))
            XM(ISEG+DIMX)=XM(ISEG+DIMX)-XN(ISEG+DIMX)*D(GLOSEG(ISEG,2))
           ENDDO
           ELSEIF(TYPEXN(1:1).EQ.'S') THEN
           DO ISEG = 1 , NSEG1
            XM(ISEG     ) = XM(ISEG     ) - XN(ISEG) * D(GLOSEG(ISEG,1))
            XM(ISEG+DIMX) = XM(ISEG+DIMX) - XN(ISEG) * D(GLOSEG(ISEG,2))
           ENDDO
           ELSEIF(TYPEXN(1:1).NE.'0') THEN
            IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
            IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
            CALL PLANTE(1)
            STOP
           ENDIF
         ELSE
           IF (LNG.EQ.1) WRITE(LU,172) TYPEXM(1:1)
           IF (LNG.EQ.2) WRITE(LU,173) TYPEXM(1:1)
           CALL PLANTE(1)
           STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'M-ND  ') THEN
C
C   DIAGONAL TERMS
C
         IF(TYPDIM(1:1).EQ.'Q') THEN
           CALL OV( 'X=X-YZ  ' , DM , DN , D , C , NDIAG )
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
           IF(TYPEXN(1:1).EQ.'Q') THEN
           DO ISEG = 1 , NSEG1
            XM(ISEG     )=XM(ISEG     )-XN(ISEG     )*D(GLOSEG(ISEG,2))
            XM(ISEG+DIMX)=XM(ISEG+DIMX)-XN(ISEG+DIMX)*D(GLOSEG(ISEG,1))
           ENDDO
           ELSEIF(TYPEXN(1:1).EQ.'S') THEN
           DO ISEG = 1 , NSEG1
            XM(ISEG     ) = XM(ISEG     ) - XN(ISEG) * D(GLOSEG(ISEG,2))
            XM(ISEG+DIMX) = XM(ISEG+DIMX) - XN(ISEG) * D(GLOSEG(ISEG,1))
           ENDDO
           ELSEIF(TYPEXN(1:1).NE.'0') THEN
            IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
            IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
            CALL PLANTE(1)
            STOP
           ENDIF
         ELSE
           IF (LNG.EQ.1) WRITE(LU,172) TYPEXM(1:1)
           IF (LNG.EQ.2) WRITE(LU,173) TYPEXM(1:1)
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
12         FORMAT(1X,'OMSEG (BIEF) : TYPDIM INCONNU :',A1)
13         FORMAT(1X,'OMSEG (BIEF) : TYPDIM UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
         ENDIF
C
C   EXTRADIAGONAL TERMS
C
         IF(TYPEXM(1:1).EQ.'S') THEN
C
         DO ISEG = 1 , NSEG1
           XM(ISEG)=XM(ISEG)*D(GLOSEG(ISEG,1))*D(GLOSEG(ISEG,2))
         ENDDO
C
         ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
C
         DO ISEG = 1 , NSEG1
           XM(ISEG     )=XM(ISEG     )
     &                      *D(GLOSEG(ISEG,1))*D(GLOSEG(ISEG,2))
           XM(ISEG+DIMX)=XM(ISEG+DIMX)
     &                      *D(GLOSEG(ISEG,1))*D(GLOSEG(ISEG,2))
         ENDDO
C
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,20) TYPEXM(1:1)
           IF (LNG.EQ.2) WRITE(LU,21) TYPEXM(1:1)
20         FORMAT(1X,'OMSEG (BIEF) : TYPEXM INCONNU :',A1)
21         FORMAT(1X,'OMSEG (BIEF) : TYPEXM UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'M+D   ') THEN
C
        CALL OV( 'X=X+Y   ' , DM , D , Z , 0.D0 , NDIAG )
C       HERE THERE IS A DOUBT ABOUT TYPDIM
        TYPDIM(1:1)='Q'
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'0     ') THEN
C
        CALL OV( 'X=C     ' , DM , Y , Z , 0.D0 , NDIAG )
C
        IF(TYPEXM(1:1).EQ.'S') THEN
           CALL OV( 'X=C     ' , XM , Y , Z , 0.D0 , NSEG1 )
        ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
           CALL OV( 'X=C     ' , XM , Y , Z , 0.D0 , NSEG1+NSEG2 )
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,710) TYPEXM(1:1)
           IF (LNG.EQ.2) WRITE(LU,711) TYPEXM(1:1)
710        FORMAT(1X,'OMSEG (BIEF) : TYPEXM INCONNU :',A1)
711        FORMAT(1X,'OMSEG (BIEF) : TYPEXM UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
C       TYPDIM IS NOT CHANGED
C       TYPDIM(1:1)='0'
C       TYPEXM IS NOT CHANGED
C       TYPEXM(1:1)='0'
C-----------------------------------------------------------------------
C
      ELSEIF(OP(3:8).EQ.'X(M)  ') THEN
C
        IF(TYPEXM(1:1).EQ.'S') THEN
          CALL OV('X=Y     ',XM(DIMX+1:DIMX+NSEG1),
     &                       XM(     1:     NSEG1),Z,C,NSEG1)
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,810) TYPEXM(1:1)
          IF (LNG.EQ.2) WRITE(LU,811) TYPEXM(1:1)
810       FORMAT(1X,'OMSEG (BIEF) : MATRICE DEJA NON SYMETRIQUE : ',A1)
811       FORMAT(1X,'OMSEG (BIEF): MATRIX ALREADY NON SYMMETRICAL: ',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
        TYPEXM(1:1)='Q'
C
C-----------------------------------------------------------------------
C
C     ELSEIF(OP(3:8).EQ.'MSK(M)') THEN
C
C     IF(TYPEXM(1:1).EQ.'S') THEN
C       J = 3
C     ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
C       J = 6
C     ELSEIF(TYPEXM(1:1).EQ.'0') THEN
C       J = 0
C     ELSE
C       IF(LNG.EQ.1) WRITE(LU,710) TYPEXM
C       IF(LNG.EQ.2) WRITE(LU,711) TYPEXM
C       J=0
C       CALL PLANTE(1)
C       STOP
C     ENDIF
C
C     IF(J.GT.0) THEN
C        DO I = 1,J
C           CALL OV ( 'X=XY    ' , XM(1,I) , D , Z , C , NELEM )
C        ENDDO
C     ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,40) OP
        IF (LNG.EQ.2) WRITE(LU,41) OP
40      FORMAT(1X,'OMSEG (BIEF) : OPERATION INCONNUE : ',A8)
41      FORMAT(1X,'OMSEG (BIEF) : UNKNOWN OPERATION : ',A8)
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