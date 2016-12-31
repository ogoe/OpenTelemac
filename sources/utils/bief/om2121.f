!                    *****************
                     SUBROUTINE OM2121
!                    *****************
!
     &(OP ,  DM,TYPDIM,XM,TYPEXM,   DN,TYPDIN,XN,TYPEXN,   D,C,
     & IKLE,NELEM,NELMAX,NDIAG)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON MATRICES WITH Q1 QUADRILATERAL
!+                OR ANY OTHER ELEMENT WITH THE SAME NUMBER OF POINTS.
!code
!+   D: DIAGONAL MATRIX
!+   C: CONSTANT
!+
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON MATRICES M AND N, D AND C.
!+
!+   THE RESULT IS MATRIX M.
!+
!+      OP = 'M=N     '  : COPIES N IN M
!+      OP = 'M=TN    '  : COPIES TRANSPOSE OF N IN M
!+      OP = 'M=CN    '  : MULTIPLIES N BY C
!+      OP = 'M=M+CN  '  : ADDS CN TO M
!+      OP = 'M=M+CTN '  : ADDS C TRANSPOSE(N) TO M
!+      OP = 'M=M+TN  '  : ADDS TRANSPOSE(N) TO M
!+      OP = 'M=M+N   '  : ADDS N TO M
!+      OP = 'M=MD    '  : M X D
!+      OP = 'M=DM    '  : D X M
!+      OP = 'M=DMD   '  : D X M X D
!+      OP = 'M=M+D   '  : ADDS D TO M
!+      OP = 'M=X(M)  '  : NOT SYMMETRICAL FORM OF M
!+                         (OLD MATSNS)
!+      OP = 'M=MSK(M)'  : MASKS M EXTRADIAGONAL TERMS
!+                         (OLD MASKEX)
!+                         THE MASK IS TAKEN FROM D
!
!code
!+  CONVENTION FOR THE STORAGE OF EXTRA-DIAGONAL TERMS:
!+
!+     XM(IELEM, 1)  ---->  M(1,2)
!+     XM(IELEM, 2)  ---->  M(1,3)
!+     XM(IELEM, 3)  ---->  M(1,4)
!+     XM(IELEM, 4)  ---->  M(2,3)
!+     XM(IELEM, 5)  ---->  M(2,4)
!+     XM(IELEM, 6)  ---->  M(3,4)
!+     XM(IELEM, 7)  ---->  M(2,1)
!+     XM(IELEM, 8)  ---->  M(3,1)
!+     XM(IELEM, 9)  ---->  M(4,1)
!+     XM(IELEM,10)  ---->  M(3,2)
!+     XM(IELEM,11)  ---->  M(4,2)
!+     XM(IELEM,12)  ---->  M(4,3)
!
!history  J-M HERVOUET (LNHE)     ; F  LEPEINTRE (LNH)
!+        21/09/93
!+        V5P6
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| A GIVEN CONSTANT USED IN OPERATION OP
!| D              |-->| A DIAGONAL MATRIX
!| DM             |<->| DIAGONAL OF M
!| DN             |-->| DIAGONAL OF N
!| IKLE           |-->| CONNECTIVITY TABLE.
!| NDIAG          |-->| NUMBER OF TERMS IN THE DIAGONAL
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| OP             |-->| OPERATION TO BE DONE (SEE ABOVE)
!| TYPDIM         |<->| TYPE OF DIAGONAL OF M:
!|                |   | TYPDIM = 'Q' : ANY VALUE
!|                |   | TYPDIM = 'I' : IDENTITY
!|                |   | TYPDIM = '0' : ZERO
!| TYPDIN         |<->| TYPE OF DIAGONAL OF N:
!|                |   | TYPDIN = 'Q' : ANY VALUE
!|                |   | TYPDIN = 'I' : IDENTITY
!|                |   | TYPDIN = '0' : ZERO
!| TYPEXM         |-->| TYPE OF OFF-DIAGONAL TERMS OF M:
!|                |   | TYPEXM = 'Q' : ANY VALUE
!|                |   | TYPEXM = 'S' : SYMMETRIC
!|                |   | TYPEXM = '0' : ZERO
!| TYPEXN         |-->| TYPE OF OFF-DIAGONAL TERMS OF N:
!|                |   | TYPEXN = 'Q' : ANY VALUE
!|                |   | TYPEXN = 'S' : SYMMETRIC
!|                |   | TYPEXN = '0' : ZERO
!| XM             |-->| OFF-DIAGONAL TERMS OF M
!| XN             |-->| OFF-DIAGONAL TERMS OF N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_OM2121 => OM2121
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NDIAG
      INTEGER, INTENT(IN) :: IKLE(NELMAX,4)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),D(*),XN(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DM(*),XM(NELMAX,*)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIM,TYPEXM,TYPDIN,TYPEXN
      DOUBLE PRECISION, INTENT(IN)    :: C
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,IELEM
!
!-----------------------------------------------------------------------
!
      IF(OP(3:8).EQ.'N     ') THEN
!
        IF(TYPDIN(1:1).EQ.'Q') THEN
          CALL OV( 'X=Y     ' , DM , DN , DN , C , NDIAG )
        ELSEIF(TYPDIN(1:1).EQ.'I'.OR.TYPDIN(1:1).EQ.'0') THEN
!         NOTHING TO DO, ONLY NEEDS TO COPY TYPDIN
        ELSE
           IF (LNG.EQ.1) WRITE(LU,5) TYPDIN(1:1)
           IF (LNG.EQ.2) WRITE(LU,6) TYPDIN(1:1)
5          FORMAT(1X,'OM2121 (BIEF) : TYPDIN INCONNU :',A1)
6          FORMAT(1X,'OM2121 (BIEF) : TYPDIN UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
        TYPDIM(1:1)=TYPDIN(1:1)
!
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO I=1,6
            CALL OV( 'X=Y     ' , XM(1,I) , XN(1,I) , XN , C , NELEM )
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          DO I=1,12
            CALL OV( 'X=Y     ' , XM(1,I) , XN(1,I) , XN , C , NELEM )
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,30) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,40) TYPEXN(1:1)
30        FORMAT(1X,'OM2121 (BIEF) : TYPEXN INCONNU :',A1)
40        FORMAT(1X,'OM2121 (BIEF) : TYPEXN UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        TYPEXM(1:1)=TYPEXN(1:1)
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'TN    ') THEN
!
        CALL OV( 'X=Y     ' , DM , DN , DN , C , NDIAG )
!
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO I=1,6
            CALL OV( 'X=Y     ' , XM(1,I) , XN(1,I) , XN , C , NELEM )
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          DO I=1,6
            CALL OV( 'X=Y     ' , XM(1,I) , XN(1,I+6) , XN , C , NELEM )
            CALL OV( 'X=Y     ' , XM(1,I+6) , XN(1,I) , XN , C , NELEM )
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,30) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,40) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        TYPDIM(1:1)=TYPDIN(1:1)
        TYPEXM(1:1)=TYPEXN(1:1)
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'CN    ') THEN
!
        CALL OV( 'X=CY    ' , DM , DN , DN , C , NDIAG )
!
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO I=1,6
            CALL OV( 'X=CY    ' , XM(1,I) , XN(1,I) , XN , C , NELEM )
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          DO I=1,12
            CALL OV( 'X=CY    ' , XM(1,I) , XN(1,I) , XN , C , NELEM )
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,30) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,40) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        TYPDIM(1:1)=TYPDIN(1:1)
        TYPEXM(1:1)=TYPEXN(1:1)
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+CN  ' .OR.
     &      (OP(3:8).EQ.'M+CTN '.AND.TYPEXN(1:1).NE.'Q') ) THEN
!
        IF(TYPDIN(1:1).EQ.'I') THEN
          CALL OV( 'X=X+C   ' , DM , DN , DN , C , NDIAG )
        ELSEIF(TYPDIN(1:1).NE.'0') THEN
          CALL OV( 'X=X+CY  ' , DM , DN , DN , C , NDIAG )
        ENDIF
!
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO I=1,6
            CALL OV( 'X=X+CY  ' , XM(1,I) , XN(1,I) , XN , C , NELEM )
          ENDDO ! I
          IF(TYPEXM(1:1).EQ.'Q') THEN
          DO I=1,6
            CALL OV( 'X=X+CY  ' , XM(1,I+6) , XN(1,I) , XN , C ,NELEM)
          ENDDO ! I
          ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
99         FORMAT(1X,'OM2121 (BIEF) : TYPEXM = ',A1,' NE CONVIENT PAS',
     &      /,1X,'POUR L''OPERATION : ',A8,' AVEC TYPEXN = ',A1)
98         FORMAT(1X,'OM2121 (BIEF) : TYPEXM = ',A1,' DOES NOT GO ',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO I=1,12
            CALL OV( 'X=X+CY  ' , XM(1,I) , XN(1,I) , XN , C , NELEM )
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,30) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,40) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+CTN ') THEN
!
!  THE CASES WHERE N IS SYMMETRICAL ARE TREATED WITH M=M+CN
!
        CALL OV( 'X=X+CY  ' , DM , DN , DN , C , NDIAG )
!
        IF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO I=1,6
            CALL OV( 'X=X+CY  ' , XM(1,I)  , XN(1,I+6) , XN ,C, NELEM )
            CALL OV( 'X=X+CY  ' , XM(1,I+6), XN(1,I  ) , XN ,C, NELEM )
          ENDDO ! I
        ELSE
          IF (LNG.EQ.1) WRITE(LU,30) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,40) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+N   '.OR.
     &      (OP(3:8).EQ.'M+TN  ').AND.TYPEXN(1:1).NE.'Q') THEN
!
        CALL OV( 'X=X+Y   ' , DM , DN , DN , C , NDIAG )
!
        IF(TYPEXN(1:1).EQ.'S') THEN
          DO I=1,6
            CALL OV( 'X=X+Y   ' , XM(1,I) , XN(1,I) , XN , C , NELEM )
          ENDDO ! I
          IF(TYPEXM(1:1).EQ.'Q') THEN
           DO I=1,6
            CALL OV( 'X=X+Y   ' , XM(1,I+6) , XN(1,I) , XN ,C, NELEM )
           ENDDO ! I
          ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO I=1,12
            CALL OV( 'X=X+Y   ' , XM(1,I) , XN(1,I) , XN , C , NELEM )
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,30) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,40) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+TN  ') THEN
!
!  THE CASES WHERE N IS SYMMETRICAL ARE TREATED WITH M=M+N
!
        CALL OV( 'X=X+Y   ' , DM , DN , DN , C , NDIAG )
!
        IF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO I=1,6
            CALL OV( 'X=X+Y   ' , XM(1,I)  , XN(1,I+6) , XN,C , NELEM )
            CALL OV( 'X=X+Y   ' , XM(1,I+6), XN(1,I  ) , XN,C , NELEM )
           ENDDO ! I
        ELSE
          IF (LNG.EQ.1) WRITE(LU,30) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,40) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'MD    ') THEN
!
!   DIAGONAL TERMS
!
        IF(TYPDIM(1:1).EQ.'Q') THEN
          CALL OV( 'X=XY    ' , DM , D , D , C , NDIAG )
        ELSEIF(TYPDIM(1:1).EQ.'I') THEN
          CALL OV( 'X=Y     ' , DM , D , D , C , NDIAG )
          TYPDIM(1:1)='Q'
        ELSEIF(TYPDIM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,12) TYPDIM(1:1)
          IF (LNG.EQ.2) WRITE(LU,13) TYPDIM(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!   EXTRADIAGONAL TERMS
!
!
        IF(TYPEXM(1:1).EQ.'Q') THEN
!
        DO IELEM = 1 , NELEM
!
          XM(IELEM, 1) = XM(IELEM, 1) * D(IKLE(IELEM,2))
          XM(IELEM, 2) = XM(IELEM, 2) * D(IKLE(IELEM,3))
          XM(IELEM, 3) = XM(IELEM, 3) * D(IKLE(IELEM,4))
!
          XM(IELEM, 4) = XM(IELEM, 4) * D(IKLE(IELEM,3))
          XM(IELEM, 5) = XM(IELEM, 5) * D(IKLE(IELEM,4))
          XM(IELEM, 6) = XM(IELEM, 6) * D(IKLE(IELEM,4))
!
          XM(IELEM, 7) = XM(IELEM, 7) * D(IKLE(IELEM,1))
          XM(IELEM, 8) = XM(IELEM, 8) * D(IKLE(IELEM,1))
          XM(IELEM, 9) = XM(IELEM, 9) * D(IKLE(IELEM,1))
!
          XM(IELEM,10) = XM(IELEM,10) * D(IKLE(IELEM,2))
          XM(IELEM,11) = XM(IELEM,11) * D(IKLE(IELEM,2))
          XM(IELEM,12) = XM(IELEM,12) * D(IKLE(IELEM,3))
!
        ENDDO ! IELEM
!
        ELSEIF(TYPEXM(1:1).EQ.'S') THEN
          IF (LNG.EQ.1) WRITE(LU,170)
          IF (LNG.EQ.2) WRITE(LU,180)
170       FORMAT(1X,'OM2121 (BIEF) : M=MD A ECRIRE SI M SYMETRIQUE')
180       FORMAT(1X,
     &    'OM2121 (BIEF) : M=MD NOT AVAILABLE IF M SYMMETRIC')
          CALL PLANTE(1)
          STOP
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,190)
          IF (LNG.EQ.2) WRITE(LU,200)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'DM    ') THEN
!
!   DIAGONAL TERMS
!
        IF(TYPDIM(1:1).EQ.'Q') THEN
          CALL OV( 'X=XY    ' , DM , D , D , C , NDIAG )
        ELSEIF(TYPDIM(1:1).EQ.'I') THEN
          CALL OV( 'X=Y     ' , DM , D , D , C , NDIAG )
          TYPDIM(1:1)='Q'
        ELSEIF(TYPDIM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,12) TYPDIM(1:1)
          IF (LNG.EQ.2) WRITE(LU,13) TYPDIM(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!   EXTRADIAGONAL TERMS
!
        IF(TYPEXM(1:1).EQ.'Q') THEN
!
        DO IELEM = 1 , NELEM
!
          XM(IELEM, 7) = XM(IELEM, 7) * D(IKLE(IELEM,2))
          XM(IELEM, 8) = XM(IELEM, 8) * D(IKLE(IELEM,3))
          XM(IELEM, 9) = XM(IELEM, 9) * D(IKLE(IELEM,4))
!
          XM(IELEM, 1) = XM(IELEM, 1) * D(IKLE(IELEM,1))
          XM(IELEM,10) = XM(IELEM,10) * D(IKLE(IELEM,3))
          XM(IELEM,11) = XM(IELEM,11) * D(IKLE(IELEM,4))
!
          XM(IELEM, 2) = XM(IELEM, 2) * D(IKLE(IELEM,1))
          XM(IELEM, 4) = XM(IELEM, 4) * D(IKLE(IELEM,2))
          XM(IELEM,12) = XM(IELEM,12) * D(IKLE(IELEM,4))
!
          XM(IELEM, 3) = XM(IELEM, 3) * D(IKLE(IELEM,1))
          XM(IELEM, 5) = XM(IELEM, 5) * D(IKLE(IELEM,2))
          XM(IELEM, 6) = XM(IELEM, 6) * D(IKLE(IELEM,3))
!
        ENDDO ! IELEM
!
        ELSEIF(TYPEXM(1:1).EQ.'S') THEN
          IF (LNG.EQ.1) WRITE(LU,220)
          IF (LNG.EQ.2) WRITE(LU,230)
220       FORMAT(1X,'OM2121 (BIEF) : M=DM A ECRIRE SI M SYMETRIQUE')
230       FORMAT(1X,
     &    'OM2121 (BIEF) : M=MD NOT AVAILABLE IF M SYMMETRIC')
          CALL PLANTE(1)
          STOP
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,190)
          IF (LNG.EQ.2) WRITE(LU,200)
190       FORMAT(1X,'OM2121 (BIEF) : TYPEXM NON PREVU : ',A1)
200       FORMAT(1X,'OM2121 (BIEF) : TYPEXM NOT AVAILABLE : ',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'DMD   ') THEN
!
!   DIAGONAL TERMS
!
        IF(TYPDIM(1:1).EQ.'Q') THEN
          CALL OV( 'X=XY    ' , DM , D , D , C , NDIAG )
          CALL OV( 'X=XY    ' , DM , D , D , C , NDIAG )
        ELSEIF(TYPDIM(1:1).EQ.'I') THEN
          CALL OV( 'X=YZ    ' , DM , D , D , C , NDIAG )
          TYPDIM(1:1)='Q'
        ELSEIF(TYPDIM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,12) TYPDIM(1:1)
          IF (LNG.EQ.2) WRITE(LU,13) TYPDIM(1:1)
12        FORMAT(1X,'OM2121 (BIEF) : TYPDIM INCONNU :',A1)
13        FORMAT(1X,'OM2121 (BIEF) : TYPDIM UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!   EXTRADIAGONAL TERMS
!
        IF(TYPEXM(1:1).EQ.'S') THEN
!
        DO IELEM = 1 , NELEM
          XM(IELEM, 1)=XM(IELEM, 1) * D(IKLE(IELEM,2))*D(IKLE(IELEM,1))
          XM(IELEM, 2)=XM(IELEM, 2) * D(IKLE(IELEM,3))*D(IKLE(IELEM,1))
          XM(IELEM, 3)=XM(IELEM, 3) * D(IKLE(IELEM,4))*D(IKLE(IELEM,1))
          XM(IELEM, 4)=XM(IELEM, 4) * D(IKLE(IELEM,3))*D(IKLE(IELEM,2))
          XM(IELEM, 5)=XM(IELEM, 5) * D(IKLE(IELEM,4))*D(IKLE(IELEM,2))
          XM(IELEM, 6)=XM(IELEM, 6) * D(IKLE(IELEM,4))*D(IKLE(IELEM,3))
        ENDDO ! IELEM
!
        ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
!
        DO IELEM = 1 , NELEM
          XM(IELEM, 1)=XM(IELEM, 1) * D(IKLE(IELEM,2))*D(IKLE(IELEM,1))
          XM(IELEM, 2)=XM(IELEM, 2) * D(IKLE(IELEM,3))*D(IKLE(IELEM,1))
          XM(IELEM, 3)=XM(IELEM, 3) * D(IKLE(IELEM,4))*D(IKLE(IELEM,1))
          XM(IELEM, 4)=XM(IELEM, 4) * D(IKLE(IELEM,3))*D(IKLE(IELEM,2))
          XM(IELEM, 5)=XM(IELEM, 5) * D(IKLE(IELEM,4))*D(IKLE(IELEM,2))
          XM(IELEM, 6)=XM(IELEM, 6) * D(IKLE(IELEM,4))*D(IKLE(IELEM,3))
          XM(IELEM, 7)=XM(IELEM, 7) * D(IKLE(IELEM,2))*D(IKLE(IELEM,1))
          XM(IELEM, 8)=XM(IELEM, 8) * D(IKLE(IELEM,3))*D(IKLE(IELEM,1))
          XM(IELEM, 9)=XM(IELEM, 9) * D(IKLE(IELEM,4))*D(IKLE(IELEM,1))
          XM(IELEM,10)=XM(IELEM,10) * D(IKLE(IELEM,3))*D(IKLE(IELEM,2))
          XM(IELEM,11)=XM(IELEM,11) * D(IKLE(IELEM,4))*D(IKLE(IELEM,2))
          XM(IELEM,12)=XM(IELEM,12) * D(IKLE(IELEM,4))*D(IKLE(IELEM,3))
        ENDDO ! IELEM
!
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,240) TYPEXM(1:1)
          IF (LNG.EQ.2) WRITE(LU,241) TYPEXM(1:1)
240       FORMAT(1X,'OM2121 (BIEF) : TYPEXM INCONNU :',A1)
241       FORMAT(1X,'OM2121 (BIEF) : TYPEXM UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+D   ') THEN
!
        IF(TYPDIM(1:1).EQ.'Q') THEN
          CALL OV( 'X=X+Y   ' , DM , D , D , C , NDIAG )
        ELSE
          IF (LNG.EQ.1) WRITE(LU,12) TYPDIM(1:1)
          IF (LNG.EQ.2) WRITE(LU,13) TYPDIM(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'0     ') THEN
!
        CALL OV( 'X=C     ' , DM , DM , DM , 0.D0 , NDIAG )
!
        IF(TYPEXM(1:1).EQ.'S') THEN
          DO I=1,6
            CALL OV( 'X=C     ' , XM(1,I) , XM , XM , 0.D0 , NELEM )
          ENDDO
        ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
          DO I=1,12
            CALL OV( 'X=C     ' , XM(1,I) , XM , XM , 0.D0 , NELEM )
          ENDDO
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,710) TYPEXM(1:1)
          IF (LNG.EQ.2) WRITE(LU,711) TYPEXM(1:1)
710       FORMAT(1X,'OM2121 (BIEF) : TYPEXM INCONNU :',A1)
711       FORMAT(1X,'OM2121 (BIEF) : TYPEXM UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        TYPDIM(1:1)='0'
!       TYPEXM(1:1) IS NOT CHANGED
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'X(M)  ') THEN
!
        IF(TYPEXM(1:1).EQ.'S') THEN
          CALL OV( 'X=Y     ' , XM(1, 7) , XM(1,1) , XM , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1, 8) , XM(1,2) , XM , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1, 9) , XM(1,3) , XM , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,10) , XM(1,4) , XM , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,11) , XM(1,5) , XM , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,12) , XM(1,6) , XM , C , NELEM )
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,810) TYPEXM(1:1)
          IF (LNG.EQ.2) WRITE(LU,811) TYPEXM(1:1)
810       FORMAT(1X,'OM2121 (BIEF) : MATRICE DEJA NON SYMETRIQUE :',A1)
811       FORMAT(1X,'OM2121 (BIEF) : MATRIX ALREADY NON SYMMETRICAL: ',
     &           A1)
          CALL PLANTE(1)
          STOP
        ENDIF
        TYPEXM(1:1)='Q'
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'MSK(M)') THEN
!
      IF(TYPEXM(1:1).EQ.'S') THEN
        J = 6
      ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
        J = 12
      ELSEIF(TYPEXM(1:1).EQ.'0') THEN
        J = 0
      ELSE
        IF(LNG.EQ.1) WRITE(LU,710) TYPEXM
        IF(LNG.EQ.2) WRITE(LU,711) TYPEXM
        J = 0
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(J.GT.0) THEN
        DO I = 1,J
          CALL OV ( 'X=XY    ' , XM(1,I) , D , D , C , NELEM )
        ENDDO ! I
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF (LNG.EQ.1) WRITE(LU,41) OP
        IF (LNG.EQ.2) WRITE(LU,42) OP
41      FORMAT(1X,'OM2121 (BIEF) : OPERATION INCONNUE : ',A8)
42      FORMAT(1X,'OM2121 (BIEF) : UNKNOWN OPERATION : ',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
