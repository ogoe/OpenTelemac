!                    *****************
                     SUBROUTINE OM1211
!                    *****************
!
     &(OP ,  DM,TYPDIM,XM,TYPEXM,   DN,TYPDIN,XN,TYPEXN,   D,C,
     & IKLE,NELEM,NELMAX,NDIAG)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON RECTANGULAR MATRICES
!+                CONSISTING OF ONE ELEMENT WITH 3 POINTS AND
!+                ONE WITH 4 POINTS (LINEAR,QUASIBUBBLE FOR EXAMPLE).
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
!+      OP = 'M=CN    '  : MULTIPLIES N BY C
!+      OP = 'M=M+CN  '  : ADDS CN TO M
!+      OP = 'M=M+N   '  : ADDS N TO M
!+      OP = 'M=MD    '  : M X D
!+      OP = 'M=DM    '  : D X M
!+      OP = 'M=M+D   '  : ADDS D TO M
!+      OP = 'M=MSK(M)'  : MASKS M EXTRADIAGONAL TERMS
!+                         (OLD MASKEX)
!+                         THE MASK IS TAKEN FROM D
!
!code
!+  CONVENTION FOR THE STORAGE OF EXTRA-DIAGONAL TERMS:
!+
!+     XM(IELEM, 1)  ---->  M(1,2)
!+     XM(IELEM, 2)  ---->  M(1,3)
!+     XM(IELEM, 3)  ---->  M(2,1)
!+     XM(IELEM, 4)  ---->  M(2,3)
!+     XM(IELEM, 5)  ---->  M(3,1)
!+     XM(IELEM, 6)  ---->  M(3,2)
!+     XM(IELEM, 7)  ---->  M(4,1)
!+     XM(IELEM, 8)  ---->  M(4,2)
!+     XM(IELEM, 9)  ---->  M(4,3)
!
!history  J-M HERVOUET (LNHE)     ; F  LEPEINTRE (LNH)
!+        21/09/93
!+        V5P1
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
      USE BIEF, EX_OM1211 => OM1211
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
      DOUBLE PRECISION Y(1),Z(1)
!
!-----------------------------------------------------------------------
!
      IF(OP(1:8).EQ.'M=N     ') THEN
!
        IF(TYPDIN(1:1).EQ.'Q') THEN
          CALL OV( 'X=Y     ' , DM , DN , Z , C , NDIAG )
        ELSEIF(TYPDIN(1:1).EQ.'I'.OR.TYPDIN(1:1).EQ.'0') THEN
!         NOTHING TO DO, ONLY NEEDS TO COPY TYPDIN
        ELSE
           IF (LNG.EQ.1) WRITE(LU,5) TYPDIN(1:1)
           IF (LNG.EQ.2) WRITE(LU,6) TYPDIN(1:1)
5          FORMAT(1X,'OM1211 (BIEF) : TYPDIN INCONNU :',A1)
6          FORMAT(1X,'OM1211 (BIEF) : TYPDIN UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
        TYPDIM(1:1)=TYPDIN(1:1)
!
        IF(TYPEXN(1:1).EQ.'Q') THEN
          DO I=1,9
            CALL OV( 'X=Y     ' , XM(1,I) , XN(1,I) , Z , C , NELEM )
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
10        FORMAT(1X,'OM1211 (BIEF) : TYPEXN INCONNU :',A1)
11        FORMAT(1X,'OM1211 (BIEF) : TYPEXN UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        TYPEXM(1:1)=TYPEXN(1:1)
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'M=CN    ') THEN
!
        CALL OV( 'X=CY    ' , DM      , DN      , Z , C , NDIAG )
!
        IF(TYPEXN(1:1).EQ.'Q') THEN
          DO I=1,9
            CALL OV( 'X=CY    ' , XM(1,I) , XN(1,I) , Z , C , NELEM )
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        TYPDIM(1:1)=TYPDIN(1:1)
        TYPEXM(1:1)=TYPEXN(1:1)
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'M=M+CN  ') THEN
!
        IF(TYPDIN(1:1).EQ.'I') THEN
          CALL OV( 'X=X+C   ' , DM , DN , Z , C , NDIAG )
        ELSEIF(TYPDIN(1:1).NE.'0') THEN
          CALL OV( 'X=X+CY  ' , DM , DN , Z , C , NDIAG )
        ENDIF
!
        IF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
99          FORMAT(1X,'OM1211 (BIEF) : TYPEXM = ',A1,' NE CONVIENT PAS',
     &       /,1X,'POUR L''OPERATION : ',A8,' AVEC TYPEXN = ',A1)
98          FORMAT(1X,'OM1211 (BIEF) : TYPEXM = ',A1,' DOES NOT GO ',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO I=1,9
            CALL OV( 'X=X+CY  ' , XM(1,I) , XN(1,I) , Z , C , NELEM )
          ENDDO ! I
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'M=M+N   ') THEN
!
        CALL OV( 'X=X+Y   ' , DM      , DN      , Z , C , NDIAG )
!
        IF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            CALL PLANTE(1)
            STOP
          ENDIF
          DO I=1,9
            CALL OV( 'X=X+Y   ' , XM(1,I) , XN(1,I) , Z , C , NELEM )
          ENDDO
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'M=MD    ') THEN
!
!   DIAGONAL TERMS (DM AND D CAN HAVE DIFFERENT TYPES)
!
!
        IF(TYPDIM(1:1).EQ.'Q') THEN
          CALL OV( 'X=XY    ' , DM , D , Z , C , NDIAG )
        ELSEIF(TYPDIM(1:1).EQ.'I') THEN
          CALL OV( 'X=Y     ' , DM , D , Z , C , NDIAG )
          TYPDIM(1:1)='Q'
        ELSEIF(TYPDIM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,12) TYPDIM(1:1)
          IF (LNG.EQ.2) WRITE(LU,13) TYPDIM(1:1)
12        FORMAT(1X,'OM1211 (BIEF) : TYPDIM INCONNU :',A1)
13        FORMAT(1X,'OM1211 (BIEF) : TYPDIM UNKNOWN :',A1)
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
          XM(IELEM, 3) = XM(IELEM, 3) * D(IKLE(IELEM,1))
          XM(IELEM, 4) = XM(IELEM, 4) * D(IKLE(IELEM,3))
          XM(IELEM, 5) = XM(IELEM, 5) * D(IKLE(IELEM,1))
          XM(IELEM, 6) = XM(IELEM, 6) * D(IKLE(IELEM,2))
          XM(IELEM, 7) = XM(IELEM, 7) * D(IKLE(IELEM,1))
          XM(IELEM, 8) = XM(IELEM, 8) * D(IKLE(IELEM,2))
          XM(IELEM, 9) = XM(IELEM, 9) * D(IKLE(IELEM,3))
!
        ENDDO
!
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,162)
          IF (LNG.EQ.2) WRITE(LU,163)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'M=TN    ') THEN
!
!       RECOPIES THE EXTRADIAGONAL TERMS
!
        IF(TYPDIN(1:1).EQ.'Q') THEN
          CALL OV( 'X=Y     ' , DM , DN , Z , C , NDIAG )
        ELSEIF(TYPDIN(1:1).EQ.'I'.OR.TYPDIN(1:1).EQ.'0') THEN
!         NOTHING TO DO, ONLY NEEDS TO COPY TYPDIN
        ELSE
          IF (LNG.EQ.1) WRITE(LU,5) TYPDIN(1:1)
          IF (LNG.EQ.2) WRITE(LU,6) TYPDIN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
        TYPDIM(1:1)=TYPDIN(1:1)
!
!       TRANSPOSES EXTRADIAGONAL TERMS
!
        IF(TYPEXM(1:1).EQ.'Q') THEN
!
        DO IELEM = 1 , NELEM
!
          XM(IELEM, 1) = XN(IELEM, 4)
          XM(IELEM, 2) = XN(IELEM, 7)
          XM(IELEM, 3) = XN(IELEM, 1)
          XM(IELEM, 4) = XN(IELEM, 8)
          XM(IELEM, 5) = XN(IELEM, 2)
          XM(IELEM, 6) = XN(IELEM, 5)
          XM(IELEM, 7) = XN(IELEM, 3)
          XM(IELEM, 8) = XN(IELEM, 6)
          XM(IELEM, 9) = XN(IELEM, 9)
!
        ENDDO
!
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,162)
          IF (LNG.EQ.2) WRITE(LU,163)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'M=DM    ') THEN
!
!   DIAGONAL TERMS
!
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
!
!   EXTRADIAGONAL TERMS
!
        IF(TYPEXM(1:1).EQ.'Q') THEN
!
        DO IELEM = 1 , NELEM
!
          XM(IELEM, 1) = XM(IELEM, 1) * D(IKLE(IELEM,1))
          XM(IELEM, 2) = XM(IELEM, 2) * D(IKLE(IELEM,1))
          XM(IELEM, 3) = XM(IELEM, 3) * D(IKLE(IELEM,2))
          XM(IELEM, 4) = XM(IELEM, 4) * D(IKLE(IELEM,2))
          XM(IELEM, 5) = XM(IELEM, 5) * D(IKLE(IELEM,3))
          XM(IELEM, 6) = XM(IELEM, 6) * D(IKLE(IELEM,3))
          XM(IELEM, 7) = XM(IELEM, 7) * D(IKLE(IELEM,4))
          XM(IELEM, 8) = XM(IELEM, 8) * D(IKLE(IELEM,4))
          XM(IELEM, 9) = XM(IELEM, 9) * D(IKLE(IELEM,4))
!
        ENDDO
!
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,162)
          IF (LNG.EQ.2) WRITE(LU,163)
162       FORMAT(1X,'OM1211 (BIEF) : TYPEXM NON PREVU : ',A1)
163       FORMAT(1X,'OM1211 (BIEF) : TYPEXM NOT AVAILABLE : ',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'M=M+D   ') THEN
!
        IF(TYPDIM(1:1).EQ.'Q') THEN
          CALL OV( 'X=X+Y   ' , DM , D , Z , C , NDIAG )
        ELSE
          IF (LNG.EQ.1) WRITE(LU,12) TYPDIM(1:1)
          IF (LNG.EQ.2) WRITE(LU,13) TYPDIM(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'M=0     ') THEN
!
        CALL OV( 'X=C     ' , DM , Y , Z , 0.D0 , NDIAG )
!
        IF(TYPEXM(1:1).EQ.'Q') THEN
          DO I=1,9
            CALL OV( 'X=C     ' , XM(1,I) , Y , Z , 0.D0 , NELEM )
          ENDDO
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,710) TYPEXM(1:1)
          IF (LNG.EQ.2) WRITE(LU,711) TYPEXM(1:1)
710       FORMAT(1X,'OM1211 (BIEF) : TYPEXM INCONNU :',A1)
711       FORMAT(1X,'OM1211 (BIEF) : TYPEXM UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        TYPDIM(1:1)='0'
!       TYPEXM(1:1) IS NOT CHANGED
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'M=MSK(M)') THEN
!
      IF(TYPEXM(1:1).EQ.'Q') THEN
        J = 9
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
          CALL OV ( 'X=XY    ' , XM(1,I) , D , Z , C , NELEM )
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF (LNG.EQ.1) WRITE(LU,40) OP
        IF (LNG.EQ.2) WRITE(LU,41) OP
40      FORMAT(1X,'OM1211 (BIEF) : OPERATION INCONNUE : ',A8)
41      FORMAT(1X,'OM1211 (BIEF) : UNKNOWN OPERATION : ',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
