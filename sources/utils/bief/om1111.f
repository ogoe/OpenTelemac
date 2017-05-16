!                    *****************
                     SUBROUTINE OM1111
!                    *****************
!
     &(OP ,  DM,TYPDIM,XM,TYPEXM,   DN,TYPDIN,XN,TYPEXN,   D,C,
     & IKLE,NELEM,NELMAX,NDIAG,DM_ERR, DN_ERR, D_ERR)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON MATRICES WITH P1 TRIANGLE.
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
!+      OP = 'M=TN    '  : COPIES TRANSPOSE OF N IN M
!+      OP = 'M=M+TN  '  : ADDS TRANSPOSE(N) TO M
!+      OP = 'M=M+CTN '  : ADDS C TRANSPOSE(N) TO M
!+      OP = 'M=M+N   '  : ADDS N TO M
!+      OP = 'M=MD    '  : M X D
!+      OP = 'M=DM    '  : D X M
!+      OP = 'M=M-ND  '  : SUBTRACTS N X D TO M
!+      OP = 'M=M-DN  '  : SUBTRACTS D X N TO M
!+      OP = 'M=DMD   '  : D X M X D
!+      OP = 'M=0     '  : SETS M TO 0
!+      OP = 'M=X(M)  '  : NOT SYMMETRICAL FORM OF M
!+                         (OLD MATSNS)
!+      OP = 'M=MSK(M)'  : MASKS M EXTRADIAGONAL TERMS
!+                         (OLD MASKEX)
!+                         THE MASK IS TAKEN FROM D
!+      OP = 'M=M+D   '  : ADDS D TO M
!
!code
!+  CONVENTION FOR THE STORAGE OF EXTRA-DIAGONAL TERMS:
!+
!+      XM(IELEM,1)  ---->  M(1,2)
!+      XM(IELEM,2)  ---->  M(1,3)
!+      XM(IELEM,3)  ---->  M(2,3)
!+      XM(IELEM,4)  ---->  M(2,1)
!+      XM(IELEM,5)  ---->  M(3,1)
!+      XM(IELEM,6)  ---->  M(3,2)
!
!history  J-M HERVOUET (LNHE)     ; F  LEPEINTRE (LNH)
!+        05/02/91
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
!history  R.NHEILI (Univerte de Perpignan, DALI)
!+        24/02/2016
!+        V7P3
!+      ADD MODASS=3
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
      USE BIEF, EX_OM1111 => OM1111
      USE DECLARATIONS_TELEMAC, ONLY : MODASS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NDIAG
      INTEGER, INTENT(IN) :: IKLE(NELMAX,3)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),D(*),XN(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DM(*),XM(NELMAX,*)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIM,TYPEXM,TYPDIN,TYPEXN
      DOUBLE PRECISION, INTENT(IN)    :: C
      DOUBLE PRECISION,OPTIONAL, INTENT(INOUT) :: DM_ERR(*)
      DOUBLE PRECISION,OPTIONAL, INTENT(IN) :: DN_ERR(*),D_ERR(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM,I,J
!
      DOUBLE PRECISION Y(1),Z(1)
!
!-----------------------------------------------------------------------
!
      IF(OP(3:8).EQ.'N     ') THEN
!
        IF(TYPDIN(1:1).EQ.'Q') THEN
          IF ( MODASS .EQ.1) THEN
            CALL OV( 'X=Y     ' , DM , DN , Z , C , NDIAG )
          ELSEIF (MODASS .EQ. 3) THEN
            CALL OV_COMP( 'X=Y     ' , DM , DN , Z , C , NDIAG,
     &            X_ERR=DM_ERR, Y_ERR=DN_ERR )
          ENDIF
!
        ELSEIF(TYPDIN(1:1).EQ.'I'.OR.TYPDIN(1:1).EQ.'0') THEN
!         NOTHING TO DO, ONLY NEEDS TO COPY TYPDIN
        ELSE
           IF (LNG.EQ.1) WRITE(LU,5) TYPDIN(1:1)
           IF (LNG.EQ.2) WRITE(LU,6) TYPDIN(1:1)
5          FORMAT(1X,'OM1111 (BIEF) : TYPDIN INCONNU :',A1)
6          FORMAT(1X,'OM1111 (BIEF) : TYPDIN UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
        TYPDIM(1:1)=TYPDIN(1:1)
!
        IF(TYPEXN(1:1).EQ.'S') THEN
           CALL OV( 'X=Y     ' , XM(1,1) , XN(1,1) , Z , C , NELEM )
           CALL OV( 'X=Y     ' , XM(1,2) , XN(1,2) , Z , C , NELEM )
           CALL OV( 'X=Y     ' , XM(1,3) , XN(1,3) , Z , C , NELEM )
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
           CALL OV( 'X=Y     ' , XM(1,1) , XN(1,1) , Z , C , NELEM )
           CALL OV( 'X=Y     ' , XM(1,2) , XN(1,2) , Z , C , NELEM )
           CALL OV( 'X=Y     ' , XM(1,3) , XN(1,3) , Z , C , NELEM )
           CALL OV( 'X=Y     ' , XM(1,4) , XN(1,4) , Z , C , NELEM )
           CALL OV( 'X=Y     ' , XM(1,5) , XN(1,5) , Z , C , NELEM )
           CALL OV( 'X=Y     ' , XM(1,6) , XN(1,6) , Z , C , NELEM )
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
10         FORMAT(1X,'OM1111 (BIEF) : TYPEXN INCONNU :',A1)
11         FORMAT(1X,'OM1111 (BIEF) : TYPEXN UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
        TYPEXM(1:1)=TYPEXN(1:1)
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'CN    ') THEN
!
        CALL OV( 'X=CY    ' , DM , DN , Z , C , NDIAG )
!
        IF(TYPEXN(1:1).EQ.'S') THEN
           CALL OV( 'X=CY    ' , XM(1,1) , XN(1,1) , Z , C , NELEM )
           CALL OV( 'X=CY    ' , XM(1,2) , XN(1,2) , Z , C , NELEM )
           CALL OV( 'X=CY    ' , XM(1,3) , XN(1,3) , Z , C , NELEM )
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
           CALL OV( 'X=CY    ' , XM(1,1) , XN(1,1) , Z , C , NELEM )
           CALL OV( 'X=CY    ' , XM(1,2) , XN(1,2) , Z , C , NELEM )
           CALL OV( 'X=CY    ' , XM(1,3) , XN(1,3) , Z , C , NELEM )
           CALL OV( 'X=CY    ' , XM(1,4) , XN(1,4) , Z , C , NELEM )
           CALL OV( 'X=CY    ' , XM(1,5) , XN(1,5) , Z , C , NELEM )
           CALL OV( 'X=CY    ' , XM(1,6) , XN(1,6) , Z , C , NELEM )
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
      ELSEIF(OP(3:8).EQ.'M+CN  ' .OR.
     &      (OP(3:8).EQ.'M+CTN ').AND.TYPEXN(1:1).NE.'Q') THEN
!
        IF(TYPDIN(1:1).EQ.'I') THEN
          CALL OV( 'X=X+C   ' , DM , DN , Z , C , NDIAG )
        ELSEIF(TYPDIN(1:1).NE.'0') THEN
          CALL OV( 'X=X+CY  ' , DM , DN , Z , C , NDIAG )
        ENDIF
!
        IF(TYPEXN(1:1).EQ.'S') THEN
           CALL OV( 'X=X+CY  ' , XM(1,1) , XN(1,1) , Z , C , NELEM )
           CALL OV( 'X=X+CY  ' , XM(1,2) , XN(1,2) , Z , C , NELEM )
           CALL OV( 'X=X+CY  ' , XM(1,3) , XN(1,3) , Z , C , NELEM )
           IF(TYPEXM(1:1).EQ.'Q') THEN
             CALL OV( 'X=X+CY  ' , XM(1,4) , XN(1,1) , Z , C , NELEM )
             CALL OV( 'X=X+CY  ' , XM(1,5) , XN(1,2) , Z , C , NELEM )
             CALL OV( 'X=X+CY  ' , XM(1,6) , XN(1,3) , Z , C , NELEM )
           ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
           IF(TYPEXM(1:1).NE.'Q') THEN
             IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
             IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
99          FORMAT(1X,'OM1111 (BIEF) : TYPEXM = ',A1,' NE CONVIENT PAS',
     &       /,1X,'POUR L''OPERATION : ',A8,' AVEC TYPEXN = ',A1)
98          FORMAT(1X,'OM1111 (BIEF) : TYPEXM = ',A1,' DOES NOT GO',
     &       /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
             CALL PLANTE(1)
             STOP
           ENDIF
           CALL OV( 'X=X+CY  ' , XM(1,1) , XN(1,1) , Z , C , NELEM )
           CALL OV( 'X=X+CY  ' , XM(1,2) , XN(1,2) , Z , C , NELEM )
           CALL OV( 'X=X+CY  ' , XM(1,3) , XN(1,3) , Z , C , NELEM )
           CALL OV( 'X=X+CY  ' , XM(1,4) , XN(1,4) , Z , C , NELEM )
           CALL OV( 'X=X+CY  ' , XM(1,5) , XN(1,5) , Z , C , NELEM )
           CALL OV( 'X=X+CY  ' , XM(1,6) , XN(1,6) , Z , C , NELEM )
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
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
        CALL OV( 'X=X+CY  ' , DM , DN , Z , C , NDIAG )
!
        IF(TYPEXN(1:1).EQ.'Q') THEN
           IF(TYPEXM(1:1).NE.'Q') THEN
             IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
             IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
             CALL PLANTE(1)
             STOP
           ENDIF
           CALL OV( 'X=X+CY  ' , XM(1,1) , XN(1,4) , Z , C , NELEM )
           CALL OV( 'X=X+CY  ' , XM(1,2) , XN(1,5) , Z , C , NELEM )
           CALL OV( 'X=X+CY  ' , XM(1,3) , XN(1,6) , Z , C , NELEM )
           CALL OV( 'X=X+CY  ' , XM(1,4) , XN(1,1) , Z , C , NELEM )
           CALL OV( 'X=X+CY  ' , XM(1,5) , XN(1,2) , Z , C , NELEM )
           CALL OV( 'X=X+CY  ' , XM(1,6) , XN(1,3) , Z , C , NELEM )
        ELSE
           IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
           CALL PLANTE(1)
           STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'TN    ') THEN
!
        CALL OV( 'X=Y     ' , DM , DN , Z , C , NDIAG )
!
        IF(TYPEXN(1:1).EQ.'S') THEN
           CALL OV( 'X=Y     ' , XM(1,1) , XN(1,1) , Z , C , NELEM )
           CALL OV( 'X=Y     ' , XM(1,2) , XN(1,2) , Z , C , NELEM )
           CALL OV( 'X=Y     ' , XM(1,3) , XN(1,3) , Z , C , NELEM )
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
           CALL OV( 'X=Y     ' , XM(1,1) , XN(1,4) , Z , C , NELEM )
           CALL OV( 'X=Y     ' , XM(1,2) , XN(1,5) , Z , C , NELEM )
           CALL OV( 'X=Y     ' , XM(1,3) , XN(1,6) , Z , C , NELEM )
           CALL OV( 'X=Y     ' , XM(1,4) , XN(1,1) , Z , C , NELEM )
           CALL OV( 'X=Y     ' , XM(1,5) , XN(1,2) , Z , C , NELEM )
           CALL OV( 'X=Y     ' , XM(1,6) , XN(1,3) , Z , C , NELEM )
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
           CALL PLANTE(1)
           STOP
        ENDIF
        TYPDIM(1:1)=TYPDIN(1:1)
        TYPEXM(1:1)=TYPEXN(1:1)
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+N   '.OR.
     &      (OP(3:8).EQ.'M+TN  ').AND.TYPEXN(1:1).NE.'Q') THEN
!
        IF ( MODASS .EQ.1) THEN
          CALL OV( 'X=X+Y   ' , DM , DN , Z , C , NDIAG )
        ELSEIF (MODASS .EQ. 3) THEN
          CALL OV_COMP( 'X=X+Y   ' , DM , DN , Z , C , NDIAG,
     &            X_ERR=DM_ERR, Y_ERR=DN_ERR )
        ENDIF
!
        IF(TYPEXN(1:1).EQ.'S') THEN
           CALL OV( 'X=X+Y   ' , XM(1,1) , XN(1,1) , Z , C , NELEM )
           CALL OV( 'X=X+Y   ' , XM(1,2) , XN(1,2) , Z , C , NELEM )
           CALL OV( 'X=X+Y   ' , XM(1,3) , XN(1,3) , Z , C , NELEM )
           IF(TYPEXM(1:1).EQ.'Q') THEN
             CALL OV( 'X=X+Y   ' , XM(1,4) , XN(1,1) , Z , C , NELEM )
             CALL OV( 'X=X+Y   ' , XM(1,5) , XN(1,2) , Z , C , NELEM )
             CALL OV( 'X=X+Y   ' , XM(1,6) , XN(1,3) , Z , C , NELEM )
           ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
           IF(TYPEXM(1:1).NE.'Q') THEN
             IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
             IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
             CALL PLANTE(1)
             STOP
           ENDIF
           CALL OV( 'X=X+Y   ' , XM(1,1) , XN(1,1) , Z , C , NELEM )
           CALL OV( 'X=X+Y   ' , XM(1,2) , XN(1,2) , Z , C , NELEM )
           CALL OV( 'X=X+Y   ' , XM(1,3) , XN(1,3) , Z , C , NELEM )
           CALL OV( 'X=X+Y   ' , XM(1,4) , XN(1,4) , Z , C , NELEM )
           CALL OV( 'X=X+Y   ' , XM(1,5) , XN(1,5) , Z , C , NELEM )
           CALL OV( 'X=X+Y   ' , XM(1,6) , XN(1,6) , Z , C , NELEM )
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
           CALL PLANTE(1)
           STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+TN  ') THEN
!
!     THE CASE WHERE N IS SYMMETRICAL HAS ALREADY BEEN TREATED
!
        CALL OV( 'X=X+Y   ' , DM , DN , Z , C , NDIAG )
!
        IF(TYPEXM(1:1).EQ.'Q') THEN
           CALL OV( 'X=X+Y   ' , XM(1,1) , XN(1,4) , Z , C , NELEM )
           CALL OV( 'X=X+Y   ' , XM(1,2) , XN(1,5) , Z , C , NELEM )
           CALL OV( 'X=X+Y   ' , XM(1,3) , XN(1,6) , Z , C , NELEM )
           CALL OV( 'X=X+Y   ' , XM(1,4) , XN(1,1) , Z , C , NELEM )
           CALL OV( 'X=X+Y   ' , XM(1,5) , XN(1,2) , Z , C , NELEM )
           CALL OV( 'X=X+Y   ' , XM(1,6) , XN(1,3) , Z , C , NELEM )
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
           CALL PLANTE(1)
           STOP
        ENDIF
        TYPDIM(1:1)=TYPDIN(1:1)
        TYPEXM(1:1)=TYPEXN(1:1)
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'MD    ') THEN
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
          XM(IELEM, 1) = XM(IELEM, 1) * D(IKLE(IELEM,2))
          XM(IELEM, 2) = XM(IELEM, 2) * D(IKLE(IELEM,3))
          XM(IELEM, 3) = XM(IELEM, 3) * D(IKLE(IELEM,3))
          XM(IELEM, 4) = XM(IELEM, 4) * D(IKLE(IELEM,1))
          XM(IELEM, 5) = XM(IELEM, 5) * D(IKLE(IELEM,1))
          XM(IELEM, 6) = XM(IELEM, 6) * D(IKLE(IELEM,2))
!
        ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'S') THEN
          IF (LNG.EQ.1) WRITE(LU,170)
          IF (LNG.EQ.2) WRITE(LU,171)
170       FORMAT(1X,'OM1111 (BIEF) : M=MD , M DOIT ETRE NON SYMETRIQUE')
171       FORMAT(1X,'OM1111 (BIEF) : M=MD , M MUST BE NON-SYMMETRIC')
          CALL PLANTE(1)
          STOP
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,172) TYPEXM(1:1)
          IF (LNG.EQ.2) WRITE(LU,173) TYPEXM(1:1)
172       FORMAT(1X,'OM1111 (BIEF) : TYPEXM NON PREVU : ',A1)
173       FORMAT(1X,'OM1111 (BIEF) : TYPEXM NOT AVAILABLE : ',A1)
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
!
        ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'S') THEN
          IF (LNG.EQ.1) WRITE(LU,180)
          IF (LNG.EQ.2) WRITE(LU,181)
180       FORMAT(1X,'OM1111 (BIEF) : M=DM A ECRIRE SI M SYMETRIQUE')
181       FORMAT(1X,
     &    'OM1111 (BIEF) : M=MD NOT AVAILABLE IF M SYMMETRIC')
          CALL PLANTE(1)
          STOP
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,172) TYPEXM(1:1)
          IF (LNG.EQ.2) WRITE(LU,173) TYPEXM(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M-DN  ') THEN
!
!   DIAGONAL TERMS
!
        IF(TYPDIM(1:1).EQ.'Q') THEN
          CALL OV( 'X=X-YZ  ' , DM , DN , D , C , NDIAG )
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
          IF(TYPEXN(1:1).EQ.'Q') THEN
          DO IELEM = 1 , NELEM
           XM(IELEM, 1) = XM(IELEM,1) - XN(IELEM, 1) * D(IKLE(IELEM,1))
           XM(IELEM, 2) = XM(IELEM,2) - XN(IELEM, 2) * D(IKLE(IELEM,1))
           XM(IELEM, 3) = XM(IELEM,3) - XN(IELEM, 3) * D(IKLE(IELEM,2))
           XM(IELEM, 4) = XM(IELEM,4) - XN(IELEM, 4) * D(IKLE(IELEM,2))
           XM(IELEM, 5) = XM(IELEM,5) - XN(IELEM, 5) * D(IKLE(IELEM,3))
           XM(IELEM, 6) = XM(IELEM,6) - XN(IELEM, 6) * D(IKLE(IELEM,3))
          ENDDO ! IELEM
          ELSEIF(TYPEXN(1:1).EQ.'S') THEN
          DO IELEM = 1 , NELEM
           XM(IELEM, 1) = XM(IELEM,1) - XN(IELEM, 1) * D(IKLE(IELEM,1))
           XM(IELEM, 2) = XM(IELEM,2) - XN(IELEM, 2) * D(IKLE(IELEM,1))
           XM(IELEM, 3) = XM(IELEM,3) - XN(IELEM, 3) * D(IKLE(IELEM,2))
           XM(IELEM, 4) = XM(IELEM,4) - XN(IELEM, 1) * D(IKLE(IELEM,2))
           XM(IELEM, 5) = XM(IELEM,5) - XN(IELEM, 2) * D(IKLE(IELEM,3))
           XM(IELEM, 6) = XM(IELEM,6) - XN(IELEM, 3) * D(IKLE(IELEM,3))
          ENDDO ! IELEM
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
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M-ND  ') THEN
!
!   DIAGONAL TERMS
!
        IF(TYPDIM(1:1).EQ.'Q') THEN
          CALL OV( 'X=X-YZ  ' , DM , DN , D , C , NDIAG )
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
          IF(TYPEXN(1:1).EQ.'Q') THEN
          DO IELEM = 1 , NELEM
          XM(IELEM, 1) = XM(IELEM,1) - XN(IELEM, 1) * D(IKLE(IELEM,2))
          XM(IELEM, 2) = XM(IELEM,2) - XN(IELEM, 2) * D(IKLE(IELEM,3))
          XM(IELEM, 3) = XM(IELEM,3) - XN(IELEM, 3) * D(IKLE(IELEM,3))
          XM(IELEM, 4) = XM(IELEM,4) - XN(IELEM, 4) * D(IKLE(IELEM,1))
          XM(IELEM, 5) = XM(IELEM,5) - XN(IELEM, 5) * D(IKLE(IELEM,1))
          XM(IELEM, 6) = XM(IELEM,6) - XN(IELEM, 6) * D(IKLE(IELEM,2))
          ENDDO ! IELEM
          ELSEIF(TYPEXN(1:1).EQ.'S') THEN
          DO IELEM = 1 , NELEM
          XM(IELEM, 1) = XM(IELEM,1) - XN(IELEM, 1) * D(IKLE(IELEM,2))
          XM(IELEM, 2) = XM(IELEM,2) - XN(IELEM, 2) * D(IKLE(IELEM,3))
          XM(IELEM, 3) = XM(IELEM,3) - XN(IELEM, 3) * D(IKLE(IELEM,3))
          XM(IELEM, 4) = XM(IELEM,4) - XN(IELEM, 1) * D(IKLE(IELEM,1))
          XM(IELEM, 5) = XM(IELEM,5) - XN(IELEM, 2) * D(IKLE(IELEM,1))
          XM(IELEM, 6) = XM(IELEM,6) - XN(IELEM, 3) * D(IKLE(IELEM,2))
          ENDDO ! IELEM
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
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'DMD   ') THEN
!
!   DIAGONAL TERMS
!
        IF(TYPDIM(1:1).EQ.'Q') THEN
          IF (MODASS.EQ. 1)THEN
            CALL OV( 'X=XY    ' , DM , D , Z , C , NDIAG )
            CALL OV( 'X=XY    ' , DM , D , Z , C , NDIAG )
          ELSEIF (MODASS.EQ. 3)THEN
            CALL OV_COMP( 'X=XY    ' , DM , D , Z , C , NDIAG,
     &                     X_ERR=DM_ERR, Y_ERR= D_ERR  )
            CALL OV_COMP( 'X=XY    ' , DM , D , Z , C , NDIAG,
     &                     X_ERR=DM_ERR, Y_ERR= D_ERR  )
          ENDIF
        ELSEIF(TYPDIM(1:1).EQ.'I') THEN
          CALL OV( 'X=YZ    ' , DM , D , D , C , NDIAG )
          TYPDIM(1:1)='Q'
        ELSEIF(TYPDIM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,12) TYPDIM(1:1)
          IF (LNG.EQ.2) WRITE(LU,13) TYPDIM(1:1)
12        FORMAT(1X,'OM1111 (BIEF) : TYPDIM INCONNU :',A1)
13        FORMAT(1X,'OM1111 (BIEF) : TYPDIM UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!   EXTRADIAGONAL TERMS
!
        IF(TYPEXM(1:1).EQ.'S') THEN
!
        DO IELEM = 1 , NELEM
          XM(IELEM,1)=XM(IELEM,1)* D(IKLE(IELEM,2)) * D(IKLE(IELEM,1))
          XM(IELEM,2)=XM(IELEM,2)* D(IKLE(IELEM,3)) * D(IKLE(IELEM,1))
          XM(IELEM,3)=XM(IELEM,3)* D(IKLE(IELEM,3)) * D(IKLE(IELEM,2))
        ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
!
        DO IELEM = 1 , NELEM
          XM(IELEM,1)=XM(IELEM,1)* D(IKLE(IELEM,2)) * D(IKLE(IELEM,1))
          XM(IELEM,2)=XM(IELEM,2)* D(IKLE(IELEM,3)) * D(IKLE(IELEM,1))
          XM(IELEM,3)=XM(IELEM,3)* D(IKLE(IELEM,3)) * D(IKLE(IELEM,2))
          XM(IELEM,4)=XM(IELEM,4)* D(IKLE(IELEM,2)) * D(IKLE(IELEM,1))
          XM(IELEM,5)=XM(IELEM,5)* D(IKLE(IELEM,3)) * D(IKLE(IELEM,1))
          XM(IELEM,6)=XM(IELEM,6)* D(IKLE(IELEM,3)) * D(IKLE(IELEM,2))
        ENDDO
!
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,20) TYPEXM(1:1)
           IF (LNG.EQ.2) WRITE(LU,21) TYPEXM(1:1)
20         FORMAT(1X,'OM1111 (BIEF) : TYPEXM INCONNU :',A1)
21         FORMAT(1X,'OM1111 (BIEF) : TYPEXM UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+D   ') THEN
!
        IF ( MODASS .EQ.1) THEN
          CALL OV( 'X=X+Y   ' , DM , D , Z , 0.D0 , NDIAG )
        ELSEIF (MODASS .EQ. 3) THEN
          CALL OV_COMP( 'X=X+Y   ' , DM , D , Z , 0.D0, NDIAG,
     &           X_ERR=DM_ERR, Y_ERR= D_ERR )
        ENDIF
!       HERE THERE IS A DOUBT ABOUT TYPDIM
        TYPDIM(1:1)='Q'
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'0     ') THEN
!
        CALL OV( 'X=C     ' , DM , Y , Z , 0.D0 , NDIAG )
!
        IF(TYPEXM(1:1).EQ.'S') THEN
           CALL OV( 'X=C     ' , XM(1,1) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,2) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,3) , Y , Z , 0.D0 , NELEM )
        ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
           CALL OV( 'X=C     ' , XM(1,1) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,2) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,3) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,4) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,5) , Y , Z , 0.D0 , NELEM )
           CALL OV( 'X=C     ' , XM(1,6) , Y , Z , 0.D0 , NELEM )
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
           IF (LNG.EQ.1) WRITE(LU,710) TYPEXM(1:1)
           IF (LNG.EQ.2) WRITE(LU,711) TYPEXM(1:1)
710        FORMAT(1X,'OM1111 (BIEF) : TYPEXM INCONNU :',A1)
711        FORMAT(1X,'OM1111 (BIEF) : TYPEXM UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
!       TYPDIM IS NOT CHANGED
!       TYPDIM(1:1)='0'
!       TYPEXM IS NOT CHANGED
!       TYPEXM(1:1)='0'
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'X(M)  ') THEN
!
        IF(TYPEXM(1:1).EQ.'S') THEN
          CALL OV( 'X=Y     ' , XM(1,4) , XM(1,1) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,5) , XM(1,2) , Z , C , NELEM )
          CALL OV( 'X=Y     ' , XM(1,6) , XM(1,3) , Z , C , NELEM )
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,810) TYPEXM(1:1)
          IF (LNG.EQ.2) WRITE(LU,811) TYPEXM(1:1)
810       FORMAT(1X,'OM1111 (BIEF) : MATRICE DEJA NON SYMETRIQUE :',A1)
811       FORMAT(1X,'OM1111 (BIEF) : MATRIX ALREADY NON SYMMETRICAL: ',
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
        J = 3
      ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
        J = 6
      ELSEIF(TYPEXM(1:1).EQ.'0') THEN
        J = 0
      ELSE
        IF(LNG.EQ.1) WRITE(LU,710) TYPEXM
        IF(LNG.EQ.2) WRITE(LU,711) TYPEXM
        J=0
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
40      FORMAT(1X,'OM1111 (BIEF) : OPERATION INCONNUE : ',A8)
41      FORMAT(1X,'OM1111 (BIEF) : UNKNOWN OPERATION : ',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
