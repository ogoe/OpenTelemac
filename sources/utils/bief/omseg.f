!                    ****************
                     SUBROUTINE OMSEG
!                    ****************
!
     &(OP ,  DM,TYPDIM,XM,TYPEXM,   DN,TYPDIN,XN,TYPEXN,   D,C,
     & NDIAG,NSEG1,NSEG2,GLOSEG,SIZGLO)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS ON MATRICES WITH AN EDGE-BASED STORAGE.
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
!+      OP = 'M=M-ND  '  : SUBTRACTS ND FROM M
!+      OP = 'M=M-DN  '  : SUBTRACTS DN FROM M
!+      OP = 'M=DMD   '  : D X M X D
!+      OP = 'M=0     '  : SETS M TO 0
!+      OP = 'M=X(M)  '  : NOT SYMMETRICAL FORM OF M
!+                         (OLD MATSNS)
!+      OP = 'M=MSK(M)'  : MASKS M EXTRADIAGONAL TERMS
!+                         (OLD MASKEX)
!+                         THE MASK IS TAKEN FROM D
!+      OP = 'M=M+D   '  : ADDS D TO M
!
!history  J-M HERVOUET (LNHE)
!+        29/12/05
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
!| NDIAG          |-->| NUMBER OF TERMS IN THE DIAGONAL
!| NSEG1          |-->| NUMBER OF SEGMENTS OF LINE ELEMENT
!| NSEG2          |-->| NUMBER OF SEGMENTS OF COLUMN ELEMENT
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
      USE BIEF, EX_OMSEG => OMSEG
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NDIAG,NSEG1,NSEG2,SIZGLO
      INTEGER, INTENT(IN) :: GLOSEG(SIZGLO,2)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),D(*)
!     XM AND XN MAY ONLY BE OF SIZE NSEG1 IF THE MATRIX IS SYMMETRICAL
!     SIZE GIVEN HERE ONLY TO CHECK BOUNDS
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NSEG1+NSEG2)
      DOUBLE PRECISION, INTENT(IN)    :: XN(NSEG1+NSEG2)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIM,TYPEXM,TYPDIN,TYPEXN
      DOUBLE PRECISION, INTENT(INOUT) :: DM(*)
      DOUBLE PRECISION, INTENT(IN)    :: C
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC MIN
!
      INTEGER ISEG,DIMX
!
      DOUBLE PRECISION Y(1),Z(1)
!
!-----------------------------------------------------------------------
!
!     ARRAYS XM AND XN ARE BASICALLY OF SIZE XM(DIMX,1 OR 2)
!     BUT IN THE CASE OF RECTANGULAR MATRICES OTHER DATA ARE STORED
!     BEYOND XM(2*DIMX)
!
      DIMX=MIN(NSEG1,NSEG2)
!
      IF(OP(3:8).EQ.'N     ') THEN
!
        IF(TYPDIN(1:1).EQ.'Q') THEN
          CALL OV( 'X=Y     ' , DM , DN , Z , C , NDIAG )
        ELSEIF(TYPDIN(1:1).EQ.'I'.OR.TYPDIN(1:1).EQ.'0') THEN
!         NOTHING TO DO, ONLY NEEDS TO COPY TYPDIN
        ELSE
          IF (LNG.EQ.1) WRITE(LU,5) TYPDIN(1:1)
          IF (LNG.EQ.2) WRITE(LU,6) TYPDIN(1:1)
5         FORMAT(1X,'OMSEG (BIEF) : TYPDIN INCONNU :',A1)
6         FORMAT(1X,'OMSEG (BIEF) : TYPDIN UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
        TYPDIM(1:1)=TYPDIN(1:1)
!
        IF(TYPEXN(1:1).EQ.'S') THEN
           CALL OV( 'X=Y     ' , XM , XN , Z , C , NSEG1 )
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
           CALL OV( 'X=Y     ' , XM , XN , Z , C , NSEG1+NSEG2 )
        ELSEIF(TYPEXN(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,10) TYPEXN(1:1)
          IF (LNG.EQ.2) WRITE(LU,11) TYPEXN(1:1)
10        FORMAT(1X,'OMSEG (BIEF) : TYPEXN INCONNU :',A1)
11        FORMAT(1X,'OMSEG (BIEF) : TYPEXN UNKNOWN :',A1)
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
          CALL OV( 'X=CY    ' , XM , XN , Z , C , NSEG1 )
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          CALL OV( 'X=CY    ' , XM , XN , Z , C , NSEG1+NSEG2 )
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
          CALL OV( 'X=X+CY  ' , XM , XN , Z , C , NSEG1 )
          IF(TYPEXM(1:1).EQ.'Q') THEN
            CALL OV( 'X=X+CY  ' , XM(DIMX+1:DIMX+NSEG1) ,XN,Z,C,NSEG1)
          ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'Q') THEN
            IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
            IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
99         FORMAT(1X,'OMSEG (BIEF) : TYPEXM = ',A1,' NE CONVIENT PAS',
     &      /,1X,'POUR L''OPERATION : ',A8,' AVEC TYPEXN = ',A1)
98         FORMAT(1X,'OMSEG (BIEF) : TYPEXM = ',A1,' DOES NOT GO',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
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
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+CTN ') THEN
!
!  THE CASES WHERE N IS SYMMETRICAL ARE TREATED WITH M=M+CN
!
        CALL OV( 'X=X+CY  ' , DM , DN , Z , C , NDIAG )
!
        IF(NSEG1.NE.NSEG2) THEN
          IF(LNG.EQ.1) WRITE(LU,*) 'M+CTN : MATRICE RECTANGULAIRE
     &                              NON IMPLEMENTE'
          IF(LNG.EQ.2) WRITE(LU,*) 'M+CTN : RECTANGULAR MATRIX
     &                              NOT IMPLEMENTED'
          CALL PLANTE(1)
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
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'TN    ') THEN
!
        CALL OV( 'X=Y     ' , DM , DN , Z , C , NDIAG )
!
        IF(TYPEXN(1:1).EQ.'S') THEN
          CALL OV( 'X=Y     ' , XM , XN , Z , C , NSEG1 )
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
          IF(TYPEXM(1:1).NE.'S'.AND.NSEG1.NE.NSEG2) THEN
            IF(LNG.EQ.1) WRITE(LU,*) 'TN : MATRICE RECTANGULAIRE
     &                                NON IMPLEMENTE'
            IF(LNG.EQ.2) WRITE(LU,*) 'TN : RECTANGULAR MATRIX
     &                                NOT IMPLEMENTED'
            CALL PLANTE(1)
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
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+N   '.OR.
     &      (OP(3:8).EQ.'M+TN  ').AND.TYPEXN(1:1).NE.'Q') THEN
!
        CALL OV( 'X=X+Y   ' , DM , DN , Z , C , NDIAG )
!
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
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+TN  ') THEN
!
!     THE CASE WHERE N IS SYMMETRICAL HAS ALREADY BEEN TREATED
!
        CALL OV( 'X=X+Y   ' , DM , DN , Z , C , NDIAG )
!
        IF(NSEG1.NE.NSEG2) THEN
          IF(LNG.EQ.1) WRITE(LU,*) 'M+TN : MATRICE RECTANGULAIRE
     &                              NON IMPLEMENTE'
          IF(LNG.EQ.2) WRITE(LU,*) 'M+TN : RECTANGULAR MATRIX
     &                              NOT IMPLEMENTED'
          CALL PLANTE(1)
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
!
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
!
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
!
!----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'DMD   ') THEN
!
!   DIAGONAL TERMS
!
        IF(TYPDIM(1:1).EQ.'Q') THEN
          CALL OV( 'X=XY    ' , DM , D , Z , C , NDIAG )
          CALL OV( 'X=XY    ' , DM , D , Z , C , NDIAG )
        ELSEIF(TYPDIM(1:1).EQ.'I') THEN
          CALL OV( 'X=YZ    ' , DM , D , D , C , NDIAG )
          TYPDIM(1:1)='Q'
        ELSEIF(TYPDIM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,12) TYPDIM(1:1)
          IF (LNG.EQ.2) WRITE(LU,13) TYPDIM(1:1)
12        FORMAT(1X,'OMSEG (BIEF) : TYPDIM INCONNU :',A1)
13        FORMAT(1X,'OMSEG (BIEF) : TYPDIM UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!   EXTRADIAGONAL TERMS
!
        IF(TYPEXM(1:1).EQ.'S') THEN
!
        DO ISEG = 1 , NSEG1
          XM(ISEG)=XM(ISEG)*D(GLOSEG(ISEG,1))*D(GLOSEG(ISEG,2))
        ENDDO
!
        ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
!
        DO ISEG = 1 , NSEG1
          XM(ISEG     )=XM(ISEG     )
     &                     *D(GLOSEG(ISEG,1))*D(GLOSEG(ISEG,2))
          XM(ISEG+DIMX)=XM(ISEG+DIMX)
     &                     *D(GLOSEG(ISEG,1))*D(GLOSEG(ISEG,2))
        ENDDO
!
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,20) TYPEXM(1:1)
          IF (LNG.EQ.2) WRITE(LU,21) TYPEXM(1:1)
20        FORMAT(1X,'OMSEG (BIEF) : TYPEXM INCONNU :',A1)
21        FORMAT(1X,'OMSEG (BIEF) : TYPEXM UNKNOWN :',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+D   ') THEN
!
        CALL OV( 'X=X+Y   ' , DM , D , Z , 0.D0 , NDIAG )
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
           CALL OV( 'X=C     ' , XM , Y , Z , 0.D0 , NSEG1 )
        ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
           CALL OV( 'X=C     ' , XM , Y , Z , 0.D0 , NSEG1+NSEG2 )
        ELSEIF(TYPEXM(1:1).NE.'0') THEN
          IF (LNG.EQ.1) WRITE(LU,710) TYPEXM(1:1)
          IF (LNG.EQ.2) WRITE(LU,711) TYPEXM(1:1)
710       FORMAT(1X,'OMSEG (BIEF) : TYPEXM INCONNU :',A1)
711       FORMAT(1X,'OMSEG (BIEF) : TYPEXM UNKNOWN :',A1)
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
!
!-----------------------------------------------------------------------
!
!     ELSEIF(OP(3:8).EQ.'MSK(M)') THEN
!
!     IF(TYPEXM(1:1).EQ.'S') THEN
!       J = 3
!     ELSEIF(TYPEXM(1:1).EQ.'Q') THEN
!       J = 6
!     ELSEIF(TYPEXM(1:1).EQ.'0') THEN
!       J = 0
!     ELSE
!       IF(LNG.EQ.1) WRITE(LU,710) TYPEXM
!       IF(LNG.EQ.2) WRITE(LU,711) TYPEXM
!       J=0
!       CALL PLANTE(1)
!       STOP
!     ENDIF
!
!     IF(J.GT.0) THEN
!        DO I = 1,J
!           CALL OV ( 'X=XY    ' , XM(1,I) , D , Z , C , NELEM )
!        ENDDO
!     ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF (LNG.EQ.1) WRITE(LU,40) OP
        IF (LNG.EQ.2) WRITE(LU,41) OP
40      FORMAT(1X,'OMSEG (BIEF) : OPERATION INCONNUE : ',A8)
41      FORMAT(1X,'OMSEG (BIEF) : UNKNOWN OPERATION : ',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
