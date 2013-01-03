!                    *******************
                     SUBROUTINE OMBORSEG
!                    *******************
!
     &(OP,DM,TYPDIM,XM,TYPEXM,DN,TYPDIN,XN,TYPEXN,D,C,
     & NDIAG,MSEG1,MSEG2,NSEG1,NSEG2,GLOSEG,SIZGLO,NBOR,NPTFR)
!
!***********************************************************************
! BIEF   V6P3                                   01/01/2013
!***********************************************************************
!
!brief    OPERATIONS ON MATRICES WITH AN EDGE-BASED STORAGE
!         WHERE N IS A BOUNDARY MATRIX
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
!+      OP = 'M=M-ND  '  : SUBTRACTS ND FROM M
!+      OP = 'M=M-DN  '  : SUBTRACTS DN FROM M
!
!history  F. DECUNG (LNHE)
!+        V6P3
!+   Adapted from omseg.f
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| A GIVEN CONSTANT USED IN OPERATION OP
!| D              |-->| A DIAGONAL MATRIX
!| DM             |<->| DIAGONAL OF M
!| DN             |-->| DIAGONAL OF N
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NDIAG          |-->| NUMBER OF TERMS IN THE DIAGONAL
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| MSEG1          |-->| NUMBER OF SEGMENTS OF LINE ELEMENT OF M
!| MSEG2          |-->| NUMBER OF SEGMENTS OF COLUMN ELEMENT OF M
!| NSEG1          |-->| NUMBER OF SEGMENTS OF LINE ELEMENT OF N
!| NSEG2          |-->| NUMBER OF SEGMENTS OF COLUMN ELEMENT OF N
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
      USE BIEF, EX_OMBORSEG => OMBORSEG
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NDIAG,MSEG1,MSEG2,NSEG1,NSEG2,SIZGLO,NPTFR
      INTEGER, INTENT(IN) :: GLOSEG(SIZGLO,2)
      INTEGER, INTENT(IN) :: NBOR(NPTFR,*)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),D(*)
!     XM AND XN MAY ONLY BE OF SIZE NSEG1 IF THE MATRIX IS SYMMETRICAL
!     SIZE GIVEN HERE ONLY TO CHECK BOUNDS
      DOUBLE PRECISION, INTENT(INOUT) :: XM(MSEG1+MSEG2)
      DOUBLE PRECISION, INTENT(IN)    :: XN(NSEG1+NSEG2)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIM,TYPEXM,TYPDIN,TYPEXN
      DOUBLE PRECISION, INTENT(INOUT) :: DM(*)
      DOUBLE PRECISION, INTENT(IN)    :: C
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTRINSIC MIN
!
      INTEGER ISEG,DIMX,DIMY
!
      DOUBLE PRECISION Y(1),Z(1)
!
!-----------------------------------------------------------------------
!
!     ARRAYS XM AND XN ARE BASICALLY OF SIZE XM(DIMX,1 OR 2)
!     BUT IN THE CASE OF RECTANGULAR MATRICES OTHER DATA ARE STORED
!     BEYOND XM(2*DIMX)
!
      DIMX=MIN(MSEG1,MSEG2)
      DIMY=MAX(NSEG1,NSEG2)     
!
      IF(OP(3:8).EQ.'N     ') THEN
!
        IF(TYPDIN(1:1).EQ.'Q') THEN
          CALL OVDB( 'X=Y     ' , DM , DN , Z , C , NBOR, NDIAG )
        ELSEIF(TYPDIN(1:1).EQ.'I'.OR.TYPDIN(1:1).EQ.'0') THEN
!         NOTHING TO DO, ONLY NEEDS TO COPY TYPDIN
        ELSE
           IF (LNG.EQ.1) WRITE(LU,5) TYPDIN(1:1)
           IF (LNG.EQ.2) WRITE(LU,6) TYPDIN(1:1)
5          FORMAT(1X,'OMBORSEG (BIEF) : TYPDIN INCONNU :',A1)
6          FORMAT(1X,'OMBORSEG (BIEF) : TYPDIN UNKNOWN :',A1)
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
10         FORMAT(1X,'OMBORSEG (BIEF) : TYPEXN INCONNU :',A1)
11         FORMAT(1X,'OMBORSEG (BIEF) : TYPEXN UNKNOWN :',A1)
           CALL PLANTE(1)
           STOP
        ENDIF
        TYPEXM(1:1)=TYPEXN(1:1)
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'CN    ') THEN
!
        CALL OVDB( 'X=CY    ' , DM , DN , Z , C , NBOR, NDIAG )
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
          CALL OVDB( 'X=X+C   ' , DM , DN , Z , C , NBOR, NDIAG )
        ELSEIF(TYPDIN(1:1).NE.'0') THEN
          CALL OVDB( 'X=X+CY  ' , DM , DN , Z , C , NBOR, NDIAG )
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
99          FORMAT(1X,'OMBORSEG (BIEF) : TYPEXM = ',A1,
     &       ' NE CONVIENT PAS',/,1X,'POUR L''OPERATION : ',A8,
     &       ' AVEC TYPEXN = ',A1)
98          FORMAT(1X,'OMBORSEG (BIEF) : TYPEXM = ',A1,
     &       ' DOES NOT GO',/,1X,'FOR THE OPERATION : ',A8,
     &       ' WITH TYPEXN = ',A1)
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
        CALL OVDB( 'X=X+CY  ' , DM , DN , Z , C , NBOR, NDIAG )
!
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
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'TN    ') THEN
!
        CALL OVDB( 'X=Y     ' , DM , DN , Z , C , NBOR, NDIAG )
!
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
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(3:8).EQ.'M+N   '.OR.
     &      (OP(3:8).EQ.'M+TN  ').AND.TYPEXN(1:1).NE.'Q') THEN
!
        CALL OVDB( 'X=X+Y   ' , DM , DN , Z , C , NBOR, NDIAG )
!
        IF(TYPEXN(1:1).EQ.'S') THEN
           CALL OV( 'X=X+Y   ' , XM , XN , Z , C , NSEG2 )
           IF(TYPEXM(1:1).EQ.'Q') THEN
           CALL OV( 'X=X+Y   ' , XM(DIMY+1:DIMY+NSEG2) , XN ,Z,C,NSEG2)
           ENDIF
        ELSEIF(TYPEXN(1:1).EQ.'Q') THEN
           IF(TYPEXM(1:1).NE.'Q') THEN
             IF (LNG.EQ.1) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
             IF (LNG.EQ.2) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
             CALL PLANTE(1)
             STOP
           ENDIF
           CALL OV( 'X=X+Y   ' , XM , XN , Z , C , NSEG2 ) ! FD : NOT SURE HERE
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
        CALL OVDB( 'X=X+Y   ' , DM , DN , Z , C , NBOR, NDIAG )
!
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
!
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
12         FORMAT(1X,'OMBORSEG (BIEF) : TYPDIM INCONNU :',A1)
13         FORMAT(1X,'OMBORSEG (BIEF) : TYPDIM UNKNOWN :',A1)
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
172       FORMAT(1X,'OMBORSEG (BIEF) : TYPEXM NON PREVU : ',A1)
173       FORMAT(1X,'OMBORSEG (BIEF) : TYPEXM NOT AVAILABLE : ',A1)
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
!-----------------------------------------------------------------------
!
      ELSE
!
        IF (LNG.EQ.1) WRITE(LU,40) OP
        IF (LNG.EQ.2) WRITE(LU,41) OP
40      FORMAT(1X,'OMBORSEG (BIEF) : OPERATION INCONNUE : ',A8)
41      FORMAT(1X,'OMBORSEG (BIEF) : UNKNOWN OPERATION : ',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
