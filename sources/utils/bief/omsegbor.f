!                    *******************
                     SUBROUTINE OMSEGBOR
!                    *******************
!
     &(OP ,  DM,TYPDIM,XM,TYPEXM,   DN,TYPDIN,XN,TYPEXN,   D,C,
     & NDIAG,NSEG1,NSEG2,NBOR,NPTFR,IELM1,IELN1,NSEG11,
     & IKLBOR,NELEBX,NELEB)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    OPERATIONS BETWEEN A MATRIX WITH EDGE-BASED STORAGE
!+                AND A BOUNDARY MATRIX.
!code
!+   D: DIAGONAL MATRIX
!+   C: CONSTANT
!+
!+   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!+   PERFORMED ON MATRICES M AND N, D AND C.
!+
!+   THE RESULT IS MATRIX M.
!+
!+      OP = 'M=M+N   '  : ADDS N TO M
!+      OP = 'M=M+TN  '  : ADDS TRANSPOSE(N) TO M
!
!note     IF BOTH MATRICES ARE QUADRATIC, THE NUMBER OF OFF-DIAGONAL TERMS
!+   IS MULTIPLIED BY 3 (THERE ARE 3 QUADRATIC SEGMENTS PER BOUNDARY
!+   SEGMENT), HENCE THE TERMS 3*NPTFR, WHICH ORIGINATES FROM THE FACT
!+   THAT SEGMENTS IN THE QUADRATIC TRIANGLE AND QUADRATIC SEGMENTS IN
!+   THE BOUNDARY SEGMENT ARE NUMBERED IN THE SAME ORDER.
!
!history  J-M HERVOUET (LNHE)
!+        12/02/2010
!+        V6P0
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
!history  J-M HERVOUET (LNHE)
!+        14/05/2012
!+        V6P2
!+   Bug corrected in the quadratic case.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        13/03/2014
!+        V7P0
!+   Now written to enable different numbering of boundary points and
!+   boundary segments.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| C              |-->| A GIVEN CONSTANT USED IN OPERATION OP
!| D              |-->| A DIAGONAL MATRIX
!| DM             |<->| DIAGONAL OF M
!| DN             |-->| DIAGONAL OF N
!| IELM1          |-->| TYPE OF ELEMENT OF M
!| IELN1          |-->| TYPE OF ELEMENT OF N
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NDIAG          |-->| NUMBER OF TERMS IN THE DIAGONAL
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSEG1          |-->| NUMBER OF SEGMENTS CONSIDERED IN M
!| NSEG11         |-->| NUMBER OF LINEAR SEGMENTS
!| NSEG2          |-->| NOT USED
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
      USE BIEF, EX_OMSEGBOR => OMSEGBOR
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NDIAG,NSEG1,NSEG2,NPTFR,IELM1,IELN1,NSEG11
      INTEGER, INTENT(IN) :: NELEBX,NELEB
      INTEGER, INTENT(IN) :: NBOR(*),IKLBOR(NELEBX,*)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),D(*),XN(NELEBX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DM(*),XM(NSEG1,*)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIM,TYPEXM,TYPDIN,TYPEXN
      DOUBLE PRECISION, INTENT(IN)    :: C
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEB,K
      DOUBLE PRECISION Z(1)
!
!-----------------------------------------------------------------------
!
      IF(OP(1:8).EQ.'M=M+N   ') THEN
!
        IF(TYPDIM.EQ.'Q'.AND.TYPDIM.EQ.'Q'.AND.NDIAG.GE.NPTFR) THEN
          CALL OVDB( 'X=X+Y   ' , DM , DN , Z , C , NBOR , NPTFR )
!         QUADRATIC POINTS IN THE MIDDLE OF SEGMENTS
          IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
            DO IELEB=1,NELEB
              K=IKLBOR(IELEB,3)
              DM(NBOR(K))=DM(NBOR(K))+DN(K)
            ENDDO
          ENDIF
        ELSE
          IF(LNG.EQ.1) WRITE(LU,198) TYPDIM(1:1),OP(1:8),TYPDIN(1:1)
          IF(LNG.EQ.2) WRITE(LU,199) TYPDIM(1:1),OP(1:8),TYPDIN(1:1)
198       FORMAT(1X,'OMSEGBOR (BIEF) : TYPDIM = ',A1,' NON PROGRAMME',
     &      /,1X,'POUR L''OPERATION : ',A8,' AVEC TYPDIN = ',A1)
199       FORMAT(1X,'OMSEGBOR (BIEF) : TYPDIM = ',A1,' NOT IMPLEMENTED',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPDIN = ',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
!       THE BOUNDARY SEGMENTS ARE NUMBERED LIKE THE BOUNDARY NUMBERING
!       OF THEIR FIRST POINT (SEE STOSEG). HENCE THE (RELATIVELY SIMPLE)
!       IMPLEMENTATION BELOW. FURTHERMORE, ORISEG IS ALWAYS 1 FOR
!       BOUNDARY SEGMENTS, WHICH ALLOWS THE SHIFT OF NSEG11 AND 2*NSEG11
!       TO GET THE FIRST THEN THE SECOND HALF SEGMENT (SEE COMP_SEG).
!
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
!
!          CASE WHERE BOTH MATRICES ARE NON SYMMETRICAL
           IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
!            HERE XN(NELEBX,6) IN SEGMENTS POINT 3 IS THE MIDDLE
!            STORING IN XN  :  1-2  1-3  2-3  2-1  3-1  2-3
             CALL OV('X=X+Y   ',XM(         1,1),XN(1,1),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(         1,2),XN(1,4),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(  NSEG11+1,1),XN(1,2),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(  NSEG11+1,2),XN(1,5),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(2*NSEG11+1,1),XN(1,3),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(2*NSEG11+1,2),XN(1,6),Z,0.D0,NELEB)
           ELSE
!            HERE XN(NELEBX,2)
             CALL OV('X=X+Y   ',XM(1,1),XN(1,1),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(1,2),XN(1,2),Z,0.D0,NELEB)
           ENDIF
!
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!          CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
           IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
!            HERE XN(NELEBX,3)
             CALL OV('X=X+Y   ',XM(         1,1),XN(1,1),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(         1,2),XN(1,1),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(  NSEG11+1,1),XN(1,2),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(  NSEG11+1,2),XN(1,2),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(2*NSEG11+1,1),XN(1,3),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(2*NSEG11+1,2),XN(1,3),Z,0.D0,NELEB)
           ELSE
!            HERE XN(NPTFR,1)
             CALL OV('X=X+Y   ',XM(1,1),XN(1,1),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(1,2),XN(1,1),Z,0.D0,NELEB)
           ENDIF
!
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!          CASE WHERE BOTH MATRICES ARE SYMMETRICAL
           IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
!            HERE XN(NELEBX,3)
             CALL OV('X=X+Y   ',XM(         1,1),XN(1,1),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(  NSEG11+1,1),XN(1,2),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(2*NSEG11+1,1),XN(1,3),Z,0.D0,NELEB)
           ELSE
!            HERE XN(NELEBX,1)
             CALL OV('X=X+Y   ',XM(1,1),XN(1,1),Z,0.D0,NELEB)
           ENDIF
!
        ELSE
!
           IF (LNG.EQ.1) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
98         FORMAT(1X,'OMSEGBOR (BIEF) : TYPEXM = ',A1,
     &      ' NE CONVIENT PAS',
     &       /,1X,'POUR L''OPERATION : ',A8,' AVEC TYPEXN = ',A1)
99         FORMAT(1X,'OMSEGBOR (BIEF) : TYPEXM = ',A1,' DOES NOT GO',
     &       /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
           CALL PLANTE(1)
           STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSEIF(OP(1:8).EQ.'M=M+TN  ') THEN
!
        IF(TYPDIM.EQ.'Q'.AND.TYPDIM.EQ.'Q'.AND.NDIAG.GE.NPTFR) THEN
          CALL OVDB( 'X=X+Y   ' , DM , DN , Z , C , NBOR , NPTFR )
!         QUADRATIC POINTS IN THE MIDDLE OF SEGMENTS
          IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
            DO IELEB=1,NELEB
              K=IKLBOR(IELEB,3)
              DM(NBOR(K))=DM(NBOR(K))+DN(K)
            ENDDO
          ENDIF
        ELSE
          IF (LNG.EQ.1) WRITE(LU,198) TYPDIM(1:1),OP(1:8),TYPDIN(1:1)
          IF (LNG.EQ.2) WRITE(LU,199) TYPDIM(1:1),OP(1:8),TYPDIN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
!
!          CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
           IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
!            HERE XN(NELEBX,6)
             CALL OV('X=X+Y   ',XM(         1,1),XN(1,4),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(         1,2),XN(1,1),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(  NSEG11+1,1),XN(1,5),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(  NSEG11+1,2),XN(1,2),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(2*NSEG11+1,1),XN(1,6),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(2*NSEG11+1,2),XN(1,3),Z,0.D0,NELEB)
           ELSE
             CALL OV('X=X+Y   ',XM(1,1),XN(1,2),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(1,2),XN(1,1),Z,0.D0,NELEB)
           ENDIF
!
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!          CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
           IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
!            HERE XN(NELEBX,3)
             CALL OV('X=X+Y   ',XM(         1,1),XN(1,1),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(         1,2),XN(1,1),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(  NSEG11+1,1),XN(1,2),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(  NSEG11+1,2),XN(1,2),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(2*NSEG11+1,1),XN(1,3),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(2*NSEG11+1,2),XN(1,3),Z,0.D0,NELEB)
           ELSE
             CALL OV('X=X+Y   ',XM(1,1),XN(1,1),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(1,2),XN(1,1),Z,0.D0,NELEB)
           ENDIF
!
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
!
!          CASE WHERE BOTH MATRICES ARE SYMMETRICAL
           IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
!            HERE XN(NELEBX,3)
             CALL OV('X=X+Y   ',XM(         1,1),XN(1,1),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(  NSEG11+1,1),XN(1,2),Z,0.D0,NELEB)
             CALL OV('X=X+Y   ',XM(2*NSEG11+1,1),XN(1,3),Z,0.D0,NELEB)
           ELSE
             CALL OV('X=X+Y   ',XM(1,1),XN(1,1),Z,0.D0,NELEB)
           ENDIF
!
        ELSE
!
           IF (LNG.EQ.1) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
           CALL PLANTE(1)
           STOP
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF (LNG.EQ.1) WRITE(LU,70) OP
        IF (LNG.EQ.2) WRITE(LU,71) OP
70      FORMAT(1X,'OMSEGBOR (BIEF) : OPERATION INCONNUE : ',A8)
71      FORMAT(1X,'OMSEGBOR (BIEF) : UNKNOWN OPERATION : ',A8)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
