C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPERATIONS BETWEEN A MATRIX WITH EDGE-BASED STORAGE
!>                AND A BOUNDARY MATRIX.
!>  @code
!>   D: DIAGONAL MATRIX
!>   C: CONSTANT
!>
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON MATRICES M AND N, D AND C.
!>
!>   THE RESULT IS MATRIX M.
!>
!>      OP = 'M=M+N   '  : ADDS N TO M
!>      OP = 'M=M+TN  '  : ADDS TRANSPOSE(N) TO M
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IF BOTH MATRICES ARE QUADRATIC, THE NUMBER OF OFF-DIAGONAL TERMS
!>   IS MULTIPLIED BY 3 (THERE ARE 3 QUADRATIC SEGMENTS PER BOUNDARY
!>   SEGMENT), HENCE THE TERMS 3*NPTFR, WHICH ORIGINATES FROM THE FACT
!>   THAT SEGMENTS IN THE QUADRATIC TRIANGLE AND QUADRATIC SEGMENTS IN
!>   THE BOUNDARY SEGMENT ARE NUMBERED IN THE SAME ORDER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, D, DM, DN, IELM1, IELN1, KP1BOR, NBOR, NDIAG, NPTFR, NSEG1, NSEG2, OP, TYPDIM, TYPDIN, TYPEXM, TYPEXN, XM, XN
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
!>    </th><td> IPTFR, NSE, NSEG11, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_OMSEGBOR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> NBSEG(), OV(), OVDB(), PLANTE()
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
!>      <td><center> 6.0                                       </center>
!> </td><td> 12/02/2010
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
!>          <tr><td>IELM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELN1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>---</td><td>
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
!>          <tr><td>NPTFR
!></td><td>---</td><td>
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
                        SUBROUTINE OMSEGBOR
     &(OP ,  DM,TYPDIM,XM,TYPEXM,   DN,TYPDIN,XN,TYPEXN,   D,C,
     & NDIAG,NSEG1,NSEG2,NBOR,KP1BOR,NPTFR,IELM1,IELN1)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
C| D             |-->| MATRICE DIAGONALE
C| DM,TYPDIM      |<->| DIAGONALE ET TYPE DE DIAGONALE DE M
C| DN,TYPDIN      |-->| DIAGONALE ET TYPE DE DIAGONALE DE N
C| IELM1          |---| 
C| IELN1          |---| 
C| IKLE           |-->| CORRESPONDANCE NUMEROTATIONS LOCALE ET GLOBALE
C| KP1BOR         |---| 
C| NBOR           |---| 
C| NDIAG          |-->| NOMBRE DE VALEURS DE LA DIAGONALE.
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NPTFR          |---| 
C| NSEG1          |---| 
C| NSEG2          |---| 
C| OP             |-->| OPERATION A EFFECTUER
C| XM,TYPEXM      |-->| TERMES EXTRA-DIAG. ET TYPE POUR M
C| XN,TYPEXN      |-->| TERMES EXTRA-DIAG. ET TYPE POUR N
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_OMSEGBOR => OMSEGBOR
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NDIAG,NSEG1,NSEG2,NPTFR,IELM1,IELN1
      INTEGER, INTENT(IN) :: NBOR(NPTFR,*),KP1BOR(NPTFR)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
      DOUBLE PRECISION, INTENT(IN)    :: DN(*),D(*),XN(NPTFR,*)
      DOUBLE PRECISION, INTENT(INOUT) :: DM(*),XM(NSEG1,*)
      CHARACTER(LEN=1), INTENT(INOUT) :: TYPDIM,TYPEXM,TYPDIN,TYPEXN
      DOUBLE PRECISION, INTENT(IN)    :: C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPTFR,NSE,NSEG11
      DOUBLE PRECISION Z(1)
C
C-----------------------------------------------------------------------
C
      IF(OP(1:8).EQ.'M=M+N   ') THEN
C
        IF(TYPDIM.EQ.'Q'.AND.TYPDIM.EQ.'Q'.AND.NDIAG.GE.NPTFR) THEN
          CALL OVDB( 'X=X+Y   ' , DM , DN , Z , C , NBOR , NPTFR )
C         QUADRATIC POINTS IN THE MIDDLE OF SEGMENTS
          IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
            DO IPTFR=1,NPTFR
              DM(NBOR(IPTFR,2))=DN(IPTFR+NPTFR)
            ENDDO
          ENDIF
        ELSE
          IF (LNG.EQ.1) WRITE(LU,198) TYPDIM(1:1),OP(1:8),TYPDIN(1:1)
          IF (LNG.EQ.2) WRITE(LU,199) TYPDIM(1:1),OP(1:8),TYPDIN(1:1)
198       FORMAT(1X,'OMSEGBOR (BIEF) : TYPDIM = ',A1,' NON PROGRAMME',
     &      /,1X,'POUR L''OPERATION : ',A8,' AVEC TYPDIN = ',A1)
199       FORMAT(1X,'OMSEGBOR (BIEF) : TYPDIM = ',A1,' NOT IMPLEMENTED',
     &      /,1X,'FOR THE OPERATION : ',A8,' WITH TYPDIN = ',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
C
C       THE BOUNDARY SEGMENTS ARE NUMBERED LIKE THE BOUNDARY NUMBERING
C       OF THEIR FIRST POINT (SEE STOSEG). HENCE THE (RELATIVELY SIMPLE)
C       IMPLEMENTATION BELOW. FURTHERMORE, ORISEG IS ALWAYS 1 FOR
C       BOUNDARY SEGMENTS, WHICH ALLOWS THE SHIFT OF NSEG11 AND 2*NSEG11
C       TO GET THE FIRST THEN THE SECOND HALF SEGMENT (SEE COMP_SEG).
C
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
C
C          CASE WHERE BOTH MATRICES ARE NON SYMMETRICAL
           IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
C            HERE XN(NPTFR,6) IN SEGMENTS POINT 3 IS THE MIDDLE
C            STORING IN XN  :  1-2  1-3  2-3  2-1  3-1  2-3
             NSEG11=NBSEG(11)
             IF(NCSIZE.GT.1) THEN
               NSE = 0
               DO IPTFR = 1 , NPTFR
                 IF(KP1BOR(IPTFR).NE.IPTFR) THEN
                   NSE = NSE + 1
                   XM(         NSE,1)=XM(         NSE,1)+XN(IPTFR,1)
                   XM(         NSE,2)=XM(         NSE,2)+XN(IPTFR,4)
                   XM(  NSEG11+NSE,1)=XM(  NSEG11+NSE,1)+XN(IPTFR,2)
                   XM(  NSEG11+NSE,2)=XM(  NSEG11+NSE,2)+XN(IPTFR,5)
                   XM(2*NSEG11+NSE,1)=XM(2*NSEG11+NSE,1)+XN(IPTFR,3)
                   XM(2*NSEG11+NSE,2)=XM(2*NSEG11+NSE,2)+XN(IPTFR,6)
                 ENDIF
               ENDDO
             ELSE
               CALL OV('X=X+Y   ',XM(         1,1),XN(1,1),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(         1,2),XN(1,4),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(  NSEG11+1,1),XN(1,2),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(  NSEG11+1,2),XN(1,5),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(2*NSEG11+1,1),XN(1,3),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(2*NSEG11+1,2),XN(1,6),Z,0.D0,NPTFR)
             ENDIF
           ELSE
C            HERE XN(NPTFR,2)
             IF(NCSIZE.GT.1) THEN
               NSE = 0
               DO IPTFR = 1 , NPTFR
                 IF(KP1BOR(IPTFR).NE.IPTFR) THEN
                   NSE = NSE + 1
                   XM(NSE,1)=XM(NSE,1)+XN(IPTFR,1)
                   XM(NSE,2)=XM(NSE,2)+XN(IPTFR,2)
                 ENDIF
               ENDDO
             ELSE
               CALL OV('X=X+Y   ',XM(1,1),XN(1,1),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(1,2),XN(1,2),Z,0.D0,NPTFR)
             ENDIF
           ENDIF
C
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
           IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
C            HERE XN(NPTFR,3)
             NSEG11=NBSEG(11)
             IF(NCSIZE.GT.1) THEN
               NSE = 0
               DO IPTFR = 1 , NPTFR
                 IF(KP1BOR(IPTFR).NE.IPTFR) THEN
                   NSE = NSE + 1
                   XM(         NSE,1)=XM(         NSE,1)+XN(IPTFR,1)
                   XM(         NSE,2)=XM(         NSE,2)+XN(IPTFR,1)
                   XM(  NSEG11+NSE,1)=XM(  NSEG11+NSE,1)+XN(IPTFR,2)
                   XM(  NSEG11+NSE,2)=XM(  NSEG11+NSE,2)+XN(IPTFR,2)
                   XM(2*NSEG11+NSE,1)=XM(2*NSEG11+NSE,1)+XN(IPTFR,3)
                   XM(2*NSEG11+NSE,2)=XM(2*NSEG11+NSE,2)+XN(IPTFR,3)
                 ENDIF
               ENDDO
             ELSE
               CALL OV('X=X+Y   ',XM(         1,1),XN(1,1),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(         1,2),XN(1,1),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(  NSEG11+1,1),XN(1,2),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(  NSEG11+1,2),XN(1,2),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(2*NSEG11+1,1),XN(1,3),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(2*NSEG11+1,2),XN(1,3),Z,0.D0,NPTFR)
             ENDIF
           ELSE
C            HERE XN(NPTFR,1)
             IF(NCSIZE.GT.1) THEN
               NSE = 0
               DO IPTFR = 1 , NPTFR
                 IF(KP1BOR(IPTFR).NE.IPTFR) THEN
                   NSE = NSE + 1
                   XM(NSE,1)=XM(NSE,1)+XN(IPTFR,1)
                   XM(NSE,2)=XM(NSE,2)+XN(IPTFR,1)
                 ENDIF
               ENDDO
             ELSE
               CALL OV('X=X+Y   ',XM(1,1),XN(1,1),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(1,2),XN(1,1),Z,0.D0,NPTFR)
             ENDIF
           ENDIF
C
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE BOTH MATRICES ARE SYMMETRICAL
           IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
C            HERE XN(NPTFR,3)
             NSEG11=NBSEG(11)
             IF(NCSIZE.GT.1) THEN
               NSE = 0
               DO IPTFR = 1 , NPTFR
                 IF(KP1BOR(IPTFR).NE.IPTFR) THEN
                   NSE = NSE + 1
                   XM(         NSE,1)=XM(         NSE,1)+XN(IPTFR,1)
                   XM(  NSEG11+NSE,1)=XM(  NSEG11+NSE,1)+XN(IPTFR,2)
                   XM(2*NSEG11+NSE,1)=XM(2*NSEG11+NSE,1)+XN(IPTFR,3)
                 ENDIF
               ENDDO
             ELSE
               CALL OV('X=X+Y   ',XM(         1,1),XN(1,1),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(  NSEG11+1,1),XN(1,2),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(2*NSEG11+1,1),XN(1,3),Z,0.D0,NPTFR)
             ENDIF
           ELSE
C            HERE XN(NPTFR,1)
             IF(NCSIZE.GT.1) THEN
               NSE = 0
               DO IPTFR = 1 , NPTFR
                 IF(KP1BOR(IPTFR).NE.IPTFR) THEN
                   NSE = NSE + 1
                   XM(NSE,1)=XM(NSE,1)+XN(IPTFR,1)
                 ENDIF
               ENDDO
             ELSE
               CALL OV('X=X+Y   ',XM(1,1),XN(1,1),Z,0.D0,NPTFR)
             ENDIF
           ENDIF
C
        ELSE
C
           IF (LNG.EQ.1) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
98         FORMAT(1X,'OMSEGBOR (BIEF) : TYPEXM = ',A1,
     &      ' NE CONVIENT PAS',
     &       /,1X,'POUR L''OPERATION : ',A8,' AVEC TYPEXN = ',A1)
99         FORMAT(1X,'OMSEGBOR (BIEF) : TYPEXM = ',A1,' DOES NOT GO',
     &       /,1X,'FOR THE OPERATION : ',A8,' WITH TYPEXN = ',A1)
           CALL PLANTE(1)
           STOP
C
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'M=M+TN  ') THEN
C
        IF(TYPDIM.EQ.'Q'.AND.TYPDIM.EQ.'Q'.AND.NDIAG.GE.NPTFR) THEN
          CALL OVDB( 'X=X+Y   ' , DM , DN , Z , C , NBOR , NPTFR )
C         QUADRATIC POINTS IN THE MIDDLE OF SEGMENTS
          IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
            DO IPTFR=1,NPTFR
              DM(NBOR(IPTFR,2))=DN(IPTFR+NPTFR)
            ENDDO
          ENDIF
        ELSE
          IF (LNG.EQ.1) WRITE(LU,198) TYPDIM(1:1),OP(1:8),TYPDIN(1:1)
          IF (LNG.EQ.2) WRITE(LU,199) TYPDIM(1:1),OP(1:8),TYPDIN(1:1)
          CALL PLANTE(1)
          STOP
        ENDIF
C
        IF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'Q') THEN
C
C          CASE WHERE BOTH MATRICES ARE NONSYMMETRICAL
           IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
C            HERE XN(NPTFR,6)
             NSEG11=NBSEG(11)
             IF(NCSIZE.GT.1) THEN
               NSE = 0
               DO IPTFR = 1 , NPTFR
                 IF(KP1BOR(IPTFR).NE.IPTFR) THEN
                   NSE = NSE + 1
                   XM(         NSE,1)=XM(         NSE,1)+XN(IPTFR,4)
                   XM(         NSE,2)=XM(         NSE,2)+XN(IPTFR,1)
                   XM(  NSEG11+NSE,1)=XM(  NSEG11+NSE,1)+XN(IPTFR,5)
                   XM(  NSEG11+NSE,2)=XM(  NSEG11+NSE,2)+XN(IPTFR,2)
                   XM(2*NSEG11+NSE,1)=XM(2*NSEG11+NSE,1)+XN(IPTFR,6)
                   XM(2*NSEG11+NSE,2)=XM(2*NSEG11+NSE,2)+XN(IPTFR,3)
                 ENDIF
               ENDDO
             ELSE
               CALL OV('X=X+Y   ',XM(         1,1),XN(1,4),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(         1,2),XN(1,1),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(  NSEG11+1,1),XN(1,5),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(  NSEG11+1,2),XN(1,2),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(2*NSEG11+1,1),XN(1,6),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(2*NSEG11+1,2),XN(1,3),Z,0.D0,NPTFR)
             ENDIF
           ELSE
             IF(NCSIZE.GT.1) THEN
               NSE = 0
               DO IPTFR = 1 , NPTFR
                 IF(KP1BOR(IPTFR).NE.IPTFR) THEN
                   NSE = NSE + 1
                   XM(NSE,1)=XM(NSE,1)+XN(IPTFR,2)
                   XM(NSE,2)=XM(NSE,2)+XN(IPTFR,1)
                 ENDIF
               ENDDO
             ELSE
               CALL OV('X=X+Y   ',XM(1,1),XN(1,2),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(1,2),XN(1,1),Z,0.D0,NPTFR)
             ENDIF
           ENDIF
C
        ELSEIF(TYPEXM(1:1).EQ.'Q'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE M CAN BE ANYTHING AND N IS SYMMETRICAL
           IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
C            HERE XN(NPTFR,3)
             NSEG11=NBSEG(11)
             IF(NCSIZE.GT.1) THEN
               NSE = 0
               DO IPTFR = 1 , NPTFR
                 IF(KP1BOR(IPTFR).NE.IPTFR) THEN
                   NSE = NSE + 1
                   XM(         NSE,1)=XM(         NSE,1)+XN(IPTFR,1)
                   XM(         NSE,2)=XM(         NSE,2)+XN(IPTFR,1)
                   XM(  NSEG11+NSE,1)=XM(  NSEG11+NSE,1)+XN(IPTFR,2)
                   XM(  NSEG11+NSE,2)=XM(  NSEG11+NSE,2)+XN(IPTFR,2)
                   XM(2*NSEG11+NSE,1)=XM(2*NSEG11+NSE,1)+XN(IPTFR,3)
                   XM(2*NSEG11+NSE,2)=XM(2*NSEG11+NSE,2)+XN(IPTFR,3)
                 ENDIF
               ENDDO
             ELSE
               CALL OV('X=X+Y   ',XM(         1,1),XN(1,1),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(         1,2),XN(1,1),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(  NSEG11+1,1),XN(1,2),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(  NSEG11+1,2),XN(1,2),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(2*NSEG11+1,1),XN(1,3),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(2*NSEG11+1,2),XN(1,3),Z,0.D0,NPTFR)
             ENDIF
           ELSE
             IF(NCSIZE.GT.1) THEN
               NSE = 0
               DO IPTFR = 1 , NPTFR
                 IF(KP1BOR(IPTFR).NE.IPTFR) THEN
                   NSE = NSE + 1
                   XM(NSE,1)=XM(NSE,1)+XN(IPTFR,1)
                   XM(NSE,2)=XM(NSE,2)+XN(IPTFR,1)
                 ENDIF
               ENDDO
             ELSE
               CALL OV('X=X+Y   ',XM(1,1),XN(1,1),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(1,2),XN(1,1),Z,0.D0,NPTFR)
             ENDIF
           ENDIF
C
        ELSEIF(TYPEXM(1:1).EQ.'S'.AND.TYPEXN(1:1).EQ.'S') THEN
C
C          CASE WHERE BOTH MATRICES ARE SYMMETRICAL
           IF(IELM1.EQ.13.AND.IELN1.EQ.2) THEN
C            HERE XN(NPTFR,3)
             NSEG11=NBSEG(11)
             IF(NCSIZE.GT.1) THEN
               NSE = 0
               DO IPTFR = 1 , NPTFR
                 IF(KP1BOR(IPTFR).NE.IPTFR) THEN
                   NSE = NSE + 1
                   XM(         NSE,1)=XM(         NSE,1)+XN(IPTFR,1)
                   XM(  NSEG11+NSE,1)=XM(  NSEG11+NSE,1)+XN(IPTFR,2)
                   XM(2*NSEG11+NSE,1)=XM(2*NSEG11+NSE,1)+XN(IPTFR,3)
                 ENDIF
               ENDDO
             ELSE
               CALL OV('X=X+Y   ',XM(         1,1),XN(1,1),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(  NSEG11+1,1),XN(1,2),Z,0.D0,NPTFR)
               CALL OV('X=X+Y   ',XM(2*NSEG11+1,1),XN(1,3),Z,0.D0,NPTFR)
             ENDIF
           ELSE
             IF(NCSIZE.GT.1) THEN
               NSE = 0
               DO IPTFR = 1 , NPTFR
                 IF(KP1BOR(IPTFR).NE.IPTFR) THEN
                   NSE = NSE + 1
                   XM(NSE,1)=XM(NSE,1)+XN(IPTFR,1)
                 ENDIF
               ENDDO
             ELSE
               CALL OV('X=X+Y   ',XM(1,1),XN(1,1),Z,0.D0,NPTFR)
             ENDIF
           ENDIF
C
        ELSE
C
           IF (LNG.EQ.1) WRITE(LU,98) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
           IF (LNG.EQ.2) WRITE(LU,99) TYPEXM(1:1),OP(1:8),TYPEXN(1:1)
           CALL PLANTE(1)
           STOP
C
         ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,70) OP
        IF (LNG.EQ.2) WRITE(LU,71) OP
70      FORMAT(1X,'OMSEGBOR (BIEF) : OPERATION INCONNUE : ',A8)
71      FORMAT(1X,'OMSEGBOR (BIEF) : UNKNOWN OPERATION : ',A8)
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