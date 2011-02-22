C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ASSEMBLES MATRICES EXTRA-DIAGONAL TERMS
!>                IN THE CASE OF EDGE-BASED STORAGE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DIM1XMT, DIM2XMT, ELTSEG, IELM1, IELM2, MESH, NAME, NELMAX, ORISEG, STO, STOXMT, TYPEXT, XM, XMT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> NELEM, STOM
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_ASSEX3
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> AS3_1111_Q(), AS3_1111_S(), AS3_1112(), AS3_1113(), AS3_1211(), AS3_1212_Q(), AS3_1212_S(), AS3_1311(), AS3_1313_Q(), AS3_1313_S(), AS3_4141_Q(), AS3_4141_S(), DIMENS(), BIEF_NBSEG(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>MATRIX()

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
!> </td><td> 05/02/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DIM1XMT
!></td><td>--></td><td>FIRST DIMENSION OF XMT
!>    </td></tr>
!>          <tr><td>DIM2XMT
!></td><td>--></td><td>SECOND DIMENSION OF XMT
!>    </td></tr>
!>          <tr><td>ELTSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NAME
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ORISEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>STO
!></td><td>--></td><td>STORAGE REQUIRED IN XM 1: EBE  3: EDGE-BASED
!>    </td></tr>
!>          <tr><td>STOXMT
!></td><td>--></td><td>STORAGE OF OFF-DIAGONAL TERMS
!>                  1: XMT(NELMAX,*)  2: XMT(*,NELMAX)
!>    </td></tr>
!>          <tr><td>TYPEXT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XM
!></td><td>--></td><td>ASSEMBLED OFF-DIAGONAL TERMS
!>    </td></tr>
!>          <tr><td>XMT
!></td><td>--></td><td>OFF-DIAGONAL TERMS OF THE WORK MATRIX
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ASSEX3
     &(XM,STO,NAME,IELM1,IELM2,TYPEXT,XMT,DIM1XMT,DIM2XMT,STOXMT,
     & MESH,NELMAX,ELTSEG,ORISEG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DIM1XMT        |-->| FIRST DIMENSION OF XMT
C| DIM2XMT        |-->| SECOND DIMENSION OF XMT
C| ELTSEG         |---| 
C| IELM1          |---| 
C| IELM2          |---| 
C| MESH           |---| 
C| NAME           |---| 
C| NELMAX         |---| 
C| ORISEG         |---| 
C| STO            |-->| STORAGE REQUIRED IN XM 1: EBE  3: EDGE-BASED
C| STOXMT         |-->| STORAGE OF OFF-DIAGONAL TERMS
C|                |   | 1: XMT(NELMAX,*)  2: XMT(*,NELMAX)
C| TYPEXT         |---| 
C| XM             |-->| ASSEMBLED OFF-DIAGONAL TERMS
C| XMT            |-->| OFF-DIAGONAL TERMS OF THE WORK MATRIX
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_ASSEX3 => ASSEX3
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(INOUT) :: STO
      CHARACTER(LEN=6), INTENT(IN)    :: NAME
      INTEGER         , INTENT(IN)    :: IELM1,IELM2,NELMAX
      INTEGER         , INTENT(IN)    :: DIM1XMT,DIM2XMT,STOXMT
      INTEGER         , INTENT(IN)    :: ELTSEG(NELMAX,*)
      INTEGER         , INTENT(IN)    :: ORISEG(NELMAX,3)
      CHARACTER(LEN=1), INTENT(IN)    :: TYPEXT
      DOUBLE PRECISION, INTENT(INOUT) :: XMT(DIM1XMT,DIM2XMT)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(*)
      TYPE(BIEF_MESH) , INTENT(IN)    :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NELEM,STOM
C
C-----------------------------------------------------------------------
C
C  EXTRACTS MATRIX M CHARACTERISTICS
C
      STOM = STO
      IF(STOM.NE.1) THEN
        IF (LNG.EQ.1) WRITE(LU,500) NAME,STOM
        IF (LNG.EQ.2) WRITE(LU,501) NAME,STOM
500     FORMAT(1X,'ASSEX3 (BIEF) : MATRICE M (NOM REEL : ',A6,')',/,1X,
     &            '                STOCKAGE NON PREVU : ',1I6)
501     FORMAT(1X,'ASSEX3 (BIEF) : MATRIX  M (REAL NAME:',A6,')',/,1X,
     &            '                UNEXPECTED STORAGE: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(DIMENS(IELM1).NE.MESH%DIM) THEN
C        BOUNDARY MATRIX : NOT TREATED HERE
         IF (LNG.EQ.1) WRITE(LU,100) NAME
         IF (LNG.EQ.2) WRITE(LU,101) NAME
         IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
         IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
         IF (LNG.EQ.1) WRITE(LU,300)
         IF (LNG.EQ.2) WRITE(LU,301)
         CALL PLANTE(0)
         STOP
      ENDIF
C
      NELEM  = MESH%NELEM
C
C-----------------------------------------------------------------------
C
      IF(IELM1.EQ.11.AND.IELM2.EQ.11) THEN
C
C       P1-P1 TRIANGLES MATRIX
C
        IF(TYPEXT.EQ.'S') THEN
          CALL AS3_1111_S(XM,BIEF_NBSEG(11,MESH),
     &                    XMT,NELMAX,NELEM,
     &                    ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3))
        ELSEIF(TYPEXT.EQ.'Q') THEN
          CALL AS3_1111_Q(XM,BIEF_NBSEG(11,MESH),
     &                    XMT,NELMAX,NELEM,
     &                    ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3),
     &                    ORISEG(1,1),ORISEG(1,2),ORISEG(1,3))
        ENDIF
C
      ELSEIF(IELM1.EQ.11.AND.IELM2.EQ.12) THEN
C
C       P1-QB TRIANGLES MATRIX
C
          CALL AS3_1112(XM,BIEF_NBSEG(IELM1,MESH),
     &                  BIEF_NBSEG(IELM2,MESH),
     &                  XMT,NELMAX,NELEM,
     &                  ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3),
     &                  ELTSEG(1,4),ELTSEG(1,5),ELTSEG(1,6),
     &                  ORISEG(1,1),ORISEG(1,2),ORISEG(1,3))
C
      ELSEIF(IELM1.EQ.11.AND.IELM2.EQ.13) THEN
C
C       P1-QUADRATIC TRIANGLES MATRIX
C
          CALL AS3_1113(XM,BIEF_NBSEG(IELM1,MESH),
     &                  BIEF_NBSEG(IELM2,MESH),
     &                  XMT,NELMAX,NELEM,ELTSEG,ORISEG)
C
      ELSEIF(IELM1.EQ.13.AND.IELM2.EQ.11) THEN
C
C       QUADRATIC-P1 TRIANGLES MATRIX
C
          CALL AS3_1311(XM,BIEF_NBSEG(IELM2,MESH),
     &                  BIEF_NBSEG(IELM1,MESH),
     &                  XMT,NELMAX,NELEM,ELTSEG,ORISEG)
C
      ELSEIF(IELM1.EQ.12.AND.IELM2.EQ.11) THEN
C
C       P1-QB TRIANGLES MATRIX
C
          CALL AS3_1211(XM,BIEF_NBSEG(11,MESH),
     &                  BIEF_NBSEG(12,MESH),
     &                  XMT,NELMAX,NELEM,
     &                  ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3),
     &                  ELTSEG(1,4),ELTSEG(1,5),ELTSEG(1,6),
     &                  ORISEG(1,1),ORISEG(1,2),ORISEG(1,3))
C
      ELSEIF(IELM1.EQ.12.AND.IELM2.EQ.12) THEN
C
C       QB-QB TRIANGLES MATRIX
C
        IF(TYPEXT.EQ.'S') THEN
          CALL AS3_1212_S(XM,BIEF_NBSEG(11,MESH),
     &                    BIEF_NBSEG(12,MESH),XMT,NELMAX,NELEM,
     &                    ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3),
     &                    ELTSEG(1,4),ELTSEG(1,5),ELTSEG(1,6))
        ELSEIF(TYPEXT.EQ.'Q') THEN
          CALL AS3_1212_Q(XM,BIEF_NBSEG(11,MESH),
     &                    BIEF_NBSEG(12,MESH),
     &                    XMT,NELMAX,NELEM,
     &                    ELTSEG(1,1),ELTSEG(1,2),ELTSEG(1,3),
     &                    ELTSEG(1,4),ELTSEG(1,5),ELTSEG(1,6),
     &                    ORISEG(1,1),ORISEG(1,2),ORISEG(1,3))
        ENDIF
C
      ELSEIF(IELM1.EQ.13.AND.IELM2.EQ.13) THEN
C
C       QUADRATIC TRIANGLES MATRIX
C
        IF(TYPEXT.EQ.'S') THEN
          CALL AS3_1313_S(XM,BIEF_NBSEG(IELM1,MESH),
     &                    XMT,DIM1XMT,DIM2XMT,STOXMT,
     &                    NELMAX,NELEM,ELTSEG)
        ELSEIF(TYPEXT.EQ.'Q') THEN
          CALL AS3_1313_Q(XM,BIEF_NBSEG(IELM1,MESH),
     &                    XMT,DIM1XMT,DIM2XMT,STOXMT,
     &                    NELMAX,NELEM,ELTSEG,ORISEG)
        ENDIF
C
      ELSEIF(IELM1.EQ.41.AND.IELM2.EQ.41) THEN
C
C       PRISMS MATRIX
C
        IF(TYPEXT.EQ.'S') THEN
          CALL AS3_4141_S(XM,BIEF_NBSEG(IELM1,MESH),
     &                    XMT,DIM1XMT,DIM2XMT,STOXMT,
     &                    NELMAX,NELEM,ELTSEG)
        ELSEIF(TYPEXT.EQ.'Q') THEN
          CALL AS3_4141_Q(XM,BIEF_NBSEG(IELM1,MESH),
     &                    XMT,DIM1XMT,DIM2XMT,STOXMT,
     &                    NELMAX,NELEM,ELTSEG,ORISEG)
        ENDIF
C
      ELSE
C
C        IELM1 / IELM2 COMBINATION NOT IMPLEMENTED: ERROR
C
         IF (LNG.EQ.1) WRITE(LU,100) NAME
         IF (LNG.EQ.2) WRITE(LU,101) NAME
         IF (LNG.EQ.1) WRITE(LU,200) IELM1,IELM2
         IF (LNG.EQ.2) WRITE(LU,201) IELM1,IELM2
         IF (LNG.EQ.1) WRITE(LU,300)
         IF (LNG.EQ.2) WRITE(LU,301)
         CALL PLANTE(1)
         STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C  NEW TYPE OF STORAGE
C
      STO=3
C
C-----------------------------------------------------------------------
C
100   FORMAT(1X,'ASSEX3 (BIEF) : MATRICE M (NOM REEL : ',A6,')')
200   FORMAT(1X,'                IELM1 = ',1I6,' IELM2 = ',1I6)
300   FORMAT(1X,'                CAS NON PREVU')
C
101   FORMAT(1X,'ASSEX3 (BIEF) : MATRIX  M (REAL NAME:',A6,')')
201   FORMAT(1X,'                IELM1 = ',1I6,' IELM2 = ',1I6)
301   FORMAT(1X,'                THIS CASE IS NOT IMPLEMENTED')
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
