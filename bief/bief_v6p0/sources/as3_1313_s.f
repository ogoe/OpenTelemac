C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ASSEMBLES MATRICES EXTRA-DIAGONAL TERMS
!>                IN THE CASE OF EDGE-BASED STORAGE.
!><br>            CASE OF QUADRATIC TRIANGLE AND SYMMETRICAL MATRIX.
!>  @code
!>            LOCAL NUMBERING OF SEGMENTS CHOSEN HERE IN A TRIANGLE
!>
!>            01 --> 1 - 2
!>            02 --> 2 - 3
!>            03 --> 3 - 1
!>            04 --> 1 - 4
!>            05 --> 2 - 5
!>            06 --> 3 - 6
!>            07 --> 2 - 4
!>            08 --> 3 - 5
!>            09 --> 1 - 6
!>            10 --> 1 - 5
!>            11 --> 2 - 6
!>            12 --> 3 - 4
!>            13 --> 4 - 5
!>            14 --> 5 - 6
!>            15 --> 6 - 4
!>
!>            LOCAL NUMBERING OF ELEMENT BY ELEMENT EXTRA-DIAGONAL TERMS
!>
!>            01 : POINTS 1-2
!>            02 : POINTS 1-3
!>            03 : POINTS 1-4
!>            04 : POINTS 1-5
!>            05 : POINTS 1-6
!>            06 : POINTS 2-3
!>            07 : POINTS 2-4
!>            08 : POINTS 2-5
!>            09 : POINTS 2-6
!>            10 : POINTS 3-4
!>            11 : POINTS 3-5
!>            12 : POINTS 3-6
!>            13 : POINTS 4-5
!>            14 : POINTS 4-6
!>            15 : POINTS 5-6
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DIM1XMT, DIM2XMT, ELTSEG, NELEM, NELMAX, NSEG1, STOXMT, XM, XMT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, ISEG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ASSEX3()

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
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DIM2XMT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ELTSEG
!></td><td>--></td><td>LISTE DES ELEMENTS DE CHAQUE SEGMENT
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>PREMIERE DIMENSION DE IKLE ET W.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>NSEG1
!></td><td>--></td><td>NOMBRE DE SEGMENTS
!>    </td></tr>
!>          <tr><td>STOXMT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XM
!></td><td><--</td><td>TERMES EXTRA-DIAGONAUX ASSEMBLES XA12,23,31
!>    </td></tr>
!>          <tr><td>XMT
!></td><td>--></td><td>TERMS EXTRA-DIAGONAUX
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE AS3_1313_S
     &(XM,NSEG1,XMT,DIM1XMT,DIM2XMT,STOXMT,NELMAX,NELEM,ELTSEG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DIM1XMT        |---| 
C| DIM2XMT        |---| 
C| ELTSEG         |-->| LISTE DES ELEMENTS DE CHAQUE SEGMENT
C| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C| NELMAX         |-->| PREMIERE DIMENSION DE IKLE ET W.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NSEG1          |-->| NOMBRE DE SEGMENTS
C| STOXMT         |---| 
C| XM             |<--| TERMES EXTRA-DIAGONAUX ASSEMBLES XA12,23,31
C| XMT            |-->| TERMS EXTRA-DIAGONAUX
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN)    :: NELMAX,NELEM,NSEG1
      INTEGER         , INTENT(IN)    :: DIM1XMT,DIM2XMT,STOXMT
      INTEGER         , INTENT(IN)    :: ELTSEG(NELMAX,15)
      DOUBLE PRECISION, INTENT(IN)    :: XMT(DIM1XMT,DIM2XMT)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NSEG1)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ISEG,IELEM
C
C-----------------------------------------------------------------------
C
C  INITIALISES
C
      DO ISEG = 1 , NSEG1
        XM(ISEG) = 0.D0
      ENDDO
C
C-----------------------------------------------------------------------
C
C  ASSEMBLES
C
C-----------------------------------------------------------------------
C
      IF(STOXMT.EQ.1) THEN
C
      DO IELEM = 1,NELEM
C
          XM(ELTSEG(IELEM,01))=XM(ELTSEG(IELEM,01))+XMT(IELEM,01)
          XM(ELTSEG(IELEM,02))=XM(ELTSEG(IELEM,02))+XMT(IELEM,06)
          XM(ELTSEG(IELEM,03))=XM(ELTSEG(IELEM,03))+XMT(IELEM,02)
          XM(ELTSEG(IELEM,04))=XM(ELTSEG(IELEM,04))+XMT(IELEM,03)
          XM(ELTSEG(IELEM,05))=XM(ELTSEG(IELEM,05))+XMT(IELEM,08)
          XM(ELTSEG(IELEM,06))=XM(ELTSEG(IELEM,06))+XMT(IELEM,12)
          XM(ELTSEG(IELEM,07))=XM(ELTSEG(IELEM,07))+XMT(IELEM,07)
          XM(ELTSEG(IELEM,08))=XM(ELTSEG(IELEM,08))+XMT(IELEM,11)
          XM(ELTSEG(IELEM,09))=XM(ELTSEG(IELEM,09))+XMT(IELEM,05)
          XM(ELTSEG(IELEM,10))=XM(ELTSEG(IELEM,10))+XMT(IELEM,04)
          XM(ELTSEG(IELEM,11))=XM(ELTSEG(IELEM,11))+XMT(IELEM,09)
          XM(ELTSEG(IELEM,12))=XM(ELTSEG(IELEM,12))+XMT(IELEM,10)
          XM(ELTSEG(IELEM,13))=XM(ELTSEG(IELEM,13))+XMT(IELEM,13)
          XM(ELTSEG(IELEM,14))=XM(ELTSEG(IELEM,14))+XMT(IELEM,15)
          XM(ELTSEG(IELEM,15))=XM(ELTSEG(IELEM,15))+XMT(IELEM,14)
C
      ENDDO
C
      ELSEIF(STOXMT.EQ.2) THEN
C
      DO IELEM = 1,NELEM
C
          XM(ELTSEG(IELEM,01))=XM(ELTSEG(IELEM,01))+XMT(01,IELEM)
          XM(ELTSEG(IELEM,02))=XM(ELTSEG(IELEM,02))+XMT(06,IELEM)
          XM(ELTSEG(IELEM,03))=XM(ELTSEG(IELEM,03))+XMT(02,IELEM)
          XM(ELTSEG(IELEM,04))=XM(ELTSEG(IELEM,04))+XMT(03,IELEM)
          XM(ELTSEG(IELEM,05))=XM(ELTSEG(IELEM,05))+XMT(08,IELEM)
          XM(ELTSEG(IELEM,06))=XM(ELTSEG(IELEM,06))+XMT(12,IELEM)
          XM(ELTSEG(IELEM,07))=XM(ELTSEG(IELEM,07))+XMT(07,IELEM)
          XM(ELTSEG(IELEM,08))=XM(ELTSEG(IELEM,08))+XMT(11,IELEM)
          XM(ELTSEG(IELEM,09))=XM(ELTSEG(IELEM,09))+XMT(05,IELEM)
          XM(ELTSEG(IELEM,10))=XM(ELTSEG(IELEM,10))+XMT(04,IELEM)
          XM(ELTSEG(IELEM,11))=XM(ELTSEG(IELEM,11))+XMT(09,IELEM)
          XM(ELTSEG(IELEM,12))=XM(ELTSEG(IELEM,12))+XMT(10,IELEM)
          XM(ELTSEG(IELEM,13))=XM(ELTSEG(IELEM,13))+XMT(13,IELEM)
          XM(ELTSEG(IELEM,14))=XM(ELTSEG(IELEM,14))+XMT(15,IELEM)
          XM(ELTSEG(IELEM,15))=XM(ELTSEG(IELEM,15))+XMT(14,IELEM)
C
      ENDDO
C
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'AS3_1313_S : STOCKAGE DE XMT INCONNU : ',STOXMT
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'AS3_1313_S: UNKNOWN STORAGE OF XMT : ',STOXMT
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C