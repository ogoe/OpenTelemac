C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ASSEMBLES EXTRA-DIAGONAL TERMS OF MATRICES (XMT)
!>                IN THE CASE OF EDGE-BASED STORAGE.
!><br>            CASE OF LINEAR - QUADRATIC ELEMENT.
!>  @code
!>            LOCAL NUMBERING OF SEGMENTS IN A TRIANGLE (SEE COMP_SEG)
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
!>            TERMS IN XMT (STORAGE GIVEN BY ARRAY CAQ(6,3,2) IN MATRIY):
!>
!>            01  -->  1-2
!>            02  -->  1-3
!>            03  -->  2-1
!>            04  -->  2-3
!>            05  -->  3-1
!>            06  -->  3-2
!>            07  -->  4-1
!>            08  -->  4-2
!>            09  -->  4-3
!>            10  -->  5-1
!>            11  -->  5-2
!>            12  -->  5-3
!>            13  -->  6-1
!>            14  -->  6-2
!>            15  -->  6-3
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ELTSEG, NELEM, NELMAX, NSEG11, NSEG13, ORISEG, XM, XMT
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, ISEG
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
!> </td><td> 02/02/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ELTSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DANS LE MAILLAGE.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>PREMIERE DIMENSION DE IKLE ET W.
!>                  (CAS D'UN MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>NSEG11
!></td><td>--></td><td>NUMBER OF LINEAR SEGMENTS
!>    </td></tr>
!>          <tr><td>NSEG13
!></td><td>--></td><td>NUMBER OF QUADRATIC SEGMENTS -
!>                  THE NUMBER OF PURELY QUADRATIC SEGMENTS
!>                  (THEY ARE NOT CONSIDERED IN RECTANGULAR
!>                  MATRICES)
!>    </td></tr>
!>          <tr><td>ORISEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TR
!></td><td>--></td><td>TABLEAU DE TRAVAIL DE TAILLE > NPTFR
!>    </td></tr>
!>          <tr><td>XM
!></td><td><--</td><td>TERMES EXTRA-DIAGONAUX ASSEMBLES XA12,23,31
!>    </td></tr>
!>          <tr><td>XM2
!></td><td>--></td><td>TERMS EXTRA-DIAGONAUX XA21,32,31
!>    </td></tr>
!>          <tr><td>XMT
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE AS3_1311
     &(XM,NSEG11,NSEG13,XMT,NELMAX,NELEM,ELTSEG,ORISEG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ELTSEG         |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DANS LE MAILLAGE.
C| NELMAX         |-->| PREMIERE DIMENSION DE IKLE ET W.
C|                |   | (CAS D'UN MAILLAGE ADAPTATIF)
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES.
C| NSEG11         |-->| NUMBER OF LINEAR SEGMENTS
C| NSEG13         |-->| NUMBER OF QUADRATIC SEGMENTS -
C|                |   | THE NUMBER OF PURELY QUADRATIC SEGMENTS
C|                |   | (THEY ARE NOT CONSIDERED IN RECTANGULAR
C|                |   | MATRICES)
C| ORISEG         |---| 
C| TR             |-->| TABLEAU DE TRAVAIL DE TAILLE > NPTFR
C| XM             |<--| TERMES EXTRA-DIAGONAUX ASSEMBLES XA12,23,31
C| XM2            |-->| TERMS EXTRA-DIAGONAUX XA21,32,31
C| XMT            |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN)    :: NELMAX,NELEM,NSEG11,NSEG13
      INTEGER         , INTENT(IN)    :: ELTSEG(NELMAX,15)
      INTEGER         , INTENT(IN)    :: ORISEG(NELMAX,15)
      DOUBLE PRECISION, INTENT(IN)    :: XMT(NELMAX,*)
      DOUBLE PRECISION, INTENT(INOUT) :: XM(NSEG11+NSEG13-3*NELEM)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ISEG,IELEM
C
C-----------------------------------------------------------------------
C
C     INITIALISES
C
C     -6*NELEM : SEGMENTS 10,11,12 NEED NO ASSEMBLY
C                SEGMENTS 13,14,15 ARE NOT CONSIDERED
      DO ISEG = 1 , NSEG11+NSEG13-6*NELEM
        XM(ISEG) = 0.D0
      ENDDO
C
C     ASSEMBLES LINEAR PART
C
      DO IELEM = 1,NELEM
C
C        SEGMENT 1 (TERMS 1-2 AND 2-1)
C
         XM(ELTSEG(IELEM,1)+NSEG11*(ORISEG(IELEM,1)-1))
     &  =XM(ELTSEG(IELEM,1)+NSEG11*(ORISEG(IELEM,1)-1))+XMT(IELEM,01)
         XM(ELTSEG(IELEM,1)+NSEG11*(2-ORISEG(IELEM,1)))
     &  =XM(ELTSEG(IELEM,1)+NSEG11*(2-ORISEG(IELEM,1)))+XMT(IELEM,03)
C
C        SEGMENT 2 (TERMS 2-3 AND 3-2)
C
         XM(ELTSEG(IELEM,2)+NSEG11*(ORISEG(IELEM,2)-1))
     &  =XM(ELTSEG(IELEM,2)+NSEG11*(ORISEG(IELEM,2)-1))+XMT(IELEM,04)
         XM(ELTSEG(IELEM,2)+NSEG11*(2-ORISEG(IELEM,2)))
     &  =XM(ELTSEG(IELEM,2)+NSEG11*(2-ORISEG(IELEM,2)))+XMT(IELEM,06)
C
C        SEGMENT 3 (TERMS 3-1 AND 1-3)
C
         XM(ELTSEG(IELEM,3)+NSEG11*(ORISEG(IELEM,3)-1))
     &  =XM(ELTSEG(IELEM,3)+NSEG11*(ORISEG(IELEM,3)-1))+XMT(IELEM,05)
         XM(ELTSEG(IELEM,3)+NSEG11*(2-ORISEG(IELEM,3)))
     &  =XM(ELTSEG(IELEM,3)+NSEG11*(2-ORISEG(IELEM,3)))+XMT(IELEM,02)
C
      ENDDO
C
C     ASSEMBLES, SEGMENTS BETWEEN LINEAR AND QUADRATIC POINTS
C     (I.E. THE REST BUT NOT 13, 14 AND 15)
C
C     ASSEMBLES THE QUADRATIC PART
C     BETWEEN XM(2*NSEG11+1) AND XM(NSEG11+NSEG13-3*NELEM)
C     SEE IN COMP_SEG HOW ELTSEG4,5,6,7,8,9,10,11,12 ARE BUILT,
C     THEIR NUMBERING STARTS AT NSEG11+1, HENCE HERE THE STORAGE IN
C     XM STARTS AT 2*NSEG11+1
C
      DO IELEM = 1,NELEM
C       TERM OF SEGMENT 1-4
        XM(ELTSEG(IELEM,04)+NSEG11) =
     &  XM(ELTSEG(IELEM,04)+NSEG11) + XMT(IELEM,07)
C       TERM OF SEGMENT 2-5
        XM(ELTSEG(IELEM,05)+NSEG11) =
     &  XM(ELTSEG(IELEM,05)+NSEG11) + XMT(IELEM,11)
C       TERM OF SEGMENT 3-6
        XM(ELTSEG(IELEM,06)+NSEG11) =
     &  XM(ELTSEG(IELEM,06)+NSEG11) + XMT(IELEM,15)
C       TERM OF SEGMENT 2-4
        XM(ELTSEG(IELEM,07)+NSEG11) =
     &  XM(ELTSEG(IELEM,07)+NSEG11) + XMT(IELEM,08)
C       TERM OF SEGMENT 3-5
        XM(ELTSEG(IELEM,08)+NSEG11) =
     &  XM(ELTSEG(IELEM,08)+NSEG11) + XMT(IELEM,12)
C       TERM OF SEGMENT 1-6
        XM(ELTSEG(IELEM,09)+NSEG11) =
     &  XM(ELTSEG(IELEM,09)+NSEG11) + XMT(IELEM,13)
      ENDDO
C
C     THESE 3 SEGMENTS ARE NOT SHARED, NO ASSEMBLY
C
      DO IELEM = 1,NELEM
C       TERM OF SEGMENT 1-5
        XM(ELTSEG(IELEM,10)+NSEG11) = XMT(IELEM,10)
C       TERM OF SEGMENT 2-6
        XM(ELTSEG(IELEM,11)+NSEG11) = XMT(IELEM,14)
C       TERM OF SEGMENT 3-4
        XM(ELTSEG(IELEM,12)+NSEG11) = XMT(IELEM,09)
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C