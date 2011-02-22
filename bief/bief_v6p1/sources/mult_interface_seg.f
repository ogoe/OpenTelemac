C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MULTIPLIES BY A CONSTANT THE INTERFACE VALUES OF A
!>                FUNCTION DEFINED ON SEGMENTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DIM1NHCOM, FSEG, NB_NEIGHB_PT_SEG, NB_NEIGHB_SEG, NH_COM_SEG, NSEG, XMUL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IADSEG, IKA, IPROC, ISEG
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CVTRVF_POS(), POSITIVE_DEPTHS(), SHARE_3D_FLUXES()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 27/02/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DIM1NHCOM
!></td><td>--></td><td>FIRST DIMENSION OF NH_COM_SEG
!>    </td></tr>
!>          <tr><td>FSEG
!></td><td><-></td><td>THE FUNCTION DEFINED ON SEGMENTS
!>    </td></tr>
!>          <tr><td>NB_NEIGHB_PT_SEG
!></td><td>--></td><td>NUMBER OF INTERFACE SEGMENTS FOR EVERY
!>                  NEIGHBOUR PROCESSOR
!>    </td></tr>
!>          <tr><td>NB_NEIGHB_SEG
!></td><td>--></td><td>NUMBER OF NEIGHBOUR PROCESSOR (FOR SEGMENTS)
!>    </td></tr>
!>          <tr><td>NH_COM_SEG
!></td><td>--></td><td>ADDRESSES OF INTERFACE SEGMENTS
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NUMBER OF SEGMENTS
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>THE CONSTANT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MULT_INTERFACE_SEG
     &(FSEG,NH_COM_SEG,DIM1NHCOM,NB_NEIGHB_SEG,
     & NB_NEIGHB_PT_SEG,XMUL,NSEG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DIM1NHCOM      |-->| FIRST DIMENSION OF NH_COM_SEG
C| FSEG           |<->| THE FUNCTION DEFINED ON SEGMENTS
C| NB_NEIGHB_PT_SE|-->| NUMBER OF INTERFACE SEGMENTS FOR EVERY
C|                |   | NEIGHBOUR PROCESSOR
C| NB_NEIGHB_SEG  |-->| NUMBER OF NEIGHBOUR PROCESSOR (FOR SEGMENTS)
C| NH_COM_SEG     |-->| ADDRESSES OF INTERFACE SEGMENTS
C| NSEG           |-->| NUMBER OF SEGMENTS
C| XMUL           |-->| THE CONSTANT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: DIM1NHCOM,NB_NEIGHB_SEG,NSEG
      INTEGER, INTENT(INOUT) :: NH_COM_SEG(DIM1NHCOM,NB_NEIGHB_SEG)
      INTEGER, INTENT(IN)    :: NB_NEIGHB_PT_SEG(NB_NEIGHB_SEG)
      DOUBLE PRECISION, INTENT(INOUT) :: FSEG(NSEG),XMUL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ISEG,IPROC,IKA,IADSEG
C
C-----------------------------------------------------------------------
C
C     DONE ONLY IF THERE IS AT LEAST ONE OTHER SUB-DOMAIN SHARING
C     A SEGMENT WITH THIS ONE
C
      IF(NB_NEIGHB_SEG.GT.0) THEN
C
C     LOOP ON ALL NEIGHBOURING SUB-DOMAINS
C
      DO IPROC=1,NB_NEIGHB_SEG
        IKA = NB_NEIGHB_PT_SEG(IPROC)
C
C       LOOP ON ALL SEGMENTS SHARED WITH THIS SUB-DOMAIN
C       WHICH CANNOT BE SHARED WITH ANOTHER SUB-DOMAIN (UNLIKE POINTS)
C
        DO ISEG=1,IKA
C         ADDRESS IN SEGMENT NUMBERING
          IADSEG=NH_COM_SEG(ISEG,IPROC)
          FSEG(IADSEG)=FSEG(IADSEG)*XMUL
        ENDDO
      ENDDO
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