C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPLETES THE REAL ADDRESS OF SEGMENTS IN NH_COM_SEG.
!>                SEE PARINI WHERE NH_COM_SEG IS INITIALISED AT -999999
!>                AND THEN FILLED WITH 4*IELEM+IFACE TO STORE IELEM AND
!>                IFACE.
!><br>            THEN THE ADDRESSES ARE ORDERED WITH RESPECT TO THE
!>                GLOBAL NUMBER OF THE FIRST AND SECOND POINT OF
!>                EVERY SEGMENT, SO THAT THE PROCESSORS SHARE THE
!>                INFORMATION ON THE SAME SEGMENTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DIM1NHCOM, DIMGLO, ELTSEG, GLOSEG, KNOLG, NB_NEIGHB_PT_SEG, NB_NEIGHB_SEG, NELEM, NH_COM_SEG, NPOIN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> B, I, I11, I12, I21, I22, IELEM, IFACE, IKA, IPROC, ISEG, IS_LE_THAN, J, NUMSEG
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INBIEF()

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
!> </td><td> 22/07/2009
!> </td><td> C. DENIS
!> </td><td> AVOIDS THE (TOO) LARGE INTEGERS OF 5.9 RELEASE
!> </td></tr>
!>      <tr>
!>      <td><center> 5.9                                       </center>
!> </td><td>
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
!>          <tr><td>DIMGLO
!></td><td>--></td><td>FIRST DIMENSION OF GLOSEG
!>    </td></tr>
!>          <tr><td>ELTSEG
!></td><td>--></td><td>GIVES THE SEGMENT NUMBER OF EDGES OF ELEMENTS
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td>--></td><td>GLOBAL NUMBERS (IN SUB-DOMAIN) OF POINTS
!>                  OF A SEGMENT
!>    </td></tr>
!>          <tr><td>KNOLG
!></td><td>--></td><td>GLOBAL NUMBERS (WHOLE MESH) FUNCTION OF
!>                  LOCAL NUMBERS OF POINTS
!>    </td></tr>
!>          <tr><td>NB_NEIGHB_PT_SEG
!></td><td>--></td><td>NUMBER OF INTERFACE SEGMENTS FOR EVERY
!>                  NEIGHBOUR PROCESSOR
!>    </td></tr>
!>          <tr><td>NB_NEIGHB_SEG
!></td><td>--></td><td>NUMBER OF NEIGHBOUR PROCESSOR (FOR SEGMENTS)
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NUMBER OF ELEMENTS
!>    </td></tr>
!>          <tr><td>NH_COM_SEG
!></td><td>--></td><td>ADDRESSES OF INTERFACE SEGMENTS
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF POINTS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE COMP_NH_COM_SEG
     &(ELTSEG,NELEM,NH_COM_SEG,DIM1NHCOM,NB_NEIGHB_SEG,NB_NEIGHB_PT_SEG,
     & GLOSEG,DIMGLO,KNOLG,NPOIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DIM1NHCOM      |-->| FIRST DIMENSION OF NH_COM_SEG
C| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
C| ELTSEG         |-->| GIVES THE SEGMENT NUMBER OF EDGES OF ELEMENTS
C| GLOSEG         |-->| GLOBAL NUMBERS (IN SUB-DOMAIN) OF POINTS
C|                |   | OF A SEGMENT
C| KNOLG          |-->| GLOBAL NUMBERS (WHOLE MESH) FUNCTION OF
C|                |   | LOCAL NUMBERS OF POINTS
C| NB_NEIGHB_PT_SE|-->| NUMBER OF INTERFACE SEGMENTS FOR EVERY
C|                |   | NEIGHBOUR PROCESSOR
C| NB_NEIGHB_SEG  |-->| NUMBER OF NEIGHBOUR PROCESSOR (FOR SEGMENTS)
C| NELEM          |-->| NUMBER OF ELEMENTS
C| NH_COM_SEG     |-->| ADDRESSES OF INTERFACE SEGMENTS
C| NPOIN          |-->| NUMBER OF POINTS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NELEM,DIM1NHCOM,NB_NEIGHB_SEG,DIMGLO
      INTEGER, INTENT(IN)    :: NPOIN
      INTEGER, INTENT(INOUT) :: NH_COM_SEG(DIM1NHCOM,NB_NEIGHB_SEG)
      INTEGER, INTENT(IN)    :: ELTSEG(NELEM,3),GLOSEG(DIMGLO,2)
      INTEGER, INTENT(IN)    :: NB_NEIGHB_PT_SEG(NB_NEIGHB_SEG)
      INTEGER, INTENT(IN)    :: KNOLG(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IELEM,IFACE,ISEG,IPROC,IKA,I,J,B,NUMSEG
      INTEGER I11,I12,I21,I22
      LOGICAL IS_LE_THAN
C
C-----------------------------------------------------------------------
C
      DO IPROC=1,NB_NEIGHB_SEG
        IKA = NB_NEIGHB_PT_SEG(IPROC)
        DO ISEG=1,IKA
          IFACE=MOD(NH_COM_SEG(ISEG,IPROC),4)
          IELEM=(NH_COM_SEG(ISEG,IPROC)-IFACE)/4
          NUMSEG=ELTSEG(IELEM,IFACE)
          NH_COM_SEG(ISEG,IPROC)=NUMSEG
        ENDDO
        IF(IKA.GT.1) THEN
          DO J=2,IKA
            B=NH_COM_SEG(J,IPROC)
            DO I=J-1,1,-1
              NUMSEG=NH_COM_SEG(I,IPROC)
              I11=KNOLG(GLOSEG(NUMSEG,1))
              I12=KNOLG(GLOSEG(NUMSEG,2))
              I21=KNOLG(GLOSEG(B     ,1))
              I22=KNOLG(GLOSEG(B     ,2))
              IF(I11.GT.I21) THEN
                IS_LE_THAN=.FALSE.
              ELSEIF(I11.LT.I21) THEN
                IS_LE_THAN=.TRUE.
              ELSEIF(I11.EQ.I21.AND.I12.GT.I22) THEN
                IS_LE_THAN=.FALSE.
              ELSEIF(I11.EQ.I21.AND.I12.LT.I22) THEN
                IS_LE_THAN=.TRUE.
              ELSEIF(I11.EQ.I21.AND.I12.EQ.I22) THEN
                IS_LE_THAN=.TRUE.
              ELSE
                WRITE(LU,*) 'UNEXPECTED CASE IN COMP_NH_COM_SEG'
                CALL PLANTE(1)
                STOP
              ENDIF
              IF(IS_LE_THAN) GO TO 10
              NH_COM_SEG(I+1,IPROC)=NH_COM_SEG(I,IPROC)
            ENDDO
 10         CONTINUE
            NH_COM_SEG(I+1,IPROC)=B
          ENDDO
        ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C