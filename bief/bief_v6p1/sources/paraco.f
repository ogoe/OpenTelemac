C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ASSEMBLES DATA SHARED BY SEVERAL PROCESSORS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF_DEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BUF_RECV, BUF_SEND, DIMBUF, DIMNHCOM, IAN, ICOM, LIST_SEND, NB_NEIGHB, NB_NEIGHB_PT, NH_COM, NPLAN, NPOIN, V1, V2, V3
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::IPID IPID@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, II, IKA, IL, IPA, J, K, PARACO_MSG_TAG, RECV_REQ, SEND_REQ
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE(), P_IREAD(), P_IWRIT(), P_WAIT_PARACO()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PARCOM2(), PARCOM2_SEG()

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
!> </td><td> 18/07/08
!> </td><td> P. VEZOLLE(IBM)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BUF_RECV
!></td><td><-></td><td>BUFFER FOR RECEIVING DATA
!>    </td></tr>
!>          <tr><td>BUF_SEND
!></td><td><-></td><td>BUFFER FOR SENDING DATA
!>    </td></tr>
!>          <tr><td>DIMBUF
!></td><td>--></td><td>FIRST DIMENSION OF BUFFERS
!>    </td></tr>
!>          <tr><td>DIMNHCOM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DIM_NH_COM
!></td><td>---</td><td>FIRST DIMENSION OF NH_COM
!>    </td></tr>
!>          <tr><td>IAN
!></td><td>--></td><td>NUMBER OF VECTORS TO BE CONDIDERED (1, 2 OR 3)
!>    </td></tr>
!>          <tr><td>ICOM
!></td><td>--></td><td>OPTION OF COMMUNICATION :
!>                  = 1 : VALUE WITH MAXIMUM ABSOLUTE VALUE
!>                  = 2 : CONTRIBUTIONS ADDED
!>                  = 3 : MAXIMUM CONTRIBUTION RETAINED
!>                  = 4 : MINIMUM CONTRIBUTION RETAINED
!>    </td></tr>
!>          <tr><td>LIST_SEND
!></td><td>--></td><td>LIST OF PROCESSORS NUMBERS
!>    </td></tr>
!>          <tr><td>NB_NEIGHB
!></td><td>--></td><td>NUMBER OF NEIGHBOURING SUB-DOMAINS
!>    </td></tr>
!>          <tr><td>NB_NEIGHB_PT
!></td><td>--></td><td>NUMBER OF POINTS SHARED WITH A SUB-DOMAIN
!>    </td></tr>
!>          <tr><td>NH_COM
!></td><td>--></td><td>NH_COM(I,IL) : GLOBAL NUMBER IN THIS
!>                  SUB-DOMAIN OF THE POINT NUMBER I IN THE LIST
!>                  OF POINTS SHARED WITH PROCESSOR NUMBER IL
!>                  WHOSE REAL NUMBER IS LIST_SEND(IL)
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>SECOND DIMENSION OF V1,V2,V3
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>FIRST DIMENSION OF V1,V2,V3
!>    </td></tr>
!>          <tr><td>V1,V2,V3
!></td><td><-></td><td>VECTORS TO BE COMPLETED
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PARACO
     &(V1,V2,V3,NPOIN,ICOM,IAN,NPLAN,NB_NEIGHB,NB_NEIGHB_PT,LIST_SEND,
     & NH_COM,DIMNHCOM,BUF_SEND,BUF_RECV,DIMBUF)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BUF_RECV       |<->| BUFFER FOR RECEIVING DATA
C| BUF_SEND       |<->| BUFFER FOR SENDING DATA
C| DIMBUF         |-->| FIRST DIMENSION OF BUFFERS
C| DIMNHCOM       |---| 
C| DIM_NH_COM     |---| FIRST DIMENSION OF NH_COM
C| IAN            |-->| NUMBER OF VECTORS TO BE CONDIDERED (1, 2 OR 3)
C| ICOM           |-->| OPTION OF COMMUNICATION :
C|                |   | = 1 : VALUE WITH MAXIMUM ABSOLUTE VALUE
C|                |   | = 2 : CONTRIBUTIONS ADDED
C|                |   | = 3 : MAXIMUM CONTRIBUTION RETAINED
C|                |   | = 4 : MINIMUM CONTRIBUTION RETAINED
C| LIST_SEND      |-->| LIST OF PROCESSORS NUMBERS
C| NB_NEIGHB      |-->| NUMBER OF NEIGHBOURING SUB-DOMAINS
C| NB_NEIGHB_PT   |-->| NUMBER OF POINTS SHARED WITH A SUB-DOMAIN
C| NH_COM         |-->| NH_COM(I,IL) : GLOBAL NUMBER IN THIS
C|                |   | SUB-DOMAIN OF THE POINT NUMBER I IN THE LIST
C|                |   | OF POINTS SHARED WITH PROCESSOR NUMBER IL
C|                |   | WHOSE REAL NUMBER IS LIST_SEND(IL)
C| NPLAN          |-->| SECOND DIMENSION OF V1,V2,V3
C| NPOIN          |-->| FIRST DIMENSION OF V1,V2,V3
C| V1,V2,V3       |<->| VECTORS TO BE COMPLETED
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF_DEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN,ICOM,IAN,NPLAN,NB_NEIGHB
      INTEGER, INTENT(IN) :: DIMNHCOM,DIMBUF
      INTEGER, INTENT(IN) :: NB_NEIGHB_PT(NB_NEIGHB)
      INTEGER, INTENT(IN) :: LIST_SEND(NB_NEIGHB),NH_COM(DIMNHCOM,*)
C
      DOUBLE PRECISION, INTENT(INOUT) :: BUF_SEND(DIMBUF,*)
      DOUBLE PRECISION, INTENT(INOUT) :: BUF_RECV(DIMBUF,*)
      DOUBLE PRECISION, INTENT(INOUT) :: V1(NPOIN,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: V2(NPOIN,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: V3(NPOIN,NPLAN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IKA,IL,II,I,J,K,IPA
C
      INTRINSIC ABS
C
      INTEGER SEND_REQ(100),RECV_REQ(100)
      INTEGER PARACO_MSG_TAG
      DATA PARACO_MSG_TAG/5000/
C
      SAVE
C
C----------------------------------------------------------------------
C
      IF(IAN.NE.1.AND.IAN.NE.2.AND.IAN.NE.3) THEN
        WRITE(LU,*) 'FALSCHE FREIWERTZAHL BEI KOMMUNIKATION',IAN,
     &              ' AUF PROZESSOR',IPID
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     MESSAGE TAG UPDATE
C
      IF(PARACO_MSG_TAG.LT.1000000) THEN
        PARACO_MSG_TAG = PARACO_MSG_TAG + 1
      ELSE
        PARACO_MSG_TAG = 5001
      ENDIF
!
C== RECEIVE STEP
!
      DO IL=1,NB_NEIGHB
        IKA = NB_NEIGHB_PT(IL)
        IPA = LIST_SEND(IL)
        CALL P_IREAD(BUF_RECV(1,IL),IAN*IKA*NPLAN*8,
     &               IPA,PARACO_MSG_TAG,RECV_REQ(IL))
      ENDDO
!
C== SEND STEP
!
      DO IL=1,NB_NEIGHB
        IKA = NB_NEIGHB_PT(IL)
        IPA = LIST_SEND(IL)
!
C** INITIALISES THE COMMUNICATION ARRAYS
!
       K = 1
       IF(IAN.EQ.3) THEN
            DO J=1,NPLAN
              DO I=1,IKA
                II=NH_COM(I,IL)
                BUF_SEND(K,IL)  =V1(II,J)
                BUF_SEND(K+1,IL)=V2(II,J)
                BUF_SEND(K+2,IL)=V3(II,J)
                K=K+3
              ENDDO
            ENDDO
       ELSEIF(IAN.EQ.2) THEN
            DO J=1,NPLAN
              DO I=1,IKA
                II=NH_COM(I,IL)
                BUF_SEND(K,IL)  =V1(II,J)
                BUF_SEND(K+1,IL)=V2(II,J)
                K=K+2
              ENDDO
            ENDDO
       ELSEIF(IAN.EQ.1) THEN
            DO J=1,NPLAN
              DO I=1,IKA
                II=NH_COM(I,IL)
                BUF_SEND(K,IL)  =V1(II,J)
                K=K+1
              ENDDO
            ENDDO
       ENDIF
!
       CALL P_IWRIT(BUF_SEND(1,IL),IAN*IKA*NPLAN*8,
     &              IPA,PARACO_MSG_TAG,SEND_REQ(IL))
!
      ENDDO
!
C== WAIT RECEIVED MESSAGES (POSSIBILITY OF COVERING)
!
      DO IL=1,NB_NEIGHB
       IKA = NB_NEIGHB_PT(IL)
       IPA = LIST_SEND(IL)
       CALL P_WAIT_PARACO(RECV_REQ(IL),1)
!
       K=1
!
       IF(ICOM.EQ.1) THEN
            IF(IAN.EQ.3) THEN
              DO J=1,NPLAN
                DO I=1,IKA
                  II=NH_COM(I,IL)
                  IF(ABS(BUF_RECV(K,IL)).GT.ABS(V1(II,J)))
     &                            V1(II,J)=BUF_RECV(K  ,IL)
                  IF(ABS(BUF_RECV(K+1,IL)).GT.ABS(V2(II,J)))
     &                            V2(II,J)=BUF_RECV(K+1,IL)
                  IF(ABS(BUF_RECV(K+2,IL)).GT.ABS(V3(II,J)))
     &                            V3(II,J)=BUF_RECV(K+2,IL)
                  K=K+3
                ENDDO
              ENDDO
            ELSEIF(IAN.EQ.2) THEN
              DO J=1,NPLAN
                DO I=1,IKA
                  II=NH_COM(I,IL)
                  IF(ABS(BUF_RECV(K,IL)).GT.ABS(V1(II,J)))
     &                            V1(II,J)=BUF_RECV(K  ,IL)
                  IF(ABS(BUF_RECV(K+1,IL)).GT.ABS(V2(II,J)))
     &                            V2(II,J)=BUF_RECV(K+1,IL)
                  K=K+2
                ENDDO
              ENDDO
            ELSEIF(IAN.EQ.1) THEN
              DO J=1,NPLAN
                DO I=1,IKA
                  II=NH_COM(I,IL)
                  IF(ABS(BUF_RECV(K,IL)).GT.ABS(V1(II,J)))
     &                            V1(II,J)=BUF_RECV(K  ,IL)
                  K=K+1
                ENDDO
              ENDDO
            ENDIF
       ELSEIF(ICOM.EQ.2) THEN
            IF(IAN.EQ.3) THEN
              DO J=1,NPLAN
                DO I=1,IKA
                  II=NH_COM(I,IL)
                  V1(II,J)=V1(II,J)+BUF_RECV(K  ,IL)
                  V2(II,J)=V2(II,J)+BUF_RECV(K+1,IL)
                  V3(II,J)=V3(II,J)+BUF_RECV(K+2,IL)
                  K=K+3
                ENDDO
              ENDDO
            ELSEIF(IAN.EQ.2) THEN
              DO J=1,NPLAN
                DO I=1,IKA
                  II=NH_COM(I,IL)
                  V1(II,J)=V1(II,J)+BUF_RECV(K  ,IL)
                  V2(II,J)=V2(II,J)+BUF_RECV(K+1,IL)
                  K=K+2
                ENDDO
              ENDDO
            ELSEIF(IAN.EQ.1) THEN
              DO J=1,NPLAN
                DO I=1,IKA
                  II=NH_COM(I,IL)
                  V1(II,J)=V1(II,J)+BUF_RECV(K  ,IL)
                  K=K+1
                ENDDO
              ENDDO
            ENDIF
       ELSEIF(ICOM.EQ.3) THEN
            IF(IAN.EQ.3) THEN
              DO J=1,NPLAN
                DO I=1,IKA
                  II=NH_COM(I,IL)
                  IF(BUF_RECV(K  ,IL).GT.V1(II,J))
     &              V1(II,J)=BUF_RECV(K  ,IL)
                  IF(BUF_RECV(K+1,IL).GT.V2(II,J))
     &              V2(II,J)=BUF_RECV(K+1,IL)
                  IF(BUF_RECV(K+2,IL).GT.V3(II,J))
     &              V3(II,J)=BUF_RECV(K+2,IL)
                  K=K+3
                ENDDO
              ENDDO
            ELSEIF(IAN.EQ.2) THEN
              DO J=1,NPLAN
                DO I=1,IKA
                  II=NH_COM(I,IL)
                  IF(BUF_RECV(K  ,IL).GT.V1(II,J))
     &              V1(II,J)=BUF_RECV(K  ,IL)
                  IF(BUF_RECV(K+1,IL).GT.V2(II,J))
     &              V2(II,J)=BUF_RECV(K+1,IL)
                  K=K+2
                ENDDO
              ENDDO
            ELSEIF(IAN.EQ.1) THEN
              DO J=1,NPLAN
                DO I=1,IKA
                  II=NH_COM(I,IL)
                  IF(BUF_RECV(K  ,IL).GT.V1(II,J))
     &              V1(II,J)=BUF_RECV(K  ,IL)
                  K=K+1
                ENDDO
              ENDDO
            ENDIF
        ELSEIF(ICOM.EQ.4) THEN
            IF(IAN.EQ.3) THEN
              DO J=1,NPLAN
                DO I=1,IKA
                  II=NH_COM(I,IL)
                  IF(BUF_RECV(K  ,IL).LT.V1(II,J))
     &              V1(II,J)=BUF_RECV(K  ,IL)
                  IF(BUF_RECV(K+1,IL).LT.V2(II,J))
     &              V2(II,J)=BUF_RECV(K+1,IL)
                  IF(BUF_RECV(K+2,IL).LT.V3(II,J))
     &              V3(II,J)=BUF_RECV(K+2,IL)
                  K=K+3
                ENDDO
              ENDDO
            ELSEIF(IAN.EQ.2) THEN
              DO J=1,NPLAN
                DO I=1,IKA
                  II=NH_COM(I,IL)
                  IF(BUF_RECV(K  ,IL).LT.V1(II,J))
     &              V1(II,J)=BUF_RECV(K  ,IL)
                  IF(BUF_RECV(K+1,IL).LT.V2(II,J))
     &              V2(II,J)=BUF_RECV(K+1,IL)
                  K=K+2
                ENDDO
              ENDDO
            ELSEIF(IAN.EQ.1) THEN
              DO J=1,NPLAN
                DO I=1,IKA
                  II=NH_COM(I,IL)
                  IF(BUF_RECV(K  ,IL).LT.V1(II,J))
     &              V1(II,J)=BUF_RECV(K  ,IL)
                  K=K+1
                ENDDO
              ENDDO
            ENDIF
        ENDIF
C
      ENDDO
!
C== WAIT SENT MESSAGES
!
      CALL P_WAIT_PARACO(SEND_REQ,NB_NEIGHB)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C