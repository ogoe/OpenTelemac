!                       *************************
                        SUBROUTINE COLLECT_VALUES
!                       *************************
!
!***********************************************************************
! TELEMAC2D   V7P2                                   23/09/2015
!***********************************************************************
!
!brief    COLLECT H AND ZF VALUES ON WEIRS NODES
!+
!
!+
!history  C.COULET (ARTELIA)
!+        01/09/2016
!+        V7P2
!+   Creation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IK,IL,II,I,J,K,IP
!
      INTRINSIC ABS
!
      INTEGER SEND_REQ(100),RECV_REQ(100)
      INTEGER, SAVE :: MSG_TAG = 5000
!
      SAVE SEND_REQ, RECV_REQ
!
!-----------------------------------------------------------------------
!
      IF (NCSIZE.GT.0) THEN
!
        SEND_REQ(:) = 0
        RECV_REQ(:) = 0
!
!       MESSAGE TAG UPDATE
!
        IF(MSG_TAG.LT.1000000) THEN
          MSG_TAG = MSG_TAG + 1
        ELSE
          MSG_TAG = 5001
        ENDIF
!
!
!== RECEIVE STEP
!
        DO IL=1,N_NGHB_W_NODES
          IP = WNODES_PROC(IL)%NUM_NEIGH
          IK = WNODES_PROC(IL)%NB_NODES
!##> JR @ RWTH: DISTINGUISHING THE NUMBER OF ELEMENTS AND THEIR SIZE
          CALL P_IREAD(W_BUF_RECV(1,IL),2*IK,8,
     &                 IP,MSG_TAG,RECV_REQ(IL))
!          CALL P_IREAD(W_BUF_RECV(1,IL),2*IK*8,
!     &                 IP,MSG_TAG,RECV_REQ(IL))
!##< JR @ RWTH
        ENDDO
!
!== SEND STEP
!
        DO IL=1,N_NGHB_WN_SEND
          IP = WN_SEND_PROC(IL)%NUM_NEIGH
          IK = WN_SEND_PROC(IL)%NB_NODES
!
!** INITIALISES THE COMMUNICATION ARRAYS
!
          K = 1
          DO I=1,IK
            II = WN_SEND_PROC(IL)%NUM_LOC(I)
            W_BUF_SEND(K  ,IL) = ZF%R(II)
            W_BUF_SEND(K+1,IL) =  H%R(II)
            K = K+2
          ENDDO
!
!##> JR @ RWTH: DISTINGUISHING THE NUMBER OF ELEMENTS AND THEIR SIZE
          CALL P_IWRIT(W_BUF_SEND(1,IL),2*IK,8,
     &                 IP,MSG_TAG,SEND_REQ(IL))
!          CALL P_IWRIT(W_BUF_SEND(1,IL),2*IK*8,
!     &                 IP,MSG_TAG,SEND_REQ(IL))
!##< JR @ RWTH
!
        ENDDO
!
!== WAIT RECEIVED MESSAGES (POSSIBILITY OF COVERING)
!
        CALL P_WAIT_PARACO(RECV_REQ,N_NGHB_W_NODES)
!
        DO IL=1,N_NGHB_W_NODES
          IK = WNODES_PROC(IL)%NB_NODES
          IP = WNODES_PROC(IL)%NUM_NEIGH
!
          K = 1
          DO I=1,IK
            II = WNODES_PROC(IL)%LIST_NODES(I)
            IF (ABS(W_BUF_RECV(K  ,IL)).GT.ABS(WNODES(II)%ZFN)) THEN
              WNODES(II)%ZFN=W_BUF_RECV(K  ,IL)
            ENDIF
            IF (ABS(W_BUF_RECV(K+1,IL)).GT.ABS(WNODES(II)%HN)) THEN
              WNODES(II)%HN=W_BUF_RECV(K+1,IL)
            ENDIF
            K = K + 2
          ENDDO
!
        ENDDO
!
!== WAIT SENT MESSAGES
!
        CALL P_WAIT_PARACO(SEND_REQ,N_NGHB_WN_SEND)
!
!-----------------------------------------------------------------------
      ELSE
        DO I = 1, N_NGHB_W_NODES
          WNODES(I)%ZFN = 0.D0
        ENDDO
        DO I = 1, N_NGHB_W_NODES
          WNODES(I)%HN = 0.D0
        ENDDO
        DO IL=1,N_NGHB_WN_SEND
          IK = WN_SEND_PROC(IL)%NB_NODES
          DO I=1,IK
            II = WN_SEND_PROC(IL)%NUM_LOC(I)
            WNODES(I)%ZFN = ZF%R(II)
            WNODES(I)%HN  =  H%R(II)
          ENDDO
        ENDDO
      ENDIF
!
      RETURN
      END
