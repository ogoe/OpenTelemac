!                    ******************
                     SUBROUTINE LECSNG2
!                    ******************
!
     &(IOPTAN,IFIC)
!
!***********************************************************************
! TELEMAC2D   V7P1                                   14/09/2015
!***********************************************************************
!
!brief    READS THE DATA DEFINING WEIRS
!+                FROM WEIR FILE IN CASE OF TYPSEUIL=2.
!
!history  C.COULET (ARTELIA)
!+        14/09/2015
!+        V7P1
!+   Creation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IFIC           |-->| LOGICAL UNIT OF FORMATED DATA FILE 1
!| IOPTAN         |<--| OPTION FOR TANGENTIAL VELOCITIES
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
      INTEGER, INTENT(IN)    :: IFIC
      INTEGER, INTENT(INOUT) :: IOPTAN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N, NB, I, IPTFR, K, L, ERROR, NB16
      DOUBLE PRECISION XDIG1, XDIG2, YDIG1, YDIG2
!
      INTEGER P_IMAX
      EXTERNAL P_IMAX
!
      INTEGER, DIMENSION(:), ALLOCATABLE :: TMP_NODES
!
!-----------------------------------------------------------------------
!
!     COMMENT LINE
      READ(IFIC,*,END=900,ERR=900)
!     NUMBER OF WEIRS, OPTION FOR TANGENTIAL VELOCITY
      READ(IFIC,*,END=900,ERR=998) NWEIRS,IOPTAN
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)'LECSNG2 : NOMBRE DE DIGUES :',NWEIRS
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*)'LECSNG2 : NUMBER OF WEIRS :',NWEIRS
      ENDIF
!
!     ALLOCATION
!
      IF(NWEIRS.GT.0) THEN
        ALLOCATE(WEIRS(NWEIRS),STAT=ERROR)
        IF (ERROR /= 0) THEN
          WRITE(LU,*)
     &      'LECSNG2: ERROR ALLOC WEIRS:',ERROR
          CALL PLANTE(1)
          STOP
        ENDIF
        ALLOCATE(TMP_NODES(8*NWEIRS),STAT=ERROR)
        IF (ERROR /= 0) THEN
          WRITE(LU,*)
     &      'LECSNG2: ERROR ALLOC TMP_NODES:',ERROR
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      DO N=1,NWEIRS
        READ(IFIC,*,ERR=990) WEIRS(N)%NUM_GLO,
     &                       XDIG1,YDIG1,WEIRS(N)%Z1,
     &                       WEIRS(N)%N_1A_1,WEIRS(N)%N_1A_2,
     &                       WEIRS(N)%N_1B_1,WEIRS(N)%N_1B_2,
     &                       XDIG2,YDIG2,WEIRS(N)%Z2,
     &                       WEIRS(N)%N_2A_1,WEIRS(N)%N_2A_2,
     &                       WEIRS(N)%N_2B_1,WEIRS(N)%N_2B_2
        WEIRS(N)%WIDTH = DSQRT((XDIG2-XDIG1)**2+
     &                         (YDIG2-YDIG1)**2)
        WEIRS(N)%Q0 = 0.D0
      ENDDO
!
!     READING THE PARALLEL REPARTITION OF WEIRS COMPUTATION
!
      IF(NCSIZE.GT.1) THEN
!
        READ(IFIC,*,END=900,ERR=900)
        DO N=1, NWEIRS
          READ(IFIC,*,ERR=996) I,NB
          WEIRS(N)%NB_NEIGH = NB
          WEIRS(N)%LIST_NEIGH(:) = -1
          IF (NB.GT.0) THEN
            READ(IFIC,*,ERR=996) (WEIRS(N)%LIST_NEIGH(L),L=1,NB)
          ENDIF
        ENDDO
!
        READ(IFIC,*,END=900,ERR=900)
        READ(IFIC,*,ERR=996) N_NGHB_WEIRS
!
!       ALLOCATION
!
        IF(N_NGHB_WEIRS.GT.0) THEN
          ALLOCATE(WEIRS_PROC(N_NGHB_WEIRS),STAT=ERROR)
          IF (ERROR /= 0) THEN
            WRITE(LU,*)
     &        'LECSNG2: ERROR ALLOC WEIRS_PROC:',ERROR
            CALL PLANTE(1)
            STOP
          ENDIF
          DO N=1, N_NGHB_WEIRS
            READ(IFIC,*,ERR=996) WEIRS_PROC(N)%NUM_NEIGH, NB
            WEIRS_PROC(N)%NB_ELEM = NB
            IF (NB.GT.0) THEN
              ALLOCATE(WEIRS_PROC(N)%LIST_ELEM(NB),STAT=ERROR)
              IF (ERROR /= 0) THEN
                WRITE(LU,*)
     &            'LECSNG2: ERROR ALLOC WEIRS_PROC%LIST_ELEM:',ERROR
                CALL PLANTE(1)
                STOP
              ENDIF
              READ(IFIC,*,ERR=996) (WEIRS_PROC(N)%LIST_ELEM(L),L=1,NB)
            ENDIF
          ENDDO
        ENDIF
      ELSE
        DO N=1, NWEIRS
          WEIRS(N)%NB_NEIGH = 0
          WEIRS(N)%LIST_NEIGH(:) = -1
        ENDDO
      ENDIF
!
!     READING THE PARALLEL INFORMATIONS OF NODES IF NEEDED (RECEIVE)
!
      IF(NCSIZE.GT.1) THEN
!
        READ(IFIC,*,END=900,ERR=900)
        READ(IFIC,*,ERR=997) NWEIRS_NODES
!
      ELSE
!     IN CASE OF SEQUENTIAL COMPUTATION, RECONSTRUCTION OF SOME STRUCTURE
!        TO ENSURE A SIMILAR TREATMENT      
        TMP_NODES(:)=0
        NWEIRS_NODES = 0
        DO N=1, NWEIRS
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_1A_1 == TMP_NODES(L)) GOTO 102
            IF(WEIRS(N)%N_1A_1 <  TMP_NODES(L)) GOTO 101
          ENDDO
101       CONTINUE
          NWEIRS_NODES = NWEIRS_NODES + 1
          DO I= NWEIRS_NODES,L+1,-1
            TMP_NODES(I) = TMP_NODES(I-1)
          END DO
          TMP_NODES(I) = WEIRS(N)%N_1A_1
102       CONTINUE
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_1A_2 == TMP_NODES(L)) GOTO 104
            IF(WEIRS(N)%N_1A_2 <  TMP_NODES(L)) GOTO 103
          ENDDO
103       CONTINUE
          NWEIRS_NODES = NWEIRS_NODES + 1
          DO I= NWEIRS_NODES,L+1,-1
            TMP_NODES(I) = TMP_NODES(I-1)
          END DO
          TMP_NODES(I) = WEIRS(N)%N_1A_2
104       CONTINUE
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_1B_1 == TMP_NODES(L)) GOTO 106
            IF(WEIRS(N)%N_1B_1 <  TMP_NODES(L)) GOTO 105
          ENDDO
105       CONTINUE
          NWEIRS_NODES = NWEIRS_NODES + 1
          DO I= NWEIRS_NODES,L+1,-1
            TMP_NODES(I) = TMP_NODES(I-1)
          END DO
          TMP_NODES(I) = WEIRS(N)%N_1B_1
106       CONTINUE
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_1B_2 == TMP_NODES(L)) GOTO 108
            IF(WEIRS(N)%N_1B_2 <  TMP_NODES(L)) GOTO 107
          ENDDO
107       CONTINUE
          NWEIRS_NODES = NWEIRS_NODES + 1
          DO I= NWEIRS_NODES,L+1,-1
            TMP_NODES(I) = TMP_NODES(I-1)
          END DO
          TMP_NODES(I) = WEIRS(N)%N_1B_2
108       CONTINUE
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_2A_1 == TMP_NODES(L)) GOTO 112
            IF(WEIRS(N)%N_2A_1 <  TMP_NODES(L)) GOTO 111
          ENDDO
111       CONTINUE
          NWEIRS_NODES = NWEIRS_NODES + 1
          DO I= NWEIRS_NODES,L+1,-1
            TMP_NODES(I) = TMP_NODES(I-1)
          END DO
          TMP_NODES(I) = WEIRS(N)%N_2A_1
112       CONTINUE
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_2A_2 == TMP_NODES(L)) GOTO 114
            IF(WEIRS(N)%N_2A_2 <  TMP_NODES(L)) GOTO 113
          ENDDO
113       CONTINUE
          NWEIRS_NODES = NWEIRS_NODES + 1
          DO I= NWEIRS_NODES,L+1,-1
            TMP_NODES(I) = TMP_NODES(I-1)
          END DO
          TMP_NODES(I) = WEIRS(N)%N_2A_2
114       CONTINUE
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_2B_1 == TMP_NODES(L)) GOTO 116
            IF(WEIRS(N)%N_2B_1 <  TMP_NODES(L)) GOTO 115
          ENDDO
115       CONTINUE
          NWEIRS_NODES = NWEIRS_NODES + 1
          DO I= NWEIRS_NODES,L+1,-1
            TMP_NODES(I) = TMP_NODES(I-1)
          END DO
          TMP_NODES(I) = WEIRS(N)%N_2B_1
116       CONTINUE
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_2B_2 == TMP_NODES(L)) GOTO 118
            IF(WEIRS(N)%N_2B_2 <  TMP_NODES(L)) GOTO 117
          ENDDO
117       CONTINUE
          NWEIRS_NODES = NWEIRS_NODES + 1
          DO I= NWEIRS_NODES,L+1,-1
            TMP_NODES(I) = TMP_NODES(I-1)
          END DO
          TMP_NODES(I) = WEIRS(N)%N_2B_2
118       CONTINUE
        ENDDO
      ENDIF      
!
!  ALLOCATION
!
      IF(NWEIRS_NODES.GT.0) THEN
        ALLOCATE(WNODES(NWEIRS_NODES),STAT=ERROR)
        IF (ERROR /= 0) THEN
          WRITE(LU,*)
     &      'LECSNG2: ERROR ALLOC WNODES:',ERROR
          CALL PLANTE(1)
          STOP
        ENDIF
!        NB16 = NWEIRS_NODES
        NB16 = NWEIRS_NODES/4
        IF(MOD(NWEIRS_NODES,4).EQ.0) THEN
          NB16 = NB16*4
        ELSE
          NB16 = NB16*4 + 4
        ENDIF
        ALLOCATE(W_BUF_RECV(2*NB16,NCSIZE),STAT=ERROR)
!        ALLOCATE(W_BUF_RECV(NB16,NCSIZE),STAT=ERROR)
        IF (ERROR /= 0) THEN
          WRITE(LU,*)
     &      'LECSNG2: ERROR ALLOC W_BUF_RECV:',ERROR
          CALL PLANTE(1)
          STOP
        ENDIF
        ALLOCATE(W_BUF_SEND(2*NB16,NCSIZE),STAT=ERROR)
!        ALLOCATE(W_BUF_SEND(NB16,NCSIZE),STAT=ERROR)
        IF (ERROR /= 0) THEN
          WRITE(LU,*)
     &      'LECSNG2: ERROR ALLOC W_BUF_SEND:',ERROR
          CALL PLANTE(1)
          STOP
        ENDIF
!
        IF(NTRAC.GT.0) THEN
          DO N=1, NWEIRS_NODES
            ALLOCATE(WNODES(N)%TRAC(NTRAC),STAT=ERROR)
            IF (ERROR /= 0) THEN
              WRITE(LU,*)
     &          'LECSNG2: ERROR ALLOC WNODES%TRAC:',ERROR
              CALL PLANTE(1)
              STOP
            ENDIF
          ENDDO
          ALLOCATE(WT_BUF_RECV(NTRAC*NB16,NCSIZE),STAT=ERROR)
!          ALLOCATE(WT_BUF_RECV(NB16,NCSIZE),STAT=ERROR)
          IF (ERROR /= 0) THEN
            WRITE(LU,*)
     &        'LECSNG2: ERROR ALLOC WT_BUF_RECV:',ERROR
            CALL PLANTE(1)
            STOP
          ENDIF
          ALLOCATE(WT_BUF_SEND(NTRAC*NB16,NCSIZE),STAT=ERROR)
!          ALLOCATE(W_BUF_SEND(NB16,NCSIZE),STAT=ERROR)
          IF (ERROR /= 0) THEN
            WRITE(LU,*)
     &        'LECSNG2: ERROR ALLOC WT_BUF_SEND:',ERROR
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
!
      ENDIF
!
!     READING THE PARALLEL INFORMATIONS OF NODES IF NEEDED (RECEIVE) - 2nd part
!
      IF(NCSIZE.GT.1) THEN
!
        DO N=1,NWEIRS_NODES
          READ(IFIC,*,ERR=990) WNODES(N)%NUM_GLO, NB
          WNODES(N)%NB_NEIGH      = NB
          WNODES(N)%LIST_NEIGH(:) = -1
          WNODES(N)%NUM_LOC(:)    = -1
          IF (NB.GT.0) THEN
            READ(IFIC,*,ERR=996) (WNODES(N)%LIST_NEIGH(L),
     &                            WNODES(N)%NUM_LOC(L),L=1,NB)
          ENDIF
        ENDDO
!
      ELSE
!
        DO N=1, NWEIRS_NODES
          WNODES(N)%NUM_GLO       = TMP_NODES(N)
          WNODES(N)%NB_NEIGH      =  0
          WNODES(N)%LIST_NEIGH(:) = -1
          WNODES(N)%NUM_LOC(:)    = -1
        ENDDO
!
!       UPDATE OF WEIRS DESCRIPTION FOR NODES
!
        DO N=1, NWEIRS
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_1A_1 == WNODES(L)%NUM_GLO) THEN
              WEIRS(N)%N_1A_1 = L
              GOTO 201
            ENDIF
          ENDDO
201       CONTINUE
        ENDDO
        DO N=1, NWEIRS
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_1A_2 == WNODES(L)%NUM_GLO) THEN
              WEIRS(N)%N_1A_2 = L
              GOTO 202
            ENDIF
          ENDDO
202       CONTINUE
        ENDDO
        DO N=1, NWEIRS
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_1B_1 == WNODES(L)%NUM_GLO) THEN
              WEIRS(N)%N_1B_1 = L
              GOTO 203
            ENDIF
          ENDDO
203       CONTINUE
        ENDDO
        DO N=1, NWEIRS
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_1B_2 == WNODES(L)%NUM_GLO) THEN
              WEIRS(N)%N_1B_2 = L
              GOTO 204
            ENDIF
          ENDDO
204       CONTINUE
        ENDDO
        DO N=1, NWEIRS
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_2A_1 == WNODES(L)%NUM_GLO) THEN
              WEIRS(N)%N_2A_1 = L
              GOTO 205
            ENDIF
          ENDDO
205       CONTINUE
        ENDDO
        DO N=1, NWEIRS
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_2A_2 == WNODES(L)%NUM_GLO) THEN
              WEIRS(N)%N_2A_2 = L
              GOTO 206
            ENDIF
          ENDDO
206       CONTINUE
        ENDDO
        DO N=1, NWEIRS
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_2B_1 == WNODES(L)%NUM_GLO) THEN
              WEIRS(N)%N_2B_1 = L
              GOTO 207
            ENDIF
          ENDDO
207       CONTINUE
        ENDDO
        DO N=1, NWEIRS
          DO L=1, NWEIRS_NODES
            IF(WEIRS(N)%N_2B_2 == WNODES(L)%NUM_GLO) THEN
              WEIRS(N)%N_2B_2 = L
              GOTO 208
            ENDIF
          ENDDO
208       CONTINUE
        ENDDO
      ENDIF
!
!     READING THE PARALLEL INFORMATIONS OF NODES IF NEEDED (RECEIVE) - 3rd part
!
      IF(NCSIZE.GT.1) THEN
        READ(IFIC,*,END=900,ERR=900)
        READ(IFIC,*,ERR=996) N_NGHB_W_NODES
      ELSE
        N_NGHB_W_NODES = 1
      ENDIF
!
!       ALLOCATION
!
!      NB16 = N_NGHB_W_NODES
      NB16 = N_NGHB_W_NODES/4
      IF(MOD(N_NGHB_W_NODES,4).EQ.0) THEN
        NB16 = NB16*4
      ELSE
        NB16 = NB16*4 + 4
      ENDIF
      ALLOCATE(WNODES_PROC(NB16),STAT=ERROR)
      IF (ERROR /= 0) THEN
        WRITE(LU,*)
     &    'LECSNG2: ERROR ALLOC WNODES_PROC:',ERROR
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     READING THE PARALLEL INFORMATIONS OF NODES IF NEEDED (RECEIVE) - 4th part
!
      IF(NCSIZE.GT.1) THEN
        IF(N_NGHB_W_NODES.GT.0) THEN
          DO N=1, N_NGHB_W_NODES
            READ(IFIC,*,ERR=996) WNODES_PROC(N)%NUM_NEIGH, NB
            WNODES_PROC(N)%NB_NODES = NB
            IF (NB.GT.0) THEN
!              NB16 = NB
              NB16 = NB/4
              IF(MOD(NB,4).EQ.0) THEN
                NB16 = NB16*4
              ELSE
                NB16 = NB16*4 + 4
              ENDIF
              ALLOCATE(WNODES_PROC(N)%LIST_NODES(NB16),STAT=ERROR)
              IF (ERROR /= 0) THEN
                WRITE(LU,*)
     &            'LECSNG2: ERROR ALLOC WNODES_PROC%LIST_NODES:',ERROR
                CALL PLANTE(1)
                STOP
              ENDIF
              ALLOCATE(WNODES_PROC(N)%NUM_GLO(NB16),STAT=ERROR)
              IF (ERROR /= 0) THEN
                WRITE(LU,*)
     &            'LECSNG2: ERROR ALLOC WNODES_PROC%NUM_GLO:',ERROR
                CALL PLANTE(1)
                STOP
              ENDIF
              ALLOCATE(WNODES_PROC(N)%NUM_LOC(NB16),STAT=ERROR)
              IF (ERROR /= 0) THEN
                WRITE(LU,*)
     &            'LECSNG2: ERROR ALLOC WNODES_PROC%NUM_LOC:',ERROR
                CALL PLANTE(1)
                STOP
              ENDIF
              READ(IFIC,*,ERR=996) (WNODES_PROC(N)%LIST_NODES(L),L=1,NB)
              READ(IFIC,*,ERR=996) (WNODES_PROC(N)%NUM_GLO(L),L=1,NB)
              READ(IFIC,*,ERR=996) (WNODES_PROC(N)%NUM_LOC(L),L=1,NB)
            ENDIF
          ENDDO
        ENDIF
!
      ELSE
!     IN CASE OF SEQUENTIAL COMPUTATION, RECONSTRUCTION OF SOME STRUCTURE
!        TO ENSURE A SIMILAR TREATMENT      
!
        WNODES_PROC(1)%NUM_NEIGH = 0
        WNODES_PROC(1)%NB_NODES  = NWEIRS_NODES
        IF (NWEIRS_NODES.GT.0) THEN
!          NB16 = NWEIRS_NODES
          NB16 = NWEIRS_NODES/4
          IF(MOD(NWEIRS_NODES,4).EQ.0) THEN
            NB16 = NB16*4
          ELSE
            NB16 = NB16*4 + 4
          ENDIF
          ALLOCATE(WNODES_PROC(1)%LIST_NODES(NB16),STAT=ERROR)
          IF (ERROR /= 0) THEN
            WRITE(LU,*)
     &        'LECSNG2: ERROR ALLOC WNODES_PROC%LIST_NODES:',ERROR
            CALL PLANTE(1)
            STOP
          ENDIF
          ALLOCATE(WNODES_PROC(1)%NUM_GLO(NB16),STAT=ERROR)
          IF (ERROR /= 0) THEN
            WRITE(LU,*)
     &        'LECSNG2: ERROR ALLOC WNODES_PROC%NUM_GLO:',ERROR
            CALL PLANTE(1)
            STOP
          ENDIF
          ALLOCATE(WNODES_PROC(1)%NUM_LOC(NB16),STAT=ERROR)
          IF (ERROR /= 0) THEN
            WRITE(LU,*)
     &        'LECSNG2: ERROR ALLOC WNODES_PROC%NUM_LOC:',ERROR
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
        DO I=1, NWEIRS_NODES
          WNODES_PROC(1)%LIST_NODES(I) = I
          WNODES_PROC(1)%NUM_GLO(I)    = WNODES(I)%NUM_GLO
          WNODES_PROC(1)%NUM_LOC(I)    = WNODES(I)%NUM_GLO
        ENDDO
      ENDIF
!
!     READING THE PARALLEL INFORMATIONS OF NODES IF NEEDED (SEND)
!
      IF(NCSIZE.GT.1) THEN
!
        READ(IFIC,*,END=900,ERR=900)
        READ(IFIC,*,ERR=997) N_WN_SEND
!
      ELSE
        N_WN_SEND = NWEIRS_NODES
      ENDIF
!
      IF(N_WN_SEND.GT.0) THEN
        ALLOCATE(WN_SEND(N_WN_SEND),STAT=ERROR)
        IF (ERROR /= 0) THEN
          WRITE(LU,*)
     &      'LECSNG2: ERROR ALLOC WN_SEND:',ERROR
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
        DO N=1,N_WN_SEND
          READ(IFIC,*,ERR=990) WN_SEND(N)%NUM_GLO, NB
          WN_SEND(N)%NB_NEIGH      = NB
          WN_SEND(N)%LIST_NEIGH(:) = -1
          IF (NB.GT.0) THEN
            READ(IFIC,*,ERR=996) (WN_SEND(N)%LIST_NEIGH(L),L=1,NB)
          ENDIF
        ENDDO
!
      ELSE
!
        DO N=1,N_WN_SEND
          WN_SEND(N)%NUM_GLO       = WNODES(N)%NUM_GLO
          WN_SEND(N)%NB_NEIGH      =  0
          WN_SEND(N)%LIST_NEIGH(:) = -1
        ENDDO
!
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
        READ(IFIC,*,END=900,ERR=900)
        READ(IFIC,*,ERR=996) N_NGHB_WN_SEND
      ELSE
        N_NGHB_WN_SEND = 1
      ENDIF
!
!       ALLOCATION
!
!      NB16 = N_NGHB_WN_SEND
      NB16 = N_NGHB_WN_SEND/4
      IF(MOD(N_NGHB_WN_SEND,4).EQ.0) THEN
        NB16 = NB16*4
      ELSE
        NB16 = NB16*4 + 4
      ENDIF
      ALLOCATE(WN_SEND_PROC(NB16),STAT=ERROR)
      IF (ERROR /= 0) THEN
        WRITE(LU,*)
     &    'LECSNG2: ERROR ALLOC WN_SEND_PROC:',ERROR
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(NCSIZE.GT.1) THEN
        IF(N_NGHB_WN_SEND.GT.0) THEN
          DO N=1, N_NGHB_WN_SEND
            READ(IFIC,*,ERR=996) WN_SEND_PROC(N)%NUM_NEIGH, NB
            WN_SEND_PROC(N)%NB_NODES = NB
            IF (NB.GT.0) THEN
!              NB16 = NB
              NB16 = NB/4
              IF(MOD(NB,4).EQ.0) THEN
                NB16 = NB16*4
              ELSE
                NB16 = NB16*4 + 4
              ENDIF
              ALLOCATE(WN_SEND_PROC(N)%LIST_NODES(NB16),STAT=ERROR)
              IF (ERROR /= 0) THEN
                WRITE(LU,*)
     &            'LECSNG2: ERROR ALLOC WN_SEND_PROC%LIST_NODES:',ERROR
                CALL PLANTE(1)
                STOP
              ENDIF
              ALLOCATE(WN_SEND_PROC(N)%NUM_GLO(NB16),STAT=ERROR)
              IF (ERROR /= 0) THEN
                WRITE(LU,*)
     &            'LECSNG2: ERROR ALLOC WN_SEND_PROC%NUM_GLO:',ERROR
                CALL PLANTE(1)
                STOP
              ENDIF
              ALLOCATE(WN_SEND_PROC(N)%NUM_LOC(NB16),STAT=ERROR)
              IF (ERROR /= 0) THEN
                WRITE(LU,*)
     &            'LECSNG2: ERROR ALLOC WN_SEND_PROC%NUM_LOC:',ERROR
                CALL PLANTE(1)
                STOP
              ENDIF
              READ(IFIC,*,ERR=996) (WN_SEND_PROC(N)%LIST_NODES(L),
     &                              L=1,NB)
              READ(IFIC,*,ERR=996) (WN_SEND_PROC(N)%NUM_GLO(L), L=1,NB)
              READ(IFIC,*,ERR=996) (WN_SEND_PROC(N)%NUM_LOC(L), L=1,NB)
            ENDIF
          ENDDO
        ENDIF
!
      ELSE
        WN_SEND_PROC(1)%NUM_NEIGH = 0
        WN_SEND_PROC(1)%NB_NODES  = NWEIRS_NODES
        IF (NWEIRS_NODES.GT.0) THEN
!          NB16 = NWEIRS_NODES
          NB16 = NWEIRS_NODES/4
          IF(MOD(NWEIRS_NODES,4).EQ.0) THEN
            NB16 = NB16*4
          ELSE
            NB16 = NB16*4 + 4
          ENDIF
          ALLOCATE(WN_SEND_PROC(1)%LIST_NODES(NB16),STAT=ERROR)
          IF (ERROR /= 0) THEN
            WRITE(LU,*)
     &        'LECSNG2: ERROR ALLOC WN_SEND_PROC%LIST_NODES:',ERROR
            CALL PLANTE(1)
            STOP
          ENDIF
          ALLOCATE(WN_SEND_PROC(1)%NUM_GLO(NB16),STAT=ERROR)
          IF (ERROR /= 0) THEN
            WRITE(LU,*)
     &        'LECSNG2: ERROR ALLOC WN_SEND_PROC%NUM_GLO:',ERROR
            CALL PLANTE(1)
            STOP
          ENDIF
          ALLOCATE(WN_SEND_PROC(1)%NUM_LOC(NB16),STAT=ERROR)
          IF (ERROR /= 0) THEN
            WRITE(LU,*)
     &        'LECSNG2: ERROR ALLOC WN_SEND_PROC%NUM_LOC:',ERROR
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
        DO I=1, NWEIRS_NODES
          WN_SEND_PROC(1)%LIST_NODES(I) = I
          WN_SEND_PROC(1)%NUM_GLO(I)    = WNODES(I)%NUM_GLO
          WN_SEND_PROC(1)%NUM_LOC(I)    = WNODES(I)%NUM_GLO
        ENDDO
      ENDIF
!
      IF (ALLOCATED(TMP_NODES)) DEALLOCATE(TMP_NODES)
!
      GO TO 1000
!
!-----------------------------------------------------------------------
!     ERROR MESSAGES
!-----------------------------------------------------------------------
!
998   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG2 : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '          FICHIER DE DONNEES DES SEUILS'
        WRITE(LU,*) '          2EME LIGNE DU FICHIER NON CONFORME.'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG2 : READ ERROR ON THE'
        WRITE(LU,*) '          WEIRS DATA FILE'
        WRITE(LU,*) '          AT LINE 2'
      ENDIF
      GO TO 2000
!
997   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG2 : ERREUR DE LECTURE SUR LES'
        WRITE(LU,*) '          DONNEES PARALLELES DU FICHIER'
        WRITE(LU,*) '          DES SEUILS'
        WRITE(LU,*) '          NOMBRE DE POINTS ILLISIBLE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG2 : READ ERROR ON THE'
        WRITE(LU,*) '          PARALLEL DATA OF THE'
        WRITE(LU,*) '          WEIR FILE'
        WRITE(LU,*) '          THE NUMBER OF POINTS CANNOT BE READ'
      ENDIF
      GO TO 2000
!
996   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG2 : ERREUR DE LECTURE SUR LA'
        WRITE(LU,*) '          DONNEE PARALLELE',N
        WRITE(LU,*) '          DU FICHIER DES SEUILS'
        WRITE(LU,*) '          DONNEES ILLISIBLE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG2 : READ ERROR ON THE'
        WRITE(LU,*) '          PARALLEL DATA',N
        WRITE(LU,*) '          OF THE WEIR FILE'
        WRITE(LU,*) '          DATA CANNOT BE READ'
      ENDIF
      GO TO 2000
!
994   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG2 : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '          FICHIER DE DONNEES DES SEUILS'
        WRITE(LU,*) '          POUR LA SINGULARITE ',N
        WRITE(LU,*) '          NUMDIGS DES POINTS ILLISIBLE'
        WRITE(LU,*) '          POUR LE COTE 2'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG2 : READ ERROR ON THE'
        WRITE(LU,*) '          WEIRS DATA FILE'
        WRITE(LU,*) '          FOR SINGULARITY NUMBER ',N
        WRITE(LU,*) '          THE NUMBER OF THE POINTS CANNOT BE READ'
        WRITE(LU,*) '          FOR SIDE NUMBER 2'
      ENDIF
      GO TO 2000
!
992   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG2 : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '          FICHIER DE DONNEES DES SEUILS'
        WRITE(LU,*) '          POUR LA SINGULARITE ',N
        WRITE(LU,*) '          COTES SUR LA DIGUE ILLISIBLES'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG2 : READ ERROR ON THE'
        WRITE(LU,*) '          WEIRS DATA FILE'
        WRITE(LU,*) '          FOR SINGULARITY NUMBER ',N
        WRITE(LU,*) '          ELEVATIONS ON THE WEIR CANNOT BE READ'
      ENDIF
      GO TO 1000
!
991   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG2 : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '          FICHIER DE DONNEES DES SEUILS'
        WRITE(LU,*) '          POUR LA SINGULARITE ',N
        WRITE(LU,*) '          COEFFICIENTS DE DEBIT ILLISIBLES'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG2 : READ ERROR ON THE'
        WRITE(LU,*) '          WEIRS DATA FILE'
        WRITE(LU,*) '          FOR SINGULARITY NUMBER ',N
        WRITE(LU,*) '          DISCHARGE COEFFICIENTS CANNOT BE READ'
      ENDIF
      GO TO 2000
!
990   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG2 : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '          FICHIER DE DONNEES DES SEUILS'
        WRITE(LU,*) '          POUR LA SINGULARITE ',N
        WRITE(LU,*) '          DESCRIPTION DU SEUIL ILLISIBLE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG2 : READ ERROR ON THE'
        WRITE(LU,*) '          WEIRS DATA FILE'
        WRITE(LU,*) '          FOR SINGULARITY NUMBER ',N
        WRITE(LU,*) '          WEIR DESCRIPTION CANNOT BE READ'
      ENDIF
      GO TO 2000
!
900   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG2 : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '          FICHIER DE DONNEES DES SEUILS'
        WRITE(LU,*) '          FIN DE FICHIER PREMATUREE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG2 : READ ERROR ON THE'
        WRITE(LU,*) '          WEIRS DATA FILE'
        WRITE(LU,*) '          UNEXPECTED END OF FILE'
      ENDIF
!
2000  CONTINUE
!
      NWEIRS = 0
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)
        WRITE(LU,*)'LECSNG2 : ERREUR DE LECTURE'
        WRITE(LU,*)'          AUCUNE SINGULARITE NE SERA'
        WRITE(LU,*)'          PRISE EN COMPTE.'
        WRITE(LU,*)
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*)
        WRITE(LU,*)'LECSNG2 : READ ERROR'
        WRITE(LU,*)'          NO SINGULARITY WILL BE TAKEN'
        WRITE(LU,*)'          INTO ACCOUNT'
        WRITE(LU,*)
      ENDIF
!
1000  CONTINUE
!
!      DO N=1, NWEIRS
!        CALL OS('X=0     ',X=QP0%ADR(N)%P)
!      ENDDO
!      CALL ALLBLO(TWEIRA,'TWEIRA')
!      CALL ALLBLO(TWEIRB,'TWEIRA')
!      IF(NTRAC.GT.0) THEN
!        CALL BIEF_ALLVEC_IN_BLOCK(TWEIRA,NTRAC,1,'TWEIRA',
!     &                            NWEIRS,MAXNPS,0,MESH)
!        CALL BIEF_ALLVEC_IN_BLOCK(TWEIRB,NTRAC,1,'TWEIRB',
!     &                            NWEIRS,MAXNPS,0,MESH)
!      ELSE
!        CALL BIEF_ALLVEC_IN_BLOCK(TWEIRA,1    ,1,'TWEIRA',
!     &                            NWEIRS,MAXNPS,0,MESH)
!        CALL BIEF_ALLVEC_IN_BLOCK(TWEIRB,1    ,1,'TWEIRB',
!     &                            NWEIRS,MAXNPS,0,MESH)
!      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
