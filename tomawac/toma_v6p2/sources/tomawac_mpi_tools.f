!                    ************************
                     MODULE TOMAWAC_MPI_TOOLS
!                    ************************
!
!
!***********************************************************************
! TOMAWAC
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
       IMPLICIT NONE
       CONTAINS
       SUBROUTINE CORRECT_GOODELT(GOODELT,NPOIN2,NPLAN,MESH)
! THIS SUBROUTINE IDENTIFIES THE POINTS AT THE EDGE OF SUB-DOMAINS
! AND THOSE AT THE EDGE OF THE WHOLE DOMAIN. IF THEY ARE NOT LOCATED IN
! THE CORRECT ELEMENTS (ON EITHER PROCESSOR) THEY ARE ASSUMED TO BE
! LOCATED THE CORRECT ELEMENT. THIS IS NECESSARY IN THE CASE OF A
! 'PARCOM' ON THE NUMBER OF SUB-ITERATIONS IN PIED_TOMAWAC_MPI
!
! |      GOODELT   || IDENTIFIANT D'UNE CARACTERISTIQUE
! |                |    | = 1 BON ELEMENT POUR REMONTEE DES CARACT.
! |                |    | = 2001 BON ELEMENT A LA FRONTIERE 2 PROCS
! |                |    | = 2000 BAD ELEMENT FRONTIERE 2 PROCS
! |                |    | = 1101 BON ELEMENT FRONTIERE 2 PROCS +
! |                |    |   FRONTIERE SOLIDE
! |                |    | = 1100 BAD ELEMENT FRONTIERE 2 PROCS +
! |                |    |   FRONTIERE SOLIDE
! |                |    | = 1011 BON ELEMENT FRONTIERE 2 PROCS +
! |                |    |   FRONTIERE LIQUIDE
! |                |    | = 1010 BAD ELEMENT FRONTIERE 2 PROCS +
! |                |    |   FRONTIERE LIQUIDE
! |      NPOIN2    | -->| NOMBRE DE POINTS 2D
! |      NPLAN     | -->| NOMBRE DE PLAN POUR DEFINIR LE 3D
! |      MESH      | -->| MAILLAGE.
!
         USE BIEF
         IMPLICIT NONE
         INTEGER,INTENT(IN) :: NPOIN2,NPLAN
         INTEGER,DIMENSION(NPOIN2,NPLAN), INTENT(INOUT) :: GOODELT
         TYPE(BIEF_MESH),INTENT(INOUT)  ::  MESH
         DOUBLE PRECISION, DIMENSION(NPOIN2,NPLAN) :: TES
         INTEGER :: IPLAN
         TES = DBLE(GOODELT)
         DO IPLAN=1,NPLAN
          CALL PARCOM2
     &    ( TES(:,IPLAN) ,
     &      TES(:,IPLAN) ,
     &      TES(:,IPLAN) ,
     &      NPOIN2 , 1 , 2 , 1 , MESH )
         ENDDO
         WHERE(TES(:,:)==2200.D0)
!            GOODELT(:,:) = 1101
             GOODELT(:,:) = 1102
          END WHERE
          WHERE(TES(:,:)==2020.D0)
            GOODELT(:,:) = 1011
          END WHERE
          WHERE(TES(:,:)==4002.D0)
            GOODELT(:,:) = 1003
          END WHERE
          WHERE(TES(:,:)==2202.D0)
            GOODELT(:,:) = 1103
          END WHERE
       END SUBROUTINE CORRECT_GOODELT
!
!
!
       SUBROUTINE ALLOC_LOCAL(NARRIV,FREQ,NF,NLOSTAGAIN,
     &                      NUMBERLOST,NARRSUM)
         USE BIEF
         USE TOMAWAC_MPI
         IMPLICIT NONE
         INTEGER, INTENT(INOUT) :: NARRIV,NLOSTAGAIN,NUMBERLOST,
     &                            NF,NARRSUM,FREQ
         INTEGER P_ISUM,P_IMAX
         EXTERNAL P_ISUM,P_IMAX
!
        NARRIV = SUM(RECVCOUNTS(:,FREQ))
        NLOSTAGAIN = COUNT(RECVCHAR(1:NARRIV,FREQ)%NEPID/=-1)
        NUMBERLOST = P_ISUM(NLOSTAGAIN)
!
           NARRSUM = P_ISUM(NARRIV)
           IF (.NOT.ALLOCATED(SH_LOC)) ALLOCATE(SH_LOC(NF))
!            IF (.NOT.ALLOCATED(SH_LOC(FREQ)%SHP1)) ALLOCATE(
!      *      SH_LOC(FREQ)%SHP1(NARRSUM),SH_LOC(FREQ)%SHP2(NARRSUM),
!      *      SH_LOC(FREQ)%SHP3(NARRSUM),SH_LOC(FREQ)%SHZ(NARRSUM),
!      *      SH_LOC(FREQ)%ELT(NARRSUM),SH_LOC(FREQ)%ETA(NARRSUM))
       ALLOCATE(SH_LOC(FREQ)%SHP1(NARRSUM))
       ALLOCATE(SH_LOC(FREQ)%SHP2(NARRSUM))
       ALLOCATE(SH_LOC(FREQ)%SHP3(NARRSUM))
       ALLOCATE(SH_LOC(FREQ)%SHZ(NARRSUM))
       ALLOCATE(SH_LOC(FREQ)%ETA(NARRSUM))
       ALLOCATE(SH_LOC(FREQ)%ELT(NARRSUM))
       SH_LOC(FREQ)%SHP1=0.D0
       SH_LOC(FREQ)%SHP2=0.D0
       SH_LOC(FREQ)%SHP3=0.D0
       SH_LOC(FREQ)%SHZ=0.D0
       SH_LOC(FREQ)%ETA=0
       SH_LOC(FREQ)%ELT=0
!
!
       END SUBROUTINE ALLOC_LOCAL
!
       SUBROUTINE ALLOC_LOCAL_4D(NARRIV,FREQ,NF,NLOSTAGAIN,
     &                      NUMBERLOST,NARRSUM)
         USE BIEF
         USE TOMAWAC_MPI
         IMPLICIT NONE
         INTEGER, INTENT(INOUT) :: NARRIV,NLOSTAGAIN,NUMBERLOST,
     &                            NF,NARRSUM,FREQ
         INTEGER P_ISUM,P_IMAX
         EXTERNAL P_ISUM,P_IMAX
!
        NARRIV = SUM(RECVCOUNTS(:,FREQ))
        NLOSTAGAIN = COUNT(RECVCHAR_4D(1:NARRIV,FREQ)%NEPID/=-1)
        NUMBERLOST = P_ISUM(NLOSTAGAIN)
!
           NARRSUM = P_ISUM(NARRIV)
           IF (.NOT.ALLOCATED(SH_LOC_4D)) ALLOCATE(SH_LOC_4D(NF))
!            IF (.NOT.ALLOCATED(SH_LOC_4D(FREQ)%SHP1)) ALLOCATE(
!      *      SH_LOC_4D(FREQ)%SHP1(NARRSUM),SH_LOC_4D(FREQ)%SHP2(NARRSUM),
!      *      SH_LOC_4D(FREQ)%SHP3(NARRSUM),SH_LOC_4D(FREQ)%SHZ(NARRSUM),
!      *      SH_LOC_4D(FREQ)%ELT(NARRSUM),SH_LOC_4D(FREQ)%ETA(NARRSUM),
!      *      SH_LOC_4D(FREQ)%FRE(NARRSUM),SH_LOC_4D(FREQ)%SHF(NARRSUM))
       ALLOCATE(SH_LOC_4D(FREQ)%SHP1(NARRSUM))
       ALLOCATE(SH_LOC_4D(FREQ)%SHP2(NARRSUM))
       ALLOCATE(SH_LOC_4D(FREQ)%SHP3(NARRSUM))
       ALLOCATE(SH_LOC_4D(FREQ)%SHZ(NARRSUM))
       ALLOCATE(SH_LOC_4D(FREQ)%SHF(NARRSUM))
       ALLOCATE(SH_LOC_4D(FREQ)%ETA(NARRSUM))
       ALLOCATE(SH_LOC_4D(FREQ)%ELT(NARRSUM))
       ALLOCATE(SH_LOC_4D(FREQ)%FRE(NARRSUM))
       SH_LOC_4D(FREQ)%SHP1=0.D0
       SH_LOC_4D(FREQ)%SHP2=0.D0
       SH_LOC_4D(FREQ)%SHP3=0.D0
       SH_LOC_4D(FREQ)%SHZ=0.D0
       SH_LOC_4D(FREQ)%SHF=0.D0
       SH_LOC_4D(FREQ)%ELT=0
       SH_LOC_4D(FREQ)%ETA=0
       SH_LOC_4D(FREQ)%FRE=0
!
!
       END SUBROUTINE ALLOC_LOCAL_4D
!
       SUBROUTINE ALLOC_AGAIN(NARRIV,FREQ,NLOSTAGAIN,NUMBERLOST,NUMBER)
        USE BIEF
        USE TOMAWAC_MPI
        IMPLICIT NONE
        INTEGER, INTENT(INOUT) :: NARRIV,FREQ,NLOSTAGAIN,NUMBERLOST,
     &                            NUMBER
        INTEGER P_ISUM,P_IMAX
        EXTERNAL P_ISUM,P_IMAX
        INTEGER :: I
!
        IF (.NOT.ALLOCATED(SENDCOUNTS_AGAIN))
     &                          ALLOCATE(SENDCOUNTS_AGAIN(NCSIZE))
        IF (.NOT.ALLOCATED(RECVCOUNTS_AGAIN))
     &                          ALLOCATE(RECVCOUNTS_AGAIN(NCSIZE))
        IF (.NOT.ALLOCATED(SDISPLS_AGAIN))
     &                          ALLOCATE(SDISPLS_AGAIN(NCSIZE))
        IF (.NOT.ALLOCATED(RDISPLS_AGAIN))
     &                          ALLOCATE(RDISPLS_AGAIN(NCSIZE))
        NLOSTAGAIN = COUNT(RECVCHAR(1:NARRIV,FREQ)%NEPID/=-1)
        NUMBERLOST = P_ISUM(NLOSTAGAIN)
         IF(.NOT.ALLOCATED(SENDAGAIN))
     &           ALLOCATE(SENDAGAIN(MAX(NUMBERLOST,1)))
!
         SENDCOUNTS_AGAIN = 0
         NUMBER = 0
         DO I=1,NARRIV
           IF (RECVCHAR(I,FREQ)%NEPID.NE.-1) THEN
             NUMBER=NUMBER+1
             SENDCOUNTS_AGAIN(RECVCHAR(I,FREQ)%NEPID+1)=
     &               SENDCOUNTS_AGAIN(RECVCHAR(I,FREQ)%NEPID+1)+1
             SENDAGAIN(NUMBER) = RECVCHAR(I,FREQ)
           ENDIF
         ENDDO
       END SUBROUTINE ALLOC_AGAIN
!
       SUBROUTINE ALLOC_AGAIN_4D(NARRIV,FREQ,NLOSTAGAIN,NUMBERLOST,
     &                                                       NUMBER)
        USE BIEF
        USE TOMAWAC_MPI
        IMPLICIT NONE
        INTEGER, INTENT(INOUT) :: NARRIV,FREQ,NLOSTAGAIN,NUMBERLOST,
     &                            NUMBER
        INTEGER P_ISUM,P_IMAX
        EXTERNAL P_ISUM,P_IMAX
        INTEGER :: I
!
        IF (.NOT.ALLOCATED(SENDCOUNTS_AGAIN))
     &                          ALLOCATE(SENDCOUNTS_AGAIN(NCSIZE))
        IF (.NOT.ALLOCATED(RECVCOUNTS_AGAIN))
     &                          ALLOCATE(RECVCOUNTS_AGAIN(NCSIZE))
        IF (.NOT.ALLOCATED(SDISPLS_AGAIN))
     &                          ALLOCATE(SDISPLS_AGAIN(NCSIZE))
        IF (.NOT.ALLOCATED(RDISPLS_AGAIN))
     &                          ALLOCATE(RDISPLS_AGAIN(NCSIZE))
        NLOSTAGAIN = COUNT(RECVCHAR_4D(1:NARRIV,FREQ)%NEPID/=-1)
        NUMBERLOST = P_ISUM(NLOSTAGAIN)
         IF(.NOT.ALLOCATED(SENDAGAIN_4D))
     &           ALLOCATE(SENDAGAIN_4D(MAX(NUMBERLOST,1)))
!
         SENDCOUNTS_AGAIN = 0
         NUMBER = 0
         DO I=1,NARRIV
           IF (RECVCHAR_4D(I,FREQ)%NEPID.NE.-1) THEN
             NUMBER=NUMBER+1
             SENDCOUNTS_AGAIN(RECVCHAR_4D(I,FREQ)%NEPID+1)=
     &            SENDCOUNTS_AGAIN(RECVCHAR_4D(I,FREQ)%NEPID+1)+1
             SENDAGAIN_4D(NUMBER) = RECVCHAR_4D(I,FREQ)
           ENDIF
         ENDDO
       END SUBROUTINE ALLOC_AGAIN_4D
!
       SUBROUTINE ORGANIZE_SENDAGAIN
        USE BIEF
        USE TOMAWAC_MPI
        IMPLICIT NONE
        INTEGER :: I,I1,I2,I3
!
        I2 = 0
        DO I=1,NCSIZE
           DO I1=1,SENDCOUNTS_AGAIN(I)
              I2=I2+1
              I3=0
             DO WHILE(SENDAGAIN(I2)%NEPID/=I-1)
                I3=I3+1
                TEMPO = SENDAGAIN(I2)
                SENDAGAIN(I2) = SENDAGAIN(I2+I3)
                SENDAGAIN(I2+I3) = TEMPO
              ENDDO
           ENDDO
        ENDDO
       END SUBROUTINE ORGANIZE_SENDAGAIN
!
       SUBROUTINE ORGANIZE_SENDAGAIN_4D
        USE BIEF
        USE TOMAWAC_MPI
        IMPLICIT NONE
        INTEGER :: I,I1,I2,I3
!
        I2 = 0
        DO I=1,NCSIZE
           DO I1=1,SENDCOUNTS_AGAIN(I)
              I2=I2+1
              I3=0
             DO WHILE(SENDAGAIN_4D(I2)%NEPID/=I-1)
                I3=I3+1
                TEMPO_4D = SENDAGAIN_4D(I2)
                SENDAGAIN_4D(I2) = SENDAGAIN_4D(I2+I3)
                SENDAGAIN_4D(I2+I3) = TEMPO_4D
              ENDDO
           ENDDO
        ENDDO
       END SUBROUTINE ORGANIZE_SENDAGAIN_4D
!
       SUBROUTINE ENVOI_AGAIN(NRECV)
!
! |      NRECV     |<-- | SUM DES CARACTERISTIQUUES RECU SUR CHAQUE PROC
!
          USE BIEF
          USE TOMAWAC_MPI
          IMPLICIT NONE
          INTEGER :: I,IER
          INTEGER , INTENT(INOUT) :: NRECV
!
!
          CALL P_MPI_ALLTOALL(SENDCOUNTS_AGAIN(:),1,MPI_INTEGER,
     &          RECVCOUNTS_AGAIN(:),1,MPI_INTEGER,
     &          MPI_COMM_WORLD,IER)
!
          SDISPLS_AGAIN(1) = 0 ! CONTIGUOUS DATA MARKER
          DO I=2,NCSIZE
             SDISPLS_AGAIN(I) = SDISPLS_AGAIN(I-1)+SENDCOUNTS_AGAIN(I-1)
          END DO
          RDISPLS_AGAIN(1) = 0 ! SAVES THE RECEIVED DATA CONTIGUOUSLY
          DO I=2,NCSIZE
            RDISPLS_AGAIN(I) = RDISPLS_AGAIN(I-1)+RECVCOUNTS_AGAIN(I-1)
          END DO
           NRECV = SUM(RECVCOUNTS_AGAIN(:))
!
         IF(.NOT.ALLOCATED(RECVAGAIN))
     &           ALLOCATE(RECVAGAIN(MAX(NRECV,1)))
          CALL P_MPI_ALLTOALLV_TOMA1
     &      (SENDAGAIN(:),SENDCOUNTS_AGAIN(:),SDISPLS_AGAIN(:),
     &       CHARACTERISTIC,
     &       RECVAGAIN(:),RECVCOUNTS_AGAIN(:),RDISPLS_AGAIN(:),
     &       CHARACTERISTIC,
     &       MPI_COMM_WORLD,IER)
!
           IF (.NOT.ASSOCIATED(SH_AGAIN%SHP1)) ALLOCATE(
     &      SH_AGAIN%SHP1(NRECV),SH_AGAIN%SHP2(NRECV),
     &      SH_AGAIN%SHP3(NRECV),SH_AGAIN%SHZ(NRECV),
     &      SH_AGAIN%ELT(NRECV),SH_AGAIN%ETA(NRECV))
!
           SH_AGAIN%SHP1 = 0.D0
           SH_AGAIN%SHP2 = 0.D0
           SH_AGAIN%SHP3 = 0.D0
           SH_AGAIN%SHZ = 0.D0
!
        END SUBROUTINE ENVOI_AGAIN
!
       SUBROUTINE ENVOI_AGAIN_4D(NRECV)
!
! |      NRECV     |<-- | SUM DES CARACTERISTIQUUES RECU SUR CHAQUE PROC
!
          USE BIEF
          USE TOMAWAC_MPI
          IMPLICIT NONE
          INTEGER :: I,IER
          INTEGER , INTENT(INOUT) :: NRECV
!
!
          CALL P_MPI_ALLTOALL(SENDCOUNTS_AGAIN(:),1,MPI_INTEGER,
     &          RECVCOUNTS_AGAIN(:),1,MPI_INTEGER,
     &          MPI_COMM_WORLD,IER)
!
          SDISPLS_AGAIN(1) = 0 ! CONTIGUOUS DATA MARKER
          DO I=2,NCSIZE
             SDISPLS_AGAIN(I) = SDISPLS_AGAIN(I-1)+SENDCOUNTS_AGAIN(I-1)
          END DO
          RDISPLS_AGAIN(1) = 0 ! SAVES THE RECEIVED DATA CONTIGUOUSLY
          DO I=2,NCSIZE
            RDISPLS_AGAIN(I) = RDISPLS_AGAIN(I-1)+RECVCOUNTS_AGAIN(I-1)
          END DO
           NRECV = SUM(RECVCOUNTS_AGAIN(:))
!
         IF(.NOT.ALLOCATED(RECVAGAIN_4D))
     &           ALLOCATE(RECVAGAIN_4D(MAX(NRECV,1)))
          CALL P_MPI_ALLTOALLV_TOMA2
     &      (SENDAGAIN_4D(:),SENDCOUNTS_AGAIN(:),SDISPLS_AGAIN(:),
     &       CHARACTER_4D,
     &       RECVAGAIN_4D(:),RECVCOUNTS_AGAIN(:),RDISPLS_AGAIN(:),
     &       CHARACTER_4D,
     &       MPI_COMM_WORLD,IER)
!
           IF (.NOT.ASSOCIATED(SH_AGAIN_4D%SHP1)) ALLOCATE(
     &      SH_AGAIN_4D%SHP1(NRECV),SH_AGAIN_4D%SHP2(NRECV),
     &      SH_AGAIN_4D%SHP3(NRECV),SH_AGAIN_4D%SHZ(NRECV),
     &      SH_AGAIN_4D%ELT(NRECV),SH_AGAIN_4D%ETA(NRECV),
     &      SH_AGAIN_4D%FRE(NRECV),SH_AGAIN_4D%SHF(NRECV))
!
           SH_AGAIN_4D%SHP1 = 0.D0
           SH_AGAIN_4D%SHP2 = 0.D0
           SH_AGAIN_4D%SHP3 = 0.D0
           SH_AGAIN_4D%SHZ = 0.D0
           SH_AGAIN_4D%SHF = 0.D0
!
        END SUBROUTINE ENVOI_AGAIN_4D
!
        SUBROUTINE SUPP_ENVOI_AGAIN(FREQ,NUMBER)
         USE BIEF
         USE TOMAWAC_MPI
         IMPLICIT NONE
         INTEGER :: I2,NUMBER,I
         INTEGER, INTENT(INOUT) :: FREQ
         DO I2=1,NCSIZE
            RECVCOUNTS(I2,FREQ) = COUNT((RECVCHAR(1:NARRV(FREQ),FREQ)
     &              %NEPID==-1)
     &             .AND.(RECVCHAR(1:NARRV(FREQ),FREQ)%MYPID==I2-1))
         ENDDO
         NUMBER = SUM(RECVCOUNTS(:,FREQ))
! THE EXCESS DATA ARE DELETED IN RECVCHAR (LIKE SHP1..
         DO I=1,NUMBER
            DO WHILE (RECVCHAR(I,FREQ)%NEPID.NE.-1)
              RECVCHAR(I:NARRV(FREQ)-1,FREQ) =
     &                                  RECVCHAR(I+1:NARRV(FREQ),FREQ)
              SH_LOC(FREQ)%SHP1(I:NARRV(FREQ)-1) =
     &                              SH_LOC(FREQ)%SHP1(I+1:NARRV(FREQ))
              SH_LOC(FREQ)%SHP2(I:NARRV(FREQ)-1) =
     &                              SH_LOC(FREQ)%SHP2(I+1:NARRV(FREQ))
              SH_LOC(FREQ)%SHP3(I:NARRV(FREQ)-1) =
     &                              SH_LOC(FREQ)%SHP3(I+1:NARRV(FREQ))
              SH_LOC(FREQ)%SHZ(I:NARRV(FREQ)-1) =
     &                              SH_LOC(FREQ)%SHZ(I+1:NARRV(FREQ))
              SH_LOC(FREQ)%ELT(I:NARRV(FREQ)-1) =
     &                              SH_LOC(FREQ)%ELT(I+1:NARRV(FREQ))
              SH_LOC(FREQ)%ETA(I:NARRV(FREQ)-1) =
     &                              SH_LOC(FREQ)%ETA(I+1:NARRV(FREQ))
              NARRV(FREQ) = NARRV(FREQ)-1
            ENDDO
         ENDDO
         NARRV(FREQ) = NUMBER
!
         END SUBROUTINE SUPP_ENVOI_AGAIN
!
        SUBROUTINE SUPP_ENVOI_AGAIN_4D(FREQ,NUMBER)
         USE BIEF
         USE TOMAWAC_MPI
         IMPLICIT NONE
         INTEGER :: I2,NUMBER,I
         INTEGER, INTENT(INOUT) :: FREQ
         DO I2=1,NCSIZE
            RECVCOUNTS(I2,FREQ) = COUNT((RECVCHAR_4D(1:NARRV(FREQ),FREQ)
     &              %NEPID==-1)
     &             .AND.(RECVCHAR_4D(1:NARRV(FREQ),FREQ)%MYPID==I2-1))
         ENDDO
         NUMBER = SUM(RECVCOUNTS(:,FREQ))
! THE EXCESS DATA ARE DELETED IN RECVCHAR (LIKE SHP1..
         DO I=1,NUMBER
            DO WHILE (RECVCHAR_4D(I,FREQ)%NEPID.NE.-1)
              RECVCHAR_4D(I:NARRV(FREQ)-1,FREQ) =
     &                                RECVCHAR_4D(I+1:NARRV(FREQ),FREQ)
              SH_LOC_4D(FREQ)%SHP1(I:NARRV(FREQ)-1) =
     &                            SH_LOC_4D(FREQ)%SHP1(I+1:NARRV(FREQ))
              SH_LOC_4D(FREQ)%SHP2(I:NARRV(FREQ)-1) =
     &                            SH_LOC_4D(FREQ)%SHP2(I+1:NARRV(FREQ))
              SH_LOC_4D(FREQ)%SHP3(I:NARRV(FREQ)-1) =
     &                            SH_LOC_4D(FREQ)%SHP3(I+1:NARRV(FREQ))
              SH_LOC_4D(FREQ)%SHZ(I:NARRV(FREQ)-1) =
     &                            SH_LOC_4D(FREQ)%SHZ(I+1:NARRV(FREQ))
              SH_LOC_4D(FREQ)%SHF(I:NARRV(FREQ)-1) =
     &                            SH_LOC_4D(FREQ)%SHF(I+1:NARRV(FREQ))
              SH_LOC_4D(FREQ)%ELT(I:NARRV(FREQ)-1) =
     &                            SH_LOC_4D(FREQ)%ELT(I+1:NARRV(FREQ))
              SH_LOC_4D(FREQ)%ETA(I:NARRV(FREQ)-1) =
     &                            SH_LOC_4D(FREQ)%ETA(I+1:NARRV(FREQ))
              SH_LOC_4D(FREQ)%FRE(I:NARRV(FREQ)-1) =
     &                            SH_LOC_4D(FREQ)%FRE(I+1:NARRV(FREQ))
!
              NARRV(FREQ) = NARRV(FREQ)-1
            ENDDO
         ENDDO
         NARRV(FREQ) = NUMBER
!
         END SUBROUTINE SUPP_ENVOI_AGAIN_4D
!
         SUBROUTINE INCREM_ENVOI_RECV(IFREQ2,NUMBER,NLOSTAGAIN,
     &              NUMBERLOST,NRECV)
         USE BIEF
         USE TOMAWAC_MPI
         IMPLICIT NONE
         INTEGER :: I,I2,I3
         INTEGER, INTENT(INOUT) :: IFREQ2,NUMBER,NLOSTAGAIN,NUMBERLOST,
     &                             NRECV
         INTEGER P_ISUM,P_IMAX
         EXTERNAL P_ISUM,P_IMAX
!
!
        DO I2=1,NCSIZE
           RECVCOUNTS(I2,IFREQ2) = COUNT(RECVCHAR(1:NUMBER,IFREQ2)
     &                                                %MYPID==I2-1)
        ENDDO
         RDISPLS(1,IFREQ2) = 0 ! SAVES THE RECEIVED DATA CONTIGUOUSLY
         DO I=2,NCSIZE
           RDISPLS(I,IFREQ2) = RDISPLS(I-1,IFREQ2)+
     &                                        RECVCOUNTS(I-1,IFREQ2)
         END DO
!
        NUMBER = SUM(RECVCOUNTS(:,IFREQ2))
        NLOSTAGAIN = 0
        SENDCOUNTS_AGAIN(:)=0
        DO I=1,NRECV
           IF (RECVAGAIN(I)%NEPID==-1) THEN
             DO I2=1,NCSIZE
               IF (RECVAGAIN(I)%MYPID == I2-1) THEN
                 DO I3 = NUMBER,RDISPLS(I2,IFREQ2)+1,-1
                    RECVCHAR(I3+1,IFREQ2) = RECVCHAR(I3,IFREQ2)
                    SH_LOC(IFREQ2)%SHP1(I3+1) = SH_LOC(IFREQ2)%SHP1(I3)
                    SH_LOC(IFREQ2)%SHP2(I3+1) = SH_LOC(IFREQ2)%SHP2(I3)
                    SH_LOC(IFREQ2)%SHP3(I3+1) = SH_LOC(IFREQ2)%SHP3(I3)
                    SH_LOC(IFREQ2)%SHZ(I3+1) = SH_LOC(IFREQ2)%SHZ(I3)
                    SH_LOC(IFREQ2)%ETA(I3+1) = SH_LOC(IFREQ2)%ETA(I3)
                    SH_LOC(IFREQ2)%ELT(I3+1) = SH_LOC(IFREQ2)%ELT(I3)
                 ENDDO
                 IF (I2/=NCSIZE) RDISPLS(I2+1:NCSIZE,IFREQ2) =
     &                           RDISPLS(I2+1:NCSIZE,IFREQ2)+1
                 NUMBER = NUMBER +1
                 RECVCHAR(I3+1,IFREQ2) = RECVAGAIN(I)
                 SH_LOC(IFREQ2)%SHP1(I3+1) = SH_AGAIN%SHP1(I)
                 SH_LOC(IFREQ2)%SHP2(I3+1) = SH_AGAIN%SHP2(I)
                 SH_LOC(IFREQ2)%SHP3(I3+1) = SH_AGAIN%SHP3(I)
                 SH_LOC(IFREQ2)%SHZ(I3+1) = SH_AGAIN%SHZ(I)
                 SH_LOC(IFREQ2)%ETA(I3+1) = SH_AGAIN%ETA(I)
                 SH_LOC(IFREQ2)%ELT(I3+1) = SH_AGAIN%ELT(I)
                 RECVCOUNTS(I2,IFREQ2) = RECVCOUNTS(I2,IFREQ2) + 1
               ENDIF
             ENDDO
           ELSE
             NLOSTAGAIN = NLOSTAGAIN +1
             SENDAGAIN(NLOSTAGAIN) = RECVAGAIN(I)
             SENDCOUNTS_AGAIN(RECVAGAIN(I)%NEPID+1)=
     &            SENDCOUNTS_AGAIN(RECVAGAIN(I)%NEPID+1)+1
           ENDIF
        ENDDO
        NUMBERLOST = P_ISUM(NLOSTAGAIN)
        DEALLOCATE(SH_AGAIN%SHP1,SH_AGAIN%SHP2,SH_AGAIN%SHP3,
     &             SH_AGAIN%SHZ,SH_AGAIN%ETA,SH_AGAIN%ELT)
        DEALLOCATE(RECVAGAIN)
!
       END SUBROUTINE INCREM_ENVOI_RECV
!
         SUBROUTINE INCREM_ENVOI_RECV_4D(IFREQ2,NUMBER,NLOSTAGAIN,
     &              NUMBERLOST,NRECV)
         USE BIEF
         USE TOMAWAC_MPI
         IMPLICIT NONE
         INTEGER :: I,I2,I3
         INTEGER, INTENT(INOUT) :: IFREQ2,NUMBER,NLOSTAGAIN,NUMBERLOST,
     &                             NRECV
         INTEGER P_ISUM,P_IMAX
         EXTERNAL P_ISUM,P_IMAX
!
!
        DO I2=1,NCSIZE
           RECVCOUNTS(I2,IFREQ2) = COUNT(RECVCHAR_4D(1:NUMBER,IFREQ2)
     &                                                %MYPID==I2-1)
        ENDDO
         RDISPLS(1,IFREQ2) = 0 ! SAVES THE RECEIVED DATA CONTIGUOUSLY
         DO I=2,NCSIZE
           RDISPLS(I,IFREQ2) = RDISPLS(I-1,IFREQ2)+
     &                                        RECVCOUNTS(I-1,IFREQ2)
         END DO
!
        NUMBER = SUM(RECVCOUNTS(:,IFREQ2))
        NLOSTAGAIN = 0
        SENDCOUNTS_AGAIN(:)=0
        DO I=1,NRECV
           IF (RECVAGAIN_4D(I)%NEPID==-1) THEN
             DO I2=1,NCSIZE
               IF (RECVAGAIN_4D(I)%MYPID == I2-1) THEN
                 DO I3 = NUMBER,RDISPLS(I2,IFREQ2)+1,-1
                   RECVCHAR_4D(I3+1,IFREQ2) = RECVCHAR_4D(I3,IFREQ2)
                   SH_LOC_4D(IFREQ2)%SHP1(I3+1) =
     &                                      SH_LOC_4D(IFREQ2)%SHP1(I3)
                   SH_LOC_4D(IFREQ2)%SHP2(I3+1) =
     &                                      SH_LOC_4D(IFREQ2)%SHP2(I3)
                   SH_LOC_4D(IFREQ2)%SHP3(I3+1) =
     &                                      SH_LOC_4D(IFREQ2)%SHP3(I3)
                   SH_LOC_4D(IFREQ2)%SHZ(I3+1) =
     &                                      SH_LOC_4D(IFREQ2)%SHZ(I3)
                   SH_LOC_4D(IFREQ2)%SHF(I3+1) =
     &                                      SH_LOC_4D(IFREQ2)%SHF(I3)
                   SH_LOC_4D(IFREQ2)%ETA(I3+1) =
     &                                      SH_LOC_4D(IFREQ2)%ETA(I3)
                   SH_LOC_4D(IFREQ2)%ELT(I3+1) =
     &                                      SH_LOC_4D(IFREQ2)%ELT(I3)
                   SH_LOC_4D(IFREQ2)%FRE(I3+1) =
     &                                      SH_LOC_4D(IFREQ2)%FRE(I3)
                 ENDDO
                 IF (I2/=NCSIZE) RDISPLS(I2+1:NCSIZE,IFREQ2) =
     &                           RDISPLS(I2+1:NCSIZE,IFREQ2)+1
                 NUMBER = NUMBER +1
                 RECVCHAR_4D(I3+1,IFREQ2) = RECVAGAIN_4D(I)
                 SH_LOC_4D(IFREQ2)%SHP1(I3+1) = SH_AGAIN_4D%SHP1(I)
                 SH_LOC_4D(IFREQ2)%SHP2(I3+1) = SH_AGAIN_4D%SHP2(I)
                 SH_LOC_4D(IFREQ2)%SHP3(I3+1) = SH_AGAIN_4D%SHP3(I)
                 SH_LOC_4D(IFREQ2)%SHZ(I3+1) = SH_AGAIN_4D%SHZ(I)
                 SH_LOC_4D(IFREQ2)%SHF(I3+1) = SH_AGAIN_4D%SHF(I)
                 SH_LOC_4D(IFREQ2)%ETA(I3+1) = SH_AGAIN_4D%ETA(I)
                 SH_LOC_4D(IFREQ2)%ELT(I3+1) = SH_AGAIN_4D%ELT(I)
                 SH_LOC_4D(IFREQ2)%FRE(I3+1) = SH_AGAIN_4D%FRE(I)
                 RECVCOUNTS(I2,IFREQ2) = RECVCOUNTS(I2,IFREQ2) + 1
               ENDIF
             ENDDO
           ELSE
             NLOSTAGAIN = NLOSTAGAIN +1
             SENDAGAIN_4D(NLOSTAGAIN) = RECVAGAIN_4D(I)
             SENDCOUNTS_AGAIN(RECVAGAIN_4D(I)%NEPID+1)=
     &            SENDCOUNTS_AGAIN(RECVAGAIN_4D(I)%NEPID+1)+1
           ENDIF
        ENDDO
        NUMBERLOST = P_ISUM(NLOSTAGAIN)
        DEALLOCATE(SH_AGAIN_4D%SHP1,SH_AGAIN_4D%SHP2,SH_AGAIN_4D%SHP3,
     &             SH_AGAIN_4D%SHZ,SH_AGAIN_4D%ETA,SH_AGAIN_4D%ELT,
     &             SH_AGAIN_4D%SHF,SH_AGAIN_4D%FRE)
        DEALLOCATE(RECVAGAIN_4D)
!
       END SUBROUTINE INCREM_ENVOI_RECV_4D
!
!
       SUBROUTINE FINAL_ORGA_RECV(NARRIV,FREQ)
        USE BIEF
        USE TOMAWAC_MPI
        IMPLICIT NONE
        INTEGER, INTENT(INOUT) :: NARRIV,FREQ
        INTEGER :: I,IER
!
         NARRIV = SUM(RECVCOUNTS(:,FREQ))
         DO I=1,NCSIZE
            RECVCOUNTS(I,FREQ) = COUNT(RECVCHAR(1:NARRIV,FREQ)
     &                          %MYPID==I-1)
         ENDDO
!
         RDISPLS(1,FREQ) = 0 ! SAVES THE RECEIVED DATA CONTIGUOUSLY
         DO I=2,NCSIZE
           RDISPLS(I,FREQ) = RDISPLS(I-1,FREQ)+RECVCOUNTS(I-1,FREQ)
         END DO
!
         CALL P_MPI_ALLTOALL(RECVCOUNTS(:,FREQ),1,MPI_INTEGER,
     &          SENDCOUNTS(:,FREQ),1,MPI_INTEGER,
     &          MPI_COMM_WORLD,IER)
          SDISPLS(1,FREQ) = 0 ! CONTIGUOUS DATA MARKER
          DO I=2,NCSIZE
            SDISPLS(I,FREQ) = SDISPLS(I-1,FREQ)+SENDCOUNTS(I-1,FREQ)
          END DO
          DEALLOCATE(SENDAGAIN)
!
         END SUBROUTINE FINAL_ORGA_RECV
!
       SUBROUTINE FINAL_ORGA_RECV_4D(NARRIV,FREQ)
        USE BIEF
        USE TOMAWAC_MPI
        IMPLICIT NONE
        INTEGER, INTENT(INOUT) :: NARRIV,FREQ
        INTEGER :: I,IER
!
         NARRIV = SUM(RECVCOUNTS(:,FREQ))
         DO I=1,NCSIZE
            RECVCOUNTS(I,FREQ) = COUNT(RECVCHAR_4D(1:NARRIV,FREQ)
     &                          %MYPID==I-1)
         ENDDO
!
         RDISPLS(1,FREQ) = 0 ! SAVES THE RECEIVED DATA CONTIGUOUSLY
         DO I=2,NCSIZE
           RDISPLS(I,FREQ) = RDISPLS(I-1,FREQ)+RECVCOUNTS(I-1,FREQ)
         END DO
!
         CALL P_MPI_ALLTOALL(RECVCOUNTS(:,FREQ),1,MPI_INTEGER,
     &          SENDCOUNTS(:,FREQ),1,MPI_INTEGER,
     &          MPI_COMM_WORLD,IER)
          SDISPLS(1,FREQ) = 0 ! CONTIGUOUS DATA MARKER
          DO I=2,NCSIZE
            SDISPLS(I,FREQ) = SDISPLS(I-1,FREQ)+SENDCOUNTS(I-1,FREQ)
          END DO
          DEALLOCATE(SENDAGAIN_4D)
!
         END SUBROUTINE FINAL_ORGA_RECV_4D
!
         SUBROUTINE RESET_COUNT(FREQ)
          USE BIEF
          USE TOMAWAC_MPI
          IMPLICIT NONE
          INTEGER :: FREQ
             RECVCOUNTS(:,FREQ) = 0
             SENDCOUNTS(:,FREQ) = 0
         END SUBROUTINE RESET_COUNT
!
         SUBROUTINE SPECTRE_SEND(SPE_SEND,NSPE_RECV,NLEO,ISLEO,
     &                           NRECV_LEO)
          USE BIEF
          USE TOMAWAC_MPI
          IMPLICIT NONE
          INTEGER, DIMENSION(NCSIZE) :: NSPE_RECV,NSPE_SEND
          INTEGER, DIMENSION(NCSIZE) :: S_DISP,R_DISP
          INTEGER :: NLEO,IDLEO
          LOGICAL, DIMENSION(NLEO) :: ISLEO
          INTEGER, DIMENSION(NLEO) :: NRECV_LEO,NSEND_LEO,NPID_SEND,
     &                                NPID_RECV,NRECV_LEO2,NBLEO
          INTEGER :: IER,SPE_SEND,I,II
          INTEGER P_ISUM,P_IMAX
          EXTERNAL P_ISUM,P_IMAX
          NSPE_RECV = 0
          NSPE_SEND = 0
          NSEND_LEO = 0
          NRECV_LEO = 0
          NRECV_LEO2 = 0
          S_DISP = 0
          R_DISP = 0
          NBLEO = 0
          DO I=1,NLEO
             IDLEO = -1
             IF (ISLEO(I)) NBLEO(I) = NBLEO(I) + 1
             NBLEO(I) = P_ISUM(NBLEO(I))
             IF (NBLEO(I).GT.1) THEN
                IF (ISLEO(I)) IDLEO = IPID
                IDLEO = P_IMAX(IDLEO)
                IF (IDLEO/=IPID) THEN
                   IF (ISLEO(I)) THEN
                   ISLEO(I) = .FALSE.
                   SPE_SEND = SPE_SEND-1
                   ENDIF
                ENDIF
             ENDIF
          ENDDO
!
          NSPE_SEND(1) = SPE_SEND
          CALL P_MPI_ALLTOALL(NSPE_SEND(:),1,MPI_INTEGER,
     &          NSPE_RECV(:),1,MPI_INTEGER,
     &          MPI_COMM_WORLD,IER)
!
          II = 0
          DO I=1,NLEO
            IF (ISLEO(I)) THEN
              II=II+1
              NSEND_LEO(II) = I
            ENDIF
          ENDDO
          II = 0
          DO I=1,NLEO
            IF (ISLEO(I)) THEN
              II=II+1
              NPID_SEND(II) = IPID
            ENDIF
          ENDDO
          DO I=2,NCSIZE
           S_DISP(I) = S_DISP(I-1)+NSPE_SEND(I-1)
           R_DISP(I) = R_DISP(I-1)+NSPE_RECV(I-1)
          ENDDO
          CALL P_MPI_ALLTOALLV
     &      (NSEND_LEO,NSPE_SEND,S_DISP,
     &       MPI_INTEGER,
     &       NRECV_LEO,NSPE_RECV,R_DISP,
     &       MPI_INTEGER,
     &       MPI_COMM_WORLD,IER)
          CALL P_MPI_ALLTOALLV
     &      (NPID_SEND,NSPE_SEND,S_DISP,
     &       MPI_INTEGER,
     &       NPID_RECV,NSPE_RECV,R_DISP,
     &       MPI_INTEGER,
     &       MPI_COMM_WORLD,IER)
          IF (IPID==0) THEN
             DO I=1,NLEO
              IF (NRECV_LEO(I).NE.0) NRECV_LEO2(NRECV_LEO(I)) =
     &                                                NPID_RECV(I)
             ENDDO
             NRECV_LEO = NRECV_LEO2
          ENDIF
          END SUBROUTINE SPECTRE_SEND
!
          SUBROUTINE BVARSOR_SENDRECV(BVARSOR,NLEO,NPOIN,ISLEO,
     &                                            NRECV_LEO)
          USE BIEF
          USE TOMAWAC_MPI
          IMPLICIT NONE
          TYPE(BIEF_OBJ),INTENT(INOUT) :: BVARSOR
          INTEGER :: NLEO,SPE_SEND,NPOIN
          INTEGER , DIMENSION(NLEO) :: NRECV_LEO
          LOGICAL , DIMENSION(NLEO) :: ISLEO
          INTEGER :: I
          INTEGER TAG,REQ(NLEO),REQ2
          DATA TAG/5001/
          DO I=1,NLEO
             IF ((ISLEO(I)).AND.IPID/=0) CALL P_IWRIT(
     &                 BVARSOR%ADR(I)%P%R,NPOIN*8,0,TAG,REQ2)
             IF ((IPID==0).AND.NRECV_LEO(I)/=0) CALL P_IREAD(
     &             BVARSOR%ADR(I)%P%R,NPOIN*8,NRECV_LEO(I),TAG,REQ(I))
             IF ((IPID==0).AND.NRECV_LEO(I)/=0) CALL P_WAIT_PARACO
     &                                                      (REQ(I),1)
          ENDDO
          END SUBROUTINE BVARSOR_SENDRECV
!
          SUBROUTINE TEXTE_SENDRECV(TEXT,NLEO,NPOIN,ISLEO,
     &                                            NRECV_LEO)
          USE BIEF
          USE TOMAWAC_MPI
          IMPLICIT NONE
          INTEGER :: NLEO,SPE_SEND,NPOIN,I
          INTEGER NRECV_LEO(NLEO)
          LOGICAL ISLEO(NLEO)
          INTEGER TAG,REQ(NLEO),REQ2
          CHARACTER(LEN=32)  ,INTENT(INOUT):: TEXT(NLEO)
          DATA TAG/5002/
          DO I=1,NLEO
            IF((ISLEO(I)).AND.IPID/=0) THEN
              CALL P_IWRIT_C(TEXT(I),32,0,TAG,REQ2)
            ENDIF
            IF((IPID==0).AND.NRECV_LEO(I)/=0) THEN
              CALL P_IREAD_C(TEXT(I),32,NRECV_LEO(I),TAG,REQ(I))
            ENDIF
            IF((IPID==0).AND.NRECV_LEO(I)/=0) THEN
              CALL P_WAIT_PARACO(REQ(I),1)
            ENDIF
          ENDDO
          END SUBROUTINE TEXTE_SENDRECV
      END MODULE TOMAWAC_MPI_TOOLS