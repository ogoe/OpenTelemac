!                    *******************
                     SUBROUTINE PIED_MPI
!                    *******************
!
     &(CX,CY,CT,DT,NRK,X,Y,TETA,IKLE2,IFABOR,ETAP1,
     & TRA01,SHP1,SHP2,SHP3,SHZ,JF,ELT,ETA,ITR01,
     & GOODELT,NPLAN,NPOIN2,NPOIN3,NF,NELEM2,MESH)
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CT             |---|
!| CX             |---|
!| CY             |---|
!| DT             |---|
!| ELT            |---|
!| ETA            |---|
!| ETAP1          |---|
!| GOODELT        |---|
!| IFABOR         |---|
!| IKLE2          |---|
!| ITR01          |---|
!| JF             |---|
!| MESH           |---|
!| NELEM2         |---|
!| NF             |---|
!| NPLAN          |---|
!| NPOIN2         |---|
!| NPOIN3         |---|
!| NRK            |---|
!| SHP1           |---|
!| SHP2           |---|
!| SHP3           |---|
!| SHZ            |---|
!| TETA           |---|
!| TRA01          |---|
!| X              |---|
!| Y              |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE TOMAWAC_MPI_TOOLS
      USE TOMAWAC_MPI, ONLY : SH_AGAIN,RECVAGAIN,SH_LOC,RECVCHAR,
     &                        NARRV,NCHARA,NLOSTCHAR,NSEND,TEST,
     &                        NCHDIM,NFREQ,IFREQ,ISPDONE,INIT_TOMAWAC,
     &                        PIEDS_TOMAWAC,PIEDS_TOMAWAC_MPI,
     &                        WIPE_HEAPED_CHAR,PREP_INITIAL_SEND,
     &                        GLOB_CHAR_COMM
      IMPLICIT NONE
      DOUBLE PRECISION CX(NPOIN3,NF) , CY(NPOIN3,NF)
      DOUBLE PRECISION CT(NPOIN3,NF)
      DOUBLE PRECISION SHP1(NPOIN3,NF) , SHP2(NPOIN3,NF)
      DOUBLE PRECISION SHP3(NPOIN3,NF) , SHZ(NPOIN3,NF)
      DOUBLE PRECISION SHF(NPOIN3,NF)
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION TETA(NPLAN)
      DOUBLE PRECISION SURDET(NELEM2)
      DOUBLE PRECISION DT,TRA01(NPOIN3,8),PROMIN
      INTEGER ELT(NPOIN3,NF),ETA(NPOIN3,NF)
      INTEGER IKLE2(NELEM2,3),IFABOR(NELEM2,7),ETAP1(NPLAN)
      INTEGER ITR01(NPOIN3,3),JF
      INTEGER NPLAN,NPOIN2,NPOIN3,NF,NELEM2,NRK
      INTEGER LAST_NOMB,NLOSTAGAIN,NUMBER,IER,NRECV,NUMBERLOST
      INTEGER ITE,IP,IPLAN,NBB,IPOIN,GOODELT(NPOIN2,NPLAN)
      INTEGER NARRSUM
      INTEGER P_ISUM,P_IMAX
      EXTERNAL P_ISUM,P_IMAX
      DOUBLE PRECISION :: TEST2(NPOIN3,NF)
!      DOUBLE PRECISION :: TES(NPOIN2,NPLAN)
      TYPE(BIEF_MESH)  ::  MESH
!
         CALL CORRECT_GOODELT(GOODELT,NPOIN2,NPLAN,MESH)
!
         IF (.NOT.ALLOCATED(NCHARA)) ALLOCATE(NCHARA(NF),NLOSTCHAR(NF),
     &                                        NSEND(NF))
         CALL INIT_TOMAWAC(NCHARA(JF),NCHDIM,1,
     &                                       NPOIN3,LAST_NOMB)
!
         IF(.NOT.ALLOCATED(TEST)) ALLOCATE(TEST(NPOIN3,NF))
         IFREQ=JF
           CALL PIEDS_TOMAWAC
     &(CX,CY,CT,DT,NRK,X,Y,TETA,IKLE2,IFABOR,
     &  ETAP1,TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),SHP1(1,JF),
     &  SHP2(1,JF),SHP3(1,JF),SHZ(1,JF),ELT(1,JF),ETA(1,JF),
     &  ITR01(1,1),NPOIN3,NPOIN2,
     &  NELEM2,NPLAN,JF,SURDET,-1,ITR01(1,2),MESH%IFAPAR%I,TEST(1,JF),
     &  NCHDIM,NCHARA(JF),MESH,GOODELT)
! CHECKS WHETHER A CHARACTERISTICS CLOSE TO THE BOUNDARY EXITS AND NOT
! THE OTHER ONE. IN THIS CASE ONLY THE MAXIMUM CONTRIBUTION (FROM BOTH)
! IS CONSIDERED AND THE EXIT CHARACTERISTICS IS NOT TREATED
!          DO IP = 1,NPOIN2
!              DO IPLAN = 1,NPLAN
!                 TES(IP,IPLAN)  =TEST(IP+NPOIN2*(IPLAN-1),JF)
!              ENDDO
!          ENDDO
         WHERE (TEST(:,JF).LT.0.5D0)
             SHP1(:,JF)=0.D0
             SHP2(:,JF)=0.D0
             SHP3(:,JF)=0.D0
             SHZ(:,JF) = 0.D0
         END WHERE
!          DO IPLAN = 1,NPLAN
!          CALL PARCOM2
!      * ( TES(1,IPLAN) ,
!      *   TES(1,IPLAN) ,
!      *   TES(1,IPLAN) ,
!      *   NPOIN2 , 1 , 2 , 1 , MESH )
!          ENDDO
!          DO IP = 1,NPOIN2
!             DO IPLAN = 1,NPLAN
!                TEST(IP+NPOIN2*(IPLAN-1),JF)=TES(IP,IPLAN)
!             ENDDO
!          ENDDO
!          WHERE (TEST(:,JF).GT.1.5D0)
!             SHP1(:,JF)=SHP1(:,JF)/TEST(:,JF)
!             SHP2(:,JF)=SHP2(:,JF)/TEST(:,JF)
!             SHP3(:,JF)=SHP3(:,JF)/TEST(:,JF)
!          END WHERE
! HEAPCHAR(NCHARA,NFREQ) AND HEAPCOUNT(NCSIZE,NFREQ)
! HEAPCOUNT=> NUMBER OF CHARACTERISTICS ON EACH PROCESSOR
         CALL WIPE_HEAPED_CHAR(TEST(1,JF),NPOIN3,.TRUE.,NSEND(JF),
     &                        NLOSTCHAR(JF),NCHDIM,
     &                        NCHARA(JF))
! IS NOT NECESSARILY USEFUL, CHECKS IF TEST==1, IN WHICH CASE IT IS DELETED
! FROM THE LIST OF CHARACTERISTICS BY ASSIGNING HEAPCAHR%NEPID==-1
!        DO WHILE(P_IMAX(NLOSTCHAR(JF))>0)! THERE ARE -REALLY- LOST TRACEBACKS SOMEWHERE
          CALL PREP_INITIAL_SEND(NSEND,NLOSTCHAR,NCHARA)
! CREATES THE ARRAY 'SDISP' AND ORDERS THE DATA (ASCENDING)
          CALL GLOB_CHAR_COMM ()
! SENDS SENDCHAR AND WRITES TO RECVCHAR
!
!
         IF(.NOT.ALLOCATED(ISPDONE)) ALLOCATE(ISPDONE(NPOIN3,NF))
         IF(.NOT.ALLOCATED(NARRV)) ALLOCATE(NARRV(NF))
         CALL ALLOC_LOCAL(NARRV(IFREQ),IFREQ,NF,NLOSTAGAIN,
     &                      NUMBERLOST,NARRSUM)
       TEST2(:,JF) = 1.D0
         IF (NUMBERLOST>0) THEN
       CALL PIEDS_TOMAWAC_MPI
     &(CX,CY,CT,DT,NRK,X,Y,TETA,IKLE2,IFABOR,ETAP1,
     &  TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),SH_LOC(JF)%SHP1,
     &  SH_LOC(JF)%SHP2,SH_LOC(JF)%SHP3,SH_LOC(JF)%SHZ,
     &  SH_LOC(JF)%ELT,SH_LOC(JF)%ETA,
     &  NARRV(JF),NPOIN2,
     &  NELEM2,NPLAN,JF,SURDET,-1,MESH%IFAPAR%I,
     &  2,NCHARA(JF),RECVCHAR(1,JF))
        CALL ALLOC_AGAIN(NARRV(IFREQ),IFREQ,NLOSTAGAIN,NUMBERLOST,
     &                   NUMBER)
        CALL ORGANIZE_SENDAGAIN()
        CALL SUPP_ENVOI_AGAIN(IFREQ,NUMBER)
!
           ITE = 0
          DO WHILE((NUMBERLOST>0).AND.(ITE.LE.20))
           ITE= ITE + 1
          CALL ORGANIZE_SENDAGAIN()
          CALL ENVOI_AGAIN(NRECV)
          TEST2(:,JF)=1.D0
          CALL PIEDS_TOMAWAC_MPI
     &(CX,CY,CT,DT,NRK,X,Y,TETA,IKLE2,IFABOR,
     &  ETAP1,TRA01,TRA01(1,2),
     &  TRA01(1,3),TRA01(1,4),TRA01(1,5),TRA01(1,6),SH_AGAIN%SHP1,
     &  SH_AGAIN%SHP2,SH_AGAIN%SHP3,SH_AGAIN%SHZ,
     &  SH_AGAIN%ELT,SH_AGAIN%ETA,
     &  NRECV,NPOIN2,
     &  NELEM2,NPLAN,JF,SURDET,-1,MESH%IFAPAR%I,
     &  2,NCHARA(JF),RECVAGAIN)
        CALL INCREM_ENVOI_RECV(IFREQ,NUMBER,NLOSTAGAIN,NUMBERLOST,
     &                         NRECV)
        ENDDO ! END OF THE DOWHILE LOOP
         CALL FINAL_ORGA_RECV(NARRV(IFREQ),IFREQ)
          ELSE
           CALL RESET_COUNT(IFREQ)
          ENDIF
      RETURN
      END 
