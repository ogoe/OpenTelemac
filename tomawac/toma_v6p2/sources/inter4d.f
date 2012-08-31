!                    ******************
                     SUBROUTINE INTER4D
!                    ******************
!
     & ( F , B , SHP1 , SHP2 , SHP3 , SHZ , SHF , ELT , ETA , FRE ,
     &   IKLE2 , ETAP1, NPOIN2 , NELEM2 , NPLAN , NF , TRA01)
!
!***********************************************************************
! TOMAWAC   V6P1                                   17/06/2011
!***********************************************************************
!
!brief    INTERPOLATES AT THE FOOT OF THE CHARACTERISTICS.
!
!history  F MARCOS (LNH)
!+        01/02/95
!+        V1P0
!+
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
!history  G.MATTAROLO (EDF - LNHE)
!+        17/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| B              |-->| JACOBIAN TO TRANSFORM N(KX,KY) INTO F(FR,TETA)
!| ELT            |-->| NUMBERS OF THE ELEMENTS 2D OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETA            |-->| NUMBERS OF THE LAYERS OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETAP1          |-->| HIGHER LAYERS TABLE
!| F              |<->| WAVE ACTION DENSITY DIRECTIONAL SPECTRUM
!| FRE            |-->| NUMBER OF THE FREQUENCIES OF THE
!|                |   | POINTS TO BE ADVECTED
!| IKLE2          |-->| TRANSITION BETWEEN LOCAL AND GLOBAL NUMBERING
!|                |   | OF THE 2D MESH
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| SHF            |-->| BARYCENTRIC COORDINATES ALONG F OF THE 
!|                |   | NODES IN THEIR ASSOCIATED FREQUENCIES "FRE"
!| SHP1,SHP2,SHP3 |-->| BARYCENTRIC COORDINATES OF THE NODES IN
!|                |   | THEIR ASSOCIATED 2D ELEMENT "ELT"
!| SHZ            |-->| BARYCENTRIC COORDINATES ALONG TETA OF THE 
!|                |   | NODES IN THEIR ASSOCIATED LAYER "ETA"
!| TRA01          |<->| WORK TABLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH
      USE TOMAWAC_MPI
      USE BIEF
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
      IMPLICIT NONE
!
      INTEGER NELEM2,NPOIN2,NPLAN,NF,IP,IFF,IFR,IPLAN
      INTEGER ETAG,ETAGP1,IPOIN1,IPOIN2,IPOIN3
!
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF),TRA01(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION SHP1(NPOIN2,NPLAN,NF),SHP2(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION B(NPOIN2,NF),SHP3(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION SHZ(NPOIN2,NPLAN,NF),UMSHZ
      DOUBLE PRECISION SHF(NPOIN2,NPLAN,NF),UMSHF
!
      INTEGER IKLE2(NELEM2,3),ELT(NPOIN2,NPLAN,NF),ETA(NPOIN2,NPLAN,NF)
      INTEGER ETAP1(NPLAN),FRE(NPOIN2,NPLAN,NF)
!BD_INCKA MODIFICATION FOR PARALLEL MODE
      INTEGER ELT2,ETA2,FRE2,NNSEND,NNRECV,IP2
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
!
!-----------------------------------------------------------------------
!
!
       DO 1 IFF=1,NF
          DO 2 IPLAN=1,NPLAN
           DO 3 IP=1,NPOIN2
      	TRA01(IP,IPLAN,IFF)=F(IP,IPLAN,IFF)*B(IP,IFF)
3            CONTINUE
2         CONTINUE
1      CONTINUE
!
      DO 10 IFF=1,NF
        DO 20 IPLAN = 1 , NPLAN
          DO 30 IP = 1 , NPOIN2
         ETAG=ETA(IP,IPLAN,IFF)
         ETAGP1=ETAP1(ETAG)
         IPOIN1=IKLE2(ELT(IP,IPLAN,IFF),1)
         IPOIN2=IKLE2(ELT(IP,IPLAN,IFF),2)
         IPOIN3=IKLE2(ELT(IP,IPLAN,IFF),3)
         IFR=FRE(IP,IPLAN,IFF)
         UMSHZ=1.D0-SHZ(IP,IPLAN,IFF)
         UMSHF=1.D0-SHF(IP,IPLAN,IFF)
           F(IP,IPLAN,IFF) = ( UMSHF *
     &      ((TRA01(IPOIN1,ETAG,IFR) * SHP1(IP,IPLAN,IFF)
     &      + TRA01(IPOIN2,ETAG,IFR) * SHP2(IP,IPLAN,IFF)
     &      + TRA01(IPOIN3,ETAG,IFR) * SHP3(IP,IPLAN,IFF)) * UMSHZ
     &     +( TRA01(IPOIN1,ETAGP1,IFR) * SHP1(IP,IPLAN,IFF)
     &      + TRA01(IPOIN2,ETAGP1,IFR) * SHP2(IP,IPLAN,IFF)
     &      + TRA01(IPOIN3,ETAGP1,IFR) * SHP3(IP,IPLAN,IFF))
     &        * SHZ(IP,IPLAN,IFF) ) + SHF(IP,IPLAN,IFF) *
     &      ((TRA01(IPOIN1,ETAG,IFR+1) * SHP1(IP,IPLAN,IFF)
     &      + TRA01(IPOIN2,ETAG,IFR+1) * SHP2(IP,IPLAN,IFF)
     &      + TRA01(IPOIN3,ETAG,IFR+1) * SHP3(IP,IPLAN,IFF)) * UMSHZ
     &     +( TRA01(IPOIN1,ETAGP1,IFR+1) * SHP1(IP,IPLAN,IFF)
     &      + TRA01(IPOIN2,ETAGP1,IFR+1) * SHP2(IP,IPLAN,IFF)
     &      + TRA01(IPOIN3,ETAGP1,IFR+1) * SHP3(IP,IPLAN,IFF))
     &            * SHZ(IP,IPLAN,IFF)))  /B(IP,IFF)
         IF (F(IP,IPLAN,IFF).LT.0.D0) F(IP,IPLAN,IFF)=0.D0
30        CONTINUE
20      CONTINUE
10    CONTINUE
!
       IF (NCSIZE.GT.1) THEN
!
       DO IFF = 1,NF
       IF (.NOT.ALLOCATED(F_SEND_4D)) ALLOCATE(F_SEND_4D(SUM(
     &                                      RECVCOUNTS(:,IFF))))
       IF (.NOT.ALLOCATED(F_RECV_4D)) ALLOCATE(F_RECV_4D(SUM(
     &                                      SENDCOUNTS(:,IFF))))
       NNSEND = SUM(SENDCOUNTS(:,IFF))
       NNRECV = SUM(RECVCOUNTS(:,IFF))
       IFREQ = IFF
!
        DO IP2 = 1,NNRECV
           ELT2 = SH_LOC_4D(IFREQ)%ELT(IP2)
           ETA2 = SH_LOC_4D(IFREQ)%ETA(IP2)
           FRE2 = SH_LOC_4D(IFREQ)%FRE(IP2)
           UMSHZ = 1.D0-SH_LOC_4D(IFREQ)%SHZ(IP2)
           UMSHF = 1.D0-SH_LOC_4D(IFREQ)%SHF(IP2)
           F_SEND_4D(IP2)%IOR = RECVCHAR_4D(IP2,IFREQ)%IOR
           F_SEND_4D(IP2)%BP = ( UMSHF *
     &    ((TRA01(IKLE2(ELT2,1),ETA2,FRE2) * SH_LOC_4D(IFREQ)%SHP1(IP2)
     &    + TRA01(IKLE2(ELT2,2),ETA2,FRE2) * SH_LOC_4D(IFREQ)%SHP2(IP2)
     &+TRA01(IKLE2(ELT2,3),ETA2,FRE2)*SH_LOC_4D(IFREQ)%SHP3(IP2))*UMSHZ
     &+(TRA01(IKLE2(ELT2,1),ETAP1(ETA2),FRE2)*SH_LOC_4D(IFREQ)%SHP1(IP2)
     & + TRA01(IKLE2(ELT2,2),ETAP1(ETA2),FRE2)
     &       * SH_LOC_4D(IFREQ)%SHP2(IP2)
     & + TRA01(IKLE2(ELT2,3),ETAP1(ETA2),FRE2)
     & * SH_LOC_4D(IFREQ)%SHP3(IP2))
     & * SH_LOC_4D(IFREQ)%SHZ(IP2)) + SH_LOC_4D(IFREQ)%SHF(IP2) *
     & ((TRA01(IKLE2(ELT2,1),ETA2,FRE2+1)
     & * SH_LOC_4D(IFREQ)%SHP1(IP2)+TRA01(IKLE2(ELT2,2),ETA2,FRE2+1)
     & * SH_LOC_4D(IFREQ)%SHP2(IP2)+TRA01(IKLE2(ELT2,3),ETA2,FRE2+1)
     &        * SH_LOC_4D(IFREQ)%SHP3(IP2)) * UMSHZ
     &     +( TRA01(IKLE2(ELT2,1),ETAP1(ETA2),FRE2+1)
     &       * SH_LOC_4D(IFREQ)%SHP1(IP2)
     & + TRA01(IKLE2(ELT2,2),ETAP1(ETA2),FRE2+1)
     &       * SH_LOC_4D(IFREQ)%SHP2(IP2)
     &      + TRA01(IKLE2(ELT2,3),ETAP1(ETA2),FRE2+1)
     & * SH_LOC_4D(IFREQ)%SHP3(IP2))*SH_LOC_4D(IFREQ)%SHZ(IP2)))
            F_SEND_4D(IP2)%F(1) = TRA01(IKLE2(ELT2,1),ETA2,FRE2)
            F_SEND_4D(IP2)%F(2) = TRA01(IKLE2(ELT2,2),ETA2,FRE2)
            F_SEND_4D(IP2)%F(3) = TRA01(IKLE2(ELT2,3),ETA2,FRE2)
            F_SEND_4D(IP2)%F(4) = TRA01(IKLE2(ELT2,1),ETAP1(ETA2),FRE2)
            F_SEND_4D(IP2)%F(5) = TRA01(IKLE2(ELT2,2),ETAP1(ETA2),FRE2)
            F_SEND_4D(IP2)%F(6) = TRA01(IKLE2(ELT2,3),ETAP1(ETA2),FRE2)
            F_SEND_4D(IP2)%F(7) = TRA01(IKLE2(ELT2,1),ETA2,FRE2+1)
            F_SEND_4D(IP2)%F(8) = TRA01(IKLE2(ELT2,2),ETA2,FRE2+1)
            F_SEND_4D(IP2)%F(9) = TRA01(IKLE2(ELT2,3),ETA2,FRE2+1)
            F_SEND_4D(IP2)%F(10)=TRA01(IKLE2(ELT2,1),ETAP1(ETA2),FRE2+1)
            F_SEND_4D(IP2)%F(11)=TRA01(IKLE2(ELT2,2),ETAP1(ETA2),FRE2+1)
            F_SEND_4D(IP2)%F(12)=TRA01(IKLE2(ELT2,3),ETAP1(ETA2),FRE2+1)
            F_SEND_4D(IP2)%MYPID = RECVCHAR_4D(IP2,IFREQ)%MYPID
            F_SEND_4D(IP2)%NEPID = RECVCHAR_4D(IP2,IFREQ)%NEPID
            F_SEND_4D(IP2)%SHP1  = SH_LOC_4D(IFREQ)%SHP1(IP2)
            F_SEND_4D(IP2)%SHP2  = SH_LOC_4D(IFREQ)%SHP2(IP2)
            F_SEND_4D(IP2)%SHP3  = SH_LOC_4D(IFREQ)%SHP3(IP2)
            F_SEND_4D(IP2)%SHZ   = SH_LOC_4D(IFREQ)%SHZ(IP2)
            F_SEND_4D(IP2)%SHF   = SH_LOC_4D(IFREQ)%SHF(IP2)
       ENDDO
       CALL GLOB_FONCTION_COMM_4D ()
        DO 110 IP = 1 ,NNSEND
           IPLAN = F_RECV_4D(IP)%IOR/NPOIN2+1
           IP2 = F_RECV_4D(IP)%IOR-(IPLAN-1)*NPOIN2
           IF (IP2==0) IPLAN = IPLAN-1
           IF (IP2==0) IP2 = NPOIN2
           F(IP2,IPLAN,IFREQ) =
     &         F_RECV_4D(IP)%BP/B(IP2,IFREQ)
           IF (F(IP2,IPLAN,IFREQ).LT.0.D0) THEN
              F(IP2,IPLAN,IFREQ) = 0.D0
           ENDIF
!
110     CONTINUE
       DEALLOCATE(F_SEND_4D,F_RECV_4D)
         DO IPLAN=1,NPLAN
           CALL PARCOM2
     &     ( F(:,IPLAN,IFREQ) ,
     &       F(:,IPLAN,IFREQ) ,
     &       F(:,IPLAN,IFREQ) ,
     &       NPOIN2 , 1 , 3 , 1 , MESH )
         ENDDO
       ENDDO
       ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
