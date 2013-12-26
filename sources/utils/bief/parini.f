!                    *****************
                     SUBROUTINE PARINI
!                    *****************
!
     &(NHP,NHM,INDPU,FAC,NPOIN,NACHB,NPLAN,MESH,NB_NEIGHB,
     & NB_NEIGHB_SEG,NELEM2,IFAPAR)
!
!***********************************************************************
! BIEF   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE ARRAYS USED IN PARALLEL MODE.
!
!history  REINHARD HINKELMANN (HANNOVER UNI.); PASCAL VEZOLLE (IBM)
!+        ??/??/????
!         V5P9
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        19/11/2013
!+        V7P0
!+   Adding an allocation of BUF_SEND%I and BUF_RECV%I  
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FAC              |<--| 1/(NUMBER OF NEIGHBOURING SUB-DOMAINS)
!| IFAPAR           |-->| IFAPAR(1:3,IELEM)=PROCESSOR NUMBERS BEHIND THE
!|                  |   | 3 ELEMENT EDGES  (NUMBERS FROM 0 TO NCSIZE-1)
!|                  |   | IFAPAR(4:6,IELEM): -LOCAL- ELEMENT NUMBERS
!|                  |   | BEHIND THE 3 EDGES
!| INDPU            |<--| INDEX TABLE : IF 0: NOT AN INTERFACE POINT
!|                  |   |               IF NOT 0: ADDRESS IN THE LIST
!|                  |   |               OF BOUNDARY POINTS.
!| MESH             |-->| MESH STRUCTURE
!| NACHB            |-->| IF 'IL' IS THE LOCAL RANK OF A NEIGHBOURING
!|                  |   | SUB-DOMAIN AND 'IP' ONE INTERFACE POINT
!|                  |   | NACHB(IL,IP) WILL BE THE REAL NUMBER OF THIS
!|                  |   | NEIGHBOURING SUB-DOMAIN
!|                  |   | THE LIST IN NACHB IS ORDERED WITH THE
!|                  |   | GLOBAL NUMBERS OF POINTS (HENCE THE POINTS
!|                  |   | WILL BE FOUND IN THE SAME ORDER BY ALL
!|                  |   | PROCESSORS)
!| NB_NEIGHB        |<--| NUMBER OF NEIGHBOURING SUB-DOMAINS (FOR POINTS)
!| NB_NEIGHB_SEG    |<--| NUMBER OF NEIGHBOURING SUB-DOMAINS (FOR EDGES)
!| NB_NEIGHB_PT_SEG |<--| NUMBER OF SEGMENTS SHARED WITH A NEIGHBOUR
!| NELEM2           |-->| NUMBER OF ELEMENTS IN 2D
!| NHM              |<--| NODE NUMBERS OF PROCESSORS WITH SMALLER RANK
!| NHP              |<--| NODE NUMBERS OF PROCESSORS WITH LARGER RANK
!| NPLAN            |-->| NUMBER OF PLANES IN 3D
!| NPOIN            |-->| NUMBER OF POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_PARINI => PARINI
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NPOIN,NPLAN,NELEM2
      INTEGER, INTENT(INOUT)         :: NB_NEIGHB,NB_NEIGHB_SEG
      INTEGER, INTENT(INOUT)         :: NHP(NBMAXDSHARE,NPTIR)
      INTEGER, INTENT(INOUT)         :: NHM(NBMAXDSHARE,NPTIR)
      INTEGER, INTENT(IN)            :: NACHB(NBMAXNSHARE,NPTIR)
      INTEGER, INTENT(IN)            :: IFAPAR(6,NELEM2)
      INTEGER, INTENT(INOUT)         :: INDPU(NPOIN)
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: FAC
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IKP(NBMAXDSHARE,2),IKM(NBMAXDSHARE,2)
      INTEGER I,J,IL,IZH,II,IMAX,IMIN,ILMAX,IELEM,IFACE
      INTEGER ILP,ILM,IPA,IKA,IPB,IKB,NB_PT_MX,DIM1HCOM,CHECKSUM
      LOGICAL NEW
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE PROCESSOR NUMBERS FOR 2D MESSAGE-PASSING
!
      DO I=1,NBMAXDSHARE
        IKP(I,1)=-1
        IKM(I,1)=-1
        IKP(I,2)=0
        IKM(I,2)=0
      ENDDO
!
!     PREPARES COMMUNICATION
!     IN THE FOLLOWING SEQUENCE :
!     1) SENDS     TO   PROCESSORS WITH NUMBER IPID + IL
!     2) RECEIVES  FROM PROCESSORS WITH NUMBER IPID - IL
!     3) SENDS     TO   PROCESSORS WITH NUMBER IPID - IL
!     4) RECEIVES  FROM PROCESSORS WITH NUMBER IPID + IL
!
!     LEVEL IL : SENDS AND RECEIVES
!
!
!     SENDS TO PROCESSORS WITH NUMBER GREATER THAN IPID
!
      IMAX=IPID
!
      IF (IPID.NE.NCSIZE-1) THEN
        IZH=1
        DO 60 IL=IPID+1,NCSIZE-1
          II=0
          DO 50 I=1,NPTIR
            DO 40 J=2,NBMAXNSHARE
              IF(NACHB(J,I).EQ.IL) THEN
                IF(IZH.GT.NBMAXDSHARE) THEN
                  IF(LNG.EQ.1) THEN
                    WRITE(LU,*) 'PARINI : NBMAXDSHARE TROP PETIT'
                  ENDIF
                  IF(LNG.EQ.2) THEN
                    WRITE(LU,*) 'PARINI: NBMAXDSHARE TOO SMALL'
                  ENDIF
                  CALL PLANTE(1)
                  STOP
                ENDIF
                II=II+1
                NHP(IZH,II)=NACHB(1,I)
              ENDIF
40          CONTINUE
50        CONTINUE
          IF(II.NE.0) THEN
            IKP(IZH,1)=IL
            IKP(IZH,2)=II
            IZH=IZH+1
            IMAX=IL
          ENDIF
60      CONTINUE
      ENDIF
!
!
!     RECEIVES FROM PROCESSORS WITH NUMBER LOWER THAN IPID
!
      IMIN=IPID
!
      IF (IPID.NE.0) THEN
        IZH=1
        DO 90 IL=IPID-1,0,-1
          II=0
          DO 80 I=1,NPTIR
           DO 70 J=2,NBMAXNSHARE
              IF(NACHB(J,I).EQ.IL) THEN
                IF(IZH.GT.NBMAXDSHARE) THEN
                  IF(LNG.EQ.1) THEN
                    WRITE(LU,*) 'PARINI : NBMAXDSHARE TROP PETIT'
                  ENDIF
                  IF(LNG.EQ.2) THEN
                    WRITE(LU,*) 'PARINI: NBMAXDSHARE TOO SMALL'
                  ENDIF
                  CALL PLANTE(1)
                  STOP
                ENDIF
                II=II+1
                NHM(IZH,II)=NACHB(1,I)
              ENDIF
70          CONTINUE
80        CONTINUE
          IF(II.NE.0) THEN
            IKM(IZH,1)=IL
            IKM(IZH,2)=II
            IZH=IZH+1
            IMIN=IL
          ENDIF
90      CONTINUE
      ENDIF
!
!**   DETERMINES ILMAX
!
      ILMAX=MAX(IMAX-IPID,IPID-IMIN)
!
!-----------------------------------------------------------------------
!
!  PASCAL VEZOLLE CODE:
!
!
!====   COMPUTES THE NUMBER OF NEIGHBOURS
!
        NB_PT_MX  = 0
        NB_NEIGHB = 0
        ILP = 1
        ILM = 1
!**     PROCESSOR OF HIGHER RANK
        DO IL=1,ILMAX
         IPA=IKP(ILP,1)
         IKA=IKP(ILP,2)
         IF(IPA.EQ.IPID+IL.AND.IKA.NE.0) THEN
           NB_NEIGHB = NB_NEIGHB + 1
           IF(IKA.GT.NB_PT_MX) NB_PT_MX=IKA
         ENDIF
         IF(IPA.EQ.IPID+IL) ILP=ILP+1
        ENDDO
!**      PROCESSOR OF LOWER RANK
        DO IL=1,ILMAX
         IPB=IKM(ILM,1)
         IKB=IKM(ILM,2)
         IF(IPB.EQ.IPID-IL.AND.IKB.NE.0) THEN
           NB_NEIGHB = NB_NEIGHB + 1
           IF(IKB.GT.NB_PT_MX) NB_PT_MX=IKB
         ENDIF
         IF(IPB.EQ.IPID-IL) ILM=ILM+1
        ENDDO
!
!====   ENDS COMPUTATION OF THE NUMBER OF NEIGHBOURS
!
        CALL BIEF_ALLVEC(2,MESH%NB_NEIGHB_PT,'NBNGPT',
     &                   NB_NEIGHB,1,0,MESH)
        CALL BIEF_ALLVEC(2,MESH%LIST_SEND   ,'LSSEND',
     &                   NB_NEIGHB,1,0,MESH)
!
! ALIGNMENT ON 16 BYTES
!
        DIM1HCOM = NB_PT_MX/4
        IF(MOD(NB_PT_MX,4).EQ.0) THEN
          DIM1HCOM = DIM1HCOM*4
        ELSE
          DIM1HCOM = DIM1HCOM*4 + 4
        ENDIF
        CALL BIEF_ALLVEC(2,MESH%NH_COM,'NH_COM',
     &                   DIM1HCOM,NB_NEIGHB,0,MESH)
!
!====   COMPUTES THE NUMBER OF INTERFACE POINTS PER NEIGHBOUR
!
        NB_NEIGHB = 0
        ILP = 1
        ILM = 1
        DO IL=1,ILMAX
         IPA=IKP(ILP,1)
         IKA=IKP(ILP,2)
         IF(IPA.EQ.IPID+IL.AND.IKA.NE.0) THEN
           NB_NEIGHB = NB_NEIGHB + 1
           MESH%NB_NEIGHB_PT%I(NB_NEIGHB) = IKA
           MESH%LIST_SEND%I(NB_NEIGHB) = IPA
           DO I=1,IKA
             MESH%NH_COM%I(DIM1HCOM*(NB_NEIGHB-1)+I)=NHP(ILP,I)
           ENDDO
         ENDIF
         IF(IPA.EQ.IPID+IL) ILP=ILP+1
        ENDDO
        DO IL=1,ILMAX
         IPB=IKM(ILM,1)
         IKB=IKM(ILM,2)
         IF(IPB.EQ.IPID-IL.AND.IKB.NE.0) THEN
           NB_NEIGHB = NB_NEIGHB + 1
           MESH%NB_NEIGHB_PT%I(NB_NEIGHB) = IKB
           MESH%LIST_SEND%I(NB_NEIGHB) = IPB
           DO I=1,IKB
             MESH%NH_COM%I(DIM1HCOM*(NB_NEIGHB-1)+I)=NHM(ILM,I)
           ENDDO
         ENDIF
         IF(IPB.EQ.IPID-IL) ILM=ILM+1
        ENDDO
!
!==== ENDS COMPUTATION OF THE NUMBER OF INTERFACE POINTS PER NEIGHBOUR
!
!=== POSSIBILITY OF SORTING LIST_SEND AND RECV FOR TORE BG
!
! ALIGNMENT ON 16BYTES BOUNDARIES
!
        NB_PT_MX = NB_PT_MX * NPLAN
        IL = NB_PT_MX/2
        IF(MOD(NB_PT_MX,2).EQ.0) THEN
          IL = IL*2
        ELSE
          IL = IL*2 + 2
        ENDIF
        CALL BIEF_ALLVEC(1,MESH%BUF_SEND,'BUSEND',IL*3,NB_NEIGHB,0,MESH)
        CALL BIEF_ALLVEC(1,MESH%BUF_RECV,'BURECV',IL*3,NB_NEIGHB,0,MESH)
!
!       ADDED FOR INTEGER COMMUNICATIONS
        ALLOCATE(MESH%BUF_SEND%I(IL*3*NB_NEIGHB))
        ALLOCATE(MESH%BUF_RECV%I(IL*3*NB_NEIGHB))       
!
!       NOTE JMH: IS THIS USEFUL ?
!
        DO I=1,IL*3*NB_NEIGHB
          MESH%BUF_SEND%R(I) = 0.D0
          MESH%BUF_RECV%R(I) = 0.D0
          MESH%BUF_SEND%I(I) = 0
          MESH%BUF_RECV%I(I) = 0
        ENDDO
!
!
!  END OF PASCAL VEZOLLE CODE
!
!-----------------------------------------------------------------------
!
!  JMH: FOR SEGMENTS
!
!     WE ASSUME HERE THAT NB_NEIGHB.GE.NB_NEIGHB_SEG
!
!     NOTE: NH_COM_SEG IS FILLED WITH 4*IELEM+IFACE
!           THIS IS TO RETRIEVE IELEM AND IFACE ONCE ELTSEG IS KNOWN
!           THE FINAL VALUE OF NH_COM_SEG IS ELTSEG(IELEM,IFACE)
!
      CALL BIEF_ALLVEC(2,MESH%NB_NEIGHB_PT_SEG,'NBNGSG',
     &                 NB_NEIGHB,1,0,MESH)
      CALL BIEF_ALLVEC(2,MESH%LIST_SEND_SEG   ,'LSSESG',
     &                 NB_NEIGHB,1,0,MESH)
      CALL BIEF_ALLVEC(2,MESH%NH_COM_SEG      ,'NH_CSG',
     &                 DIM1HCOM,NB_NEIGHB,0,MESH)
!
      NB_NEIGHB_SEG=0
!
!     INITIALISES NH_COM_SEG (SEE COMP_NH_COM_SEG)
!
      DO I=1,DIM1HCOM*NB_NEIGHB
        MESH%NH_COM_SEG%I(I)=-999999
      ENDDO
!
      DO IELEM=1,NELEM2
!
!       LOOKS FOR A FACE WITH THE OTHER SIDE IN ANOTHER SUB-DOMAIN
!
!       ELEMENTS WITHOUT ANY INTERFACE SEGMENT HAVE 3 ZEROS
        CHECKSUM=IFAPAR(1,IELEM)**2+
     &           IFAPAR(2,IELEM)**2+
     &           IFAPAR(3,IELEM)**2
!
        IF(CHECKSUM.NE.0) THEN
        DO IFACE=1,3
!
          ILM=IFAPAR(IFACE,IELEM)
          IF(ILM.GE.0.AND.ILM.NE.IPID) THEN
!           NEW INTERFACE SEGMENT FOUND
            IF(NB_NEIGHB_SEG.EQ.0) THEN
!             THE FIRST ONE
              NB_NEIGHB_SEG=1
              MESH%NB_NEIGHB_PT_SEG%I(1)=1
              MESH%LIST_SEND_SEG%I(1)=ILM
              MESH%NH_COM_SEG%I(1)=4*IELEM+IFACE
            ELSE
!             FROM THE SECOND ON
!             IS IT A NEW PROCESSOR
              NEW=.TRUE.
              DO IL=1,NB_NEIGHB_SEG
                IF(ILM.EQ.MESH%LIST_SEND_SEG%I(IL)) THEN
!                 NEW SEGMENT, OLD PROCESSOR
                  MESH%NB_NEIGHB_PT_SEG%I(IL)=
     &            MESH%NB_NEIGHB_PT_SEG%I(IL)+1
                  I=MESH%NB_NEIGHB_PT_SEG%I(IL)
                  MESH%NH_COM_SEG%I(DIM1HCOM*(IL-1)+I)=4*IELEM+IFACE
                  NEW=.FALSE.
                  EXIT
                ENDIF
              ENDDO
              IF(NEW) THEN
!               NEW SEGMENT, NEW PROCESSOR
                NB_NEIGHB_SEG=NB_NEIGHB_SEG+1
                MESH%NB_NEIGHB_PT_SEG%I(NB_NEIGHB_SEG)=1
                MESH%LIST_SEND_SEG%I(NB_NEIGHB_SEG)=ILM
                MESH%NH_COM_SEG%I(DIM1HCOM*(NB_NEIGHB_SEG-1)+1)=
     &                            4*IELEM+IFACE
              ENDIF
            ENDIF
          ENDIF
!
        ENDDO
        ENDIF
!
      ENDDO
!
      IF(NB_NEIGHB_SEG.GT.NB_NEIGHB) THEN
        WRITE(LU,*) 'IN PARINI NB_NEIGHB    =',NB_NEIGHB
        WRITE(LU,*) '          NB_NEIGHB_SEG=',NB_NEIGHB_SEG
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE FACTORS FOR LATER QUADRATIC NORMS
!     IDENTIFIES INTERNAL NODES/PROCESSORS
!     INDEX TABLE FOR BUFFER IN COMMUNICATION
!
      DO I=1,NPOIN
        INDPU(I)=0
      ENDDO
!
      CALL OS( 'X=C      ' ,X=FAC , C=1.D0 )
!
!  COEFFICIENTS FOR THE SCALAR PRODUCT:
!
      IF(NPTIR.GT.0) THEN
!
        DO I=1,NPTIR
!
!         FAC = 1/(NUMBER OF DOMAINS NEIGHBOURING A POINT)
!         SEE ALSO SUBROUTINE COMP_FAC FOR COMPLETION WITH QUADRATIC
!         ELEMENTS
!
          DO J=NBMAXNSHARE,3,-1
            IF(NACHB(J,I).EQ.-1) FAC%R(NACHB(1,I))=1.D0/(DBLE(J)-1.D0)
          ENDDO
!
          INDPU(NACHB(1,I))=I
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
