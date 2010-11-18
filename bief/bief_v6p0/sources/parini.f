C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE ARRAYS USED IN PARALLEL MODE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FAC, IFAPAR, INDPU, MESH, NACHB, NB_NEIGHB, NB_NEIGHB_SEG, NELEM2, NHM, NHP, NPLAN, NPOIN
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::IPID IPID@endlink, 
!> @link BIEF_DEF::NBMAXDSHARE NBMAXDSHARE@endlink, 
!> @link BIEF_DEF::NBMAXNSHARE NBMAXNSHARE@endlink, 
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink, 
!> @link BIEF_DEF::NPTIR NPTIR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CHECKSUM, DIM1HCOM, I, IELEM, IFACE, II, IKA, IKB, IKM, IKP, IL, ILM, ILMAX, ILP, IMAX, IMIN, IPA, IPB, IZH, J, NB_PT_MX, NEW
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PARINI
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ALLVEC(), OS(), PLANTE()
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
!>      <td><center> 5.9                                       </center>
!> </td><td> 02/10/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td>
!> </td><td> REINHARD HINKELMANN (HANNOVER UNI.); PASCAL VEZOLLE (IBM)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>FAC
!></td><td><--</td><td>FELD FUER FAKTOR QUADRATNORM
!>                  (WITH QUADRATIC ELEMENTS WILL BE COMPLETED
!>                  LATER IN SUBROUTINE COMP_FAC)
!>    </td></tr>
!>          <tr><td>IFAPAR
!></td><td>--></td><td>IFAPAR(1:3,IELEM)=PROCESSOR NUMBERS BEHIND THE
!>                  3 ELEMENT EDGES  (NUMBERS FROM 0 TO NCSIZE-1)
!>                  IFAPAR(4:6,IELEM): -LOCAL- ELEMENT NUMBERS
!>                  BEHIND THE 3 EDGES
!>    </td></tr>
!>          <tr><td>IKM
!></td><td><--</td><td>KLEINERE NACHBARPROZESSORNR
!>                  UND ANZAHL GEMEINSAME KNOTEN
!>    </td></tr>
!>          <tr><td>IKP
!></td><td><--</td><td>GROESSERE NACHBARPROZESSORNR
!>                  UND ANZAHL GEMEINSAME KNOTEN
!>    </td></tr>
!>          <tr><td>INDPU
!></td><td><--</td><td>INDEXTABELLE FUER PUFFER IN KOMMUNIKATION
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MESH STRUCTURE
!>    </td></tr>
!>          <tr><td>NACHB
!></td><td>--></td><td>IF 'IL' IS THE LOCAL RANK OF A NEIGHBOURING
!>                  SUB-DOMAIN AND 'IP' ONE INTERFACE POINT
!>                  NACHB(IL,IP) WILL BE THE REAL NUMBER OF THIS
!>                  NEIGHBOURING SUB-DOMAIN
!>                  THE LIST IN NACHB IS ORDERED WITH THE
!>                  GLOBAL NUMBERS OF POINTS (HENCE THE POINTS
!>                  WILL BE FOUND IN THE SAME ORDER BY ALL
!>                  PROCESSORS)
!>    </td></tr>
!>          <tr><td>NB_NEIGHB
!></td><td><--</td><td>NUMBER OF NEIGHBOURING SUB-DOMAINS (FOR POINTS)
!>    </td></tr>
!>          <tr><td>NB_NEIGHB_SEG
!></td><td><--</td><td>NUMBER OF NEIGHBOURING SUB-DOMAINS (FOR EDGES)
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NUMBER OF ELEMENTS IN 2D
!>    </td></tr>
!>          <tr><td>NHM
!></td><td><--</td><td>GEMEINSAME KNOTENNUMMERN ZU
!>                  KLEINEREN NACHBARPROZESSOREN
!>    </td></tr>
!>          <tr><td>NHP
!></td><td><--</td><td>GEMEINSAME KNOTENNUMMERN ZU
!>                  GROESSEREN NACHBARPROZESSOREN
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF PLANES IN 3D
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PARINI
     &(NHP,NHM,INDPU,FAC,NPOIN,NACHB,NPLAN,MESH,NB_NEIGHB,
     & NB_NEIGHB_SEG,NELEM2,IFAPAR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FAC            |<--| FELD FUER FAKTOR QUADRATNORM
C|                |   | (WITH QUADRATIC ELEMENTS WILL BE COMPLETED
C|                |   | LATER IN SUBROUTINE COMP_FAC)
C| IFAPAR         |-->| IFAPAR(1:3,IELEM)=PROCESSOR NUMBERS BEHIND THE
C|                |   | 3 ELEMENT EDGES  (NUMBERS FROM 0 TO NCSIZE-1)
C|                |   | IFAPAR(4:6,IELEM): -LOCAL- ELEMENT NUMBERS
C|                |   | BEHIND THE 3 EDGES
C| IKM            |<--| KLEINERE NACHBARPROZESSORNR
C|                |   | UND ANZAHL GEMEINSAME KNOTEN
C| IKP            |<--| GROESSERE NACHBARPROZESSORNR
C|                |   | UND ANZAHL GEMEINSAME KNOTEN
C| INDPU          |<--| INDEXTABELLE FUER PUFFER IN KOMMUNIKATION
C| MESH           |-->| MESH STRUCTURE
C| NACHB          |-->| IF 'IL' IS THE LOCAL RANK OF A NEIGHBOURING
C|                |   | SUB-DOMAIN AND 'IP' ONE INTERFACE POINT
C|                |   | NACHB(IL,IP) WILL BE THE REAL NUMBER OF THIS
C|                |   | NEIGHBOURING SUB-DOMAIN
C|                |   | THE LIST IN NACHB IS ORDERED WITH THE
C|                |   | GLOBAL NUMBERS OF POINTS (HENCE THE POINTS
C|                |   | WILL BE FOUND IN THE SAME ORDER BY ALL
C|                |   | PROCESSORS)
C| NB_NEIGHB      |<--| NUMBER OF NEIGHBOURING SUB-DOMAINS (FOR POINTS)
C| NB_NEIGHB_SEG  |<--| NUMBER OF NEIGHBOURING SUB-DOMAINS (FOR EDGES)
C| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
C| NHM            |<--| GEMEINSAME KNOTENNUMMERN ZU
C|                |   | KLEINEREN NACHBARPROZESSOREN
C| NHP            |<--| GEMEINSAME KNOTENNUMMERN ZU
C|                |   | GROESSEREN NACHBARPROZESSOREN
C| NPLAN          |-->| NUMBER OF PLANES IN 3D
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PARINI => PARINI
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: NPOIN,NPLAN,NELEM2
      INTEGER, INTENT(INOUT)         :: NB_NEIGHB,NB_NEIGHB_SEG
      INTEGER, INTENT(INOUT)         :: NHP(NBMAXDSHARE,NPTIR)
      INTEGER, INTENT(INOUT)         :: NHM(NBMAXDSHARE,NPTIR)
      INTEGER, INTENT(IN)            :: NACHB(NBMAXNSHARE,NPTIR)
      INTEGER, INTENT(IN)            :: IFAPAR(6,NELEM2)
      INTEGER, INTENT(INOUT)         :: INDPU(NPOIN)
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: FAC
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IKP(NBMAXDSHARE,2),IKM(NBMAXDSHARE,2)
      INTEGER I,J,IL,IZH,II,IMAX,IMIN,ILMAX,IELEM,IFACE
      INTEGER ILP,ILM,IPA,IKA,IPB,IKB,NB_PT_MX,DIM1HCOM,CHECKSUM
      LOGICAL NEW
C
C-----------------------------------------------------------------------
C
C     INITIALISES THE PROCESSOR NUMBERS FOR 2D MESSAGE-PASSING
C
      DO I=1,NBMAXDSHARE
        IKP(I,1)=-1
        IKM(I,1)=-1
        IKP(I,2)=0
        IKM(I,2)=0
      ENDDO
C
C     PREPARES COMMUNICATION
C     IN THE FOLLOWING SEQUENCE :
C     1) SENDS     TO   PROCESSORS WITH NUMBER IPID + IL
C     2) RECEIVES  FROM PROCESSORS WITH NUMBER IPID - IL
C     3) SENDS     TO   PROCESSORS WITH NUMBER IPID - IL
C     4) RECEIVES  FROM PROCESSORS WITH NUMBER IPID + IL
C
C     LEVEL IL : SENDS AND RECEIVES
C
C
C     SENDS TO PROCESSORS WITH NUMBER GREATER THAN IPID
C
      IMAX=IPID
C
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
C
C
C     RECEIVES FROM PROCESSORS WITH NUMBER LOWER THAN IPID
C
      IMIN=IPID
C
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
C
C**   DETERMINES ILMAX
C
      ILMAX=MAX(IMAX-IPID,IPID-IMIN)
C
C-----------------------------------------------------------------------
C
C  PASCAL VEZOLLE CODE:
C
!
C====   COMPUTES THE NUMBER OF NEIGHBOURS
!
        NB_PT_MX  = 0
        NB_NEIGHB = 0
        ILP = 1
        ILM = 1
C**     PROCESSOR OF HIGHER RANK
        DO IL=1,ILMAX
         IPA=IKP(ILP,1)
         IKA=IKP(ILP,2)
         IF(IPA.EQ.IPID+IL.AND.IKA.NE.0) THEN
           NB_NEIGHB = NB_NEIGHB + 1
           IF(IKA.GT.NB_PT_MX) NB_PT_MX=IKA
         ENDIF
         IF(IPA.EQ.IPID+IL) ILP=ILP+1
        ENDDO
C**      PROCESSOR OF LOWER RANK
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
C====   ENDS COMPUTATION OF THE NUMBER OF NEIGHBOURS
!
        CALL ALLVEC(2,MESH%NB_NEIGHB_PT,'NBNGPT',NB_NEIGHB,1,0)
        CALL ALLVEC(2,MESH%LIST_SEND   ,'LSSEND',NB_NEIGHB,1,0)
!
C ALIGNMENT ON 16 BYTES
!
        DIM1HCOM = NB_PT_MX/4
        IF(MOD(NB_PT_MX,4).EQ.0) THEN
          DIM1HCOM = DIM1HCOM*4
        ELSE
          DIM1HCOM = DIM1HCOM*4 + 4
        ENDIF
        CALL ALLVEC(2,MESH%NH_COM,'NH_COM',DIM1HCOM,NB_NEIGHB,0)
!
C====   COMPUTES THE NUMBER OF INTERFACE POINTS PER NEIGHBOUR
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
C====   ENDS COMPUTATION OF THE NUMBER OF INTERFACE POINTS PER NEIGHBOUR
!
C=== POSSIBILITY OF SORTING LIST_SEND AND RECV FOR TORE BG
!
C ALIGNMENT ON 16BYTES BOUNDARIES
!
        NB_PT_MX = NB_PT_MX * NPLAN
        IL = NB_PT_MX/2
        IF(MOD(NB_PT_MX,2).EQ.0) THEN
          IL = IL*2
        ELSE
          IL = IL*2 + 2
        ENDIF
        CALL ALLVEC(1,MESH%BUF_SEND,'BUSEND',IL*3,NB_NEIGHB,0)
        CALL ALLVEC(1,MESH%BUF_RECV,'BURECV',IL*3,NB_NEIGHB,0)
!
        DO I=1,IL*3*NB_NEIGHB
          MESH%BUF_SEND%R(I) = 0.D0
          MESH%BUF_RECV%R(I) = 0.D0
        ENDDO
C
C
C  END OF PASCAL VEZOLLE CODE
C
C-----------------------------------------------------------------------
C
C  JMH: FOR SEGMENTS
C
C     WE ASSUME HERE THAT NB_NEIGHB.GE.NB_NEIGHB_SEG
C
C     NOTE: NH_COM_SEG IS FILLED WITH 4*IELEM+IFACE
C           THIS IS TO RETRIEVE IELEM AND IFACE ONCE ELTSEG IS KNOWN
C           THE FINAL VALUE OF NH_COM_SEG IS ELTSEG(IELEM,IFACE)
C
      CALL ALLVEC(2,MESH%NB_NEIGHB_PT_SEG,'NBNGSG',NB_NEIGHB,1,0)
      CALL ALLVEC(2,MESH%LIST_SEND_SEG   ,'LSSESG',NB_NEIGHB,1,0)
      CALL ALLVEC(2,MESH%NH_COM_SEG      ,'NH_CSG',DIM1HCOM,NB_NEIGHB,0)
C
      NB_NEIGHB_SEG=0
C
C     INITIALISES NH_COM_SEG (SEE COMP_NH_COM_SEG)
C
      DO I=1,DIM1HCOM*NB_NEIGHB
        MESH%NH_COM_SEG%I(I)=-999999
      ENDDO
C
      DO IELEM=1,NELEM2
C
C       LOOKS FOR A FACE WITH THE OTHER SIDE IN ANOTHER SUB-DOMAIN
C
C       ELEMENTS WITHOUT ANY INTERFACE SEGMENT HAVE 3 ZEROS
        CHECKSUM=IFAPAR(1,IELEM)**2+
     &           IFAPAR(2,IELEM)**2+
     &           IFAPAR(3,IELEM)**2
C
        IF(CHECKSUM.NE.0) THEN
        DO IFACE=1,3
C
          ILM=IFAPAR(IFACE,IELEM)
          IF(ILM.GE.0.AND.ILM.NE.IPID) THEN
C           NEW INTERFACE SEGMENT FOUND
            IF(NB_NEIGHB_SEG.EQ.0) THEN
C             THE FIRST ONE
              NB_NEIGHB_SEG=1
              MESH%NB_NEIGHB_PT_SEG%I(1)=1
              MESH%LIST_SEND_SEG%I(1)=ILM
              MESH%NH_COM_SEG%I(1)=4*IELEM+IFACE
            ELSE
C             FROM THE SECOND ON
C             IS IT A NEW PROCESSOR
              NEW=.TRUE.
              DO IL=1,NB_NEIGHB_SEG
                IF(ILM.EQ.MESH%LIST_SEND_SEG%I(IL)) THEN
C                 NEW SEGMENT, OLD PROCESSOR
                  MESH%NB_NEIGHB_PT_SEG%I(IL)=
     &            MESH%NB_NEIGHB_PT_SEG%I(IL)+1
                  I=MESH%NB_NEIGHB_PT_SEG%I(IL)
                  MESH%NH_COM_SEG%I(DIM1HCOM*(IL-1)+I)=4*IELEM+IFACE
                  NEW=.FALSE.
                  EXIT
                ENDIF
              ENDDO
              IF(NEW) THEN
C               NEW SEGMENT, NEW PROCESSOR
                NB_NEIGHB_SEG=NB_NEIGHB_SEG+1
                MESH%NB_NEIGHB_PT_SEG%I(NB_NEIGHB_SEG)=1
                MESH%LIST_SEND_SEG%I(NB_NEIGHB_SEG)=ILM
                MESH%NH_COM_SEG%I(DIM1HCOM*(NB_NEIGHB_SEG-1)+1)=
     &                            4*IELEM+IFACE
              ENDIF
            ENDIF
          ENDIF
C
        ENDDO
        ENDIF
C
      ENDDO
C
      IF(NB_NEIGHB_SEG.GT.NB_NEIGHB) THEN
        WRITE(LU,*) 'IN PARINI NB_NEIGHB    =',NB_NEIGHB
        WRITE(LU,*) '          NB_NEIGHB_SEG=',NB_NEIGHB_SEG
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C     COMPUTES THE FACTORS FOR LATER QUADRATIC NORMS
C     IDENTIFIES INTERNAL NODES/PROCESSORS
C     INDEX TABLE FOR BUFFER IN COMMUNICATION
C
      DO I=1,NPOIN
        INDPU(I)=0
      ENDDO
C
      CALL OS( 'X=C      ' ,X=FAC , C=1.D0 )
C
C  COEFFICIENTS FOR THE SCALAR PRODUCT:
C
      IF(NPTIR.GT.0) THEN
C
        DO I=1,NPTIR
C
C         FAC = 1/(NUMBER OF DOMAINS NEIGHBOURING A POINT)
C         SEE ALSO SUBROUTINE COMP_FAC FOR COMPLETION WITH QUADRATIC
C         ELEMENTS
C
          DO J=NBMAXNSHARE,3,-1
            IF(NACHB(J,I).EQ.-1) FAC%R(NACHB(1,I))=1.D0/(DBLE(J)-1.D0)
          ENDDO
C
          INDPU(NACHB(1,I))=I
C
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