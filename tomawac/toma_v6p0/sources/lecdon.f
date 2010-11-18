C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       THIS SUBROUTINE PROJECTS THE CURRENTS / WINDS ON THE
!>                COMPUTATION MESH.
!><br>           (INSPIRED FROM SUBROUTINE FOND IN TELEMAC2D)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BINDON, CHDON, COURAN, DONTEL, IDTEL, INDIC, NBOR, NDON, NPMAX, NPOIN2, NPTFR, NPTT, TRA01, TRA02, TRA03, U, UR, V, VR, X, XRELV, Y, YRELV
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TOMAWAC :<br>
!> @link DECLARATIONS_TOMAWAC::MESH MESH@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ATT, BDX, BID, C, DX, DY, I, IB, ID, ISTAT, J, NCOL, NLIG, NP, NVAR, TEXTE, TITCAS, W, XMAX, XMIN, YMAX, YMIN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> COUUTI(), FASP(), LIT(), PLANTE(), VENUTI()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CONDIW()

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
!>      <td><center> 1.0                                       </center>
!> </td><td> 01/02/95
!> </td><td> F.MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BINDON
!></td><td>--></td><td>BINAIRE DU FICHIER DES DONNEES  (INDIC>2)
!>    </td></tr>
!>          <tr><td>CHDON
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COURAN
!></td><td>--></td><td>LOGIQUE INDIQUANT LA PRESENCE DE DONNEES
!>    </td></tr>
!>          <tr><td>DONTEL
!></td><td>--></td><td>LOGIQUE INDIQUANT SI ON RECUPERE
!>                  UNE VARIABLE TELEMAC
!>    </td></tr>
!>          <tr><td>IDTEL
!></td><td>--></td><td>RANG DE LA VARIABLE TELEMAC A RECUPERER
!>    </td></tr>
!>          <tr><td>INDIC
!></td><td>--></td><td>TYPE DE FORMAT DE LECTURE
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROTATION DES POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NDON
!></td><td>--></td><td>NUMERO D'UNITE LOGIQUE DU FICHIER
!>    </td></tr>
!>          <tr><td>NPMAX
!></td><td>--></td><td>NOMBRE DE POINTS RELEVES MAXIMUM
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE  POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NPTT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TRA02
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TRA03
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>U,V
!></td><td><--</td><td>COURANT OU VENT AUX NOEUDS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>UR,VR
!></td><td><-></td><td>TABLEAU DES COURANTS RELEVES
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DU MAILLAGE
!>    </td></tr>
!>          <tr><td>XRELV
!></td><td><-></td><td>TABLEAU DES ABSCISSES DES POINTS RELEVES
!>    </td></tr>
!>          <tr><td>YRELV
!></td><td><-></td><td>TABLEAU DES ORDONNEES DES POINTS RELEVES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE LECDON
     &( U , V , X, Y, NPOIN2, NDON, BINDON, NBOR, NPTFR, XRELV, YRELV,
     &  UR, VR, TRA01,TRA02,TRA03,IDTEL,NPTT,DONTEL,COURAN,INDIC,NPMAX,
     &  CHDON)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BINDON         |-->| BINAIRE DU FICHIER DES DONNEES  (INDIC>2)
C| CHDON          |---| 
C| COURAN         |-->| LOGIQUE INDIQUANT LA PRESENCE DE DONNEES
C| DONTEL         |-->| LOGIQUE INDIQUANT SI ON RECUPERE
C|                |   | UNE VARIABLE TELEMAC
C| IDTEL          |-->| RANG DE LA VARIABLE TELEMAC A RECUPERER
C| INDIC          |-->| TYPE DE FORMAT DE LECTURE
C| NBOR           |-->| NUMEROTATION DES POINTS FRONTIERE
C| NDON           |-->| NUMERO D'UNITE LOGIQUE DU FICHIER
C| NPMAX          |-->| NOMBRE DE POINTS RELEVES MAXIMUM
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPTFR          |-->| NOMBRE DE  POINTS FRONTIERE
C| NPTT           |---| 
C| TRA01          |<->| TABLEAU DE TRAVAIL
C| TRA02          |<->| TABLEAU DE TRAVAIL
C| TRA03          |<->| TABLEAU DE TRAVAIL
C| U,V            |<--| COURANT OU VENT AUX NOEUDS DU MAILLAGE
C| UR,VR          |<->| TABLEAU DES COURANTS RELEVES
C| X,Y            |-->| COORDONNEES DU MAILLAGE
C| XRELV          |<->| TABLEAU DES ABSCISSES DES POINTS RELEVES
C| YRELV          |<->| TABLEAU DES ORDONNEES DES POINTS RELEVES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NP,NDON,NPOIN2,NPTFR,INDIC,NCOL,NLIG,BID,I,J
C
      INTEGER NPMAX,NBOR(NPTFR,2)
C
      DOUBLE PRECISION X(NPOIN2),Y(NPOIN2),U(NPOIN2),V(NPOIN2)
      DOUBLE PRECISION TRA01(NPMAX),TRA02(NPMAX),TRA03(NPOIN2)
      DOUBLE PRECISION XRELV(NPMAX),YRELV(NPMAX),UR(NPMAX),VR(NPMAX)
      DOUBLE PRECISION XMAX,XMIN,YMAX,YMIN,DX,DY,ATT,BDX(2)
C
      INTEGER NVAR,IB(10),ISTAT,NPTT,ID(3),IDTEL
C
      CHARACTER*3  BINDON,C
      CHARACTER*7  CHDON
      CHARACTER*32 TEXTE(20)
      CHARACTER*72 TITCAS
C
      LOGICAL DONTEL,COURAN
C
C-----------------------------------------------------------------------
C
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NPMAX))
C
C-----------------------------------------------------------------------
C        READS THE POINTS FROM LOGICAL UNIT NDON
C-----------------------------------------------------------------------
C
      IF (INDIC.EQ.1) THEN
C
C     ------------------------------------------------------------------
C     WAM-LIKE FORMAT - FINITE DIFFERENCES
C     ------------------------------------------------------------------
C
         READ(NDON,10,END=100,ERR=100)
     &      NCOL,NLIG,YMIN,YMAX,XMIN,XMAX,BID,BID
         DX=(XMAX-XMIN)/REAL(NCOL-1)
         DY=(YMAX-YMIN)/REAL(NLIG-1)
         NP=NCOL*NLIG
         WRITE(LU,*)
     &    '-----------------------------------------------------'
         IF (LNG.EQ.1) THEN
            WRITE(LU,*)'LECDON : LECTURE DU FICHIER DE DONNEES'
            WRITE(LU,*)'         NOMBRE DE LIGNES   : ',NLIG
            WRITE(LU,*)'         NOMBRE DE COLONNES : ',NCOL
            WRITE(LU,*)'         ABSCISSE MINIMALE : ',XMIN
            WRITE(LU,*)'         ABSCISSE MAXIMALE : ',XMAX
            WRITE(LU,*)'         ORDONNEE MINIMALE : ',YMIN
            WRITE(LU,*)'         ORDONNEE MAXIMALE : ',YMAX
         ELSE
            WRITE(LU,*)'LECDON : READING OF THE DATA FILE '
            WRITE(LU,*)'         NUMBER OF LINES   : ',NLIG
            WRITE(LU,*)'         NUMBER OF COLUMNS : ',NCOL
            WRITE(LU,*)'         MINIMAL ABSCISSAE : ',XMIN
            WRITE(LU,*)'         MAXIMAL ABSCISSAE : ',XMAX
            WRITE(LU,*)'         MINIMAL ORDINATES : ',YMIN
            WRITE(LU,*)'         MAXIMAL ORDINATES : ',YMAX
         ENDIF
         WRITE(LU,*)
     &    '-----------------------------------------------------'
         IF (NP.GT.NPMAX) THEN
          WRITE(LU,*)
     &     '*****************************************************'
          IF (LNG.EQ.1) THEN
             WRITE(LU,*)
     &        ' LA DIMENSION PREVUE PAR DEFAUT POUR LE TABLEAU '
             WRITE(LU,*)
     &        ' DE DONNEES :',NPMAX,' EST TROP FAIBLE POUR '
             WRITE(LU,*)
     &        ' CONTENIR LA TOTALITE DES DONNEES :',NP
          ELSE
             WRITE(LU,*)
     &        ' THE DEFAULT DIMENSION ALLOWED FOR THE ARRAY OF '
             WRITE(LU,*)
     &        ' DATA :',NPMAX,' IS TOO LOW TO HOLD'
             WRITE(LU,*)
     &        ' ALL THE DATA :',NP
          ENDIF
          WRITE(LU,*)
     &        '*****************************************************'
          CALL PLANTE(0)
         ENDIF
         READ(NDON,*)
         READ(NDON,20,END=100,ERR=100)
     &      (UR(I),I=1,NCOL*NLIG)
         READ(NDON,*)
         READ(NDON,20,END=100,ERR=100)
     &      (VR(I),I=1,NCOL*NLIG)
         DO 30 I=1,NCOL
             DO 40 J=1,NLIG
                XRELV((I-1)*NLIG+J)=XMIN+DX*(I-1)
                YRELV((I-1)*NLIG+J)=YMIN+DY*(J-1)
40       CONTINUE
30       CONTINUE
C
C
      ELSEIF (INDIC.EQ.2) THEN
C
C     ------------------------------------------------------------------
C     SINUSX-LIKE FORMAT - SCATTER OF POINTS
C     ------------------------------------------------------------------
C
          DO 50 I=1,100000
            READ(NDON,*,END=60,ERR=100)
            NP=I
50        CONTINUE
60        CONTINUE
          WRITE(LU,*)
     &     '-----------------------------------------------------'
          IF (LNG.EQ.1) THEN
             WRITE(LU,*)'LECDON : LECTURE DU FICHIER DE DONNEES'
             WRITE(LU,*)'         NOMBRE DE POINTS   : ',NP
          ELSE
             WRITE(LU,*)'LECDON : READING OF THE DATA FILE '
             WRITE(LU,*)'         NUMBER OF POINTS   : ',NP
          ENDIF
          WRITE(LU,*)
     &     '-----------------------------------------------------'
          IF (NP.GT.NPMAX) THEN
           WRITE(LU,*)
     &     '*****************************************************'
           IF (LNG.EQ.1) THEN
             WRITE(LU,*)
     &        ' LA DIMENSION PREVUE PAR DEFAUT POUR LE TABLEAU '
             WRITE(LU,*)
     &        ' DE DONNEES :',NPMAX,' EST TROP FAIBLE POUR '
             WRITE(LU,*)
     &        ' CONTENIR LA TOTALITE DES DONNEES :',NP
           ELSE
             WRITE(LU,*)
     &        ' THE DEFAULT DIMENSION ALLOWED FOR THE ARRAY OF '
             WRITE(LU,*)
     &        ' DATA :',NPMAX,' IS TOO LOW TO HOLD'
             WRITE(LU,*)
     &        ' ALL THE DATA :',NP
           ENDIF
           WRITE(LU,*)
     &        '*****************************************************'
           CALL PLANTE(0)
          ENDIF
          REWIND NDON
          DO 70 I=1,NP
             READ(NDON,*,ERR=100) XRELV(I),YRELV(I),UR(I),VR(I)
70        CONTINUE
C
C
      ELSEIF (INDIC.EQ.3) THEN
C
C     ------------------------------------------------------------------
C     TELEMAC-LIKE FORMAT - MESH CAN BE DIFFERENT
C          (BINARY)                 FROM COWADIS MESH
C     ------------------------------------------------------------------
C
C      READS TITLE
C
          CALL LIT(X,W,IB,TITCAS,72,'CH',NDON,BINDON,ISTAT)
C
C      READS NUMBER OF VARIABLES AND THEIR NAMES
C
          CALL LIT(X,W,IB,C,2,'I ',NDON,BINDON,ISTAT)
          NVAR=IB(1)
          DO 80 I=1,NVAR
            CALL LIT(X,W,IB,TEXTE(I),32,'CH',NDON,BINDON,ISTAT)
80        CONTINUE
C
C      FORMAT AND GEOMETRY
C
          READ(NDON)
          CALL LIT(X,W,IB,C,4,'I ',NDON,BINDON,ISTAT)
          NP=IB(2)
          WRITE(LU,*)
     &        '-----------------------------------------------------'
          IF (LNG.EQ.1) THEN
             WRITE(LU,*)'LECDON : LECTURE DU FICHIER TELEMAC'
             WRITE(LU,*)'         NOMBRE DE POINTS   : ',NP
          ELSE
             WRITE(LU,*)'LECDON : READING OF TELEMAC DATA FILE '
             WRITE(LU,*)'         NUMBER OF POINTS   : ',NP
          ENDIF
          IF (NP.GT.NPMAX) THEN
           WRITE(LU,*)
     &        '*****************************************************'
           IF (LNG.EQ.1) THEN
             WRITE(LU,*)
     &        ' LA DIMENSION PREVUE PAR DEFAUT POUR LE TABLEAU '
             WRITE(LU,*)
     &        ' DE DONNEES :',NPMAX,' EST TROP FAIBLE POUR '
             WRITE(LU,*)
     &        ' CONTENIR LA TOTALITE DES DONNEES :',NP
           ELSE
             WRITE(LU,*)
     &        ' THE DEFAULT DIMENSION ALLOWED FOR THE ARRAY OF '
             WRITE(LU,*)
     &        ' DATA :',NPMAX,' IS TOO LOW TO HOLD'
             WRITE(LU,*)
     &        ' ALL THE DATA :',NP
           ENDIF
           WRITE(LU,*)
     &        '*****************************************************'
           CALL PLANTE(0)
           ENDIF
          READ(NDON)
          READ(NDON)
C
C      X AND Y
C
          CALL LIT(XRELV,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
          CALL LIT(YRELV,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
C
C      TIME STEP AND VARIABLES
C
          DO 110 J=1,(NPTT-1)*(NVAR+1)
             READ(NDON)
110       CONTINUE
C
          IF (DONTEL) ID(3)=IDTEL
          IF (COURAN) THEN
             ID(1)=1
             ID(2)=2
          ENDIF
C
          CALL LIT(BDX(1),W,IB,C,1,'R4',NDON,BINDON,ISTAT)
          ATT=BDX(1)
          DO 90 I=1,NVAR
            IF (I.EQ.ID(1)) THEN
               CALL LIT(UR,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
            ELSEIF (I.EQ.ID(2)) THEN
               CALL LIT(VR,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
            ELSEIF ((I.EQ.ID(3)).AND.(DONTEL)) THEN
               CALL LIT(TRA01,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
            ELSE
               READ(NDON)
            ENDIF
90        CONTINUE
C
C      WRITES TO THE LISTING
C
          IF (LNG.EQ.1) THEN
            WRITE(LU,*)'         TITRE DU CAS TELEMAC : '
            WRITE(LU,*)'           ',TITCAS
            WRITE(LU,*)'         TEMPS DE TELEMAC : ',ATT
            WRITE(LU,*)'         VARIABLES DE TELEMAC RETENUE(S) : '
          ELSE
            WRITE(LU,*)'         TITLE OF TELEMAC CASE : '
            WRITE(LU,*)'           ',TITCAS
            WRITE(LU,*)'         TIME OF TELEMAC RECORD : ',ATT
            WRITE(LU,*)'         VARIABLE(S) OF TELEMAC READ : '
          ENDIF
          IF (COURAN) THEN
              WRITE(LU,*)'           ',TEXTE(ID(1))
              WRITE(LU,*)'           ',TEXTE(ID(2))
          ENDIF
          IF (DONTEL)
     &            WRITE(LU,*)'           ',TEXTE(ID(3))
          WRITE(LU,*)
     &        '-----------------------------------------------------'
C
C
      ELSEIF (INDIC.EQ.4) THEN
C
C     ------------------------------------------------------------------
C       READS A USER-DEFINED FORMAT
C     ------------------------------------------------------------------
        IF(CHDON(1:1).EQ.'C') THEN
C         READS A CURRENT FIELD
              CALL COUUTI
     &    (X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,0.,0.,0.,0.,
     &     NP,XRELV,YRELV,UR,VR,TRA03,TRA03,TRA03,TRA03,NPMAX)
        ELSEIF(CHDON(1:1).EQ.'V' .OR. CHDON(1:1).EQ.'W') THEN
C         READS A WIND FIELD
          CALL VENUTI
     &    (X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,0.,0.,0.,0.,
     &     NP,XRELV,YRELV,UR,VR,TRA03,TRA03,TRA03,TRA03,NPMAX)
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'LE TYPE DE DONNEES A LIRE EST INCONNU'
          ELSE
            WRITE(LU,*) 'UNKNOWN DATA'
          ENDIF
            CALL PLANTE(0)
         ENDIF
C
      ELSE
        WRITE(LU,*)'***********************************************'
        IF (LNG.EQ.1) THEN
          WRITE(LU,*)'LECDON : INDICATEUR DE FORMAT INCONNU   '
          WRITE(LU,*)'         POUR LE FICHIER DES DONNEES :',INDIC
        ELSE
          WRITE(LU,*)'LECDON : INDICATOR OF FORMAT FOR THE   '
          WRITE(LU,*)'         DATA FILE UNKNOWN :',INDIC
        ENDIF
        WRITE(LU,*)'***********************************************'

          CALL PLANTE(0)
      ENDIF

C
C-----------------------------------------------------------------------
C   THE CURRENTS ARE INTERPOLATED ONTO ALL THE INTERIOR POINTS
C                         TO THE DOMAIN
C-----------------------------------------------------------------------
C
      IF (COURAN) THEN
        CALL FASP(X,Y,U,NPOIN2,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
        CALL FASP(X,Y,V,NPOIN2,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
      ENDIF
      IF (DONTEL)
     &CALL FASP(X,Y,TRA03,NPOIN2,XRELV,YRELV,TRA01,NP,NBOR,
     &                                      MESH%KP1BOR%I,NPTFR,1.D-6)
C
C-----------------------------------------------------------------------
C
C     FORMATS
C
10    FORMAT (2I4,4F9.3,2I2)
20    FORMAT (10F6.2)
C
      DEALLOCATE(W)
      RETURN
C
C     IF FAILED TO READ THE FILE ...
C
100   CONTINUE
      WRITE(LU,*)'*********************************************'
      IF (LNG.EQ.1) THEN
         WRITE(LU,*)'  ERREUR A LA LECTURE DU FICHIER DE DONNEES  '
         WRITE(LU,*)'      OU FIN DE FICHIER PREMATUREE           '
      ELSE
         WRITE(LU,*)'  ERROR WHILE READING DATA FILE '
         WRITE(LU,*)'    OR UNEXPECTED END OF FILE           '
      ENDIF
      WRITE(LU,*)'*********************************************'
      CALL PLANTE(1)
C
      DEALLOCATE(W)
      RETURN
      END
C
C#######################################################################
C