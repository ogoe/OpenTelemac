C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       THIS SUBROUTINE PROJECTS THE CURRENTS / WINDS ON THE
!>                COMPUTATION MESH AND INTERPOLATES TO FIRST TIME STEP.
!><br>           (INSPIRED FROM SUBROUTINE FOND IN TELEMAC2D AMONGST OTHERS)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, BINDON, CHDON, DDC, INDIC, NBOR, NDON, NP, NPMAX, NPOIN2, NPTFR, NVAR, TRA, TV1, TV2, U1, U2, UD, UR, V1, V2, VD, VR, X, XRELV, Y, YRELV
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
!>    </th><td> ATB, ATT, BID, C, COEF, DAT1, DAT2, DX, DY, I, IB, ID, ISTAT, J, NCOL, NLIG, TEXTE, TITCAS, W, XMAX, XMIN, YMAX, YMIN, Z
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> COUUTI(), FASP(), LIT(), OV(), PLANTE(), TEMP(), VENUTI()
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
!>      <td><center> 5.0                                       </center>
!> </td><td>
!> </td><td>
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>BINDON
!></td><td>--></td><td>BINAIRE DU FICHIER DES DONNEES
!>    </td></tr>
!>          <tr><td>CHDON
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DDC
!></td><td>--></td><td>DATE DU DEBUT DU CALCUL
!>    </td></tr>
!>          <tr><td>INDIC
!></td><td>--></td><td>TYPE DE FORMAT DE LECTURE
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROTATION DES POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NDON
!></td><td>--></td><td>NUMERO D'UNITE LOGIQUE DU FICHIER DE DONNEES
!>    </td></tr>
!>          <tr><td>NP
!></td><td><-></td><td>NOMBRE DE POINTS RELEVES
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
!>          <tr><td>NVAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TV1
!></td><td><-></td><td>TEMPS DU CHAMPS DE DONNEES 1
!>    </td></tr>
!>          <tr><td>TV2
!></td><td><-></td><td>TEMPS DU CHAMPS DE DONNEES 2
!>    </td></tr>
!>          <tr><td>U1,V1,U2,V2
!></td><td><-></td><td>DONNEES AUX NOEUDS DU MAILLAGE A TV1 ET TV2
!>    </td></tr>
!>          <tr><td>UD,VD
!></td><td><--</td><td>DONNEE AUX NOEUDS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>UR,VR
!></td><td><-></td><td>TABLEAU DES DONNEES RELEVEES
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
                        SUBROUTINE LECDOI
     &( UD , VD  , X  , Y  , NPOIN2, NDON , BINDON, NBOR , NPTFR,
     &  AT , DDC , TV1, TV2, NP   , XRELV, YRELV , UR   , VR   ,
     &  TRA, U1  , V1 , U2 , V2   , INDIC, NPMAX , CHDON, NVAR )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS
C| BINDON         |-->| BINAIRE DU FICHIER DES DONNEES
C| CHDON          |---| 
C| DDC            |-->| DATE DU DEBUT DU CALCUL
C| INDIC          |-->| TYPE DE FORMAT DE LECTURE
C| NBOR           |-->| NUMEROTATION DES POINTS FRONTIERE
C| NDON           |-->| NUMERO D'UNITE LOGIQUE DU FICHIER DE DONNEES
C| NP             |<->| NOMBRE DE POINTS RELEVES
C| NPMAX          |-->| NOMBRE DE POINTS RELEVES MAXIMUM
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPTFR          |-->| NOMBRE DE  POINTS FRONTIERE
C| NVAR           |---| 
C| TRA            |---| 
C| TV1            |<->| TEMPS DU CHAMPS DE DONNEES 1
C| TV2            |<->| TEMPS DU CHAMPS DE DONNEES 2
C| U1,V1,U2,V2    |<->| DONNEES AUX NOEUDS DU MAILLAGE A TV1 ET TV2
C| UD,VD          |<--| DONNEE AUX NOEUDS DU MAILLAGE
C| UR,VR          |<->| TABLEAU DES DONNEES RELEVEES
C| X,Y            |-->| COORDONNEES DU MAILLAGE
C| XRELV          |<->| TABLEAU DES ABSCISSES DES POINTS RELEVES
C| YRELV          |<->| TABLEAU DES ORDONNEES DES POINTS RELEVES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH
      USE INTERFACE_TOMAWAC, EX_LECDOI => LECDOI
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NP,NDON,NPOIN2,NPTFR,INDIC,NCOL,NLIG,BID,I,J
      INTEGER NVAR,ISTAT,IB(10),ID(2)
C
      INTEGER NPMAX,NBOR(NPTFR,2)
C
      DOUBLE PRECISION X(NPOIN2)    , Y(NPOIN2)
      DOUBLE PRECISION UD(NPOIN2)   , VD(NPOIN2)
      DOUBLE PRECISION XRELV(NPMAX) , YRELV(NPMAX)
      DOUBLE PRECISION UR(NPMAX)    , VR(NPMAX), TRA(NPMAX)
      DOUBLE PRECISION U1(NPOIN2)   , V1(NPOIN2)
      DOUBLE PRECISION U2(NPOIN2)   , V2(NPOIN2)
      DOUBLE PRECISION XMAX,XMIN,YMAX,YMIN,DX,DY,AT,TV1,TV2
      DOUBLE PRECISION DDC,DAT1,DAT2,COEF,Z(1),ATT, ATB(1)
C
      CHARACTER*3  BINDON,C
      CHARACTER*7  CHDON
      CHARACTER*72 TITCAS
      CHARACTER*32 TEXTE(10)
C
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NPMAX))
C
C
C-----------------------------------------------------------------------
C     READS THE POINTS FROM LOGICAL UNIT NDON
C-----------------------------------------------------------------------
C
      IF (INDIC.EQ.1) THEN
C
C      -----------------------------------------------------------------
C      WAM FORMAT, FINITE DIFFERENCES + INTERPOLATION TO THE MESH POINTS
C
C      -----------------------------------------------------------------
C
       REWIND NDON
C
       READ(NDON,10,END=100,ERR=100)
     &      NCOL,NLIG,YMIN,YMAX,XMIN,XMAX,BID,BID
       DX=(XMAX-XMIN)/REAL(NCOL-1)
       DY=(YMAX-YMIN)/REAL(NLIG-1)
       NP=NCOL*NLIG
       IF(LNG.EQ.1) THEN
        WRITE(LU,*) '--------------------------------------------------'
        WRITE(LU,*) 'LECDOI : LECTURE DU FICHIER DE ',CHDON
        WRITE(LU,*) '         NOMBRE DE LIGNES   : ',NLIG
        WRITE(LU,*) '         NOMBRE DE COLONNES :',NCOL
        WRITE(LU,*) '         ABSCISSE OU LONGITUDE MINIMALE : ',XMIN
        WRITE(LU,*) '         ABSCISSE OU LONGITUDE MAXIMALE : ',XMAX
        WRITE(LU,*) '         ORDONNEE OU LATITUDE MINIMALE  : ',YMIN
        WRITE(LU,*) '         ORDONNEE OU LATITUDE MAXIMALE  : ',YMAX
        IF (NP.GT.NPMAX) THEN
         WRITE(LU,*) '*************************************************'
         WRITE(LU,*) ' LA DIMENSION PREVUE PAR DEFAUT POUR LE TABLEAU '
         WRITE(LU,*) ' DE DONNEES :',NPMAX,' EST TROP FAIBLE POUR '
         WRITE(LU,*) ' CONTENIR LA TOTALITE DES DONNEES :',NP
         WRITE(LU,*) '*************************************************'
         CALL PLANTE(1)
         STOP
        ENDIF
       ELSE
        WRITE(LU,*) '--------------------------------------------------'
        WRITE(LU,*)'LECDOI : READING OF THE ',CHDON,' DATA FILE '
        WRITE(LU,*)'         NUMBER OF LINES   : ',NLIG
        WRITE(LU,*)'         NUMBER OF COLUMNS : ',NCOL
        WRITE(LU,*)'         MINIMAL ABSCISSAE : ',XMIN
        WRITE(LU,*)'         MAXIMAL ABSCISSAE : ',XMAX
        WRITE(LU,*)'         MINIMAL ORDINATES : ',YMIN
        WRITE(LU,*)'         MAXIMAL ORDINATES : ',YMAX
        IF (NP.GT.NPMAX) THEN
         WRITE(LU,*) '*************************************************'
         WRITE(LU,*) ' THE DEFAULT DIMENSION ALLOWED FOR THE ARRAY OF '
         WRITE(LU,*) ' DATA :',NPMAX,' IS TOO LOW TO HOLD'
         WRITE(LU,*) ' ALL THE DATA :',NP
         WRITE(LU,*) '*************************************************'
         CALL PLANTE(1)
         STOP
        ENDIF
       ENDIF
C      READS THE DATE OF THE FIRST RECORD
       READ(NDON,*) DAT1
       CALL TEMP(TV1,DAT1,DDC)
       IF (TV1.GT.AT) THEN
        WRITE(LU,*) '*************************************************'
        IF(LNG.EQ.1) THEN
         WRITE(LU,*) ' LE PREMIER ENREGISTREMENT DU FICHIER DE ',CHDON
         WRITE(LU,*) '   ',DAT1,' EST POSTERIEUR AU TEMPS '
         WRITE(LU,*) '   DU DEBUT DU CALCUL',DDC
        ELSE
         WRITE(LU,*) ' THE FIRST RECORDING OF THE ',CHDON,' FILE '
         WRITE(LU,*) '   ',DAT1,' IS OLDER THAN THE DEGINNING '
         WRITE(LU,*) '   OF THE COMPUTATION',DDC
        ENDIF
        WRITE(LU,*) '*************************************************'
        CALL PLANTE(1)
        STOP
       ENDIF
C
       DO 50 I=1,NCOL
          DO 40 J=1,NLIG
                XRELV((I-1)*NLIG+J)=XMIN+DX*(I-1)
                YRELV((I-1)*NLIG+J)=YMIN+DY*(J-1)
40        CONTINUE
50     CONTINUE
C
90     CONTINUE
       READ(NDON,*,END=100,ERR=100)
       READ(NDON,20,END=100,ERR=100) (UR(I),I=1,NP)
       READ(NDON,*)
       READ(NDON,20,END=100,ERR=100) (VR(I),I=1,NP)
       CALL OV( 'X=C     ' , U1 , Y , Z , 0.D0 , NPOIN2)
       CALL OV( 'X=C     ' , V1 , Y , Z , 0.D0 , NPOIN2)
C
       READ(NDON,*) DAT2
       CALL TEMP(TV2,DAT2,DDC)
       IF (TV2.LT.AT) THEN
         TV1=TV2
         GOTO 90
       ENDIF
       CALL FASP(X,Y,U1,NPOIN2,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
       CALL FASP(X,Y,V1,NPOIN2,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
C
       READ(NDON,*,END=100,ERR=100)
       READ(NDON,20,END=100,ERR=100) (UR(I),I=1,NP)
       READ(NDON,*,END=100,ERR=100)
       READ(NDON,20,END=100,ERR=100) (VR(I),I=1,NP)
       CALL FASP(X,Y,U2,NPOIN2,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
       CALL FASP(X,Y,V2,NPOIN2,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
C
C
      ELSEIF(INDIC.EQ.3) THEN
C
C      -----------------------------------------------------------------
C      TELEMAC FORMAT,
C          VARIABLES 1 AND 2 ARE THE X AND Y COMPONENTS OF THE WIND
C      -----------------------------------------------------------------
C
       REWIND NDON
       ID(1)=1
       ID(2)=2
C
C      READS TITLE
C
       CALL LIT(X,W,IB,TITCAS,72,'CH',NDON,BINDON,ISTAT)
C
C      READS NUMBER OF VARIABLES AND THEIR NAMES
C
       CALL LIT(X,W,IB,C,2,'I ',NDON,BINDON,ISTAT)
       NVAR=IB(1)
       DO I=1,NVAR
         CALL LIT(X,W,IB,TEXTE(I),32,'CH',NDON,BINDON,ISTAT)
       ENDDO
C
C      FORMAT AND GEOMETRY
C
       CALL LIT(X,W,IB,C,10,'I ',NDON,BINDON,ISTAT)
       CALL LIT(X,W,IB,C, 4,'I ',NDON,BINDON,ISTAT)
       NP=IB(2)
       WRITE(LU,*) '--------------------------------------------'
       IF (LNG.EQ.1) THEN
        WRITE(LU,*)'LECDOI : LECTURE DU FICHIER TELEMAC'
        WRITE(LU,*) '         TITRE DU CAS LU : ',TITCAS
        WRITE(LU,*)'         NOMBRE DE POINTS   : ',NP
       ELSE
        WRITE(LU,*)'LECDOI : READING OF TELEMAC DATA FILE '
        WRITE(LU,*) '         FILE TITLE : ',TITCAS
        WRITE(LU,*)'         NUMBER OF POINTS   : ',NP
       ENDIF
       WRITE(LU,*) '--------------------------------------------'
       IF (NP.GT.NPMAX) THEN
        WRITE(LU,*) '**************************************************'
        IF(LNG.EQ.1) THEN
         WRITE(LU,*)
     &             ' LA DIMENSION PREVUE PAR DEFAUT POUR LE TABLEAU DE'
         WRITE(LU,*)
     &             ' DONNEES :',NPMAX,' EST TROP FAIBLE POUR CONTENIR'
         WRITE(LU,*) ' LA TOTALITE DES DONNEES :',NCOL*NLIG
        ELSE
         WRITE(LU,*) ' THE DEFAULT DIMENSION ALLOWED FOR THE ARRAY OF '
         WRITE(LU,*) ' DATA :',NPMAX,' IS TOO LOW TO HOLD'
         WRITE(LU,*) ' ALL THE DATA :',NP
        ENDIF
        WRITE(LU,*) '**************************************************'
        CALL PLANTE(1)
        STOP
       ENDIF
C      ARRAY OF INTEGERS IKLE
       READ(NDON)
C      ARRAY OF INTEGERS IPOBO
       READ(NDON)
C
C      X AND Y
C
       CALL LIT(XRELV,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
       CALL LIT(YRELV,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
C
C      TIME STEP AND VARIABLES
C
       CALL LIT(ATB,W,IB,C,1,'R4',NDON,BINDON,ISTAT)
       IF(CHDON(1:1).EQ.'C') THEN
        TV1=ATB(1)
       ELSE
        ATT=ATB(1)*1.D2
        CALL TEMP(TV1,ATT,DDC)
       ENDIF
       IF (TV1.GT.AT) THEN
        WRITE(LU,*) '*************************************************'
        IF(LNG.EQ.1) THEN
         WRITE(LU,*) ' LE PREMIER ENREGISTREMENT DU FICHIER DE ',CHDON
         WRITE(LU,*) '   ',ATT,' EST POSTERIEUR AU TEMPS '
         WRITE(LU,*) '   DU DEBUT DU CALCUL',DDC
        ELSE
         WRITE(LU,*) ' THE FIRST RECORDING OF THE ',CHDON,' FILE '
         WRITE(LU,*) '   ',ATT,' IS OLDER THAN THE BEGINNING '
         WRITE(LU,*) '   OF THE COMPUTATION',DDC
        ENDIF
        WRITE(LU,*) '*************************************************'
        CALL PLANTE(1)
        STOP
       ENDIF
C
110    CONTINUE
       DO I =1,NVAR
        IF(I.EQ.ID(1)) THEN
         CALL LIT(UR,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
        ELSEIF(I.EQ.ID(2)) THEN
         CALL LIT(VR,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
        ELSE
         READ(NDON)
        ENDIF
       ENDDO
C
       CALL LIT(ATB,W,IB,C,1,'R4',NDON,BINDON,ISTAT)
       IF(CHDON(1:1).EQ.'C') THEN
        TV2=ATB(1)
       ELSE
        ATT=ATB(1)*1.D2
        CALL TEMP(TV2,ATT,DDC)
       ENDIF
       IF (TV2.LT.AT) THEN
        TV1=TV2
        GOTO 110
       ENDIF
C
C      INTERPOLATES IN SPACE THE VARIABLES GIVEN AT TIME 'TV1'
       CALL FASP(X,Y,U1,NPOIN2,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
       CALL FASP(X,Y,V1,NPOIN2,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
C
       DO I =1,NVAR
        IF(I.EQ.ID(1)) THEN
         CALL LIT(UR,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
        ELSEIF(I.EQ.ID(2)) THEN
         CALL LIT(VR,W,IB,C,NP,'R4',NDON,BINDON,ISTAT)
        ELSE
         READ(NDON)
        ENDIF
       ENDDO
       WRITE(LU,*) 'T',CHDON,'1:',TV1
       WRITE(LU,*) 'T',CHDON,'2:',TV2
C
C      INTERPOLATES IN SPACE THE VARIABLES GIVEN AT TIME 'TV2'
       CALL FASP(X,Y,U2,NPOIN2,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
       CALL FASP(X,Y,V2,NPOIN2,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
C
      ELSEIF (INDIC.EQ.4) THEN
C       READS A USER-DEFINED FORMAT
        IF(CHDON(1:1).EQ.'C') THEN
C         READS A CURRENT FIELD
              CALL COUUTI
     &    (X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &     NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NPMAX)
        ELSEIF(CHDON(1:1).EQ.'V' .OR. CHDON(1:1).EQ.'W') THEN
C         READS A WIND FIELD
          CALL VENUTI
     &    (X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &     NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NPMAX)
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'LE TYPE DE DONNEES A LIRE EST INCONNU'
          ELSE
            WRITE(LU,*) 'UNKNOWN DATA'
          ENDIF
            CALL PLANTE(1)
            STOP
        ENDIF
C
      ELSE
        WRITE(LU,*) '************************************************'
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECDOI : INDICATEUR DE FORMAT INCONNU : ',INDIC
        WRITE(LU,*) '         POUR LE FICHIER DE ',CHDON
        ELSE
          WRITE(LU,*)'LECDOI : UNKNOWN INDICATOR OF FORMAT : ',INDIC
          WRITE(LU,*)'         FOR THE ',CHDON,' DATA FILE '
        ENDIF
        WRITE(LU,*) '************************************************'
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C   INTERPOLATES IN TIME
C-----------------------------------------------------------------------
C
      COEF=(AT-TV1)/(TV2-TV1)
      DO I=1,NPOIN2
        UD(I)=(U2(I)-U1(I))*COEF+U1(I)
        VD(I)=(V2(I)-V1(I))*COEF+V1(I)
      ENDDO
C
C-----------------------------------------------------------------------
C
C     FORMATS
C
10    FORMAT (2I4,4F9.3,2I2)
20    FORMAT (10F6.2)
C
C-----------------------------------------------------------------------
C
      DEALLOCATE(W)
C
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
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
