C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE CURRENT / WIND VELOCITY
!>                FOR THE CURRENT TIME STEP AND ON THE COMPUTATION MESH.
!><br>           (INSPIRED FROM SUBROUTINE FOND IN TELEMAC2D)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, BINDON, CHDON, DDC, INDIC, NBOR, NDON, NP, NPOIN, NPTFR, NVAR, TRA, TV1, TV2, U1, U2, UR, UV, V1, V2, VR, VV, X, XRELV, Y, YRELV
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
!>    </th><td> C, C1, COEF, DAT2, DAT2B, I, ID, ISTAT, IW, W, Z
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> COUUTI(), FASP(), LIT(), OV(), PLANTE(), TEMP(), VENUTI()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CORMAR(), SEMIMP()

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
!></td><td>--></td><td>BINAIRE DU FICHIER DE DONNEES
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
!></td><td><-></td><td>NOMBRE DE POINTS DU MAILLAGE DES DONNEES
!>    </td></tr>
!>          <tr><td>NPOIN
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
!></td><td><-></td><td>DONNEES AUX NOEUDS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>UR,VR
!></td><td><-></td><td>TABLEAU DES COURANTS RELEVES
!>    </td></tr>
!>          <tr><td>UV,VV
!></td><td><--</td><td>DONNEE AUX NOEUDS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DU MAILLAGE
!>    </td></tr>
!>          <tr><td>XRELV
!></td><td><--</td><td>TABLEAU DES ABSCISSES DES POINTS RELEVES
!>    </td></tr>
!>          <tr><td>YRELV
!></td><td><--</td><td>TABLEAU DES ORDONNEES DES POINTS RELEVES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE NOUDON
     &(UV , VV , X  , Y  , NPOIN, NDON , BINDON, NBOR, NPTFR,
     & AT , DDC, TV1, TV2, NP   , XRELV, YRELV , UR  , VR   ,
     & TRA, U1 , V1 , U2 , V2   , INDIC, CHDON , NVAR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS
C| BINDON         |-->| BINAIRE DU FICHIER DE DONNEES
C| CHDON          |---| 
C| DDC            |-->| DATE DU DEBUT DU CALCUL
C| INDIC          |-->| TYPE DE FORMAT DE LECTURE
C| NBOR           |-->| NUMEROTATION DES POINTS FRONTIERE
C| NDON           |-->| NUMERO D'UNITE LOGIQUE DU FICHIER DE DONNEES
C| NP             |<->| NOMBRE DE POINTS DU MAILLAGE DES DONNEES
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPTFR          |-->| NOMBRE DE  POINTS FRONTIERE
C| NVAR           |---| 
C| TRA            |---| 
C| TV1            |<->| TEMPS DU CHAMPS DE DONNEES 1
C| TV2            |<->| TEMPS DU CHAMPS DE DONNEES 2
C| U1,V1,U2,V2    |<->| DONNEES AUX NOEUDS DU MAILLAGE
C| UR,VR          |<->| TABLEAU DES COURANTS RELEVES
C| UV,VV          |<--| DONNEE AUX NOEUDS DU MAILLAGE
C| X,Y            |-->| COORDONNEES DU MAILLAGE
C| XRELV          |<--| TABLEAU DES ABSCISSES DES POINTS RELEVES
C| YRELV          |<--| TABLEAU DES ORDONNEES DES POINTS RELEVES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NP,NDON,NPOIN,NPTFR,INDIC,I,ISTAT,NVAR,IW(1)
C
      INTEGER NBOR(NPTFR,2),ID(2)
C
      DOUBLE PRECISION X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION UV(NPOIN),VV(NPOIN),UR(NP),VR(NP)
      DOUBLE PRECISION U1(NPOIN),V1(NPOIN),U2(NPOIN),V2(NPOIN)
      DOUBLE PRECISION XRELV(NP),YRELV(NP),TRA(NP)
      DOUBLE PRECISION AT,TV1,TV2
      DOUBLE PRECISION DDC,DAT2,DAT2B(1),Z(1),C,COEF
C
      CHARACTER*3 BINDON, C1
      CHARACTER*7 CHDON
C
C-----------------------------------------------------------------------
C
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NP))
C
C-----------------------------------------------------------------------
C
      IF (AT.GT.TV2) THEN
C
C       ----------------------------------------------------------------
C       GOES TO NEXT RECORD : 2 BECOMES 1 AND READS A NEW 2
C       ----------------------------------------------------------------
        TV1=TV2
        CALL OV('X=Y     ', U1 , U2 , Z , C , NPOIN)
        CALL OV('X=Y     ', V1 , V2 , Z , C , NPOIN)
C
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) '   NOUDON : LECTURE D''UN NOUVEL ENREGISTREMENT'
        ELSE
          WRITE(LU,*) '   NOUDON : READING A NEW RECORDING'
        ENDIF
C
        IF (INDIC.EQ.1) THEN
C
C     ------------------------------------------------------------------
C          READS A FORMATTED FINITE DIFFERENCES FILE OF TYPE: WAM CYCLE 4
C     ------------------------------------------------------------------
 90        CONTINUE
C          READS THE DATE OF THE RECORD
           READ(NDON,*,END=100,ERR=100) DAT2
           CALL TEMP(TV2,DAT2,DDC)
C          READS THE DATA
           READ(NDON,*,END=100,ERR=100)
           READ(NDON,20,END=100,ERR=100) (UR(I),I=1,NP)
           READ(NDON,*,END=100,ERR=100)
           READ(NDON,20,END=100,ERR=100) (VR(I),I=1,NP)
C
           IF (TV2.LT.AT) THEN
             IF(LNG.EQ.1) THEN
               WRITE(LU,*) ' NOUDON : ON SAUTE 1 ENREGISTREMENT ..'
             ELSE
               WRITE(LU,*) ' NOUDON : JUMP OF 1 RECORDED DATA SERIES'
             ENDIF
             TV1=TV2
             CALL FASP(X,Y,U1,NPOIN,XRELV,YRELV,UR,NP,NBOR,
     &                                       MESH%KP1BOR%I,NPTFR,0.D0)
             CALL FASP(X,Y,V1,NPOIN,XRELV,YRELV,VR,NP,NBOR,
     &                                       MESH%KP1BOR%I,NPTFR,0.D0)
             GOTO 90
           ENDIF
C
           CALL FASP(X,Y,U2,NPOIN,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
           CALL FASP(X,Y,V2,NPOIN,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
C
        ELSEIF (INDIC.EQ.3) THEN
C
C     ------------------------------------------------------------------
C       READS A SELAFIN FILE OF TYPE: TELEMAC
C     ------------------------------------------------------------------
C
        ID(1)=1
        ID(2)=2
 95     CONTINUE
C       READS THE DATE OF THE RECORD
        CALL LIT(DAT2B,W,IW,C1,1,'R4',NDON,BINDON,ISTAT)
        IF(CHDON(1:1).EQ.'C') THEN
         TV2=DAT2B(1)
        ELSE
         DAT2=DAT2B(1)*1.D2
         CALL TEMP(TV2,DAT2,DDC)
        ENDIF
C      READS THE DATA
       DO I =1,NVAR
        IF(I.EQ.ID(1)) THEN
         CALL LIT(UR,W,IW,C1,NP,'R4',NDON,BINDON,ISTAT)
        ELSEIF(I.EQ.ID(2)) THEN
         CALL LIT(VR,W,IW,C1,NP,'R4',NDON,BINDON,ISTAT)
        ELSE
         READ(NDON)
        ENDIF
       ENDDO
C
        IF (TV2.LT.AT) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) ' NOUDON : ON SAUTE 1 ENREGISTREMENT ..'
          ELSE
            WRITE(LU,*) ' NOUDON : JUMP OF 1 RECORDED DATA SERIES'
          ENDIF
          TV1=TV2
C         INTERPOLATES IN SPACE
          CALL FASP(X,Y,U1,NPOIN,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
          CALL FASP(X,Y,V1,NPOIN,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
          GOTO 95
        ENDIF
C
        WRITE(LU,*) 'T',CHDON,'1:',TV1
        WRITE(LU,*) 'T',CHDON,'2:',TV2
C
C       INTERPOLATES IN SPACE
        CALL FASP(X,Y,U2,NPOIN,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
        CALL FASP(X,Y,V2,NPOIN,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
C
        ELSEIF (INDIC.EQ.4) THEN
C
C     ------------------------------------------------------------------
C        READS A USER-DEFINED FILE FORMAT
C     ------------------------------------------------------------------
C
          IF(CHDON(1:1).EQ.'C') THEN
          CALL COUUTI
     &    (X,Y,NPOIN,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &     NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NP)
          ELSE
          CALL VENUTI
     &    (X,Y,NPOIN,NDON,BINDON,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &     NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NP)
          ENDIF
C
C
        ELSE
C
        WRITE(LU,*) '************************************************'
        IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'NOUDON : INDICATEUR DE FORMAT INCONNU : ',INDIC
        ELSE
          WRITE(LU,*)'NOUDON : UNKNOWN INDICATOR OF FORMAT : ',INDIC
        ENDIF
        WRITE(LU,*) '************************************************'
        CALL PLANTE(1)
        ENDIF
C
      ENDIF
C
C       --------------------------------------------------------------
C          INTERPOLATES
C       --------------------------------------------------------------
C
      COEF=(AT-TV1)/(TV2-TV1)
      DO 60 I=1,NPOIN
         UV(I)=(U2(I)-U1(I))*COEF+U1(I)
         VV(I)=(V2(I)-V1(I))*COEF+V1(I)
60    CONTINUE
C
C-----------------------------------------------------------------------
C
C     FORMATS
C
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
      DEALLOCATE(W)
      CALL PLANTE(1)
C
      RETURN
      END
C
C#######################################################################
C