C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE TIDE FOR THE CURRENT TIME STEP
!>                AND ON THE COMPUTATION MESH.
!><br>           (INSPIRED FROM SUBROUTINE FOND IN TELEMAC2D)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, BINDON, DDC, DZHDT, IDHMA, INDIM, NBOR, NDON, NP, NPOIN2, NPTFR, NVHMA, TM1, TM2, TRA, X, XRELV, Y, YRELV, Z1, Z2, ZM, ZR
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
!>    </th><td> ATT, C, C1, COE1, COE2, DAT2, DAT2B, I, ISTAT, IW, W, Z
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FASP(), LIT(), MARUTI(), OV(), PLANTE(), TEMP()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CORMAR()

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
!>          <tr><td>DDC
!></td><td>--></td><td>DATE DU DEBUT DU CALCUL
!>    </td></tr>
!>          <tr><td>DZHDT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IDHMA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INDIM
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
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE  POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NVHMA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TM1
!></td><td><-></td><td>TEMPS DU CHAMPS DE DONNEES 1
!>    </td></tr>
!>          <tr><td>TM2
!></td><td><-></td><td>TEMPS DU CHAMPS DE DONNEES 2
!>    </td></tr>
!>          <tr><td>TRA
!></td><td>---</td><td>
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
!>          <tr><td>Z1,Z2
!></td><td><-></td><td>DONNEES AUX NOEUDS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>ZM
!></td><td><--</td><td>DONNEE AUX NOEUDS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>ZR
!></td><td><-></td><td>TABLEAU DES COURANTS RELEVES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE NOUMAR
     &(ZM , DZHDT, X  , Y  , NPOIN2, NDON , BINDON, NBOR , NPTFR,
     & AT , DDC  , TM1, TM2, NP    , XRELV, YRELV , ZR   , TRA  ,
     & Z1 , Z2   , INDIM, IDHMA , NVHMA )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS
C| BINDON         |-->| BINAIRE DU FICHIER DE DONNEES
C| DDC            |-->| DATE DU DEBUT DU CALCUL
C| DZHDT          |---| 
C| IDHMA          |---| 
C| INDIM          |-->| TYPE DE FORMAT DE LECTURE
C| NBOR           |-->| NUMEROTATION DES POINTS FRONTIERE
C| NDON           |-->| NUMERO D'UNITE LOGIQUE DU FICHIER DE DONNEES
C| NP             |<->| NOMBRE DE POINTS DU MAILLAGE DES DONNEES
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPTFR          |-->| NOMBRE DE  POINTS FRONTIERE
C| NVHMA          |---| 
C| TM1            |<->| TEMPS DU CHAMPS DE DONNEES 1
C| TM2            |<->| TEMPS DU CHAMPS DE DONNEES 2
C| TRA            |---| 
C| X,Y            |-->| COORDONNEES DU MAILLAGE
C| XRELV          |<--| TABLEAU DES ABSCISSES DES POINTS RELEVES
C| YRELV          |<--| TABLEAU DES ORDONNEES DES POINTS RELEVES
C| Z1,Z2          |<->| DONNEES AUX NOEUDS DU MAILLAGE
C| ZM             |<--| DONNEE AUX NOEUDS DU MAILLAGE
C| ZR             |<->| TABLEAU DES COURANTS RELEVES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TOMAWAC ,ONLY : MESH
      USE INTERFACE_TOMAWAC, EX_NOUMAR => NOUMAR
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NP,NDON,NPOIN2,NPTFR,INDIM,I,ISTAT,IW(1)
C
      INTEGER NBOR(NPTFR,2), IDHMA,NVHMA
C
      DOUBLE PRECISION X(NPOIN2) ,Y(NPOIN2)
      DOUBLE PRECISION ZM(NPOIN2),ZR(NP), DZHDT(NPOIN2)
      DOUBLE PRECISION Z1(NPOIN2),Z2(NPOIN2)
      DOUBLE PRECISION XRELV(NP),YRELV(NP),TRA(NP)
      DOUBLE PRECISION AT,TM1,TM2
      DOUBLE PRECISION DDC,DAT2,DAT2B(1),Z(1),C,COE1,COE2,ATT
C
      CHARACTER*3 BINDON, C1
C
C-----------------------------------------------------------------------
C
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(NP))
C
C-----------------------------------------------------------------------
C
      IF (AT.GE.TM2) THEN
C
C       ----------------------------------------------------------------
C       GOES TO NEXT RECORD : 2 BECOMES 1 AND READS A NEW 2
C       ----------------------------------------------------------------
        TM1=TM2
        CALL OV('X=Y     ', Z1 , Z2 , Z , C , NPOIN2)
C
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) '   NOUMAR : LECTURE D''UN NOUVEL ENREGISTREMENT'
          WRITE(LU,*) '            DE LA HAUTEUR DE LA MAREE          '
        ELSE
          WRITE(LU,*) '   NOUMAR : READING A NEW RECORDING '
          WRITE(LU,*) '            OF THE TIDE LEVEL       '
        ENDIF
C
        IF (INDIM.EQ.1) THEN
C
C     ------------------------------------------------------------------
C          READS A FORMATTED FINITE DIFFERENCES FILE OF TYPE: WAM CYCLE 4
C     ------------------------------------------------------------------
 90        CONTINUE
C          READS THE DATE OF THE RECORD
           READ(NDON,*,END=100,ERR=100) DAT2
           CALL TEMP(TM2,DAT2,DDC)
C          READS THE DATA
           READ(NDON,*,END=100,ERR=100)
           READ(NDON,20,END=100,ERR=100) (ZR(I),I=1,NP)
C
           IF (TM2.LE.AT) THEN
             IF(LNG.EQ.1) THEN
               WRITE(LU,*) ' NOUMAR : ON SAUTE 1 ENREGISTREMENT ..'
             ELSE
               WRITE(LU,*) ' NOUMAR : JUMP OF 1 RECORDED DATA SERIES'
             ENDIF
             TM1=TM2
             CALL FASP(X,Y,Z1,NPOIN2,XRELV,YRELV,ZR,NP,NBOR,
     &                                         MESH%KP1BOR%I,NPTFR,0.D0)
             GOTO 90
           ENDIF
C
           CALL FASP(X,Y,Z2,NPOIN2,XRELV,YRELV,ZR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
C
        ELSEIF (INDIM.EQ.2) THEN
C
C     ------------------------------------------------------------------
C       READS A SELAFIN FILE OF TYPE: TELEMAC
C     ------------------------------------------------------------------
C
 95     CONTINUE
C       READS THE DATE OF THE RECORD
        CALL LIT(DAT2B,W,IW,C1,1,'R4',NDON,BINDON,ISTAT)
        TM2=DAT2B(1)
C       READS THE DATA
        DO I =1,NVHMA
          IF(I.EQ.IDHMA) THEN
            CALL LIT(ZR,W,IW,C1,NP,'R4',NDON,BINDON,ISTAT)
          ELSE
            READ(NDON)
          ENDIF
        ENDDO
C
        IF (TM2.LE.AT) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) ' NOUMAR : ON SAUTE 1 ENREGISTREMENT ..'
          ELSE
            WRITE(LU,*) ' NOUMAR : JUMP OF 1 RECORDED DATA SERIES'
          ENDIF
          TM1=TM2
C         INTERPOLATES IN SPACE (TIME 1)
          CALL FASP(X,Y,Z1,NPOIN2,XRELV,YRELV,ZR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
          GOTO 95
        ENDIF
C
        WRITE(LU,*) 'TMENT1=',TM1
        WRITE(LU,*) 'TMENT2=',TM2
C       INTERPOLATES IN SPACE (TIME 2)
        CALL FASP(X,Y,Z2,NPOIN2,XRELV,YRELV,ZR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,1.D-6)
C
        ELSEIF (INDIM.EQ.3) THEN
C
C     ------------------------------------------------------------------
C        READS A USER-DEFINED FILE FORMAT
C     ------------------------------------------------------------------
C
          CALL MARUTI
     &    (X,Y,NPOIN2,NDON,BINDON,NBOR,NPTFR,AT,DDC,TM1,TM2,
     &     NP,XRELV,YRELV,ZR,Z1,Z2,NP)
C
C
        ELSE
C
        WRITE(LU,*) '************************************************'
        IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'NOUMAR : INDICATEUR DE FORMAT INCONNU : ',INDIM
        ELSE
          WRITE(LU,*)'NOUMAR : UNKNOWN INDICATOR OF FORMAT : ',INDIM
        ENDIF
        WRITE(LU,*) '************************************************'
        CALL PLANTE(1)
        ENDIF
C
      ENDIF
C
C     -------------------------------------------------
C       INTERPOLATES IN TIME
C       AND COMPUTES THE TEMPORAL GRADIENT OF THE TIDE
C     -------------------------------------------------
C
      COE1=(TM2-TM1)
      IF (COE1.LT.1.D-4) THEN
         WRITE(LU,*) '****************************************'
         IF(LNG.EQ.1) THEN
           WRITE(LU,*) ' DEUX TEMPS IDENTIQUES                '
           WRITE(LU,*) ' DANS LE FICHIER DES HAUTEURS DE MAREE'
         ELSE
           WRITE(LU,*) ' TWO IDENTICAL TIMES IN THE TIDAL FILE'
         ENDIF
         WRITE(LU,*) '****************************************'
         CALL PLANTE(0)
      ENDIF
      COE2=(AT-TM1)/COE1
      DO I=1,NPOIN2
         ATT     = (Z2(I)-Z1(I))
         ZM(I)   = ATT*COE2+Z1(I)
         DZHDT(I)= ATT/COE1
      ENDDO
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
         WRITE(LU,*)'  ERREUR A LA LECTURE DU FICHIER DE MAREE '
         WRITE(LU,*)'      OU FIN DE FICHIER PREMATUREE        '
      ELSE
         WRITE(LU,*)'  ERROR WHILE READING DATA FILE '
         WRITE(LU,*)'    OR UNEXPECTED END OF FILE           '
      ENDIF
      WRITE(LU,*)'*********************************************'
      CALL PLANTE(0)
C
      RETURN
      END
C
C#######################################################################
C
