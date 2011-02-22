C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       THIS SUBROUTINE PROJECTS THE WINDS ON THE COMPUTATION
!>                MESH AND INTERPOLATES TO FIRST TIME STEP.
!><br>           (INSPIRED FROM SUBROUTINE FOND IN TELEMAC2D AMONGST OTHERS)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, BINVEN, DDC, INDIC, ITR01, NBOR, NP, NPMAX, NPOIN, NPTFR, NVEN, TV1, TV2, U1, U2, UV, V1, V2, VV, X, XRELV, Y, YRELV
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
!>    </th><td> ATB, ATT, BID, C, COEF, DAT1, DAT2, DX, DY, I, IB, ISTAT, J, NCOL, NI, NLIG, NVAR, TEXTE, TITCAS, UR, VR, W, XMAX, XMIN, YMAX, YMIN, Z
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FASP(), LIT(), OV(), PLANTE(), TEMP(), VENUTI()
!>   </td></tr>
!>     </table>

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
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>BINVEN
!></td><td>--></td><td>BINAIRE DU FICHIER DES VENTS (SI INDIC>=2)
!>    </td></tr>
!>          <tr><td>DDC
!></td><td>--></td><td>DATE DU DEBUT DU CALCUL
!>    </td></tr>
!>          <tr><td>INDIC
!></td><td>--></td><td>TYPE DE FORMAT DE LECTURE
!>    </td></tr>
!>          <tr><td>ITR01
!></td><td>--></td><td>TABLEAU DE TRAVAIL ENTIER
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROTATION DES POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NP
!></td><td><-></td><td>NOMBRE DE POINTS RELEVES
!>    </td></tr>
!>          <tr><td>NPMAX
!></td><td>--></td><td>NOMBRE DE POINTS RELEVES MAXIMUM
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE  POINTS FRONTIERE
!>    </td></tr>
!>          <tr><td>NVEN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TV1
!></td><td><-></td><td>TEMPS DU CHAMPS DE VENT 1
!>    </td></tr>
!>          <tr><td>TV2
!></td><td><-></td><td>TEMPS DU CHAMPS DE VENT 2
!>    </td></tr>
!>          <tr><td>U1,V1,U2,V2
!></td><td><-></td><td>VENT AUX NOEUDS DU MAILLAGE DU VENT
!>    </td></tr>
!>          <tr><td>UR,VR
!></td><td><-></td><td>TABLEAU DES COURANTS RELEVES
!>    </td></tr>
!>          <tr><td>UV,VV
!></td><td><--</td><td>VENT AUX NOEUDS DU MAILLAGE
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
                        SUBROUTINE INIVEN
     &(UV,VV,X,Y,NPOIN,NVEN, BINVEN,NBOR,NPTFR,AT,DDC,TV1,TV2,
     & NP,XRELV,YRELV,U1,V1,U2,V2,INDIC,NPMAX,ITR01)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS
C| BINVEN         |-->| BINAIRE DU FICHIER DES VENTS (SI INDIC>=2)
C| DDC            |-->| DATE DU DEBUT DU CALCUL
C| INDIC          |-->| TYPE DE FORMAT DE LECTURE
C| ITR01          |-->| TABLEAU DE TRAVAIL ENTIER
C| NBOR           |-->| NUMEROTATION DES POINTS FRONTIERE
C| NP             |<->| NOMBRE DE POINTS RELEVES
C| NPMAX          |-->| NOMBRE DE POINTS RELEVES MAXIMUM
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPTFR          |-->| NOMBRE DE  POINTS FRONTIERE
C| NVEN           |---| 
C| TV1            |<->| TEMPS DU CHAMPS DE VENT 1
C| TV2            |<->| TEMPS DU CHAMPS DE VENT 2
C| U1,V1,U2,V2    |<->| VENT AUX NOEUDS DU MAILLAGE DU VENT
C| UR,VR          |<->| TABLEAU DES COURANTS RELEVES
C| UV,VV          |<--| VENT AUX NOEUDS DU MAILLAGE
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
      INTEGER NP,NVEN,NPOIN,NPTFR,INDIC,NCOL,NLIG,BID,I,J
      INTEGER NVAR,NI,ISTAT,IB(10),ITR01(*)
C
      INTEGER NPMAX,NBOR(NPTFR,2)
C
      DOUBLE PRECISION X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION UV(NPOIN),VV(NPOIN)
      DOUBLE PRECISION XRELV(NPMAX),YRELV(NPMAX)
      DOUBLE PRECISION U1(NPMAX),V1(NPMAX),U2(NPMAX),V2(NPMAX)
      DOUBLE PRECISION XMAX,XMIN,YMAX,YMIN,DX,DY,AT,TV1,TV2
      DOUBLE PRECISION DDC,DAT1,DAT2,COEF,Z(1),ATT, ATB(1)
C
      CHARACTER*3 BINVEN,C
      CHARACTER*72 TITCAS
      CHARACTER*32 TEXTE(10)
C
      DOUBLE PRECISION, ALLOCATABLE :: UR(:),VR(:)
      REAL, ALLOCATABLE :: W(:)
      ALLOCATE(W(MAX(NPMAX,72)))
C
C-----------------------------------------------------------------------
C        READS THE POINTS FROM LOGICAL UNIT NVEN
C-----------------------------------------------------------------------
C
      IF(INDIC.EQ.1) THEN
C
      REWIND NVEN
C
C     ------------------------------------------------------------------
C     WAM FORMAT, FINITE DIFFERENCES + INTERPOLATION TO THE MESH POINTS
C
C     ------------------------------------------------------------------
C
       READ(NVEN,10,END=100,ERR=100)
     & NCOL,NLIG,YMIN,YMAX,XMIN,XMAX,BID,BID
       DX=(XMAX-XMIN)/REAL(NCOL-1)
       DY=(YMAX-YMIN)/REAL(NLIG-1)
       NP=NCOL*NLIG
       ALLOCATE(UR(1:NPMAX),VR(1:NPMAX))
       WRITE(LU,*) '---------------------------------------------------'
       WRITE(LU,*) 'INIVEN : LECTURE DU FICHIER DE VENT'
       WRITE(LU,*) '         NOMBRE DE LIGNES   : ',NLIG
       WRITE(LU,*) '         NOMBRE DE COLONNES :',NCOL
       WRITE(LU,*) '         ABSCISSE OU LONGITUDE MINIMALE : ',XMIN
       WRITE(LU,*) '         ABSCISSE OU LONGITUDE MAXIMALE : ',XMAX
       WRITE(LU,*) '         ORDONNEE OU LATITUDE MINIMALE  : ',YMIN
       WRITE(LU,*) '         ORDONNEE OU LATITUDE MAXIMALE  : ',YMAX
       IF (NP.GT.NPMAX) THEN
        WRITE(LU,*) '**************************************************'
        WRITE(LU,*) ' LA DIMENSION PREVUE PAR DEFAUT POUR LE TABLEAU   '
        WRITE(LU,*) ' DE VENT :',NPMAX,' EST TROP FAIBLE POUR CONTENIR'
        WRITE(LU,*) ' CONTENIR LA TOTALITE DES DONNEES :',NCOL*NLIG
        WRITE(LU,*) '**************************************************'
        CALL PLANTE(0)
       ENDIF
       READ(NVEN,*) DAT1
       CALL TEMP(TV1,DAT1,DDC)
       IF (TV1.GT.AT) THEN
        WRITE(LU,*) '******************************************'
        WRITE(LU,*) ' LE PREMIER ENREGISTREMENT DU FICHIER DES'
        WRITE(LU,*) ' VENTS : ',DAT1,' EST POSTERIEUR AU TEMPS'
        WRITE(LU,*) ' DU DEBUT DU CALCUL',DDC
        WRITE(LU,*) '******************************************'
        CALL PLANTE(0)
       ENDIF
C
       DO 50 I=1,NCOL
          DO 40 J=1,NLIG
                XRELV((I-1)*NLIG+J)=XMIN+DX*(I-1)
                YRELV((I-1)*NLIG+J)=YMIN+DY*(J-1)
40        CONTINUE
50     CONTINUE
C
90    CONTINUE
      READ(NVEN,*,END=100,ERR=100)
      READ(NVEN,20,END=100,ERR=100)
     &       (UR(I),I=1,NP)
          READ(NVEN,*)
         READ(NVEN,20,END=100,ERR=100)
     &       (VR(I),I=1,NP)
          CALL OV( 'X=C     ' , U1 , Y , Z , 0.D0 , NPMAX)
          CALL OV( 'X=C     ' , V1 , Y , Z , 0.D0 , NPMAX)
          CALL FASP(X,Y,U1,NPOIN,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
          CALL FASP(X,Y,V1,NPOIN,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
C
           READ(NVEN,*) DAT2
           CALL TEMP(TV2,DAT2,DDC)
           IF (TV2.LT.AT) THEN
              TV1=TV2
              GOTO 90
           ENDIF
C
          READ(NVEN,*,END=100,ERR=100)
          READ(NVEN,20,END=100,ERR=100)
     &      (UR(I),I=1,NP)
        READ(NVEN,*,END=100,ERR=100)
        READ(NVEN,20,END=100,ERR=100)
     &      (VR(I),I=1,NP)
          CALL FASP(X,Y,U2,NPOIN,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
          CALL FASP(X,Y,V2,NPOIN,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,
     &                                                    NPTFR,0.D0)
C
C
      ELSEIF (INDIC.EQ.2) THEN
C
       REWIND NVEN
C
C     ------------------------------------------------------------------
C     TELEMAC FORMAT, DISCRETISATION ON THE SAME MESH
C         VARIABLES 1 AND 2 ARE THE X AND Y COMPONENTS OF THE WIND
C     ------------------------------------------------------------------
C
C      READS TITLE
C
       CALL LIT(X,W,IB,TITCAS,72,'CH',NVEN,BINVEN,ISTAT)
C
C      READS NUMBER OF VARIABLES AND THEIR NAMES
C
       CALL LIT(X,W,IB,C,2,'I ',NVEN,BINVEN,ISTAT)
       NVAR=IB(1)
       DO 80 I=1,NVAR
          CALL LIT(X,W,IB,TEXTE(I),32,'CH',NVEN,BINVEN,ISTAT)
80     CONTINUE
C
C      FORMAT AND GEOMETRY
C
       CALL LIT(X,W,IB,C,10,'I ',NVEN,BINVEN,ISTAT)
       CALL LIT(X,W,IB,C, 4,'I ',NVEN,BINVEN,ISTAT)
       NP=IB(2)
       NI=IB(1)*IB(3)
       WRITE(LU,*) '--------------------------------------------'
       WRITE(LU,*) 'INIVEN : LECTURE DU FICHIER TELEMAC'
       WRITE(LU,*) '         TITRE DU CAS LU : ',TITCAS
       WRITE(LU,*) '         NOMBRE DE POINTS   : ',NP
       WRITE(LU,*) '--------------------------------------------'
       IF (NP.NE.NPOIN) THEN
       WRITE(LU,*) '***************************************************'
       WRITE(LU,*)'VOUS UTILISEZ UN FORMAT DE LECTURE TELEMAC SUPPOSANT'
       WRITE(LU,*)' L''EGALITE DES MAILLAGES LUS ET UTILISES. CECI EST '
       WRITE(LU,*)' IMPOSSIBLE CAR LE NOMBRE DE POINTS LUS :',NP,' EST'
       WRITE(LU,*)' DIFFERENT DU NOMBRE DE POINTS DU MAILLAGE :',NPOIN
       WRITE(LU,*) '***************************************************'
       CALL PLANTE(1)
       ENDIF
       ALLOCATE(UR(1:NPMAX),VR(1:NPMAX))
       CALL LIT(X,W,ITR01,C,NI,'I ',NVEN,BINVEN,ISTAT)
       CALL LIT(X,W,ITR01,C,NP,'I ',NVEN,BINVEN,ISTAT)
C
C      X AND Y
C
       CALL LIT(XRELV,W,IB,C,NP,'R4',NVEN,BINVEN,ISTAT)
       CALL LIT(YRELV,W,IB,C,NP,'R4',NVEN,BINVEN,ISTAT)
C
C      TIME STEP AND VARIABLES
C
       CALL LIT(ATB,W,IB,C,1,'R4',NVEN,BINVEN,ISTAT)
       ATT=ATB(1)*1.D2
       CALL TEMP(TV1,ATT,DDC)
       IF (TV1.GT.AT) THEN
          WRITE(LU,*) 'ERREUR DEMARAGE LECTURE',TV1,AT
        CALL PLANTE(0)
          ENDIF
110       CONTINUE
        CALL LIT(U1,W,IB,C,NP,'R4',NVEN,BINVEN,ISTAT)
        CALL LIT(V1,W,IB,C,NP,'R4',NVEN,BINVEN,ISTAT)
C
        CALL LIT(ATB,W,IB,C,1,'R4',NVEN,BINVEN,ISTAT)
        ATT=ATB(1)*1.D2
        CALL TEMP(TV2,ATT,DDC)
        IF (TV2.LT.AT) THEN
           TV1=TV2
           GOTO 110
          ENDIF
          CALL LIT(U2,W,IB,C,NP,'R4',NVEN,BINVEN,ISTAT)
          CALL LIT(V2,W,IB,C,NP,'R4',NVEN,BINVEN,ISTAT)
        WRITE(LU,*) 'TVENT1:',TV1
        WRITE(LU,*) 'TVENT2:',TV2
C
C
      ELSEIF (INDIC.EQ.3) THEN
C
        ALLOCATE(UR(1:NPMAX),VR(1:NPMAX))
        CALL VENUTI
     & (X,Y,NPOIN,NVEN,BINVEN,NBOR,NPTFR,AT,DDC,TV1,TV2,
     &  NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NPMAX)
C
      ELSE
C
        WRITE(LU,*) '***********************************************'
        WRITE(LU,*) 'INIVEN : INDICATEUR DE FORMAT INCONNU   '
        WRITE(LU,*) '         POUR LE FICHIER DES VENTS :',INDIC
        WRITE(LU,*) '***********************************************'
        CALL PLANTE(1)
      ENDIF
C
C-----------------------------------------------------------------------
C   INTERPOLATES
C-----------------------------------------------------------------------
C
        COEF=(AT-TV1)/(TV2-TV1)
        DO 120 I=1,NPOIN
           UV(I)=(U2(I)-U1(I))*COEF+U1(I)
           VV(I)=(V2(I)-V1(I))*COEF+V1(I)
120     CONTINUE
C
C-----------------------------------------------------------------------
C
      DEALLOCATE(UR,VR)
C
C     FORMATS
C
10    FORMAT (2I4,4F9.3,2I2)
20    FORMAT (10F6.2)
C
      RETURN
C
C     IF FAILED TO READ THE FILE ...
C
100   CONTINUE
      WRITE(LU,*) '*********************************************'
      WRITE(LU,*) '  ERREUR A LA LECTURE DU FICHIER DE VENT     '
      WRITE(LU,*) '      OU FIN DE FICHIER PREMATUREE           '
      WRITE(LU,*) '*********************************************'
      CALL PLANTE(1)
C
C-----------------------------------------------------------------------
C
      DEALLOCATE(W)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C