C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES FLUXES THROUGH CONTROL SECTIONS
!>                AND SUMS THEM UP TO OBTAIN OSCILLATING VOLUMES.
!><br>            GRIDS OF DIMENSION 2 AND WATER DEPTH CONSIDERED.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BM1, BM2, COMFLU, CTRLSC, CUMFLO, CV1, DT, H, HPROP, IFABOR, IKLE, INFO, KNOGL, MESH, MSKSEC, NCP, NELEM, NELMAX, S, T1, TPS, U, V, X, XEL, Y, YEL
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ARR, DEJA, DEP, DIST, DIST1, DIST2, DIST3, ELBEST, ERR, FLX, H1, H2, I1, I2, I3, IEL, IELEM, IELMH, IELMU, IGBEST, ILBEST, ILPREC, ISEC, ISEG, J1, J2, J3, LISTE, NSEC, NSEG, NSEMAX, NX, NY, OK, PT, SUR6, UN1, UN2, VOLNEG, VOLPOS, X1, X2, Y1, Y2
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FLUSEC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FLUXPR(), MATRIX(), MATVEC(), PLANTE()
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
!>      <td><center> 5.8                                       </center>
!> </td><td> 14/01/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BM2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COMFLU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CTRLSC
!></td><td>--></td><td>DONNEES SUR LES SECTIONS DE CONTROLE.
!>    </td></tr>
!>          <tr><td>CUMFLO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CV1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS.
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>HPROP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IFABOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>TABLEAUX DE CONNECTIVITE LOCAL-GLOBAL
!>    </td></tr>
!>          <tr><td>INFO
!></td><td>--></td><td>SI OUI : IMPRESSIONS.
!>    </td></tr>
!>          <tr><td>KNOGL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSKSEC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NCP
!></td><td>--></td><td>TWO TIMES THE NUMBER OF CONTROL SECTIONS
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TPS
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>CHAMP DE VITESSE
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>XEL,YEL
!></td><td>--></td><td>COORDONNEES DES POINTS PAR ELEMENT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLUSEC
     &(U,V,H,IKLE,XEL,YEL,NELMAX,NELEM,X,Y,DT,NCP,CTRLSC,INFO,TPS,
     & KNOGL,MSKSEC,BM1,BM2,T1,HPROP,MESH,S,CV1,IFABOR,COMFLU,CUMFLO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BM1            |---| 
C| BM2            |---| 
C| COMFLU         |---| 
C| CTRLSC         |-->| DONNEES SUR LES SECTIONS DE CONTROLE.
C| CUMFLO         |---| 
C| CV1            |---| 
C| DT             |-->| PAS DE TEMPS.
C| H             |-->| HAUTEUR D'EAU
C| HPROP          |---| 
C| IFABOR         |---| 
C| IKLE           |-->| TABLEAUX DE CONNECTIVITE LOCAL-GLOBAL
C| INFO           |-->| SI OUI : IMPRESSIONS.
C| KNOGL          |---| 
C| MESH           |---| 
C| MSKSEC         |---| 
C| NCP            |-->| TWO TIMES THE NUMBER OF CONTROL SECTIONS
C| NELEM          |-->| NOMBRE D'ELEMENTS.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS.
C| S             |---| 
C| T1             |---| 
C| TPS            |-->| TEMPS
C| U,V            |-->| CHAMP DE VITESSE
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE
C| XEL,YEL        |-->| COORDONNEES DES POINTS PAR ELEMENT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_FLUSEC => FLUSEC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELMAX,NELEM,NCP
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*),CTRLSC(NCP),KNOGL(*)
      INTEGER, INTENT(IN) :: IFABOR(NELMAX,*)
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),TPS,DT
      DOUBLE PRECISION, INTENT(IN) :: XEL(NELMAX,*),YEL(NELMAX,*)
      LOGICAL, INTENT(IN) :: INFO,COMFLU,CUMFLO
      TYPE(BIEF_OBJ), INTENT(IN)    :: HPROP,S,U,V,H
      TYPE(BIEF_OBJ), INTENT(INOUT) :: BM1,BM2,T1,MSKSEC,CV1
      TYPE(BIEF_MESH) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NSEMAX,ERR
      PARAMETER(NSEMAX=50)
C
      INTEGER IELEM,IEL,I1,I2,I3,J1,J2,J3,ELBEST,IGBEST,ILBEST,IELMH
      INTEGER ILPREC,ISEG,ISEC,NSEC,PT,DEP,ARR,IELMU
C
      DOUBLE PRECISION DIST,DIST1,DIST2,DIST3
      DOUBLE PRECISION H1,H2,X1,Y1,X2,Y2,UN1,UN2,NX,NY,SUR6
C
      LOGICAL OK
C
      DOUBLE PRECISION, ALLOCATABLE :: FLX(:),VOLNEG(:),VOLPOS(:)
      INTEGER, ALLOCATABLE :: NSEG(:),LISTE(:,:,:)
C
      LOGICAL DEJA
      DATA DEJA/.FALSE./
C
      SAVE LISTE,DEJA,NSEG,VOLNEG,VOLPOS,FLX
C
C-----------------------------------------------------------------------
C
      SUR6=1.D0/6.D0
      NSEC = NCP/2
C
C  LOOKS FOR PATHS CONNECTING THE POINT PAIRS:
C
      IF(.NOT.DEJA) THEN
C
C     DYNAMICALLY ALLOCATES FLX, VOLNEG, VOLPOS
C
      ALLOCATE(FLX(NCP)             ,STAT=ERR)
      ALLOCATE(VOLNEG(NCP)          ,STAT=ERR)
      ALLOCATE(VOLPOS(NCP)          ,STAT=ERR)
      ALLOCATE(NSEG(NCP)            ,STAT=ERR)
      ALLOCATE(LISTE(NCP,NSEMAX,2)  ,STAT=ERR)
C
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,100) ERR
        IF(LNG.EQ.2) WRITE(LU,200) ERR
100     FORMAT(1X,'FLUSEC : ERREUR A L''ALLOCATION DE MEMOIRE : ',/,1X,
     &            'CODE D''ERREUR : ',1I6)
200     FORMAT(1X,'FLUSEC: ERROR DURING ALLOCATION OF MEMORY: ',/,1X,
     &            'ERROR CODE: ',1I6)
      ENDIF
C
      DO 60 ISEC =1,NSEC
C
        DEP = CTRLSC(1+2*(ISEC-1))
        ARR = CTRLSC(2+2*(ISEC-1))
        VOLNEG(ISEC)=0.D0
        VOLPOS(ISEC)=0.D0
        IF(NCSIZE.GT.1) THEN
          DEP=KNOGL(DEP)
          ARR=KNOGL(ARR)
          IF(DEP.EQ.0.AND.ARR.EQ.0) THEN
            NSEG(ISEC)=0
            GO TO 60
          ENDIF
          IF((DEP.EQ.0.AND.ARR.NE.0).OR.(DEP.NE.0.AND.ARR.EQ.0)) THEN
            NSEG(ISEC)=-1
            GO TO 60
          ENDIF
        ENDIF
        PT = DEP
        ISEG = 0
        DIST=(X(DEP)-X(ARR))**2+(Y(DEP)-Y(ARR))**2
10      CONTINUE
C
        DO 20 IELEM =1,NELEM
C
          I1 = IKLE(IELEM,1)
          I2 = IKLE(IELEM,2)
          I3 = IKLE(IELEM,3)
C         IF THE ELEMENT CONTAINS THE CURRENT POINT:
          IF(PT.EQ.I1.OR.PT.EQ.I2.OR.PT.EQ.I3) THEN
            DIST1 = (X(I1)-X(ARR))**2 + (Y(I1)-Y(ARR))**2
            DIST2 = (X(I2)-X(ARR))**2 + (Y(I2)-Y(ARR))**2
            DIST3 = (X(I3)-X(ARR))**2 + (Y(I3)-Y(ARR))**2
            IF(DIST1.LT.DIST) THEN
              DIST = DIST1
              ELBEST = IELEM
              IGBEST = I1
              ILBEST = 1
              IF(I1.EQ.PT) ILPREC = 1
              IF(I2.EQ.PT) ILPREC = 2
              IF(I3.EQ.PT) ILPREC = 3
            ENDIF
            IF(DIST2.LT.DIST) THEN
              DIST = DIST2
              ELBEST = IELEM
              IGBEST = I2
              ILBEST = 2
              IF(I1.EQ.PT) ILPREC = 1
              IF(I2.EQ.PT) ILPREC = 2
              IF(I3.EQ.PT) ILPREC = 3
            ENDIF
            IF(DIST3.LT.DIST) THEN
              DIST = DIST3
              ELBEST = IELEM
              IGBEST = I3
              ILBEST = 3
              IF(I1.EQ.PT) ILPREC = 1
              IF(I2.EQ.PT) ILPREC = 2
              IF(I3.EQ.PT) ILPREC = 3
            ENDIF
          ENDIF
C
20      CONTINUE
C
        IF(IGBEST.EQ.PT) THEN
          IF(LNG.EQ.1) WRITE(LU,32)
          IF(LNG.EQ.2) WRITE(LU,33)
32        FORMAT(1X,'FLUSEC : BLOCAGE DE L''ALGORITHME')
33        FORMAT(1X,'FLUSEC : ALGORITHM FAILED')
          CALL PLANTE(1)
          STOP
        ELSE
          PT = IGBEST
          ISEG = ISEG + 1
          IF(ISEG.GT.NSEMAX) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'FLUSEC : TROP DE SEGMENTS DANS UNE'
              WRITE(LU,*) '         SECTION. AUGMENTER NSEMAX'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'FLUSEC: TOO MANY SEGMENTS IN A   '
              WRITE(LU,*) '        SECTION. INCREASE  NSEMAX'
            ENDIF
            STOP
          ENDIF
          LISTE(ISEC,ISEG,1) = IKLE(ELBEST,ILPREC)
          LISTE(ISEC,ISEG,2) = IKLE(ELBEST,ILBEST)
          IF(IGBEST.NE.ARR) GO TO 10
        ENDIF
C
        NSEG(ISEC) = ISEG
C
C       IF COMPATIBLE COMPUTATION OF FLUXES=YES
        IF(COMFLU.AND.NSEG(ISEC).GE.1) THEN
C
C       LOOKS AT ALL ELEMENTS TOUCHING THE SECTION WITH 2 POINTS
C       MARKS THEM WITH 1 ON ONE SIDE AND -1 ON THE OTHER SIDE
C
        DO IEL=1,NELEM
          MSKSEC%ADR(ISEC)%P%R(IEL)=0.D0
          J1=IKLE(IEL,1)
          J2=IKLE(IEL,2)
          J3=IKLE(IEL,3)
          DO ISEG=1,NSEG(ISEC)
            I1 = LISTE(ISEC,ISEG,1)
            I2 = LISTE(ISEC,ISEG,2)
C           LEFT SIDE
            IF    ( (J1.EQ.I1.AND.J2.EQ.I2) .OR.
     &              (J2.EQ.I1.AND.J3.EQ.I2) .OR.
     &              (J3.EQ.I1.AND.J1.EQ.I2)      ) THEN
              MSKSEC%ADR(ISEC)%P%R(IEL)=1.D0
C           RIGHT SIDE
            ELSEIF( (J1.EQ.I2.AND.J2.EQ.I1) .OR.
     &              (J2.EQ.I2.AND.J3.EQ.I1) .OR.
     &              (J3.EQ.I2.AND.J1.EQ.I1)      ) THEN
              MSKSEC%ADR(ISEC)%P%R(IEL)=-1.D0
            ENDIF
          ENDDO
        ENDDO
C
C       OTHER TRIANGLES WITH ONLY 1 POINT TOUCHING THE SECTION
C       LOOKS AT NEIGHBOURS TO FIND THEIR SIDE
C
999     CONTINUE
        OK=.TRUE.
        DO IEL=1,NELEM
          J1=IKLE(IEL,1)
          J2=IKLE(IEL,2)
          J3=IKLE(IEL,3)
          DO ISEG=1,NSEG(ISEC)
            I1 = LISTE(ISEC,ISEG,1)
            I2 = LISTE(ISEC,ISEG,2)
            IF((J1.EQ.I1.OR.J2.EQ.I1.OR.J3.EQ.I1.OR.
     &          J1.EQ.I2.OR.J2.EQ.I2.OR.J3.EQ.I2).AND.
     &          ABS(MSKSEC%ADR(ISEC)%P%R(IEL)).LT.0.5D0) THEN
C             LOOKS AT NEIGHBOURS
              IF(IFABOR(IEL,1).GT.0) THEN
                IELEM=IFABOR(IEL,1)
                IF(ABS(MSKSEC%ADR(ISEC)%P%R(IELEM)).GT.0.5D0) THEN
                  MSKSEC%ADR(ISEC)%P%R(IEL)=MSKSEC%ADR(ISEC)%P%R(IELEM)
                ENDIF
              ENDIF
              IF(IFABOR(IEL,2).GT.0) THEN
                IELEM=IFABOR(IEL,2)
                IF(ABS(MSKSEC%ADR(ISEC)%P%R(IELEM)).GT.0.5D0) THEN
                  MSKSEC%ADR(ISEC)%P%R(IEL)=MSKSEC%ADR(ISEC)%P%R(IELEM)
                ENDIF
              ENDIF
              IF(IFABOR(IEL,3).GT.0) THEN
                IELEM=IFABOR(IEL,3)
                IF(ABS(MSKSEC%ADR(ISEC)%P%R(IELEM)).GT.0.5D0) THEN
                  MSKSEC%ADR(ISEC)%P%R(IEL)=MSKSEC%ADR(ISEC)%P%R(IELEM)
                ENDIF
              ENDIF
C             CORRECTION JMH 11/01/2008
C             IF(MSKSEC%ADR(ISEC)%P%R(IEL).LT.0.5D0) OK=.FALSE.
              IF(ABS(MSKSEC%ADR(ISEC)%P%R(IEL)).LT.0.5D0) OK=.FALSE.
            ENDIF
          ENDDO
        ENDDO
        IF(.NOT.OK) GO TO 999
C
C       IF(COMFLU.AND.NSEG(ISEC).GE.1) THEN
        ENDIF
C
60    CONTINUE
C
C     IF(.NOT.DEJA) THEN
      ENDIF
C
C-----------------------------------------------------------------------
C
      DEJA = .TRUE.
C
C-----------------------------------------------------------------------
C
      IELMH=HPROP%ELM
      IELMU=U%ELM
C
      DO 70 ISEC = 1 , NSEC
      FLX(ISEC) = 0.D0
      IF(NSEG(ISEC).GE.1) THEN
C
      IF(COMFLU) THEN
C
C     COMPUTES THE FLUX AS IN THE CONTINUITY EQUATION
C     (HOWEVER IMPLICITATION SHOULD PERHAPS BE ALSO USED)
C
      CALL MATRIX(BM1,'M=N     ','MATFGR         X',IELMH,IELMU,
     &            1.D0,HPROP,S,S,S,S,S,MESH,.TRUE.,MSKSEC%ADR(ISEC)%P)
      CALL MATRIX(BM2,'M=N     ','MATFGR         Y',IELMH,IELMU,
     &            1.D0,HPROP,S,S,S,S,S,MESH,.TRUE.,MSKSEC%ADR(ISEC)%P)
C
      CALL MATVEC( 'X=AY    ',CV1,BM1,U,0.D0,MESH)
      CALL MATVEC( 'X=X+AY  ',CV1,BM2,V,0.D0,MESH)
C
C     SUMS UP CV1 FOR ALL POINTS OF THE SECTION, THIS IS THE FLUX !
C     (OBTAINED BY CONTINUITY EQUATION AND AN INTEGRATION BY PARTS)
C
      DO ISEG = 1 , NSEG(ISEC)
        I1   = LISTE(ISEC,ISEG,1)
        FLX(ISEC) = FLX(ISEC) + CV1%R(I1)
      ENDDO
C     LAST SEGMENT, ADDS THE LAST POINT
      I2   = LISTE(ISEC,NSEG(ISEC),2)
      FLX(ISEC) = FLX(ISEC) + CV1%R(I2)
C
C     WHEN BOTH UPWIND AND DOWNSTREAM ELEMENTS ARE TAKEN INTO ACCOUNT
C     WITH DIFFERENT SIGNS, THE FLUX IS COUNTED TWICE
C
      FLX(ISEC)=FLX(ISEC)*0.5D0
C
      ELSE
C
C       COMPUTES THE FLUX DIRECTLY, REGARDLESS OF THE WEAK FORM
C       OF THE IMPERMEABILITY CONDITION
C
        DO ISEG = 1 , NSEG(ISEC)
          I1 = LISTE(ISEC,ISEG,1)
          I2 = LISTE(ISEC,ISEG,2)
          X1 = X(I1)
          X2 = X(I2)
          Y1 = Y(I1)
          Y2 = Y(I2)
          H1 = H%R(I1)
          H2 = H%R(I2)
          NX = Y1-Y2
          NY = X2-X1
          UN1= U%R(I1)*NX + V%R(I1)*NY
          UN2= U%R(I2)*NX + V%R(I2)*NY
          FLX(ISEC) = FLX(ISEC) + ((H1+H2)*(UN1+UN2)+H2*UN2+H1*UN1)*SUR6
        ENDDO
C
      ENDIF
C
      IF(FLX(ISEC).GT.0.D0) THEN
        VOLPOS(ISEC) = VOLPOS(ISEC) + FLX(ISEC)*DT
      ELSE
        VOLNEG(ISEC) = VOLNEG(ISEC) + FLX(ISEC)*DT
      ENDIF
C
C     IF(NSEG(ISEC).GT.1)...
      ENDIF
C
70    CONTINUE
C
C-----------------------------------------------------------------------
C
C     PRINTS OUT THE RESULTS
C
      CALL FLUXPR(NSEC,CTRLSC,FLX,VOLNEG,VOLPOS,INFO,TPS,NSEG,NCSIZE,
     &            CUMFLO)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C