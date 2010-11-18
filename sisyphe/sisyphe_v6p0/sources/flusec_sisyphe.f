C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES FLUXES THROUGH CONTROL SECTIONS
!>                AND ADDS THEM UP TO OBTAIN OSCILLATING VOLUMES.
!><br>            MESHES OF DIMENSION 2 AND CONSIDERED WATER DEPTH.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CHARR, CTRLSC, DT, H, IKLE, INFO, KNOGL, NCP, NELEM, NELMAX, QSXC, QSXS, QSYC, QSYS, SUSP, TPS, U, V, X, Y
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::CHAIN CHAIN@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ARR, DEJA, DEP, DIST, DIST1, DIST2, DIST3, ELBEST, ERR, FLX, FLXC, FLXS, H1, H2, I1, I2, I3, IELEM, IGBEST, ILBEST, ILPREC, ISEC, ISEG, LISTE, NSEC, NSEG, NSEMAX, NX, NY, OLD_METHOD, PT, SUR6, UN1, UN2, VOLNEG, VOLNEGC, VOLNEGS, VOLPOS, VOLPOSC, VOLPOSS, X1, X2, Y1, Y2
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FLUSEC_SISYPHE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FLUXPR_SISYPHE(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE()

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
!>      <td><center> 5.7                                       </center>
!> </td><td> 27/12/2006
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CHARR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CS
!></td><td>--></td><td>BLOC DE TRACEURS EN SUSPENSION
!>    </td></tr>
!>          <tr><td>CTRLSC
!></td><td>--></td><td>DONNEES SUR LES SECTIONS DE CONTROLE.
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS.
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU
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
!>          <tr><td>NCP
!></td><td>--></td><td>TWO TIMES THE NUMBER OF CONTROL SECTIONS
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS.
!>    </td></tr>
!>          <tr><td>QSXC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSXS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSYC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSYS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SUSP
!></td><td>--></td><td>LOGIQUE INDIQUANT DE PRENDRE EN COMPTE
!>                  LE BLOC DE TRACEURS EN SUSPENSION
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
                        SUBROUTINE FLUSEC_SISYPHE
     &(U,V,H,QSXC,QSYC,CHARR,QSXS,QSYS,SUSP,
     & IKLE,NELMAX,NELEM,X,Y,DT,NCP,CTRLSC,INFO,TPS,KNOGL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHARR          |---| 
C| CS             |-->| BLOC DE TRACEURS EN SUSPENSION
C| CTRLSC         |-->| DONNEES SUR LES SECTIONS DE CONTROLE.
C| DT             |-->| PAS DE TEMPS.
C| H             |-->| HAUTEUR D'EAU
C| IKLE           |-->| TABLEAUX DE CONNECTIVITE LOCAL-GLOBAL
C| INFO           |-->| SI OUI : IMPRESSIONS.
C| KNOGL          |---| 
C| NCP            |-->| TWO TIMES THE NUMBER OF CONTROL SECTIONS
C| NELEM          |-->| NOMBRE D'ELEMENTS.
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS.
C| QSXC           |---| 
C| QSXS           |---| 
C| QSYC           |---| 
C| QSYS           |---| 
C| SUSP           |-->| LOGIQUE INDIQUANT DE PRENDRE EN COMPTE
C|                |   | LE BLOC DE TRACEURS EN SUSPENSION
C| TPS            |-->| TEMPS
C| U,V            |-->| CHAMP DE VITESSE
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE
C| XEL,YEL        |-->| COORDONNEES DES POINTS PAR ELEMENT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY: CHAIN
      USE INTERFACE_SISYPHE, EX_FLUSEC_SISYPHE => FLUSEC_SISYPHE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)          :: NELMAX,NELEM,NCP
      INTEGER, INTENT(IN)          :: IKLE(NELMAX,*)
      INTEGER, INTENT(IN)          :: CTRLSC(NCP),KNOGL(*)
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),TPS,DT
       LOGICAL, INTENT(IN)          :: INFO,SUSP,CHARR
      TYPE(BIEF_OBJ), INTENT(IN)   :: U,V,H,QSXC,QSYC,QSXS,QSYS
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NSEMAX,ERR
      PARAMETER(NSEMAX=50)
C
      INTEGER IELEM,I1,I2,I3,ELBEST,IGBEST,ILBEST
      INTEGER ILPREC,ISEG,ISEC,NSEC,PT,DEP,ARR
C
      DOUBLE PRECISION DIST,DIST1,DIST2,DIST3
      DOUBLE PRECISION H1,H2,X1,Y1,X2,Y2,UN1,UN2,NX,NY,SUR6
C
      DOUBLE PRECISION, ALLOCATABLE :: FLX(:),VOLNEG(:),VOLPOS(:)
      DOUBLE PRECISION, ALLOCATABLE :: FLXS(:),FLXC(:)
      DOUBLE PRECISION, ALLOCATABLE :: VOLNEGS(:),VOLPOSS(:)
      DOUBLE PRECISION, ALLOCATABLE :: VOLNEGC(:),VOLPOSC(:)
      INTEGER, ALLOCATABLE :: NSEG(:),LISTE(:,:,:)
C
      LOGICAL DEJA
      DATA DEJA/.FALSE./
      LOGICAL :: OLD_METHOD=.FALSE.
C
      SAVE LISTE,DEJA,NSEG,VOLNEG,VOLPOS,FLX,FLXS,VOLNEGS,VOLPOSS
      SAVE FLXC,VOLNEGC,VOLPOSC,OLD_METHOD
C
C-----------------------------------------------------------------------
C
      WRITE(LU,*) '-> ENTERING FLUSEC_SISYPHE'
      WRITE(LU,*) 'NCP: ',NCP
      WRITE(LU,*) 'CTRLSC: ',CTRLSC(:)

      SUR6 = 1.D0/6.D0
      NSEC = NCP/2
C
C  LOOKS FOR WAYS THAT JOIN THE POINT COUPLES:
C
      IF(.NOT.DEJA) THEN
C
C     DYNAMICALLY ALLOCATES FLX, VOLNEG, VOLPOS, ETC.
C
      ALLOCATE(FLX(NSEC)           ,STAT=ERR)
      ALLOCATE(VOLNEG(NSEC)        ,STAT=ERR)
      ALLOCATE(VOLPOS(NSEC)        ,STAT=ERR)
      ALLOCATE(NSEG(NCP)           ,STAT=ERR)
      ALLOCATE(LISTE(NCP,NSEMAX,2) ,STAT=ERR)
C     S FOR SUSPENSION, C FOR BEDLOAD
      ALLOCATE(FLXS(NSEC)          ,STAT=ERR)
      ALLOCATE(VOLNEGS(NSEC)       ,STAT=ERR)
      ALLOCATE(VOLPOSS(NSEC)       ,STAT=ERR)
      ALLOCATE(FLXC(NSEC)          ,STAT=ERR)
      ALLOCATE(VOLNEGC(NSEC)       ,STAT=ERR)
      ALLOCATE(VOLPOSC(NSEC)       ,STAT=ERR)
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
      IF (.NOT.ALLOCATED(CHAIN)) OLD_METHOD=.TRUE.
C
      DO ISEC=1,NSEC
        FLX(ISEC)=0.0D0
        VOLNEG(ISEC) =0.D0
        VOLPOS(ISEC) =0.D0
        VOLNEGS(ISEC)=0.D0
        VOLPOSS(ISEC)=0.D0
        VOLNEGC(ISEC)=0.D0
        VOLPOSC(ISEC)=0.D0
      ENDDO
C
      DO 60 ISEC =1,NSEC
C
C!JAJ #### IN THE SERIAL CASE, OR "CLASSICAL" IN PARALLEL,
C     FOLLOW THE ALGORITHM OF FINDING SEGMENT CHAINS
C
C NOTE: IF YOU CHANGE THE ALGORITHM, CHANGE IT IN PARTEL AS WELL
C
        IF (NCSIZE.LE.1 .OR. OLD_METHOD) THEN
C
        DEP = CTRLSC(1+2*(ISEC-1))
        ARR = CTRLSC(2+2*(ISEC-1))
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
!
CJAJ #### THIS PART TO BE DONE IN THE PARALLEL CASE; FILL LISTE
C WITH READY SEGMENT CHAINS PROVIDED BY PARTEL: SEE READ_SECTIONS
C NOTE: FUTURE OPTIMISATION - USE CHAIN STRUCTURE IN THE WHOLE ROUTINE
!
        ELSE
C
          NSEG(ISEC) = CHAIN(ISEC)%NSEG
          LISTE(ISEC,:,:)=0

          DO ISEG=1,NSEG(ISEC)
            LISTE(ISEC,ISEG,1) = CHAIN(ISEC)%LISTE(ISEG,1)
            LISTE(ISEC,ISEG,2) = CHAIN(ISEC)%LISTE(ISEG,2)
          END DO

          WRITE(LU,*) 'CHAIN@SISYPHE -> LISTE@SISYPHE:'
          WRITE(LU,*) 'ISEC,NSEG(ISEC): ',ISEC,NSEG(ISEC)
          DO ISEG=1,NSEG(ISEC)
            WRITE(LU,*) LISTE(ISEC,ISEG,:)
          END DO

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
      DO ISEC = 1 , NSEC
C
      FLX(ISEC)  = 0.D0
      FLXS(ISEC) = 0.D0
      FLXC(ISEC) = 0.D0
C
      IF(NSEG(ISEC).GE.1) THEN
C
C     COMPUTES THE FLUX DIRECTLY, REGARDLESS OF THE WEAK FORM
C     OF THE IMPERMEABILITY CONDITION
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
        IF(SUSP) THEN
          UN1= QSXS%R(I1)*NX + QSYS%R(I1)*NY
          UN2= QSXS%R(I2)*NX + QSYS%R(I2)*NY
          FLXS(ISEC) = FLXS(ISEC) + 0.5D0*(UN1+UN2)
        ENDIF
        IF(CHARR) THEN
          UN1= QSXC%R(I1)*NX + QSYC%R(I1)*NY
          UN2= QSXC%R(I2)*NX + QSYC%R(I2)*NY
          FLXC(ISEC) = FLXC(ISEC) + 0.5D0*(UN1+UN2)
        ENDIF
      ENDDO
C
      IF(FLX(ISEC).GT.0.D0) THEN
        VOLPOS(ISEC) = VOLPOS(ISEC) + FLX(ISEC)*DT
      ELSE
        VOLNEG(ISEC) = VOLNEG(ISEC) + FLX(ISEC)*DT
      ENDIF
C
      IF(SUSP) THEN
        IF(FLXS(ISEC).GT.0.D0) THEN
          VOLPOSS(ISEC) = VOLPOSS(ISEC) + FLXS(ISEC)*DT
        ELSE
          VOLNEGS(ISEC) = VOLNEGS(ISEC) + FLXS(ISEC)*DT
        ENDIF
      ENDIF
C
      IF(CHARR) THEN
        IF(FLXC(ISEC).GT.0.D0) THEN
          VOLPOSC(ISEC) = VOLPOSC(ISEC) + FLXC(ISEC)*DT
        ELSE
          VOLNEGC(ISEC) = VOLNEGC(ISEC) + FLXC(ISEC)*DT
        ENDIF
      ENDIF
C
C     IF(NSEG(ISEC).GT.1)...
      ENDIF
C
      ENDDO
C
C-----------------------------------------------------------------------
C
C     PRINTS THE RESULTS / !JAJ HERE ALLREDUCES FOR VALUES
C
      CALL FLUXPR_SISYPHE(NSEC,CTRLSC,FLX,VOLNEG,VOLPOS,INFO,TPS,
     &                    NSEG,NCSIZE,
     &                    FLXS,VOLNEGS,VOLPOSS,SUSP,
     &                    FLXC,VOLNEGC,VOLPOSC,CHARR)
C
C-----------------------------------------------------------------------
C
      WRITE(LU,*) '-> LEAVING FLUSEC_SISYPHE'
      RETURN
      END
C
C#######################################################################
C