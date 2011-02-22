C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPERATIONS ON STRUCTURES.
!>  @code
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON VECTORS X,Y AND Z AND CONSTANT C.<br>
!>   THE RESULT IS VECTOR X.<br>
!>   ON ARRAYS OR VECTORS :<br>
!>   OP = 'X=C     '     :  SETS X TO C
!>   OP = 'X=0     '     :  SETS X TO 0
!>   OP = 'X=Y     '     :  COPIES Y IN X
!>   OP = 'X=+Y    '     :  IDEM
!>   OP = 'X=-Y    '     :  COPIES -Y IN X
!>   OP = 'X=1/Y   '     :  COPIES INVERSE OF Y IN X
!>   OP = 'X=Y+Z   '     :  ADDS Y AND Z
!>   OP = 'X=Y-Z   '     :  SUBTRACTS Z TO Y
!>   OP = 'X=YZ    '     :  Y.Z
!>   OP = 'X=-YZ   '     :  -Y.Z
!>   OP = 'X=XY    '     :  X.Y
!>   OP = 'X=X+YZ  '     :  ADDS Y.Z TO X
!>   OP = 'X=X-YZ  '     :  SUBTRACTS Y.Z FROM X
!>   OP = 'X=CXY   '     :  C.X.Y
!>   OP = 'X=CYZ   '     :  C.Y.Z
!>   OP = 'X=CXYZ  '     :  C.X.Y.Z
!>   OP = 'X=X+CYZ '     :  ADDS C.Y.Z TO X
!>   OP = 'X=Y/Z   '     :  DIVIDES Y BY Z
!>   OP = 'X=CY/Z  '     :  DIVIDES C.Y BY Z
!>   OP = 'X=CXY/Z '     :  DIVIDES C.X.Y BY Z
!>   OP = 'X=X+CY/Z'     :  ADDS C.Y/Z TO X
!>   OP = 'X=X+Y   '     :  ADDS Y TO X
!>   OP = 'X=X-Y   '     :  SUBTRACTS Y FROM X
!>   OP = 'X=CX    '     :  MULTIPLIES X BY C
!>   OP = 'X=CY    '     :  MULTIPLIES Y BY C
!>   OP = 'X=Y+CZ  '     :  ADDS C.Z TO Y
!>   OP = 'X=X+CY  '     :  ADDS C.Y TO X
!>   OP = 'X=SQR(Y)'     :  SQUARE ROOT OF Y
!>   OP = 'X=ABS(Y)'     :  ABSOLUTE VALUE OF Y
!>   OP = 'X=N(Y,Z)'     :  NORM OF THE VECTOR WITH COMPONENTS Y AND Z
!>   OP = 'X=Y+C   '     :  ADDS C TO Y
!>   OP = 'X=X+C   '     :  ADDS C TO X
!>   OP = 'X=Y**C  '     :  Y TO THE POWER C
!>   OP = 'X=COS(Y)'     :  COSINE OF Y
!>   OP = 'X=SIN(Y)'     :  SINE OF Y
!>   OP = 'X=ATN(Y)'     :  ARC TANGENT OF Y
!>   OP = 'X=A(Y,Z)'     :  ATAN2(Y,Z)
!>   OP = 'X=+(Y,C)'     :  MAXIMUM OF Y AND C
!>   OP = 'X=-(Y,C)'     :  MINIMUM OF Y AND C
!>   OP = 'X=+(Y,Z)'     :  MAXIMUM OF Y AND Z
!>   OP = 'X=-(Y,Z)'     :  MINIMUM OF Y AND Z
!>   OP = 'X=YIFZ<C'     :  COPIES Y IN X IF Z < C , FOR EACH POINT
!>   OP = 'X=C(Y-Z)'     :  MULTIPLIES C BY (Y-Z)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  OPERATIONS 1/Y AND Y/Z INTERNALLY TAKE CARE OF DIVISIONS
!>            BY 0. SUCCESSFUL EXIT OF OS IS THEREFORE NOT A PROOF THAT
!>            Y OR Z ARE NOT 0

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, INFINI, IOPT, OP, X, Y, Z, ZERO
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CC, IBL, IDIM, N, NMAX, TYPX, YAC, YAY, YAZ, YY, ZZ
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_OS, YY, ZZ
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CMPOBJ(), CPSTVC(), OV(), OVD(), OVD_2(), OV_2(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS(), ASSIGNSTR(), BEDLOAD_BAILARD(), BEDLOAD_BIJKER(), BEDLOAD_DIBWAT(), BEDLOAD_DIFFIN(), BEDLOAD_EFFPNT(), BEDLOAD_ENGEL(), BEDLOAD_EVOL(), BEDLOAD_FORMULA(), BEDLOAD_HIDING_FACTOR(), BEDLOAD_HUNZ_MEYER(), BEDLOAD_MAIN(), BEDLOAD_MEYER(), BEDLOAD_SECCURRENT(), BEDLOAD_SOLIDISCHARGE(), BEDLOAD_SOLVS_FE(), BEDLOAD_SOLVS_VF(), BERKHO(), BILAN(), BILANT(), BILANT1(), BYPASS_CRUSHED_POINTS_EBE(), BYPASS_CRUSHED_POINTS_SEG(), CALCMN(), CALCTM(), CALCUE(), CALRE2(), CALRES(), CELERITE(), CFLPSI(), CFLVF(), CGSQUA(), CGSTAB(), CHECK_DIGITS(), CHECK_DOT(), CHPCON(), COEFRO(), CONDIH(), CONDIM(), CONDIM_SUSP(), CONDIN(), CONDIN_ADJ(), CONDIS_SISYPHE(), CORRECTION_DEPTH_2D(), CORRECTION_DEPTH_3D(), COST_FUNCTION(), CVDF3D(), CVDFTR(), CVTRVF(), CVTRVF_POS(), DEBIMP3D(), DIFF3D(), DIFFIN(), DIFSOU(), DIRAUX(), DIRI01(), DIRI04(), DIRI09(), DRAGFO(), DREDGESIM_INTERFACE(), DRSURR(), EQUNOR(), ERODNC(), ERRMIN(), FILTER(), FILTER_H(), FLUX3D(), FONSTR(), FRICTI(), FRICTION_CHOICE(), FRICTION_UNIF(), FRICTION_ZONES(), FSGRAD(), GESTIO(), GMRES(), GRACJG(), GRAD2D(), HOMERE_ADJ_T2D(), HPROPA(), HREF(), INBIEF(), INCIDE(), INITSTR(), INIT_SEDIMENT(), INIT_TRANSPORT(), INIT_ZERO(), KEPSIL(), LICHEK(), LUMP(), MASBAS2D(), MASKTO(), MASQUE_ARTEMIS(), MATRIX(), MATVEC(), MAXSLOPE(), MESH_PROP(), MESURES(), MURD3D_POS(), PARCOM_BORD(), PARINI(), PARMOY(), POINT_SISYPHE(), POINT_TELEMAC2D(), POSITIVE_DEPTHS(), PREBD4(), PREBD9(), PRECD1(), PRECD4(), PRECD9(), PRECON(), PREDES(), PREDIV(), PRERES_TELEMAC2D(), PRERES_TELEMAC3D(), PROPAG(), PROPAG_ADJ(), PROPIN_TELEMAC2D(), PROSOU(), RADIA1(), RADIA2(), RESCJG(), RESCUE(), RESOLU(), SISYPHE(), SIS_ARRET(), SOLVE(), SOUKOM(), SOURCES_SINKS(), SUSPENSION_BILAN(), SUSPENSION_COMPUTATION(), SUSPENSION_CONV(), SUSPENSION_DEPOT(), SUSPENSION_DISPERSION(), SUSPENSION_EROSION(), SUSPENSION_EVOL(), SUSPENSION_MAIN(), TELEMAC2D(), TELEMAC3D(), THOMPS(), TOB_SISYPHE(), TRISOU(), UM1X04(), UM1X09(), VALRO(), VGFPSI(), VISCKE(), VISCKO(), VISCLM(), VISCOS(), VISSMA(), VITCHU(), WAVE_EQUATION(), WCTURB(), WSTARW()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 18/08/05
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C
!></td><td>--></td><td>CONSTANTE DONNEE
!>    </td></tr>
!>          <tr><td>INFINI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IOPT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>CHAINE DE CARACTERES INDIQUANT L'OPERATION
!>                  A EFFECTUER.
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>STRUCTURE RESULTAT
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>STRUCTURE OPERANDE
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>STRUCTURE OPERANDE
!>    </td></tr>
!>          <tr><td>ZERO
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE OS
     & ( OP , X , Y , Z , C , IOPT , INFINI , ZERO )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
C| INFINI         |---| 
C| IOPT           |---| 
C| OP             |-->| CHAINE DE CARACTERES INDIQUANT L'OPERATION
C|                |   | A EFFECTUER.
C| X             |<--| STRUCTURE RESULTAT
C| Y             |-->| STRUCTURE OPERANDE
C| Z             |-->| STRUCTURE OPERANDE
C| ZERO           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_OS => OS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     OPTIONAL ARGUMENTS
C
      INTEGER,          INTENT(IN), OPTIONAL :: IOPT
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: INFINI
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: ZERO
C
C     ARGUMENTS
C
      TYPE(BIEF_OBJ),   INTENT(INOUT), OPTIONAL, TARGET :: X
      TYPE(BIEF_OBJ),   INTENT(IN)   , OPTIONAL, TARGET :: Y,Z
      DOUBLE PRECISION, INTENT(IN)   , OPTIONAL :: C
      CHARACTER(LEN=8), INTENT(IN)              :: OP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     LOCAL VARIABLES
C
      INTEGER IBL,TYPX,IDIM,N,NMAX
      LOGICAL YAY,YAZ,YAC
      TYPE(BIEF_OBJ), POINTER :: YY,ZZ
      DOUBLE PRECISION CC
C
C-----------------------------------------------------------------------
C
      TYPX = X%TYPE
C
      YAY=.FALSE.
      YAZ=.FALSE.
      YAC=.FALSE.
      IF(OP(3:3).EQ.'Y'.OR.OP(4:4).EQ.'Y'.OR.OP(5:5).EQ.'Y'.OR.
     &   OP(6:6).EQ.'Y'.OR.OP(7:7).EQ.'Y'.OR.OP(8:8).EQ.'Y') YAY=.TRUE.
      IF(OP(3:3).EQ.'Z'.OR.OP(4:4).EQ.'Z'.OR.OP(5:5).EQ.'Z'.OR.
     &   OP(6:6).EQ.'Z'.OR.OP(7:7).EQ.'Z'.OR.OP(8:8).EQ.'Z') YAZ=.TRUE.
C
C     CHECKS THAT CONSTANT C IS IN THE REQUIRED OPERATION
C     I.E. IF THERE IS C IN OP, EXCEPT WHEN IT IS X=COS(Y)
C
      IF((OP(3:3).EQ.'C'.AND.OP(4:4).NE.'O').OR.
     &    OP(4:4).EQ.'C'.OR.OP(5:5).EQ.'C'.OR.
     &    OP(6:6).EQ.'C'.OR.OP(7:7).EQ.'C'.OR.OP(8:8).EQ.'C') YAC=.TRUE.
C
      IF(PRESENT(C)) THEN
        CC=C
      ELSE
        IF(YAC) THEN
          IF (LNG.EQ.1) WRITE(LU,1) OP
          IF (LNG.EQ.2) WRITE(LU,2) OP
1         FORMAT(1X,'OS (BIEF) : C ABSENT ET OPERATION ',A8,' DEMANDEE')
2         FORMAT(1X,'OS (BIEF) : C MISSING AND OPERATION ',A8,' ASKED')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
C
      IF(YAY) THEN
        IF(PRESENT(Y)) THEN
          YY=>Y
        ELSE
          IF (LNG.EQ.1) WRITE(LU,10) OP
          IF (LNG.EQ.2) WRITE(LU,11) OP
10        FORMAT(1X,'OS (BIEF) : Y ABSENT ET OPERATION ',A8,' DEMANDEE')
11        FORMAT(1X,'OS (BIEF) : Y MISSING AND OPERATION ',A8,' ASKED')
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        YY=>X
      ENDIF
C
C     OPERATION WITH Y AND Z (IF THERE IS Z THERE SHOULD BE Y)
C
      IF(YAZ) THEN
C
        IF(PRESENT(Z)) THEN
C
        ZZ=>Z
C
C       COMPARES TYPES OF Y AND Z
C
        IF(.NOT.CMPOBJ(Y,Z)) THEN
          IF (LNG.EQ.1) WRITE(LU,40) Y%NAME,Y%ELM,Z%NAME,Z%ELM
          IF (LNG.EQ.2) WRITE(LU,41) Y%NAME,Y%ELM,Z%NAME,Z%ELM
40        FORMAT(1X,'OS (BIEF) : TYPES DIFFERENTS POUR ',A6,' (',1I2,
     &              ') ET ',A6,' (',1I2,')')
41        FORMAT(1X,'OS (BIEF) : DIFFERENT TYPES FOR ',A6,' (',1I2,
     &              ') AND ',A6,' (',1I2,')')
          CALL PLANTE(1)
          STOP
        ENDIF
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,20) OP
          IF (LNG.EQ.2) WRITE(LU,21) OP
20        FORMAT(1X,'OS (BIEF) : Z ABSENT ET OPERATION ',A8,' DEMANDEE')
21        FORMAT(1X,'OS (BIEF) : Z MISSING AND OPERATION ',A8,' ASKED')
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
      ELSE
        ZZ=>X
      ENDIF
C
C-----------------------------------------------------------------------
C     VECTORS
C-----------------------------------------------------------------------
C
      IF(TYPX.EQ.2) THEN
C
C     OPERATION WITH Y : Y IS CHECKED
C
        IF(YAY) THEN
C         DIFFERENT TYPES: X THEN TAKES ITS STRUCTURE FROM Y
          IF(.NOT.CMPOBJ(X,Y)) CALL CPSTVC(Y,X)
        ENDIF
C
C       CHECKS MEMORY
C
        IF(X%DIM1.GT.X%MAXDIM1) THEN
          IF (LNG.EQ.1) WRITE(LU,100) X%NAME
          IF (LNG.EQ.2) WRITE(LU,101) X%NAME
100       FORMAT(1X,'OS (BIEF) : DEPASSEMENT DE MEMOIRE SUR : ',A6)
101       FORMAT(1X,'OS (BIEF) : BEYOND ALLOWED MEMORY IN: ',A6)
          CALL PLANTE(1)
          STOP
        ENDIF
C
        IF(.NOT.PRESENT(IOPT)) THEN
C
        IF(X%DIM2.GT.1) THEN
C
          DO IDIM = 1 , X%DIM2
            CALL OV_2(OP,X%R,IDIM,YY%R,IDIM,
     &                            ZZ%R,IDIM,CC,X%MAXDIM1,X%DIM1)
          END DO
C
        ELSE
C
          CALL OV(OP,X%R,YY%R,ZZ%R,CC,X%DIM1)
C
        ENDIF
C
        ELSE
C
        IF(X%DIM2.GT.1) THEN
C
          DO IDIM = 1 , X%DIM2
            CALL OVD_2(OP,X%R,IDIM,YY%R,IDIM,ZZ%R,IDIM,CC,
     &                 X%MAXDIM1,X%DIM1,IOPT,INFINI,ZERO)
          END DO
C
        ELSE
C
          CALL OVD(OP,X%R,YY%R,ZZ%R,CC,X%DIM1,IOPT,INFINI,ZERO)
C
        ENDIF
C
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(TYPX.EQ.4) THEN
C
C-----------------------------------------------------------------------
C     BLOCKS
C-----------------------------------------------------------------------
C
       DO 60 IBL = 1 , X%N
         IF(YAY) THEN
           IF(.NOT.CMPOBJ(X%ADR(IBL)%P,Y%ADR(IBL)%P)) THEN
             CALL CPSTVC(Y%ADR(IBL)%P,X%ADR(IBL)%P)
           ENDIF
         ENDIF
C
C        CHECKS MEMORY
C
         N = X%ADR(IBL)%P%DIM1
         NMAX = X%ADR(IBL)%P%MAXDIM1
         IF(N.GT.NMAX) THEN
           IF (LNG.EQ.1) WRITE(LU,100) X%ADR(IBL)%P%NAME
           IF (LNG.EQ.2) WRITE(LU,101) X%ADR(IBL)%P%NAME
           IF (LNG.EQ.1) WRITE(LU,200) X%NAME
           IF (LNG.EQ.2) WRITE(LU,201) X%NAME
200        FORMAT(1X,'            CE VECTEUR EST DANS LE BLOC : ',A6)
201        FORMAT(1X,'            THIS VECTOR IS IN BLOCK: ',A6)
           CALL PLANTE(1)
           STOP
         ENDIF
C
         IF(.NOT.PRESENT(IOPT)) THEN
C
         IF(X%ADR(IBL)%P%DIM2.GT.1) THEN
C
         DO IDIM = 1 , X%ADR(IBL)%P%DIM2
           CALL OV_2(OP,X%ADR(IBL)%P%R,IDIM,
     &                 YY%ADR(IBL)%P%R,IDIM,
     &                 ZZ%ADR(IBL)%P%R,IDIM, CC , NMAX , N )
         END DO
C
         ELSE
C
           CALL OV(OP,X%ADR(IBL)%P%R,
     &               YY%ADR(IBL)%P%R,
     &               ZZ%ADR(IBL)%P%R, CC , N )
C
         ENDIF
C
         ELSE
C
         IF(X%ADR(IBL)%P%DIM2.GT.1) THEN
C
         DO IDIM = 1 , X%ADR(IBL)%P%DIM2
           CALL OVD_2(OP,X%ADR(IBL)%P%R,IDIM,
     &                  YY%ADR(IBL)%P%R,IDIM,
     &                  ZZ%ADR(IBL)%P%R,IDIM, CC , NMAX , N ,
     &                  IOPT,INFINI,ZERO)
         END DO
C
         ELSE
C
           CALL OVD(OP,X%ADR(IBL)%P%R,
     &                YY%ADR(IBL)%P%R,
     &                ZZ%ADR(IBL)%P%R, CC , N ,IOPT,INFINI,ZERO)
C
         ENDIF
C
         ENDIF
C
C
60     CONTINUE
C
C-----------------------------------------------------------------------
C
C     ERROR OR OBJECT NOT TREATED
C
      ELSE
C
        IF (LNG.EQ.1) WRITE(LU,1000) X%TYPE
        IF (LNG.EQ.2) WRITE(LU,1001) X%TYPE
1000    FORMAT(1X,'OS (BIEF) : TYPE D''OBJET NON TRAITE: ',1I3)
1001    FORMAT(1X,'OS (BIEF) : TYPE OF OBJECT NOT IMPLEMENTED: ',1I3)
        CALL PLANTE(1)
        STOP
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