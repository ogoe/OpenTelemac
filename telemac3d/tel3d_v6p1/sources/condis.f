C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES THE SEDIMENT VARIABLES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CONC, CONSOL, EPAI, GIBSON, HDEP, IVIDE, NCOUCH, NPF, NPFMAX, NPOIN2, NPOIN3, PRIVE, TASSE, TEMP, TREST, X, Y, ZF, ZR
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::CFDEP CFDEP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::CFMAX CFMAX@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DT DT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::EPAI0 EPAI0@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GRAV GRAV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RHOS RHOS@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CVGCSL, DTCSL, ECOUCH, ERR, IC, IPF, IPOIN, ITCSL, NITCSL, RESCSL, TCAR, TRA01, TRA02, TRA03, ZNOE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> GESTDP(), OV(), TASSEM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!> </td><td> 18/07/06
!> </td><td> NOEMIE DURAND (CHC-NRC); C LE NORMANT (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CONC
!></td><td><--</td><td>CONCENTRATION OF MUD BED LAYER
!>                  (MULTILAYER MODEL)
!>    </td></tr>
!>          <tr><td>CONSOL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>EPAI
!></td><td><--</td><td>THICKNESS OF SOLID FRACTION oF THE BED LAYER
!>                  (EPAI=DZ/(1+IVIDE), DZ BED LAYER THICKNESS)
!>    </td></tr>
!>          <tr><td>GIBSON
!></td><td>--></td><td>GIBSON SETTLING MODEL
!>    </td></tr>
!>          <tr><td>HDEP
!></td><td><--</td><td>THICKNESS OF FRESH DEPOSIT(FLUID MUD LAYER)
!>    </td></tr>
!>          <tr><td>IVIDE
!></td><td><--</td><td>VOID RATIO
!>                  (GIBSON MODEL ONLY)
!>    </td></tr>
!>          <tr><td>NCOUCH
!></td><td>--></td><td>NUMBER OF LAYERS WITHIN THE BED
!>                  (GIBSON MODEL)
!>    </td></tr>
!>          <tr><td>NPF
!></td><td><--</td><td>NUMBER OF POINTS WITHIN THE BED ALONG THE VERTICAL
!>    </td></tr>
!>          <tr><td>NPFMAX
!></td><td>--></td><td>MAXIMUM NUMBER OF HORIZONTAL PLANES WITHIN THE BED
!>                  (GIBSON MODEL)
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF POINTS IN 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NUMBER OF POINTS IN 3D
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td>--></td><td>NUMBER OR ARRAYS IN BLOCK PRIVE
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>BLOCK OF PRIVATE ARRAYS FOR USER
!>    </td></tr>
!>          <tr><td>TASSE
!></td><td>--></td><td>MULTILAYER SETTLING MODEL
!>    </td></tr>
!>          <tr><td>TEMP
!></td><td><--</td><td>TIME COUNTER FOR CONSOLIDATION MODEL
!>                  (MULTILAYER MODEL)
!>    </td></tr>
!>          <tr><td>TREST
!></td><td><--</td><td>CONSOLIDATION TIME SCALE
!>                  (ONLY FOR MULTILAYER MODEL)
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDINATES OF 2D MESH
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>BOTTOM ELEVATION
!>    </td></tr>
!>          <tr><td>ZR
!></td><td><--</td><td>ELEVATION OF RIDIG BED
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CONDIS
     &(IVIDE, EPAI  , TREST , CONC , TEMP   , HDEP   ,
     & ZR   , ZF    , X     , Y    , NPOIN2 , NPOIN3 ,
     & NPF  , NPFMAX, NCOUCH, TASSE, GIBSON , PRIVE  , CONSOL )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CONC           |<--| CONCENTRATION OF MUD BED LAYER
C|                |   | (MULTILAYER MODEL)
C| CONSOL         |---| 
C| EPAI           |<--| THICKNESS OF SOLID FRACTION oF THE BED LAYER
C|                |   | (EPAI=DZ/(1+IVIDE), DZ BED LAYER THICKNESS)
C| GIBSON         |-->| GIBSON SETTLING MODEL
C| HDEP           |<--| THICKNESS OF FRESH DEPOSIT(FLUID MUD LAYER)
C| IVIDE          |<--| VOID RATIO
C|                |   | (GIBSON MODEL ONLY)
C| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE BED
C|                |   | (GIBSON MODEL)
C| NPF            |<--| NUMBER OF POINTS WITHIN THE BED ALONG THE VERTICAL
C| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES WITHIN THE BED
C|                |   | (GIBSON MODEL)
C| NPOIN2         |-->| NUMBER OF POINTS IN 2D
C| NPOIN3         |-->| NUMBER OF POINTS IN 3D
C| NPRIV          |-->| NUMBER OR ARRAYS IN BLOCK PRIVE
C| PRIVE          |-->| BLOCK OF PRIVATE ARRAYS FOR USER
C| TASSE          |-->| MULTILAYER SETTLING MODEL
C| TEMP           |<--| TIME COUNTER FOR CONSOLIDATION MODEL
C|                |   | (MULTILAYER MODEL)
C| TREST          |<--| CONSOLIDATION TIME SCALE
C|                |   | (ONLY FOR MULTILAYER MODEL)
C| X,Y            |-->| COORDINATES OF 2D MESH
C| ZF             |-->| BOTTOM ELEVATION
C| ZR             |<--| ELEVATION OF RIDIG BED
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C#####> NOE-CHANGES
      USE DECLARATIONS_TELEMAC3D, ONLY: NPRIV,
     &                                  EPAI0,CFDEP,RHOS,GRAV,CFMAX,DT
C#####< NOE-CHANGES
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3,NPFMAX,NCOUCH
      DOUBLE PRECISION, INTENT(OUT)   :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(OUT)   :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(OUT)   :: CONC(NCOUCH)
      DOUBLE PRECISION, INTENT(OUT)   :: TEMP(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(OUT)   :: HDEP(NPOIN2)
      DOUBLE PRECISION, INTENT(OUT)   :: ZR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TREST(NCOUCH)
      INTEGER, INTENT(OUT)            :: NPF(NPOIN2)
      TYPE(BIEF_OBJ)                  :: PRIVE
      LOGICAL, INTENT(IN)             :: TASSE, GIBSON,CONSOL
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION ECOUCH , TCAR
      INTEGER IPOIN, IC, IPF
!
      INTRINSIC LOG10,MAX
C#####> NOE-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      LOGICAL CVGCSL
      INTEGER NITCSL, ITCSL, ERR
      DOUBLE PRECISION DTCSL,RESCSL
      DOUBLE PRECISION, ALLOCATABLE :: TRA01(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: TRA02(:),TRA03(:),ZNOE(:)
!
C ALLOCATES MEMORY AND SETS INITIAL INTERFACE BETWEEN HDEP AND EPAI
      ALLOCATE(TRA01(NPFMAX,6),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'CONDIS : MAUVAISE ALLOCATION DE TRA01'
        IF(LNG.EQ.2) WRITE(LU,*) 'CONDIS: WRONG ALLOCATION OF TRA01'
        STOP
      ENDIF
      ALLOCATE(TRA02(NPFMAX),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'CONDIS : MAUVAISE ALLOCATION DE TRA02'
        IF(LNG.EQ.2) WRITE(LU,*) 'CONDIS: WRONG ALLOCATION OF TRA02'
        STOP
      ENDIF
      ALLOCATE(TRA03(NPFMAX),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'CONDIS : MAUVAISE ALLOCATION DE TRA03'
        IF(LNG.EQ.2) WRITE(LU,*) 'CONDIS: WRONG ALLOCATION OF TRA03'
        STOP
      ENDIF
      ALLOCATE(ZNOE(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'CONDIS : MAUVAISE ALLOCATION DE ZNOE'
        IF(LNG.EQ.2) WRITE(LU,*) 'CONDIS: WRONG ALLOCATION OF ZNOE'
        STOP
      ENDIF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C#####< NOE-CHANGES
!
!=======================================================================
!
C     -----  INITIALISES HDEP                  -----
C     -----  NOT USED BY THE MULTILAYER MODEL  -----
!
C     HERE, THE DEFAULT ACTION IS TO SET HDEP TO 100., HENCE ENABLE THE
C     EROSION PROCESS. THIS VALUE IS COMPATIBLE WITH SUBROUTINE NOEROD
C     IN SISYPHE.
!
      CALL OV('X=C     ',HDEP,HDEP,HDEP,100.D0,NPOIN2)
!
C     -----  INITIALISES ZR  -----
!
      CALL OV('X=Y-Z   ' ,ZR,ZF,HDEP,0.D0,NPOIN2)
!
C#####> NOE-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C     NOTE THAT THE USE OF HDEP & ZR HERE IS ONLY TEMPORARY, IN THE CASE
C     OF NPRIV.GE.1
C     BEFORE SPLIT AND CONSOLIDATION, HDEP WILL BE SET TO ZF - ZR(USER)
C DELETED BY JMH, AWAITING EXPLANATIONS/COMMENTS
C     IF(NPRIV.GE.1) THEN
C       CALL OV('X=Y     ',ZR,PRIVE%ADR(1)%P%R,ZR,0.D0,NPOIN2)
C       CALL OV( 'X=Y-Z   ' ,HDEP,ZF,ZR,0.D0,NPOIN2)
C     ENDIF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C#####< NOE-CHANGES
!
!     -------------------------------------------
C     INITIAL CONDITIONS FOR THE MULTILAYER MODEL
!     -------------------------------------------
!
      IF(TASSE) THEN
!
C       -----  INITIALISES EPAI  -----
!
        CALL OV('X=C     ',EPAI,EPAI,EPAI,0.D0,NPOIN2*NCOUCH)
!
C       -----  INITIALISES CONC  -----
!
        CALL OV('X=C     ',CONC,CONC,CONC,0.D0,NCOUCH)
!
C       DEFAULT OPTION:
C             CONCENTRATIONS ARE COMPUTED AS A FUNCTION
C             OF CONSOLIDATION TIME SCALE
C            (EMPIRICAL RELATION, LOIRE ESTUARY , FRISTCH ET AL. 1989)
!
        TCAR = TREST(NCOUCH)/2.D0
        DO IC = NCOUCH , 1 , -1
          IF (IC.LT.NCOUCH) TCAR = TCAR + (TREST(IC+1)+TREST(IC))/2.D0
          IF (TCAR.LT.24.D0) THEN
            CONC(IC) = 136.2D0*LOG10(TCAR+5.424D0)
          ELSE
            CONC(IC) = 200.D0+70.D0*LOG10(TCAR/24.D0)
          ENDIF
        ENDDO
!
C       -----  CHANGES HOURS INTO SECONDS  -----
!
        CALL OV( 'X=CX    ',TREST,TREST,TREST,3600.D0,NCOUCH)
!
C       -----  INITIALISES TEMP  -----
!
        CALL OV( 'X=C     ',TEMP,TEMP,TEMP,0.D0,NPOIN2*NCOUCH)
!
C       -----  MODIFIES ZR  -----
!
        DO IPOIN=1,NPOIN2
          DO IC=1,NCOUCH
            ZR(IPOIN)=ZR(IPOIN)-EPAI(IC,IPOIN)
          ENDDO
        ENDDO
!
!     ---------------------------------------
C     INITIAL CONDITIONS FOR THE GIBSON MODEL
!     ---------------------------------------
!
      ELSEIF (GIBSON) THEN
!
C       -----  INITIALISES NPF  -----
!
        DO IPOIN=1,NPOIN2
          NPF(IPOIN)=0
        ENDDO
!
C       -----  INITIALISES IVIDE  -----
!
        CALL OV( 'X=C     ', IVIDE ,IVIDE , IVIDE, 0.D0, NPOIN2*NPFMAX)
!
C       -----  INITIALISES EPAI  -----
!
        CALL OV( 'X=C     ', EPAI, EPAI, EPAI, 0.D0, NPOIN2*(NPFMAX-1))
!
C       -----  MODIFIES ZR  -----
!
        DO IPOIN=1,NPOIN2
          DO IPF=1,NPF(IPOIN)-1
            ZR(IPOIN)=ZR(IPOIN)-EPAI(IPF,IPOIN)
            ECOUCH=(IVIDE(IPF,IPOIN)+IVIDE(IPF+1,IPOIN))/2.D0
            EPAI(IPF,IPOIN)=EPAI(IPF,IPOIN)/(1.D0+ECOUCH)
          ENDDO
        ENDDO
!
C#####> NOE-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C       HERE IS OUR CHANCE TO SET
C       IVIDE AND (TRUE) EPAI REPRESENTATIVE OF CONSOLIDATED MATTER
!
C       CONSOL SHOULD BE .TRUE. IF THE LAYER DISTRIBUTION IS NOT KNOWN
C       AND THE USER WANTS TO LET THE MODEL CONSOLIDATE THE BED
!
        IF(CONSOL) THEN
!
C TIME FOR CONSOLIDATION, WHICH COULD ALSO BE READ FROM PRIVE%ADR(2)
          DTCSL = DT*10.D0
C          NITCSL = 100
!
C START PAST TIME LOOP
!
C          DO ITCSL = 1,NITCSL
          CVGCSL = .FALSE.
          DO WHILE(.NOT.CVGCSL)
!
C     -----  MANAGES THE DEPOSITED SEDIMENT:   -----
C     -----  ADDS NEW LAYERS TO THE MUDDY BED  -----
!
            CALL GESTDP( IVIDE,EPAI,HDEP,
     &        NPOIN2,NPFMAX,NPF, EPAI0,CFDEP,RHOS )
!
C     -----  CONSOLIDATES THE MUDDY BED  -----
C     -----  USING GIBSON EQUATION       -----
!
            CALL TASSEM( IVIDE,EPAI, NPOIN2,NPFMAX,NPF, GRAV,RHOS,
     &        DTCSL, CFMAX, TRA01,TRA02,TRA03 )
!
C     -----  UPDATES THE BOTTOM LAYER  -----
C     -----  RECOMPUTES THE INTERFACE  -----
!
      RESCSL = 0.D0
      DO IPOIN = 1,NPOIN2
        ZNOE(IPOIN) = ZR(IPOIN)
        DO IPF = 1,NPF(IPOIN)-1
          ECOUCH=(IVIDE(IPF,IPOIN)+IVIDE(IPF+1,IPOIN))/2.D0
          ZNOE(IPOIN) = ZNOE(IPOIN)+(1.D0+ECOUCH)*EPAI(IPF,IPOIN)
        ENDDO
        IF(ZF(IPOIN).LT.ZNOE(IPOIN)) THEN
        RESCSL = RESCSL + ( MAX(
     & ZF(IPOIN)-(ZNOE(IPOIN)-(1.D0+ECOUCH)*EPAI(NPF(IPOIN)-1,IPOIN)),
     &  0.D0) - HDEP(IPOIN) )**2
      ECOUCH =(IVIDE(NPF(IPOIN)-1,IPOIN)+IVIDE(NPF(IPOIN),IPOIN))/2.D0
        HDEP(IPOIN) = MAX(
     &  ZF(IPOIN)-(ZNOE(IPOIN)-(1.D0+ECOUCH)*EPAI(NPF(IPOIN)-1,IPOIN)),
     &        0.D0)
            EPAI(NPF(IPOIN)-1,IPOIN) = 0.D0
            NPF(IPOIN) = NPF(IPOIN)-1
          ELSE
            RESCSL = RESCSL + (ZF(IPOIN)-ZNOE(IPOIN)-HDEP(IPOIN))**2
            HDEP(IPOIN) = ZF(IPOIN) - ZNOE(IPOIN)
          ENDIF
        ENDDO
        CVGCSL = RESCSL.LT.(1.D-8)
      ENDDO
!
C END PAST TIME LOOP
!
        ENDIF
!
C END IF CONSOL
!
      ENDIF
!
C END IF TASSE/GIBSON
!
      DEALLOCATE(TRA01)
      DEALLOCATE(TRA02)
      DEALLOCATE(TRA03)
      DEALLOCATE(ZNOE)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C#####< NOE-CHANGES
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C