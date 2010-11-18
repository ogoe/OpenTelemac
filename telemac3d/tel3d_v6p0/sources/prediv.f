C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE PRESSURE POISSON EQUATION
!>               (FOR THE DYNAMIC PRESSURE):
!><br>            CASE.
!>  @code
!>                           ----->
!>    NABLA^2 ( PD ) = DIV ( U_DIFF )
!>         ----->
!>   DIV ( U_DIFF )  IS HERE THE COMPUTED SOURCE TERM
!>
!> DIFF_MATRIX (PD, VISC=1) = MASS_MATRIX (DIVU)  (AND BC'S)
!>
!> THE PHYSICAL DYNAMIC PRESSURE IS OBTAINED BY MULTIPLYING: PD*(RHO/DT)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  S0P AND S1P ARE NOT USED.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BC, INFO, PD, UP, VP, WP
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::DM1 DM1@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DSSUDT DSSUDT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IELM2H IELM2H@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IELM2V IELM2V@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IELM3 IELM3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IKLE3 IKLE3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IT1 IT1@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IT2 IT2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIPBOF LIPBOF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIPBOL LIPBOL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIPBOS LIPBOS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MASK MASK@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MASKBR MASKBR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MASKPT MASKPT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MAT2D MAT2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MDIFF MDIFF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MESH2D MESH2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MESH3D MESH3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MSK MSK@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MTRA2 MTRA2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NBOR3 NBOR3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NELEM2 NELEM2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NETAGE NETAGE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN3 NPOIN3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPTFR2 NPTFR2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPTFR3 NPTFR3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NSCE NSCE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPTBAN OPTBAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PBORF PBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PBORL PBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PBORS PBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SEM3D SEM3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SIGMAG SIGMAG@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SLVPOI SLVPOI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SOURCES SOURCES@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SVIDE SVIDE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_01 T2_01@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_01 T3_01@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_02 T3_02@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_03 T3_03@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TRAV2 TRAV2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TRAV3 TRAV3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::U U@endlink, 
!> @link DECLARATIONS_TELEMAC3D::U2D U2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::X X@endlink, 
!> @link DECLARATIONS_TELEMAC3D::Y Y@endlink, 
!> @link DECLARATIONS_TELEMAC3D::Z Z@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ZCONV ZCONV@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::KADH KADH@endlink, 
!> @link DECLARATIONS_TELEMAC::KDDL KDDL@endlink, 
!> @link DECLARATIONS_TELEMAC::KDIR KDIR@endlink, 
!> @link DECLARATIONS_TELEMAC::KENT KENT@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, ELMD, ELMX, FORMUL, I, IIS, IPLAN, IPOIN2, IPOIN3, IPTFR3, IS, OPT, SIZD, SIZX, TETAP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), DIRI01(), EXTMSK(), FLUPRI(), MATRIX(), OS(), OSDB(), OV(), PARCOM(), PLANTE(), SOLVE(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D(), WAVE_EQUATION()

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
!>      <td><center> 6.0                                       </center>
!> </td><td> 21/05/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18; JACEK A. JANKOWSKI - UNIVERSITAET HANNOVER
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 02/09/2002
!> </td><td> JMH
!> </td><td> TIDAL FLATS
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BC
!></td><td>--></td><td>LOGICAL, IF YES, BOUNDARY CONDITIONS
!>                  ARE TAKEN INTO ACCOUNT (SEE CALL FLUPRI...)
!>                  AND DIRICHLET VALUES ARE CONSIDERED
!>                  IF NO, DIRICHLET VALUES ARE SET TO 0
!>                  AND BOUNDARY CONDITIONS ARE DISCARDED
!>    </td></tr>
!>          <tr><td>INFO
!></td><td>--></td><td>INFORMATION ASKED ON SOLVER ITERATIONS
!>    </td></tr>
!>          <tr><td>OPT
!></td><td>--></td><td>OPTION (1 OR 2)
!>    </td></tr>
!>          <tr><td>PD
!></td><td><-></td><td>DYNAMIC PRESSURE
!>    </td></tr>
!>          <tr><td>UP,VP,WP
!></td><td>--></td><td>INTERMEDIATE VELOCITY FIELD
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PREDIV
     & ( PD, UP, VP, WP, INFO , BC )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BC             |-->| LOGICAL, IF YES, BOUNDARY CONDITIONS
C|                |   | ARE TAKEN INTO ACCOUNT (SEE CALL FLUPRI...)
C|                |   | AND DIRICHLET VALUES ARE CONSIDERED
C|                |   | IF NO, DIRICHLET VALUES ARE SET TO 0
C|                |   | AND BOUNDARY CONDITIONS ARE DISCARDED
C| INFO           |-->| INFORMATION ASKED ON SOLVER ITERATIONS
C| OPT            |-->| OPTION (1 OR 2)
C| PD             |<->| DYNAMIC PRESSURE
C| UP,VP,WP       |-->| INTERMEDIATE VELOCITY FIELD
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: PD
      TYPE(BIEF_OBJ), INTENT(IN)      :: UP, VP, WP
      LOGICAL, INTENT(IN)             :: INFO,BC
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPOIN3,IPOIN2,IPTFR3,IPLAN,IS,IIS,SIZD,SIZX,ELMD,ELMX,I
      DOUBLE PRECISION C,TETAP
      CHARACTER(LEN=16) FORMUL
C
      INTEGER OPT
      OPT=1
!
!=======================================================================
C RIGHT HAND SIDE VECTOR SEM3D = - DIV (INTERMEDIATE VELOCITY)
!=======================================================================
!
      IF(OPT.EQ.1) THEN
C                     2 FILTERING CRUSHED ELEMENTS
        FORMUL='GRADF 2         '
        CALL VECTOR
     &   (SEM3D, '=',FORMUL(1:15)//'X', IELM3,-1.D0,UP, SVIDE,SVIDE,
     &    SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
        CALL VECTOR
     &   (SEM3D, '+',FORMUL(1:15)//'Y', IELM3,-1.D0,VP, SVIDE,SVIDE,
     &    SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
        CALL VECTOR
     &   (SEM3D, '+',FORMUL(1:15)//'Z', IELM3,-1.D0,WP, SVIDE,SVIDE,
     &    SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
!
C  FORGOT TERM : ESSAI (DOES NOT WORK)
!
C                                   SOLID BOUNDARIES
C       CALL EXTMSK(MASKBR,MASK%ADR(5)%P%R,NPTFR2,NETAGE)
C       CALL VECTOR
C    &   (T3_01, '=','FLUBOR          ',IELM2V,1.D0,SVIDE,SVIDE,SVIDE,
C    &    UP,VP,SVIDE,MESH3D,.TRUE.,MASKBR)
C       CALL OSDB( 'X=X+Y   ' ,SEM3D,T3_01,T3_01,0.D0,MESH3D)
!
C       CORRECT BOUNDARY CONDITIONS
C       BUT AT THE MOMENT, THOSE AT THE SURFACE ARE OVERWRITTEN
C       WITH THE P=0 CONDITION
!
        IF(BC) THEN
          CALL FLUPRI(SEM3D%R,1.D0,UP%R,VP%R,WP%R,
     &                X,Y,Z,IKLE3%I,MESH3D%NELEM,
     &                MESH3D%NELMAX,NELEM2,NPOIN2,NPOIN3,
     &                MESH2D%W%R(         1:  NELEM2),
     &                MESH2D%W%R(  NELEM2+1:2*NELEM2),
     &                MESH2D%W%R(2*NELEM2+1:3*NELEM2)  )
C         TERM IN D(ZS)/DT (BUT BE CAREFUL DSSUDT=(H-HN)/DT)
          CALL VECTOR(T2_01,'=', 'MASVEC          ',IELM2H,-1.D0,DSSUDT,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH2D,MSK,MASKEL)
          CALL OV('X=X+Y   ',SEM3D%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                       T2_01%R,T2_01%R,0.D0,NPOIN2)
C         TERM IN D(ZF)/DT IS MISSING
        ENDIF
!
      ELSEIF(OPT.EQ.2) THEN
!
        CALL VECTOR(SEM3D,'=','VGRADP       TOT',IELM3,1.D0,
     &              DM1,ZCONV,SVIDE,UP,VP,WP,MESH3D,MSK,MASKEL)
C       CALL OS('X=X-Y   ',X=SEM3D,Y=FLUEXT)
        CALL EXTMSK(MASKBR,MASK%ADR(8)%P%R,NPTFR2,NETAGE)
        CALL VECTOR
     &   (T3_01, '=','FLUBOR          ',IELM2V,1.D0,SVIDE,SVIDE,SVIDE,
     &    UP,VP,SVIDE,MESH3D,.TRUE., MASKBR)
        CALL OSDB( 'X=X-Y   ' ,SEM3D,T3_01,T3_01,0.D0,MESH3D)
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,*) 'OPTION INCONNUE DANS PREDIV: ',OPT
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOWN OPTION IN PREDIV: ',OPT
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!=======================================================================
C SOURCES INSIDE THE DOMAIN : DIV(U) WILL NOT BE 0
!=======================================================================
!
      IF(NSCE.GE.1) THEN
C
        DO IS=1,NSCE
          IIS=IS
C         HERE SOURCES IN THE NON ASSEMBLED (PARCOM) FORM
C         SEE SOURCES_SINKS
          IF(NCSIZE.GT.1) IIS=IS+NSCE
          CALL OS('X=X+Y   ',X=SEM3D,Y=SOURCES%ADR(IIS)%P)
        ENDDO
C
      ENDIF
!
!=======================================================================
C DIFFUSION MATRIX AND BOUNDARY TERMS (BC)
!=======================================================================
!
      CALL CPSTVC(SEM3D,T3_01)
      CALL OS('X=C     ', X=T3_01,C=1.D0)
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        FORMUL='MATDIF       MON'
      ELSE
        FORMUL='MATDIF          '
      ENDIF
      CALL MATRIX(MDIFF, 'M=N     ', FORMUL, IELM3, IELM3,1.D0,
     &            T3_01, T3_01, T3_01,
     &            SVIDE, SVIDE, SVIDE, MESH3D, MSK, MASKEL)
!
!=======================================================================
C IMPOSED (DIRICHLET) BC + PRECONDITIONING + LINEAR EQUATION SOLVER
!=======================================================================
!
C IMPOSED BC
!
      DO IPOIN3 = 1,NPOIN3
        IT1%I(IPOIN3) = KDDL
        IT2%I(IPOIN3) = 0
        T3_03%R(IPOIN3) = 0.D0
      ENDDO
!
C     LATERAL BOUNDARIES
!
      IF(BC) THEN
        DO IPTFR3 = 1,NPTFR3
          IF(LIPBOL%I(IPTFR3).EQ.KENT.OR.LIPBOL%I(IPTFR3).EQ.KADH) THEN
            IT1%I(NBOR3%I(IPTFR3)) = KDIR
            T3_03%R(NBOR3%I(IPTFR3)) = PBORL%R(IPTFR3)
          ENDIF
        ENDDO
      ELSE
C       PBORL REPLACED BY 0
        DO IPTFR3 = 1,NPTFR3
          IF(LIPBOL%I(IPTFR3).EQ.KENT.OR.LIPBOL%I(IPTFR3).EQ.KADH) THEN
            IT1%I(NBOR3%I(IPTFR3)) = KDIR
            T3_03%R(NBOR3%I(IPTFR3)) = 0.D0
          ENDIF
        ENDDO
      ENDIF
!
C     BOTTOM
!
      IF(BC) THEN
        DO IPOIN2 = 1,NPOIN2
          IF(LIPBOF%I(IPOIN2).EQ.KENT.OR.LIPBOF%I(IPOIN2).EQ.KADH) THEN
            IT1%I(IPOIN2) = KDIR
            T3_03%R(IPOIN2) = PBORF%R(IPOIN2)
          ENDIF
        ENDDO
      ELSE
        DO IPOIN2 = 1,NPOIN2
          IF(LIPBOF%I(IPOIN2).EQ.KENT.OR.LIPBOF%I(IPOIN2).EQ.KADH) THEN
            IT1%I(IPOIN2) = KDIR
            T3_03%R(IPOIN2) = 0.D0
          ENDIF
        ENDDO
      ENDIF
!
C     FREE SURFACE
!
      IF(NCSIZE.GT.1) THEN
        DO IPOIN2 = 1,NPOIN2
          IF(LIPBOS%I(IPOIN2).EQ.KENT.OR.LIPBOS%I(IPOIN2).EQ.KADH) THEN
            IPOIN3=NPOIN3-NPOIN2+IPOIN2
            IT1%I(IPOIN3) = KDIR
            T3_03%R(IPOIN3) = PBORS%R(IPOIN2)
            MDIFF%D%R(IPOIN3)=MESH3D%FAC%R(IPOIN3)
          ENDIF
        ENDDO
      ELSE
        DO IPOIN2 = 1,NPOIN2
          IF(LIPBOS%I(IPOIN2).EQ.KENT.OR.LIPBOS%I(IPOIN2).EQ.KADH) THEN
            IPOIN3=NPOIN3-NPOIN2+IPOIN2
            IT1%I(IPOIN3) = KDIR
            T3_03%R(IPOIN3) = PBORS%R(IPOIN2)
            MDIFF%D%R(IPOIN3)=1.D0
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
C     CRUSHED ELEMENTS AND TIDAL FLATS: FIRST TREATED AS DIRICHLET
C     A CRUSHED POINT HAS NO DIAGONAL
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        IF(NCSIZE.GT.1) THEN
          CALL OS('X=Y     ',X=T3_02,Y=MDIFF%D)
C         T3_02 WILL BE THE ASSEMBLED DIAGONAL
          CALL PARCOM(T3_02,2,MESH3D)
          DO IPOIN3=1,NPOIN3
            IF(T3_02%R(IPOIN3).LT.1.D-10) THEN
              MDIFF%D%R(IPOIN3)= MESH3D%FAC%R(IPOIN3)
              IT1%I(IPOIN3)    = KDIR
              IT2%I(IPOIN3)    = 1
              T3_03%R(IPOIN3)  = PD%R(IPOIN3)
            ENDIF
          ENDDO
        ELSE
          DO IPOIN3=1,NPOIN3
            IF(MDIFF%D%R(IPOIN3).LT.1.D-10) THEN
              MDIFF%D%R(IPOIN3)= 1.D0
              IT1%I(IPOIN3)    = KDIR
              IT2%I(IPOIN3)    = 1
              T3_03%R(IPOIN3)  = PD%R(IPOIN3)
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      CALL DIRI01(PD,MDIFF,SEM3D,T3_03,IT1%I,T3_01,T3_02,MESH3D,
     &            KDIR,MSK,MASKPT)
!
C     SOLVES THE LINEAR EQUATION SYSTEM MDIFF * PD = SEM3D
!
      IF(NPLAN.EQ.2.AND.MDIFF%STO.EQ.3.AND.
     &   MDIFF%ELMLIN.EQ.41.AND.MDIFF%ELMCOL.EQ.41) THEN
!
C       SINCE THE UPPER PLANE IS DIRICHLET, THIS IS A 2D PROBLEM
!
C       SEM3D AND PD TURNED INTO 2D
        CALL CPSTVC(U2D,PD)
        CALL CPSTVC(U2D,SEM3D)
!
C       PROPERTIES OF MATRIX MODIFIED
C       THE FIRST SEGMENTS IN A 3D MATRIX ARE THE HORIZONTAL
C       SEGMENTS OF THE BOTTOM, HENCE JUST LIKE A 2D MATRIX
C       THE FIRST POINTS IN THE DIAGONAL ARE THE BOTTOM POINTS
!
        SIZD=MDIFF%D%DIM1
        SIZX=MDIFF%X%DIM1
        ELMD=MDIFF%D%ELM
        MDIFF%D%DIM1=NPOIN2
        MDIFF%X%DIM1=MESH2D%NELEM
        MDIFF%ELMLIN=11
        MDIFF%ELMCOL=11
        MDIFF%D%ELM=11
!
        CALL SOLVE(PD,MDIFF,SEM3D,TRAV2,SLVPOI,INFO,MESH2D,
     &             MAT2D%ADR(1)%P)
!
C       OLD PROPERTIES RESTORED
        MDIFF%ELMLIN=41
        MDIFF%ELMCOL=41
        MDIFF%D%DIM1=SIZD
        MDIFF%X%DIM1=SIZX
        MDIFF%D%ELM=ELMD
        CALL CPSTVC(U,PD)
        CALL CPSTVC(U,SEM3D)
!
      ELSE
!
C       REAL 3D PROBLEM
!
        CALL SOLVE(PD,MDIFF,SEM3D,TRAV3,SLVPOI,INFO,MESH3D,MTRA2)
!
      ENDIF
!
!=======================================================================
!
C   POINTS WITH NO DIAGONAL ARE EQUAL TO THE POINT ABOVE
C   THEY HAD BEEN TREATED ABOVE AS DIRICHLET
!
!=======================================================================
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        DO IPLAN=NPLAN-1,1,-1
          DO IPOIN2=1,NPOIN2
            IPOIN3=IPOIN2+(IPLAN-1)*NPOIN2
            IF(IT2%I(IPOIN3).EQ.1) THEN
C             COPIES THE VALUE OF THE UPPER POINT
              PD%R(IPOIN3)=PD%R(IPOIN3+NPOIN2)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C