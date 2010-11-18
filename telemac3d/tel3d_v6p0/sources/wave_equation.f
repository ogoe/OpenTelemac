C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DIFFUSION AND PROPAGATION STEP IN 3D USING THE WAVE
!>                EQUATION METHOD.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D, INTERFACE_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ISOUSI, LT
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::AGGLOH AGGLOH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AUBORF AUBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AUBORL AUBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AUBORS AUBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BUBORF BUBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BUBORL BUBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BUBORS BUBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BVBORF BVBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BVBORL BVBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BVBORS BVBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::CLDYN CLDYN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::CONV CONV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DH DH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DM1 DM1@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DP DP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DPWAVEQ DPWAVEQ@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DSSUDT DSSUDT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DT DT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLBOR FLBOR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLINT2 FLINT2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GRADZF GRADZF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GRADZN GRADZN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GRADZS GRADZS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GRAV GRAV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::H H@endlink, 
!> @link DECLARATIONS_TELEMAC3D::HBOR HBOR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::HN HN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IELM2H IELM2H@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IELM2V IELM2V@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IELM3 IELM3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IKLE2 IKLE2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::INCHYD INCHYD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::INFOGR INFOGR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IORDRH IORDRH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IPBOT IPBOT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIHBOR LIHBOR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIMPRO LIMPRO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIUBOF LIUBOF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIUBOL LIUBOL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIUBOS LIUBOS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIVBOF LIVBOF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIVBOL LIVBOL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIVBOS LIVBOS@endlink, 
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
!> @link DECLARATIONS_TELEMAC3D::NBOR2 NBOR2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NELEM2 NELEM2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NETAGE NETAGE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NONHYD NONHYD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN3 NPOIN3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPTFR2 NPTFR2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPTFR3 NPTFR3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NUWAVE NUWAVE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPTBAN OPTBAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPT_HNEG OPT_HNEG@endlink, 
!> @link DECLARATIONS_TELEMAC3D::S0U S0U@endlink, 
!> @link DECLARATIONS_TELEMAC3D::S0V S0V@endlink, 
!> @link DECLARATIONS_TELEMAC3D::S1U S1U@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SCHCVI SCHCVI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SEM2D SEM2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SIGMAG SIGMAG@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SLVPRO SLVPRO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SMH SMH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SVIDE SVIDE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_01 T2_01@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_02 T2_02@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_03 T2_03@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_04 T2_04@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_05 T2_05@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_01 T3_01@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_02 T3_02@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_03 T3_03@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_04 T3_04@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_05 T3_05@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_06 T3_06@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_08 T3_08@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_09 T3_09@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_10 T3_10@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TE1 TE1@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TETADI TETADI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TETAH TETAH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TETAU TETAU@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TETAZCOMP TETAZCOMP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TRAV2 TRAV2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::U U@endlink, 
!> @link DECLARATIONS_TELEMAC3D::U2D U2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UBORF UBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UBORL UBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UBORS UBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UC UC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UCONV UCONV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UCONVC UCONVC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UD UD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UN UN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UNSV2D UNSV2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UNSV3D UNSV3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::V V@endlink, 
!> @link DECLARATIONS_TELEMAC3D::V2D V2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VBORF VBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VBORL VBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VBORS VBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VC VC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VCONV VCONV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VCONVC VCONVC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VD VD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VELPROBOT VELPROBOT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VELPROLAT VELPROLAT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VISCVI VISCVI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VN VN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VOLU2D VOLU2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::W W@endlink, 
!> @link DECLARATIONS_TELEMAC3D::WD WD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::Z Z@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ZCONV ZCONV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ZF ZF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ZFLATS ZFLATS@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::ADV_CAR ADV_CAR@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_SUP ADV_SUP@endlink, 
!> @link DECLARATIONS_TELEMAC::KADH KADH@endlink, 
!> @link DECLARATIONS_TELEMAC::KDIR KDIR@endlink, 
!> @link DECLARATIONS_TELEMAC::KENT KENT@endlink, 
!> @link DECLARATIONS_TELEMAC::KENTU KENTU@endlink, 
!> @link DECLARATIONS_TELEMAC::KLOG KLOG@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUB, C, FORMUL, I, I1, I2, I2D, I3, I3D, I3DP, IELEM, IP, IPLAN, IPOIN2, IPTFR3, NP, TRIC, TRID, TRIE, UAUX, VAUX, VNORM
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_WAVE_EQUATION, TRIC, TRID, TRIE, UAUX, VAUX
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> AIRWIK3(), CPSTVC(), DIRICH(), EXTMSK(), GETTRI(), IELBOR(), LUMP(), MATRIX(), MATVEC(), NUWAVE_P0(), OM(), OS(), OSBD(), OSDB(), OV(), PARCOM(), PLANTE(), PREDIV(), SLOPES(), SOLVE(), STRESS(), SUMVER(), TRID3D(), VECTOR(), VELRES(), VERMOY()
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
!>      <td><center> 6.0                                       </center>
!> </td><td> 05/05/2010
!> </td><td> J.M. HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> MODIFIED CASE DPWAVEQ (SECOND COMPUTATION OF
!>           DYNAMIC PRESSURE CANCELLED IN TELEMAC3D.F
!>           AND SOME TUNING HERE)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 20/08/2009
!> </td><td> JMH
!> </td><td> NOW COMPUTES UNSV3D IN MESH_PROP
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 18/08/2009
!> </td><td> JMH
!> </td><td> COMPUTES UCONVC AND VCONVC AT THE END (SEE PRECON)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 27/07/2009
!> </td><td> JMH
!> </td><td> MODIFIED TREATMENT OF FRICTION TERMS ON DRY ZONES ;
!>           CLIPPING OF UNSV3D USELESS (IT IS IN MATMAS)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 23/01/2009
!> </td><td> JMH
!> </td><td> SUMS FRICTION TERMS IN T3_04 ;
!>           IF(NCSIZE.GT.1) CALLS PARCOM AT THE END
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ISOUSI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LT
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE WAVE_EQUATION
     &(LT,ISOUSI)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ISOUSI         |---| 
C| LT             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_WAVE_EQUATION => WAVE_EQUATION
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: LT,ISOUSI
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=16) :: FORMUL
      INTEGER           :: I,IPTFR3,IPOIN2,IPLAN,I1,I2,I3,IELEM,NP
      INTEGER           :: I3DP,I2D,I3D,IP
      DOUBLE PRECISION  :: C,AUB,VNORM
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
!-----------------------------------------------------------------------
!
C     DEFINES POINTERS TO RENAME PARTS OF MEMORY
C     MTRA2%X%R HAS AT LEAST THE SIZE 30*NELMAX (STORAGE 1)
C                                  OR  2*NSEG3D (STORAGE 3)
!
C     WE NEED: 8*NPOIN
C           OR 8*NPOIN
!
!
      DOUBLE PRECISION, POINTER :: TRIC(:),TRID(:),TRIE(:)
      DOUBLE PRECISION, POINTER :: UAUX(:),VAUX(:)
      TRIC=>MTRA2%X%R(          1:   NPOIN3)
      TRID=>MTRA2%X%R(   NPOIN3+1: 2*NPOIN3)
      TRIE=>MTRA2%X%R( 2*NPOIN3+1: 3*NPOIN3)
C         =>MTRA2%X%R( 3*NPOIN3+1: 4*NPOIN3) USED IN TRID3D
C         =>MTRA2%X%R( 4*NPOIN3+1: 5*NPOIN3) USED IN TRID3D
      UAUX=>MTRA2%X%R( 5*NPOIN3+1: 6*NPOIN3)
      VAUX=>MTRA2%X%R( 6*NPOIN3+1: 7*NPOIN3)
!
      IF(7*NPOIN3.GT.30*MESH3D%NELMAX.OR.
     &   7*NPOIN3.GT.2*MESH3D%NSEG) THEN
        WRITE(LU,*) 'PROBLEME DE PLACE MEMOIRE DANS WAVE_EQUATION'
        WRITE(LU,*) 'NPOIN3=',NPOIN3
        WRITE(LU,*) 'NELMAX=',MESH3D%NELMAX
        WRITE(LU,*) 'NSEG=',MESH3D%NSEG
        CALL PLANTE(1)
        STOP
      ENDIF
!
!=======================================================================
C    1) COMPUTES THE DIFFUSION TERMS DIFF
!
C       AND THEN UC + DT(F - DIFF -G GRAD(Z))
!
C       STORED IN T3_01 AND T3_02
!
!=======================================================================
!
      FORMUL='MATDIF          '
      IF(INCHYD) FORMUL(7:7)='2'
!
      CALL MATRIX(MDIFF,'M=N     ',FORMUL,IELM3,IELM3,1.D0,
     &            VISCVI%ADR(1)%P,VISCVI%ADR(2)%P,VISCVI%ADR(3)%P,
     &            SVIDE,SVIDE,SVIDE,MESH3D,MSK,MASKEL)
!
C     IMPLICITATION OF DIAGONAL TERMS
C     BUILDS A TRIDIAGONAL MATRIX IN OFF-DIAGONAL TERMS OF MTRA2
!
      CALL GETTRI(MTRA2%X%R,MDIFF,TETADI,MESH3D,NPLAN,MESH2D%NPOIN,
     &            MESH2D%NSEG)
!
      DO I=1,U%DIM1
        TRIC(I)=TRIC(I)*UNSV3D%R(I)*DT
        TRID(I)=TRID(I)*UNSV3D%R(I)*DT
        TRIE(I)=TRIE(I)*UNSV3D%R(I)*DT
      ENDDO
!
C     EXPLICIT DIFFUSION TERMS
!
      CALL MATVEC ('X=AY     ',T3_01,MDIFF,UN,0.D0,MESH3D)
      CALL MATVEC ('X=AY     ',T3_02,MDIFF,VN,0.D0,MESH3D)
!
C     EXPLICIT STRESS TERMS
!
      CALL STRESS(T3_01,'X=X-Y   ',T2_02,T3_04,
     &            BUBORL,BUBORF,BUBORS,NPOIN2,NPOIN3,MESH2D,
     &            MESH3D,IELM3,IELM2H,IELM2V,SVIDE,MSK,MASKBR,MASKEL)
      CALL STRESS(T3_02,'X=X-Y   ',T2_02,T3_04,
     &            BVBORL,BVBORF,BVBORS,NPOIN2,NPOIN3,MESH2D,
     &            MESH3D,IELM3,IELM2H,IELM2V,SVIDE,MSK,MASKBR,MASKEL)
!
C     REQUIRES REAL VALUES IN PARALLEL MODE
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(T3_01,2,MESH3D)
        CALL PARCOM(T3_02,2,MESH3D)
      ENDIF
!
C     DIVIDES BY VOLUME OF BASES
!
      CALL OS('X=XY    ',X=T3_01,Y=UNSV3D)
      CALL OS('X=XY    ',X=T3_02,Y=UNSV3D)
!
C     COMPUTES UC + DT(F - DIFF -G GRAD(Z))
C     STARTS THE COMPUTATION OF UAUX AND VAUX
!
C     NEW IN VERSION 5.8 : SUPG SCHEME IS POSSIBLE
C                          AND RESULT OF ADVECTION IS THEN IN UD AND VD
C                          AFTER CALL TO CVDF3D (BUT DIFFUSION AND
C                          SOURCES TERMS NOT DONE IN CVDF3D, SEE
C                          SCHDVI_HOR AND YAS0U, YAS1U IN TELEMAC3D.F)
!
      IF(SCHCVI.EQ.ADV_SUP) THEN
        CALL OS('X=Y     ',X=UC,Y=UD)
        CALL OS('X=Y     ',X=VC,Y=VD)
      ENDIF
!
      IF(S0U%TYPR.NE.'0') THEN
        DO I=1,U%DIM1
          I2=MOD(I-1,NPOIN2)+1
          T3_01%R(I)=UC%R(I)
     &    +DT*(S0U%R(I)-T3_01%R(I)-TETAZCOMP*GRAV*GRADZN%ADR(1)%P%R(I2))
          T3_02%R(I)=VC%R(I)
     &    +DT*(S0V%R(I)-T3_02%R(I)-TETAZCOMP*GRAV*GRADZN%ADR(2)%P%R(I2))
        ENDDO
      ELSE
        DO I=1,U%DIM1
          I2=MOD(I-1,NPOIN2)+1
          T3_01%R(I)=UC%R(I)
     &    +DT*(-T3_01%R(I)-TETAZCOMP*GRAV*GRADZN%ADR(1)%P%R(I2))
          T3_02%R(I)=VC%R(I)
     &    +DT*(-T3_02%R(I)-TETAZCOMP*GRAV*GRADZN%ADR(2)%P%R(I2))
        ENDDO
      ENDIF
!
!=======================================================================
C    2) COMPUTES 1/(1+DT*(FROT3D+S1U))      IN T3_04
!
C       IT IS ASSUMED THAT S1V=S1U         (IT SHOULD FOR TENSORIALITY)
C                     THAT AVBORL=AUBORL
C                     THAT AVBORF=AUBORF
C                     THAT AVBORS=AUBORS
!
!=======================================================================
!
C     2.1) COMPUTES THE FRICTION TERM FROT3D + IMPLICIT SOURCES
!
      CALL CPSTVC(S1U,T3_04)
C     ERASES ALL VALUES (ONLY BOUNDARY VALUES WILL BE CHANGED BELOW)
      CALL OS('X=0     ',X=T3_04)
!
C     BOTTOM (MASS-LUMPED FORM AS IN 2D):
!
      IF(AUBORF%TYPR.NE.'0') THEN
C       VERSION WITH 1/COS(SLOPE) (OTHERWISE USE VOLU2D INSTEAD OF T2_01)
        CALL SLOPES(TE1,ZF,MESH2D)
        CALL VECTOR(T2_01,'=','MASBAS          ',IELM2H,1.D0,SVIDE,
     &              SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH2D,.TRUE.,TE1)
C       IF(NCSIZE.GT.1) DONE ON T3_04 AT THE END
!
        IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
C         TREATS CRUSHED LAYERS AND TIDAL FLATS IN THE SAME WAY
          DO IPOIN2=1,NPOIN2
            DO NP=0,IPBOT%I(IPOIN2)
              I=NP*NPOIN2+IPOIN2
              T3_04%R(I)=-AUBORF%R(IPOIN2)*T2_01%R(IPOIN2)
            ENDDO
          ENDDO
        ELSE
          DO I=1,NPOIN2
            T3_04%R(I)=-AUBORF%R(I)*T2_01%R(I)
          ENDDO
        ENDIF
      ENDIF
!
C     LATERAL FACES (MASS-LUMPED FORM)
!
      IF(AUBORL%TYPR.NE.'0') THEN
        CALL VECTOR(T3_06, '=','MASBAS          ',IELM2V,+1.D0,SVIDE,
     &              SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH3D,MSK, MASKEL)
C       IF(NCSIZE.GT.1) : DONE ON THE FINAL T3_04
        CALL OSDB( 'X=X-YZ  ' ,T3_04,AUBORL,T3_06,C,MESH3D)
      ENDIF
!
C     SURFACE (MASS-LUMPED FORM):
!
      IF(AUBORS%TYPR.NE.'0') THEN
        CALL OV('X=X-YZ  ',T3_04%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                     AUBORS%R,VOLU2D%R,C,NPOIN2)
      ENDIF
!
      IF(NCSIZE.GT.1) CALL PARCOM(T3_04,2,MESH3D)
!
C     2.2) COMPUTES THE INVERSE OF THE DENOMINATOR FOR U(N+1) AND V(N+1)
!
      IF(S1U%TYPR.NE.'0') THEN
        DO I=1,U%DIM1
          TRID(I)=TRID(I)+1.D0+DT*(S1U%R(I)+T3_04%R(I)*UNSV3D%R(I))
        ENDDO
      ELSE
        DO I=1,U%DIM1
          TRID(I)=TRID(I)+1.D0+DT*(T3_04%R(I)*UNSV3D%R(I))
        ENDDO
      ENDIF
!
C     COMPUTES THE SOLUTION OF TRI * X = UNITY VECTOR EVERYWHERE
C     PUT IN INV1
!
      CALL OS('X=C     ',X=T3_04,C=1.D0)
      CALL TRID3D(MTRA2%X%R,DM1%R,T3_04%R,NPOIN3,NPOIN2)
!
C     LATERAL BOUNDARY CONDITION: CANCELS DM1 FOR THE VELOCITY DIRICHLET
!
      DO IPTFR3 = 1,NPTFR3
        IF(LIUBOL%I(IPTFR3).EQ.KENT.OR.
     &     LIUBOL%I(IPTFR3).EQ.KENTU.OR.LIUBOL%I(IPTFR3).EQ.KADH) THEN
          DM1%R(MESH3D%NBOR%I(IPTFR3)) = 0.D0
        ENDIF
      ENDDO
!
!=======================================================================
C    2) COMPUTES THE NEW DEPTH WITH WAVE EQUATION
!=======================================================================
!
C     STARTS COMPUTATION OF THE SECOND MEMBER (IN SEM2D%ADR(1)%P)
!
      CALL OS('X=Y     ',X=SEM2D%ADR(1)%P,Y=SMH)
!
C     PSEUDO-VISCOSITY IN THE WAVE EQUATION (IN NUWAVE, P0 FUNCTION)
!
      CALL NUWAVE_P0(NUWAVE%R,DM1%R,Z,T3_03%R,IKLE2%I,
     &               NPOIN2,NPLAN,MESH2D%NELMAX,NELEM2,
     &               GRAV*TETAH*TETAU*DT)
!
C     CORRESPONDING DIFFUSION MATRIX
!
      CALL MATRIX(MAT2D%ADR(1)%P,'M=N     ','MATDIF          ',
     &            IELM2H,IELM2H,1.D0,SVIDE,SVIDE,SVIDE,
     &            NUWAVE,SVIDE,SVIDE,MESH2D,MSK,MASKEL)
!
C     STORES THIS MATRIX FOR THE COMPUTATION OF FLINT2
!
      CALL OM('M=N     ',MAT2D%ADR(2)%P,MAT2D%ADR(1)%P,
     &        SVIDE,0.D0,MESH2D)
!
C     SEM2D%ADR(1)%P = SEM2D%ADR(1)%P + INTEGRAL ON OMEGA3D
!
C     3D VECTOR TO INTEGRATE (IN UCONV, VCONV)
!
C     COMPUTES UAUX BY SOLVING TRIDIAGONAL SYSTEMS
!
      CALL TRID3D(MTRA2%X%R,UAUX,T3_01%R,NPOIN3,NPOIN2)
      CALL TRID3D(MTRA2%X%R,VAUX,T3_02%R,NPOIN3,NPOIN2)
!
C     TAKES THE PRESSURE GRADIENT INTO ACCOUNT
!
      IF(NONHYD.AND.DPWAVEQ) THEN
!
C       COMPUTES AN ESTIMATE OF THE DYNAMIC PRESSURE WITH UAUX TAKEN
C       AS U(N+1). THIS ESTIMATE WILL BE THE RESULT GIVEN IN THE RESULT
C       FILE, AS DP IN THE SECOND CALL TO PREDIV IS (ONLY) INCREMENTAL.
!
        CALL CPSTVC(UN,T3_04)
        CALL CPSTVC(VN,T3_05)
        CALL CPSTVC(WD,T3_06)
        CALL OV('X=Y     ',T3_04%R,UAUX,UAUX,0.D0,NPOIN3)
        CALL OV('X=Y     ',T3_05%R,VAUX,VAUX,0.D0,NPOIN3)
        CALL OS('X=Y     ',X=T3_06,Y=WD)
C       NON COMPATIBLE PART OF FREE SURFACE GRADIENT
C      (WHICH IS NOT IN UAUX, SEE ALSO FINAL COMPUTATION OF U AND V)
        IF(ABS(1.D0-TETAZCOMP).GT.1.D-6) THEN
          C=-DT*GRAV*(1.D0-TETAZCOMP)
          DO I=1,U%DIM1
            T3_04%R(I)=T3_04%R(I)+
     &      C*GRADZN%ADR(1)%P%R(MOD(I-1,NPOIN2)+1)*DM1%R(I)
            T3_05%R(I)=T3_05%R(I)+
     &      C*GRADZN%ADR(2)%P%R(MOD(I-1,NPOIN2)+1)*DM1%R(I)
          ENDDO
        ENDIF
!
C       TAKES DH INTO ACCOUNT, IF KNOWN (I.E. FROM 2ND SUBITERATIONS ON)
C       TAKING DH FROM PREVIOUS TIMESTEP IS NOT A GOOD IDEA
!
        IF(ISOUSI.GT.1) THEN
          CALL VECTOR(T2_02,'=','GRADF          X',
     &                IELM2H,-GRAV*TETAH,DH,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &                MESH2D,MSK,MASKEL)
          CALL VECTOR(T2_03,'=','GRADF          Y',
     &                IELM2H,-GRAV*TETAH,DH,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &                MESH2D,MSK,MASKEL)
          IF(NCSIZE.GT.1) THEN
            CALL PARCOM(T2_02,2,MESH2D)
            CALL PARCOM(T2_03,2,MESH2D)
          ENDIF
          CALL OS('X=XY    ',X=T2_03,Y=UNSV2D)
          CALL OS('X=XY    ',X=T2_02,Y=UNSV2D)
          DO I=1,U%DIM1
            T3_04%R(I)=T3_04%R(I)+DT*T2_02%R(MOD(I-1,NPOIN2)+1)*DM1%R(I)
            T3_05%R(I)=T3_05%R(I)+DT*T2_03%R(MOD(I-1,NPOIN2)+1)*DM1%R(I)
          ENDDO
        ENDIF
!
C       APPLIES LATERAL BOUNDARY CONDITIONS
C       NOT VERY SIGNIFICANT
!
C       DO IPTFR3 = 1,NPTFR3
C         IF(LIUBOL%I(IPTFR3).EQ.KENT .OR.
C    *       LIUBOL%I(IPTFR3).EQ.KENTU.OR.
C    *       LIUBOL%I(IPTFR3).EQ.KADH) THEN
C            T3_04%R(MESH3D%NBOR%I(IPTFR3)) = UBORL%R(IPTFR3)
C         ENDIF
C         IF(LIVBOL%I(IPTFR3).EQ.KENT .OR.
C    *       LIVBOL%I(IPTFR3).EQ.KENTU.OR.
C    *       LIVBOL%I(IPTFR3).EQ.KADH) THEN
C            T3_05%R(MESH3D%NBOR%I(IPTFR3)) = VBORL%R(IPTFR3)
C         ENDIF
C       ENDDO
!
C       BEWARE: PREDIV WILL ERASE ALL T3_** WORK ARRAYS BECAUSE CALLS SOLVE
        CALL PREDIV(DP,T3_04,T3_05,T3_06,INFOGR,.TRUE.)
C       APPLIES CORRECTION TO UAUX
        CALL VELRES(UAUX,VAUX,WD%R,DP,
     &              T3_08,T3_09,T3_10,MSK,MASKEL,MESH3D,
     &              SVIDE,IELM3,NPLAN,OPTBAN,UNSV3D,NPOIN3,NPOIN2,
     &              SIGMAG,IPBOT%I)
!
      ENDIF
!
C     END OF 'TAKES THE PRESSURE GRADIENT INTO ACCOUNT'
!
!
C     LATERAL BOUNDARY CONDITION IN UAUX AND VAUX
!
      DO IPTFR3 = 1,NPTFR3
        IF(LIUBOL%I(IPTFR3).EQ.KENT.OR.
     &     LIUBOL%I(IPTFR3).EQ.KENTU.OR.LIUBOL%I(IPTFR3).EQ.KADH) THEN
          UAUX(MESH3D%NBOR%I(IPTFR3)) = UBORL%R(IPTFR3)
        ENDIF
        IF(LIVBOL%I(IPTFR3).EQ.KENT.OR.
     &     LIVBOL%I(IPTFR3).EQ.KENTU.OR.LIVBOL%I(IPTFR3).EQ.KADH) THEN
          VAUX(MESH3D%NBOR%I(IPTFR3)) = VBORL%R(IPTFR3)
        ENDIF
      ENDDO
!
C     COMPUTES TETAU * UAUX + (1-TETAU) * UN
!
      DO I=1,U%DIM1
        UCONV%R(I)=TETAU*UAUX(I)+(1.D0-TETAU)*UN%R(I)
        VCONV%R(I)=TETAU*VAUX(I)+(1.D0-TETAU)*VN%R(I)
      ENDDO
!
C     SEM2D%ADR(1)%P = SEM2D%ADR(1)%P - FLUX2D
!
C     UNONNEU=8 : 1 IF NOT A WALL
      CALL EXTMSK(MASKBR,MASK%ADR(8)%P%R,MESH2D%NPTFR,NETAGE)
      CALL VECTOR(T3_06,'=','FLUBOR          ',IELBOR(IELM3,2),
     &            1.D0,SVIDE,SVIDE,SVIDE,UCONV,VCONV,SVIDE,
     &            MESH3D,.TRUE.,MASKBR)
!
      CALL SUMVER(FLBOR%R,T3_06%R,NPOIN2,NPLAN,MESH2D%NPTFR)
!
      CALL OSDB( 'X=X-Y   ',SEM2D%ADR(1)%P,FLBOR,FLBOR,C,MESH2D)
!
C     MULTIPLIES BY THE GRADIENT OF THE 3D BASES
!
      FORMUL = 'VGRADP       HOR'
      CALL VECTOR(T3_01,'=',FORMUL,IELM3,1.D0,SVIDE,SVIDE,SVIDE,
     &            UCONV,VCONV,SVIDE,MESH3D,MSK,MASKEL)
!
C     SUM ON THE VERTICAL
C     FLINT2 WILL BE ADDED TO SEM2D, BUT MAY BE USED TO CHECK IN TRIDW2
C     THAT THE SUM ON THE VERTICAL OF FLUINT = FLINT2
!
      CALL OS('X=0     ',X=FLINT2)
      DO IPLAN=1,NPLAN
        DO I=1,NPOIN2
          FLINT2%R(I)=FLINT2%R(I)+T3_01%R(I+(IPLAN-1)*NPOIN2)
        ENDDO
      ENDDO
!
!=======================================================================
C     CONTRIBUTION OF NON COMPATIBLE LAPLACIAN
C     SEE ALSO MODIFICATION OF ZCONV LATER ON
!=======================================================================
!
      IF(ABS(1.D0-TETAZCOMP).GT.1.D-6) THEN
!
C       ADDS NON COMPATIBLE LAPLACIAN
C       FOR THE CONTRIBUTION OF EXPLICIT FREE-SURFACE
C       TO THE VELOCITY
C       TETAZCOMP=1 : COMPATIBLE
C       TETAZCOMP=0 : NON COMPATIBLE
!
C       ADDS THE NON COMPATIBLE EXPLICIT FREE SURFACE GRADIENT
        CALL CPSTVC(H,T2_04)
        CALL CPSTVC(H,T2_05)
        IF(OPTBAN.EQ.1) THEN
C         FREE SURFACE PIECE-WISE LINEAR IN ZFLATS
          CALL VECTOR(FLINT2,'+','VGRADP 2        ',IELM2H,
     &                -(1.D0-TETAZCOMP)/TETAH,
     &                SVIDE,SVIDE,SVIDE,NUWAVE,ZFLATS,SVIDE,
     &                MESH2D,MSK,MASKEL)
        ELSE
C         FREE SURFACE LINEAR IN T2_04
          DO I=1,NPOIN2
            T2_04%R(I)=HN%R(I)+ZF%R(I)
          ENDDO
          CALL VECTOR(FLINT2,'+','VGRADP 2        ',IELM2H,
     &                -(1.D0-TETAZCOMP)/TETAH,
     &                SVIDE,SVIDE,SVIDE,NUWAVE,T2_04,SVIDE,
     &                MESH2D,MSK,MASKEL)
        ENDIF
!
      ENDIF
!
      CALL OS('X=X+Y   ',X=SEM2D%ADR(1)%P,Y=FLINT2)
!
!=======================================================================
C     ADDS THE MASS MATRIX (LUMPED OR NOT) TO THE SYSTEM MATRIX
C     SOLVES THE EQUATION
!=======================================================================
!
C     COMPUTES THE PARTIALLY LUMPED 2D MASS MATRIX
!
      FORMUL='MATMAS          '
C     NOTE: THERE IS LOCAL LUMPING IN PROPAG
C           ON THE TIDAL FLATS
      CALL MATRIX(MAT2D%ADR(3)%P,'M=N     ',FORMUL,IELM2H,IELM2H,
     &            1.D0/DT,
     &            SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &            MESH2D,MSK,MASKEL)
      CALL LUMP(T2_01,MAT2D%ADR(3)%P,MESH2D,AGGLOH)
      CALL OM('M=M+CN  ',MAT2D%ADR(1)%P,MAT2D%ADR(3)%P,
     &        SVIDE,1.D0-AGGLOH,MESH2D)
      CALL OM('M=M+D   ',MAT2D%ADR(1)%P,MAT2D%ADR(1)%P,
     &        T2_01,C,MESH2D)
!
C     INITIAL GUESS FOR DH
!
      IF(IORDRH.EQ.0) THEN
        CALL OS('X=0     ',X=DH)
      ELSEIF(IORDRH.EQ.1) THEN
        IF(LT.EQ.1.AND.ISOUSI.EQ.1) CALL OS('X=0     ',X=DH)
      ELSE
        IF(LNG.EQ.1) WRITE(LU,30) IORDRH
        IF(LNG.EQ.2) WRITE(LU,31) IORDRH
30      FORMAT(1X,'WAVE_EQUATION : IORDRH=',1I6,' VALEUR NON PREVUE')
31      FORMAT(1X,'WAVE_EQUATION: IORDRH=',1I6,' VALUE OUT OF RANGE')
        CALL PLANTE(1)
        STOP
      ENDIF
!
C     SAVES THE ORIGINAL MATRIX, BEFORE DIRICHLETS AND PRECONDITIONING
!
      IF(OPTBAN.EQ.1.AND.OPT_HNEG.EQ.2.AND.NPTFR2.GT.0) THEN
        CALL OM('M=N     ',MAT2D%ADR(3)%P,MAT2D%ADR(1)%P,
     &          SVIDE,0.D0,MESH2D)
      ENDIF
!
C     DIRICHLET + SOLVES THE SYSTEM
!
      CALL OSBD( 'X=X-Y   ' , HBOR , HN , HN , C , MESH2D )
      CALL DIRICH(DH,MAT2D%ADR(1)%P,SEM2D%ADR(1)%P,
     &            HBOR,LIMPRO%I,TRAV2,MESH2D,KDIR,MSK,MASKPT)
      CALL SOLVE(DH,MAT2D%ADR(1)%P,SEM2D%ADR(1)%P,
     &           TRAV2,SLVPRO,INFOGR,MESH2D,MAT2D%ADR(2)%P)
      CALL OSBD( 'X=X+Y   ' , HBOR , HN , HN , C , MESH2D )
!
C     RECOVERS THE NEW DEPTH
!
      CALL OS('X=Y+Z   ',X=H,Y=HN,Z=DH)
!
C     COMPLETES THE 2D INTERNAL FLUXES
C     COMPATIBLE WITH CONTINUITY EQUATION
!
C     BOUNDARY FLUXES THAT SOLVE THE CONTINUITY EQUATION
C     WHEN DEPTHS ARE PRESCRIBED
!
      IF(OPTBAN.EQ.1.AND.OPT_HNEG.EQ.2.AND.NPTFR2.GT.0) THEN
        CALL MATVEC('X=AY    ',T2_01,MAT2D%ADR(3)%P,DH,1.D0,MESH2D)
        DO I=1,NPTFR2
          IF(LIMPRO%I(I).EQ.KDIR) THEN
            FLBOR%R(I)=FLINT2%R(NBOR2%I(I))-T2_01%R(NBOR2%I(I))
          ENDIF
        ENDDO
      ENDIF
!
C     UNCOMMENT THIS LINE
C     TO CHECK THAT SUM OF FLUINT = FLINT2 (IN TRIDW2)
!
C     CALL MATVEC ('X=X+CAY  ',FLINT2,MAT2D%ADR(2)%P,DH,-1.D0,MESH2D)
!
C     PREPARES THE PIECE-WISE LINEAR FUNCTION ZCONV
C    (THE ADVECTING FIELD WILL BE UCONV-GRAV*DT*TETAU*TETAH*GRAD(ZCONV))
!
      DO IELEM=1,NELEM2
        ZCONV%R(IELEM         )=DH%R(IKLE2%I(IELEM         ))
        ZCONV%R(IELEM+  NELEM2)=DH%R(IKLE2%I(IELEM+  NELEM2))
        ZCONV%R(IELEM+2*NELEM2)=DH%R(IKLE2%I(IELEM+2*NELEM2))
      ENDDO
      IF(ABS(1.D0-TETAZCOMP).GT.1.D-6) THEN
        C=(1.D0-TETAZCOMP)/TETAH
        IF(OPTBAN.EQ.1) THEN
C         FREE SURFACE PIECE-WISE LINEAR IN ZFLATS
          CALL OS('X=X+CY  ',X=ZCONV,Y=ZFLATS,C=C)
        ELSE
C         FREE SURFACE LINEAR
          DO IELEM=1,NELEM2
            I1=IKLE2%I(IELEM         )
            I2=IKLE2%I(IELEM+  NELEM2)
            I3=IKLE2%I(IELEM+2*NELEM2)
            ZCONV%R(IELEM         )=ZCONV%R(IELEM         )+
     &      C*(HN%R(I1)+ZF%R(I1))
            ZCONV%R(IELEM+  NELEM2)=ZCONV%R(IELEM+  NELEM2)+
     &      C*(HN%R(I2)+ZF%R(I2))
            ZCONV%R(IELEM+2*NELEM2)=ZCONV%R(IELEM+2*NELEM2)+
     &      C*(HN%R(I3)+ZF%R(I3))
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
C     3) COMPUTES NEW VELOCITIES
!=======================================================================
!
C     3.1) GRADIENT OF DH (IN T2_02 AND T2_03)
!
C     COMPONENT X (IN T2_02)
!
      CALL VECTOR
     & (T2_02,'=','GRADF          X',IELM2H,-GRAV*TETAH,DH,SVIDE,SVIDE,
     &  SVIDE,SVIDE,SVIDE, MESH2D, MSK, MASKEL)
      IF (NCSIZE.GT.1) CALL PARCOM(T2_02,2,MESH2D)
      CALL OS ( 'X=XY    ' ,X=T2_02,Y=UNSV2D)
!
C     COMPONENT Y (IN T2_03)
!
      CALL VECTOR
     & (T2_03,'=','GRADF          Y',IELM2H,-GRAV*TETAH,DH,SVIDE,SVIDE,
     &  SVIDE,SVIDE,SVIDE, MESH2D, MSK, MASKEL)
      IF(NCSIZE.GT.1) CALL PARCOM(T2_03,2,MESH2D)
      CALL OS ( 'X=XY    ' ,X=T2_03,Y=UNSV2D)
!
C     THE NON COMPATIBLE PART OF THE LAPLACIAN FOR THE FREE SURFACE
C     GRADIENT HAS NOT BEEN PUT IN UAUX, IT IS ADDED HERE IN A
C     COMPATIBLE WAY TO COMPUTE U AND V
!
      IF(ABS(1.D0-TETAZCOMP).GT.1.D-6) THEN
        C=-GRAV*(1.D0-TETAZCOMP)
        CALL OS('X=X+CY  ',X=T2_02,Y=GRADZN%ADR(1)%P,C=C)
        CALL OS('X=X+CY  ',X=T2_03,Y=GRADZN%ADR(2)%P,C=C)
      ENDIF
!
C     3.2) FINAL COMPUTATION OF U AND V
!
      DO I=1,U%DIM1
        U%R(I)=UAUX(I)+DT*T2_02%R(MOD(I-1,NPOIN2)+1)*DM1%R(I)
        V%R(I)=VAUX(I)+DT*T2_03%R(MOD(I-1,NPOIN2)+1)*DM1%R(I)
      ENDDO
!
C     MODIFIES DM1 FOR USE IN PRECON, FLUX3D, ETC
!
      CALL OS('X=CX    ',X=DM1,C=-DT*GRAV*TETAH*TETAU)
!
C     DIRICHLET TYPE BOUNDARY CONDITIONS
!
C     LATERAL BOUNDARY CONDITION
!
      DO IPTFR3 = 1,NPTFR3
        IF(LIUBOL%I(IPTFR3).EQ.KENT.OR.
     &     LIUBOL%I(IPTFR3).EQ.KENTU.OR.LIUBOL%I(IPTFR3).EQ.KADH) THEN
          U%R(MESH3D%NBOR%I(IPTFR3)) = UBORL%R(IPTFR3)
        ENDIF
        IF(LIVBOL%I(IPTFR3).EQ.KENT.OR.
     &     LIVBOL%I(IPTFR3).EQ.KENTU.OR.LIVBOL%I(IPTFR3).EQ.KADH) THEN
          V%R(MESH3D%NBOR%I(IPTFR3)) = VBORL%R(IPTFR3)
        ENDIF
      ENDDO
!
C     BOTTOM AND SURFACE BOUNDARY CONDITION
!
      DO IPOIN2 = 1,NPOIN2
        IF(LIUBOF%I(IPOIN2).EQ.KENT.OR.LIUBOF%I(IPOIN2).EQ.KADH) THEN
          U%R(IPOIN2) = UBORF%R(IPOIN2)
        ENDIF
        IF(LIVBOF%I(IPOIN2).EQ.KENT.OR.LIVBOF%I(IPOIN2).EQ.KADH) THEN
          V%R(IPOIN2) = VBORF%R(IPOIN2)
        ENDIF
        IF(LIUBOS%I(IPOIN2).EQ.KENT.OR.LIUBOS%I(IPOIN2).EQ.KADH) THEN
          U%R(NPOIN3-NPOIN2+IPOIN2) = UBORS%R(IPOIN2)
        ENDIF
        IF(LIVBOS%I(IPOIN2).EQ.KENT.OR.LIVBOS%I(IPOIN2).EQ.KADH) THEN
          V%R(NPOIN3-NPOIN2+IPOIN2) = VBORS%R(IPOIN2)
        ENDIF
      ENDDO
!
C     PROJECTION ON SOLID BOUNDARIES (KLOG)
!
      IF(VELPROLAT) THEN
        CALL AIRWIK3(LIHBOR%I,U%R,V%R,MESH2D%XNEBOR%R,MESH2D%YNEBOR%R,
     &               NBOR2%I,NPTFR2,NPLAN,NPOIN2,KLOG)
      ENDIF
!
C     THIS SEQUENCE WILL BE DONE AFTER IF DYNAMIC PRESSURE HAS NOT BEEN
C     COMPUTED HERE
!
      IF(NONHYD.AND.DPWAVEQ) THEN
C       WD HAS RECEIVED THE PRESSURE GRADIENT BEFORE
        CALL OS ('X=Y     ', X=W , Y=WD  )
C       BOUNDARY CONDITION ON FREE SURFACE STRONGLY ENFORCED
        IF(CLDYN) THEN
          CALL OV('X=Y     ',W%R(NPOIN3-NPOIN2+1:NPOIN3),DSSUDT%R,
     &                       DSSUDT%R,0.D0,NPOIN2)
          CALL OV('X=X+YZ  ',W%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                       GRADZS%ADR(1)%P%R,
     &                       U%R(NPOIN3-NPOIN2+1:NPOIN3),0.D0,NPOIN2)
          CALL OV('X=X+YZ  ',W%R(NPOIN3-NPOIN2+1:NPOIN3),
     &                       GRADZS%ADR(2)%P%R,
     &                       V%R(NPOIN3-NPOIN2+1:NPOIN3),0.D0,NPOIN2)
        ENDIF
C       BOUNDARY CONDITION ON BOTTOM STRONGLY ENFORCED
        IF(VELPROBOT) THEN
          IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
            DO I=1,NPOIN2
              DO IP=0,IPBOT%I(I)
                I3D=IP*NPOIN2+I
                W%R(I3D)=GRADZF%ADR(1)%P%R(I)*U%R(I3D)
     &                  +GRADZF%ADR(2)%P%R(I)*V%R(I3D)
              ENDDO
            ENDDO
          ELSE
            DO I=1,NPOIN2
              W%R(I)=GRADZF%ADR(1)%P%R(I)*U%R(I)
     &              +GRADZF%ADR(2)%P%R(I)*V%R(I)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
!
C     VELOCITIES OF FIRST FREE POINT COPIED BELOW
!
      IF(SIGMAG.OR.OPTBAN.EQ.1) THEN
        DO I2=1,NPOIN2
          IF(IPBOT%I(I2).GT.0) THEN
            I1=I2+IPBOT%I(I2)*NPOIN2
C           VALUE OF THE FIRST FREE POINT IS COPIED BELOW
            DO NP=0,IPBOT%I(I2)-1
              I3=I2+NP*NPOIN2
              U%R(I3)=U%R(I1)
              V%R(I3)=V%R(I1)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      CALL VERMOY(U2D%R,V2D%R,U%R,V%R,2,Z,
     &            T3_01%R,T3_02%R,T3_03%R,1,NPLAN,NPOIN2,NPLAN,OPTBAN)
!
!-----------------------------------------------------------------------
!
C     CLASSICAL ADVECTION FIELD IS USED FOR SUPG OR CHARACTERISTICS
C     IT IS REBUILT HERE IN UCONVC AND VCONVC FOR USE IN PRECON
C    (IT COULD BE DONE BETTER WITH SUPG - TO BE IMPLEMENTED)
!
      IF(CONV(ADV_CAR).OR.CONV(ADV_SUP)) THEN
        CALL OS( 'X=CY    ' , X=UCONVC, Y=UN , C=1.D0-TETAU )
        CALL OS( 'X=X+CY  ' , X=UCONVC, Y=U  , C=     TETAU )
        CALL OS( 'X=CY    ' , X=VCONVC, Y=VN , C=1.D0-TETAU )
        CALL OS( 'X=X+CY  ' , X=VCONVC, Y=V  , C=     TETAU )
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C