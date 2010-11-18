C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       APPLIES VARIOUS TECHNIQUES TO TREAT NEGATIVE DEPTHS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D, INTERFACE_TELEMAC2D, INTERFACE_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DIMGLO, GLOSEG, W2D, W3D
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::DM1 DM1@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DT DT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLBOR FLBOR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLODEL FLODEL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLULIM FLULIM@endlink, 
!> @link DECLARATIONS_TELEMAC3D::H H@endlink, 
!> @link DECLARATIONS_TELEMAC3D::HBOR HBOR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::HMIN HMIN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::HN HN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::INFOGR INFOGR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIMPRO LIMPRO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MAT2D MAT2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MESH2D MESH2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MESH3D MESH3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MSK MSK@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPTFR2 NPTFR2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPTBAN OPTBAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPT_HNEG OPT_HNEG@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SMH SMH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SVIDE SVIDE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_01 T2_01@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_02 T2_02@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_03 T2_03@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_01 T3_01@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UCONV UCONV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UNSV2D UNSV2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VCONV VCONV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ZCONV ZCONV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ZPROP ZPROP@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::KDIR KDIR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> FORMUL, I, IELCON, IELEM, IELEM3, IOPT, ISEG, ISEG3D, SAVEZ
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CORRECTION_DEPTH_3D, SAVEZ
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CLIP(), FILTER(), FLUX_EF_VF_3D(), NBPTS(), OS(), POSITIVE_DEPTHS(), VECTOS()
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
!> </td><td> 28/07/2009
!> </td><td> J.M. HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DIMGLO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W3D
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CORRECTION_DEPTH_3D
     &(W2D,W3D,GLOSEG,DIMGLO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DIMGLO         |---| 
C| GLOSEG         |---| 
C| W2D            |---| 
C| W3D            |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC3D,
     &                     EX_CORRECTION_DEPTH_3D => CORRECTION_DEPTH_3D
      USE INTERFACE_TELEMAC2D
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
      INTEGER IOPT, ISEG, ISEG3D
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: DIMGLO
      INTEGER, INTENT(IN)             :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(INOUT) :: W2D(NELEM2,3),W3D(NELEM3,6)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=16) :: FORMUL
      INTEGER I,IELEM,IELEM3,IELCON
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
!
!-----------------------------------------------------------------------
!
      IF(OPTBAN.EQ.1) THEN
!
        IF(OPT_HNEG.EQ.2) THEN
!
C         FOR THE TIME BEING, FLODEL IS A WORKING ARRAY, HENCE
C         YAFLODEL=.FALSE.; TO USE FLODEL IN TEL4DEL WOULD REQUIRE
C         TRANSFORMING IT INTO 3D.
!
          FORMUL = 'VGRADP 2     HOR'
          SAVEZ     =>MESH3D%Z%R
          MESH3D%Z%R=>ZPROP%R
          IELCON=UCONV%ELM
C         CALL VECTOR(T3_01,'=',FORMUL,IELCON,-1.D0,DM1,ZCONV,ZCONV,
C    *                UCONV,VCONV,VCONV,MESH3D,MSK,MASKEL)
C         EQUIVALENT TO CALL VECTOR BUT WITHOUT ASSEMBLING T3_01
C         BECAUSE ARGUMENT LEGO IS SET TO FALSE :
          CALL VECTOS(T3_01%R,'=',FORMUL,-1.D0,
     &                DM1%R,ZCONV%R,ZCONV%R,UCONV%R,VCONV%R,VCONV%R,
     &                DM1,  ZCONV,  ZCONV,  UCONV,  VCONV,  VCONV,
C                                 LEGO
     &                MESH3D%W%R,.FALSE.,
     &                MESH3D%XEL%R,MESH3D%YEL%R,MESH3D%ZEL%R,
     &                MESH3D%SURFAC%R,MESH3D%IKLE%I,MESH3D%NBOR%I,
     &                MESH3D%XSGBOR%R,MESH3D%YSGBOR%R,MESH3D%ZSGBOR%R,
     &                NBPTS(IELCON),MESH3D%NELEM,MESH3D%NELMAX,
     &                IELCON,MESH3D%LV,MSK,MASKEL%R,MESH3D)
          MESH3D%Z%R=>SAVEZ
C                     T3_01 IS NOT USED AS AN ASSEMBLED VECTOR
C                     BUT TO GET THE NON ASSEMBLED FORM MESH3D%W,
C                     WHICH IS HERE ALSO W3D
C###> MST @ HRW
C         CALCULATES 3D FLODEL (NOTE JMH: THIS WILL BE REDONE IN FLUX3D
C                                         OPTIMISATION MAYBE POSSIBLE  )
          IOPT=2
          CALL FLUX_EF_VF_3D(FLODEL%R,MESH2D%W%R,MESH3D%W%R,
     &                       MESH2D%NSEG,MESH3D%NSEG,MESH2D%NELEM,
     &                       MESH3D%NELEM,MESH2D,MESH3D,.TRUE.,IOPT,1)
!
C         CALCULATES 2D FLODEL (PUT IN FIRST PLANE OF 3D FLODEL)
          DO ISEG = 1,MESH2D%NSEG
            DO I = 2,NPLAN
              ISEG3D = ISEG + (I-1)*MESH2D%NSEG
              FLODEL%R(ISEG) = FLODEL%R(ISEG) + FLODEL%R(ISEG3D)
            ENDDO
          ENDDO
C###
!
          CALL POSITIVE_DEPTHS(T2_01,T2_02,H,HN,
     &                         MESH2D,FLODEL,.FALSE.,
     &                         FLBOR,DT,UNSV2D,NPOIN2,
     &                         GLOSEG(1:DIMGLO,1),
     &                         GLOSEG(1:DIMGLO,2),
     &                         MESH2D%NBOR%I,NPTFR2,.FALSE.,
C                                                    YAFLODEL
     &                         SMH,.TRUE.,2,
C                                   YASMH OPTSOU
C                              SMH IN PROJECTED FORM IN T3D
     &                         FLULIM%R,LIMPRO%I,HBOR%R,KDIR,INFOGR,
     &                         MESH2D%W%R)
!
        ELSEIF(OPT_HNEG.EQ.1) THEN
!
C         CONSERVATIVE SMOOTHING OF NEGATIVE DEPTHS
!
C         1) PUTS NEGATIVE VALUES IN T2_01 AND REMOVES THEM FROM H
!
          CALL OS( 'X=-(Y,C)' , X=T2_01 , Y=H     , C=0.D0 )
          CALL OS( 'X=X-Y   ' , X=H     , Y=T2_01 )
!
C         2) SMOOTHES NEGATIVE VALUES (TWO LOOPS HERE)
C            AND MASKS TO NOT AFFECT THE TIDAL FLATS
!
          IF(OPTBAN.EQ.1) THEN
            CALL FILTER(T2_01,.TRUE.,T2_02,T2_03,
     &                  MAT2D%ADR(1)%P,'MATMAS          ',
     &                  1.D0,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &                  MESH2D,MSK,MASKEL,2)
          ENDIF
!
C         3) PUTS BACK IN H THE SMOOTHED NEGATIVE VALUES
!
          CALL OS( 'X=X+Y   ' , X=H , Y=T2_01 )
!
        ENDIF
!
      ENDIF
!
C CLIPS H AND COMPUTES Z
!
      IF(OPTBAN.EQ.2) THEN
        CALL CLIP(H,HMIN,.TRUE., 1.D6, .FALSE., 0)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C