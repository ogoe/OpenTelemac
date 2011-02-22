C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       APPLIES VARIOUS TECHNIQUES TO TREAT NEGATIVE DEPTHS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC2D, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DIMGLO, GLOSEG, YAFLODEL, YASMH
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::AM1 AM1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DEBUG DEBUG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DM1 DM1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DT DT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ENTET ENTET@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FLBOR FLBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FLODEL FLODEL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FLULIM FLULIM@endlink, 
!> @link DECLARATIONS_TELEMAC2D::H H@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HBOR HBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HMIN HMIN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HN HN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HNEG HNEG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HPROP HPROP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LIMPRO LIMPRO@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MESH MESH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MSK MSK@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::OPTBAN OPTBAN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::OPTSOU OPTSOU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::OPT_HNEG OPT_HNEG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::S S@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SMH SMH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SOLSYS SOLSYS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T1 T1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2 T2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T3 T3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TE5 TE5@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UDEL UDEL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UNSV2D UNSV2D@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VDEL VDEL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::W1 W1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ZCONV ZCONV@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::KDIR KDIR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> FORMUL
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_CORRECTION_DEPTH_2D
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CLIP(), FILTER(), FILTER_H(), NBPTS(), OS(), POSITIVE_DEPTHS(), VECTOS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!> </td><td> 25/08/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
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
!>          <tr><td>YAFLODEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YASMH
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CORRECTION_DEPTH_2D
     &(GLOSEG,DIMGLO,YAFLODEL,YASMH,YAFLULIM)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG 
C| GLOSEG         |-->| GLOBAL NUMBERS OF APICES OF SEGMENTS 
C| YAFLODEL       |-->| GIVE BACK THE FLUXES THAT WERE NOT TRANSMITTED 
C| YAFLULIM       |<->| IF YES, FLULIM WILL BE APPLIED TO SEGMENT FLUXES
C|                |   | WHEN CALLING CVDFTR
C| YASMH          |-->| THE RIGHT-HAND SIDE SMH HAS TO BE TAKEN 
C|                |   | INTO ACCOUNT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC2D,
     &                     EX_CORRECTION_DEPTH_2D => CORRECTION_DEPTH_2D
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: DIMGLO
      INTEGER, INTENT(IN)    :: GLOSEG(DIMGLO,2)
      LOGICAL, INTENT(IN)    :: YAFLODEL,YASMH
      LOGICAL, INTENT(INOUT) :: YAFLULIM
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=16) :: FORMUL
!
!-----------------------------------------------------------------------
!
      IF(OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
      IF(DEBUG.GT.0) WRITE(LU,*) 'TRAITEMENT BANCS DECOUVRANTS'
C
      IF(OPT_HNEG.EQ.2) THEN
C
C     LIMITS FLUXES TO HAVE POSITIVE DEPTHS
C
      FORMUL='HUGRADP         '
      IF(SOLSYS.EQ.2) FORMUL(8:8)='2'
C     CALL VECTOR(T1,'=',FORMUL,H%ELM,-1.D0,
C    *            HPROP,DM1,ZCONV,UDEL,VDEL,VDEL,MESH,MSK,MASKEL)
C                 T1 AS HUGRADP IS NOT USED AS AN ASSEMBLED VECTOR
C                 BUT TO GET THE NON ASSEMBLED FORM MESH%W
C     JUST LIKE CALL VECTOR BUT WITHOUT ASSEMBLING T1 BECAUSE LEGO IS SET
C     TO FALSE
      CALL VECTOS(T1%R,'=',FORMUL,-1.D0,
     &            HPROP%R,DM1%R,ZCONV%R,UDEL%R,VDEL%R,VDEL%R,
     &            HPROP,  DM1  ,ZCONV  ,UDEL  ,VDEL  ,VDEL  ,
C                           LEGO
     &            MESH%W%R,.FALSE.,
     &            MESH%XEL%R  , MESH%YEL%R  , MESH%ZEL%R  ,
     &            MESH%SURFAC%R,MESH%IKLE%I,MESH%NBOR%I,
     &            MESH%XSGBOR%R, MESH%YSGBOR%R, MESH%ZSGBOR%R,
     &            BIEF_NBPTS(H%ELM,MESH),MESH%NELEM,MESH%NELMAX,
     &            H%ELM,MESH%LV,MSK,MASKEL%R,MESH)
C
C     BEWARE, WILL BE ONLY VALID FOR ADVECTION WITH UDEL,VDEL
C     HENCE FOR TRACERS AND IF SOLSYS=2
C
      YAFLULIM=.TRUE.        
C
      CALL POSITIVE_DEPTHS(T1,T2,T3,T4,H,HN,MESH,FLODEL,.TRUE.,
     &                     FLBOR,DT,UNSV2D,
     &                     NPOIN,GLOSEG(1:DIMGLO,1),GLOSEG(1:DIMGLO,2),
     &                     MESH%NBOR%I,MESH%NPTFR,YAFLODEL,SMH,YASMH,
     &                     OPTSOU,FLULIM%R,LIMPRO%I,HBOR%R,KDIR,ENTET,
     &                     MESH%W%R,NAMECODE,2)
C                                            2 HARDCODED OPTION
C                                            FOR POSITIVE DEPTH ALGORITHM
C                                            INDEPENDENT OF SEGMENT
C                                            NUMBERING
C
      ELSEIF(OPT_HNEG.EQ.1) THEN
C
C     CONSERVATIVE SMOOTHING OF THE NEGATIVE DEPTHS
C
C     1) OPTIONAL THRESHOLD (HNEG IS NEGATIVE)
C
      IF(HNEG.LT.-1.D-6) CALL OS('X=X+C   ',X=H,C=-HNEG)
C
C     2) NEGATIVE DEPTHS IN T1 AND TAKEN OUT OF H
C
      CALL OS( 'X=-(Y,C)' , X=T1 , Y=H  , C=0.D0 )
      CALL OS( 'X=X-Y   ' , X=H  , Y=T1 )
C
C     3) NEGATIVE DEPTHS ARE SMOOTHED (TWICE HERE)
C        AND MASKED TO AVOID SPREAD OVER THE WETTING/DRYING AREAS
C
      IF(OPTBAN.EQ.1) THEN
        CALL FILTER_H(T1,T2,MESH,MSK,MASKEL,2,FLODEL,
     &                YAFLODEL,DT,W1,UNSV2D)
      ELSEIF(OPTBAN.EQ.3) THEN
C            FILTER_H DOES NOT WORK WITH POROSITY
C       CALL FILTER_H(T1,T2,MESH,.TRUE.,TE5,2,FLODEL,
C    *                YAFLODEL,DT,W1,UNSV2D)
!
C       THIS WILL BE SLIGHTLY WRONG WITH DELWAQ
        CALL FILTER(T1,.TRUE.,T2,T3,
     &              AM1,'MATMAS          ',
     &              1.D0,S,S,S,S,S,S,MESH,.TRUE.,TE5,2)
      ENDIF
C
C     4) SMOOTHED NEGATIVE DEPTHS TRANSFERRED BACK TO H
C
      CALL OS( 'X=X+Y   ' , X=H , Y=T1 )
C
C     5) OPTIONAL THRESHOLD IS SUBTRACTED
C
      IF(HNEG.LT.-1.D-6) CALL OS('X=X+C   ',X=H,C=HNEG)
C
      ENDIF
C
C     OPTIONAL CLIPPING OF NEGATIVE VALUES
      IF(CLIPH) CALL CLIP(H,HMIN,.TRUE.,1.D6,.FALSE.,0)
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'FIN DU TRAITEMENT BANCS DECOUVRANTS'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C
