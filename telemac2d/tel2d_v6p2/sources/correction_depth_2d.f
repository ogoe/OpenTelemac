!                    ******************************
                     SUBROUTINE CORRECTION_DEPTH_2D
!                    ******************************
!
     &(GLOSEG,DIMGLO,YAFLODEL,YASMH,YAFLULIM)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    APPLIES VARIOUS TECHNIQUES TO TREAT NEGATIVE DEPTHS.
!
!history  J-M HERVOUET (LNHE)
!+        25/08/2009
!+        V6P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIMGLO         |-->| FIRST DIMENSION OF GLOSEG
!| GLOSEG         |-->| GLOBAL NUMBERS OF APICES OF SEGMENTS
!| YAFLODEL       |-->| GIVE BACK THE FLUXES THAT WERE NOT TRANSMITTED
!| YAFLULIM       |<->| IF YES, FLULIM WILL BE APPLIED TO SEGMENT FLUXES
!|                |   | WHEN CALLING CVDFTR
!| YASMH          |-->| THE RIGHT-HAND SIDE SMH HAS TO BE TAKEN
!|                |   | INTO ACCOUNT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC2D,
     &                     EX_CORRECTION_DEPTH_2D => CORRECTION_DEPTH_2D
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: DIMGLO
      INTEGER, INTENT(IN)    :: GLOSEG(DIMGLO,2)
      LOGICAL, INTENT(IN)    :: YAFLODEL,YASMH
      LOGICAL, INTENT(INOUT) :: YAFLULIM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=16) :: FORMUL
!
!-----------------------------------------------------------------------
!
      IF(OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
      IF(DEBUG.GT.0) WRITE(LU,*) 'TRAITEMENT BANCS DECOUVRANTS'
!
      IF(OPT_HNEG.EQ.2) THEN
!
!     LIMITS FLUXES TO HAVE POSITIVE DEPTHS
!
      FORMUL='HUGRADP         '
      IF(SOLSYS.EQ.2) FORMUL(8:8)='2'
!     CALL VECTOR(T1,'=',FORMUL,H%ELM,-1.D0,
!    *            HPROP,DM1,ZCONV,UDEL,VDEL,VDEL,MESH,MSK,MASKEL)
!                 T1 AS HUGRADP IS NOT USED AS AN ASSEMBLED VECTOR
!                 BUT TO GET THE NON ASSEMBLED FORM MESH%W
!     JUST LIKE CALL VECTOR BUT WITHOUT ASSEMBLING T1 BECAUSE LEGO IS SET
!     TO FALSE
      CALL VECTOS(T1%R,'=',FORMUL,-1.D0,
     &            HPROP%R,DM1%R,ZCONV%R,UDEL%R,VDEL%R,VDEL%R,
     &            HPROP,  DM1  ,ZCONV  ,UDEL  ,VDEL  ,VDEL  ,
!                           LEGO
     &            MESH%W%R,.FALSE.,
     &            MESH%XEL%R  , MESH%YEL%R  , MESH%ZEL%R  ,
     &            MESH%SURFAC%R,MESH%IKLE%I,MESH%NBOR%I,
     &            MESH%XSGBOR%R, MESH%YSGBOR%R, MESH%ZSGBOR%R,
     &            BIEF_NBPTS(H%ELM,MESH),MESH%NELEM,MESH%NELMAX,
     &            H%ELM,MESH%LV,MSK,MASKEL%R,MESH)
!
!     BEWARE, WILL BE ONLY VALID FOR ADVECTION WITH UDEL,VDEL
!     HENCE FOR TRACERS AND IF SOLSYS=2
!
      YAFLULIM=.TRUE.
!
      CALL POSITIVE_DEPTHS(T1,T2,T3,T4,H,HN,MESH,FLODEL,.TRUE.,
     &                     FLBOR,DT,UNSV2D,
     &                     NPOIN,GLOSEG(1:DIMGLO,1),GLOSEG(1:DIMGLO,2),
     &                     MESH%NBOR%I,MESH%NPTFR,YAFLODEL,SMH,YASMH,
     &                     OPTSOU,FLULIM%R,LIMPRO%I,HBOR%R,KDIR,ENTET,
     &                     MESH%W%R,NAMECODE,2)
!                                            2 HARDCODED OPTION
!                                            FOR POSITIVE DEPTH ALGORITHM
!                                            INDEPENDENT OF SEGMENT
!                                            NUMBERING
!
      ELSEIF(OPT_HNEG.EQ.1) THEN
!
!     CONSERVATIVE SMOOTHING OF THE NEGATIVE DEPTHS
!
!     1) OPTIONAL THRESHOLD (HNEG IS NEGATIVE)
!
      IF(HNEG.LT.-1.D-6) CALL OS('X=X+C   ',X=H,C=-HNEG)
!
!     2) NEGATIVE DEPTHS IN T1 AND TAKEN OUT OF H
!
      CALL OS( 'X=-(Y,C)' , X=T1 , Y=H  , C=0.D0 )
      CALL OS( 'X=X-Y   ' , X=H  , Y=T1 )
!
!     3) NEGATIVE DEPTHS ARE SMOOTHED (TWICE HERE)
!        AND MASKED TO AVOID SPREAD OVER THE WETTING/DRYING AREAS
!
      IF(OPTBAN.EQ.1) THEN
        CALL FILTER_H(T1,T2,MESH,MSK,MASKEL,2,FLODEL,
     &                YAFLODEL,DT,W1,UNSV2D)
      ELSEIF(OPTBAN.EQ.3) THEN
!            FILTER_H DOES NOT WORK WITH POROSITY
!       CALL FILTER_H(T1,T2,MESH,.TRUE.,TE5,2,FLODEL,
!    *                YAFLODEL,DT,W1,UNSV2D)
!
!       THIS WILL BE SLIGHTLY WRONG WITH DELWAQ
        CALL FILTER(T1,.TRUE.,T2,T3,
     &              AM1,'MATMAS          ',
     &              1.D0,S,S,S,S,S,S,MESH,.TRUE.,TE5,2)
      ENDIF
!
!     4) SMOOTHED NEGATIVE DEPTHS TRANSFERRED BACK TO H
!
      CALL OS( 'X=X+Y   ' , X=H , Y=T1 )
!
!     5) OPTIONAL THRESHOLD IS SUBTRACTED
!
      IF(HNEG.LT.-1.D-6) CALL OS('X=X+C   ',X=H,C=HNEG)
!
      ENDIF
!
!     OPTIONAL CLIPPING OF NEGATIVE VALUES
      IF(CLIPH) CALL CLIP(H,HMIN,.TRUE.,1.D6,.FALSE.,0)
!
      IF(DEBUG.GT.0) WRITE(LU,*) 'FIN DU TRAITEMENT BANCS DECOUVRANTS'
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
