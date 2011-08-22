!                    ***********************
                     SUBROUTINE RHS_PRESSURE
!                    ***********************
!
     &(DIVU,UP,VP,WP,IELM3,DM1,ZCONV,SVIDE,MESH3D,MSK,MASKEL,FLUEXT,
     & NSCE,RAIN,PLUIE,SOURCES,GRADZF,VOLU2D,DSSUDT,NPOIN2,NPOIN3,NPLAN)
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DIVU           |<--| RESULT
!| IELM3          |-->| TYPE OF ELEMENT
!| UP             |-->| X COMPONENT OF INTERMEDIATE VELOCITY FIELD
!| VP             |-->| Y COMPONENT OF INTERMEDIATE VELOCITY FIELD
!| WP             |-->| Z COMPONENT OF INTERMEDIATE VELOCITY FIELD
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,         INTENT(IN)    :: IELM3,NSCE,NPOIN2,NPOIN3,NPLAN
      LOGICAL,         INTENT(IN)    :: MSK,RAIN
      TYPE(BIEF_OBJ),  INTENT(INOUT) :: DIVU
      TYPE(BIEF_OBJ),  INTENT(IN)    :: UP,VP,WP,PLUIE,SOURCES,GRADZF
      TYPE(BIEF_OBJ),  INTENT(IN)    :: DM1,ZCONV,SVIDE,MASKEL,FLUEXT
      TYPE(BIEF_OBJ),  INTENT(IN)    :: VOLU2D,DSSUDT
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,IIS,IPLAN,IPOIN2,IPOIN3,ILEVEL,IUPPER,ILOWER
      CHARACTER(LEN=16) FORMUL
!
!=======================================================================
! RIGHT HAND SIDE VECTOR SEM3D = - DIV (INTERMEDIATE VELOCITY)
!=======================================================================
!
!     VERTICAL FLUXES FLUVER2
!
      CALL CPSTVC(UP,DIVU)
      CALL FLUVER_2(DIVU,UP,VP,WP,GRADZF,VOLU2D,DSSUDT,NPLAN,NPOIN2)
!
!     FLUINT
!
      IF(UP%NAME.EQ.'UCONV ') THEN
!       FOR UCONV, CONTRIBUTION OF DM1*ZCONV IS NECESSARY
        FORMUL = 'VGRADP 2     HOR'
      ELSE
        FORMUL = 'VGRADP       HOR'
      ENDIF
      CALL VECTOR(DIVU,'+',FORMUL,IELM3,1.D0,
     &            DM1,ZCONV,SVIDE,UP,VP,SVIDE,MESH3D,MSK,MASKEL)
!
!     - FLUEXT
!
      CALL OS('X=X-Y   ',X=DIVU,Y=FLUEXT)
!
!     + SOURCE
!
      IF(NSCE.GE.1) THEN
        DO IS=1,NSCE
          IIS=IS
!         HERE SOURCES IN THE NON ASSEMBLED (PARCOM) FORM
!         SEE SOURCES_SINKS
          IF(NCSIZE.GT.1) IIS=IS+NSCE
          CALL OS('X=X+Y   ',X=DIVU,Y=SOURCES%ADR(IIS)%P)
        ENDDO
      ENDIF
!
!     + RAIN
!
      IF(RAIN) THEN
        DO IPOIN2=1,NPOIN2
          IPOIN3=NPOIN3-NPOIN2+IPOIN2
          DIVU%R(IPOIN3)=DIVU%R(IPOIN3)+PLUIE%R(IPOIN2)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END