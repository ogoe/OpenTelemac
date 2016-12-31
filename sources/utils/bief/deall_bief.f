!                    *********************
                     SUBROUTINE DEALL_BIEF
!
!***********************************************************************
! BIEF   V7P0
!***********************************************************************
!
!brief    CLEAN UP THE DATA FROM BIEF
!
!history Y AUDOUIN (LNHE)
!+       21/05/2015
!+       V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MESH           |-->| THE MESH TO BE DEALLOCATED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE STREAMLINE, ONLY: DEALLOC_STREAMLINE
      USE ALGAE_TRANSP, ONLY: DEALLOC_ALGAE
      USE DECLARATIONS_TELEMAC
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! 
!     Old saved variables
!
      ! solve
      IF(.NOT.FIRST_SOLVE) THEN
        CALL BIEF_DEALLOBJ(TBB)
        CALL BIEF_DEALLOBJ(BB)
        CALL BIEF_DEALLOBJ(BX)
      ENDIF
      ! CVTRVF_POS_2
      IF(DEJA_CPOS2) THEN
        DEALLOCATE(INDIC_CPOS2)
      ENDIF
      ! POSITIVE_DEPTHS
      IF(DEJA_PDEPT) THEN
        DEALLOCATE(INDIC_PDEPT)
      ENDIF
      ! CVTRVF_POS
      IF(DEJA_CPOS) THEN
        DEALLOCATE(INDIC_CPOS)
      ENDIF
      ! SD_SOLVE_1
      IF(SIZE_IN.NE.0) THEN
        DEALLOCATE(IN_SS1)
      ENDIF
      IF(SIZE_IP.NE.0) THEN
        DEALLOCATE(IP_SS1)
      ENDIF
      IF(SIZE_ISEGIP.NE.0) THEN
        DEALLOCATE(ISEGIP_SS1)
      ENDIF
      IF(SIZE_IW1.NE.0) THEN
        DEALLOCATE(IW1_SS1)
      ENDIF
      IF(SIZE_INDTRI.NE.0) THEN
        DEALLOCATE(INDTRI_SS1)
      ENDIF
      IF(SIZE_INX.NE.0) THEN
        DEALLOCATE(INX_SS1)
      ENDIF
      IF(SIZE_IPX.NE.0) THEN
        DEALLOCATE(IPX_SS1)
      ENDIF
      IF(SIZE_AC.NE.0) THEN
        DEALLOCATE(AC_SS1)
      ENDIF
      IF(SIZE_ACTRI.NE.0) THEN
        DEALLOCATE(ACTRI_SS1)
      ENDIF
      IF(SIZE_ISP.NE.0) THEN
        DEALLOCATE(ISP_SS1)
      ENDIF
      IF(SIZE_RSP.NE.0) THEN
        DEALLOCATE(RSP_SS1)
      ENDIF
      ! SD_SOLVE_4
      IF(SIZE_GLOSEG4.NE.0) THEN
        DEALLOCATE(GLOSEG4_SS4)
      ENDIF
      IF(SIZE_DA.NE.0) THEN
        DEALLOCATE(DA_SS4)
      ENDIF
      IF(SIZE_XA.NE.0) THEN
        DEALLOCATE(XA_SS4)
      ENDIF
      IF(SIZE_RHS.NE.0) THEN
        DEALLOCATE(RHS_SS4)
      ENDIF
      IF(SIZE_XINC.NE.0) THEN
        DEALLOCATE(XINC_SS4)
      ENDIF
      ! PRE4_MUMPS
      IF(SIZE_GLOSEG4_P4M.NE.0) THEN
        DEALLOCATE(GLOSEG4_P4M)
      ENDIF
      IF(SIZE_DA_P4M.NE.0) THEN
        DEALLOCATE(DA_P4M)
      ENDIF
      IF(SIZE_XA_P4M.NE.0) THEN
        DEALLOCATE(XA_P4M)
      ENDIF
      IF(SIZE_RHS_P4M.NE.0) THEN
        DEALLOCATE(RHS_P4M)
      ENDIF
      IF(SIZE_XINC_P4M.NE.0) THEN
        DEALLOCATE(XINC_P4M)
      ENDIF
      ! CHARAC
      IF(DEJA_CHARAC) THEN
        CALL BIEF_DEALLOBJ(T1WEAK)
        CALL BIEF_DEALLOBJ(T2WEAK)
        CALL BIEF_DEALLOBJ(T3WEAK)
        CALL BIEF_DEALLOBJ(T4WEAK)
        CALL BIEF_DEALLOBJ(T5WEAK)
        CALL BIEF_DEALLOBJ(T6WEAK)
        CALL BIEF_DEALLOBJ(T7WEAK)
        CALL BIEF_DEALLOBJ(SHPWEA)
        CALL BIEF_DEALLOBJ(FTILD_WEAK)
        CALL BIEF_DEALLOBJ(SHPBUF)
        CALL BIEF_DEALLOBJ(SHZBUF)
        CALL BIEF_DEALLOBJ(SHZWEA)
      ENDIF
      ! DERIVE
      IF(DEJA_DERIVE) THEN
        CALL BIEF_DEALLOBJ(SVOID_DERIVE)
      ENDIF
      IF(.NOT.INIT_ALG) THEN
        DEALLOCATE(BUFF_1D_D)
        DEALLOCATE(BUFF_2D_D)
      ENDIF
!
!     Streamline
!
      CALL DEALLOC_STREAMLINE()
      
!
!     Algae
!
      CALL DEALLOC_ALGAE()

!
!-----------------------------------------------------------------------
!
      RETURN
      END
