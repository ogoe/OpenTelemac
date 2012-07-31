!                    ****************
                     SUBROUTINE CVSP_MAIN
!                    ****************
!
     &(ZFCL_W,NLAYER,ZR,ZF,ESTRAT,ELAY,MASBAS,ACLADM,NSICLA,NPOIN,
     & ELAY0,VOLTOT,ES,AVAIL,CONST_ALAYER,DTS,ESTRATNEW,NLAYNEW)
!
!***********************************************************************
! SISYPHE   V6P2                                   01/06/2012
!***********************************************************************
!
!brief    Continous Vertical Sorting Model
!+        COMPUTES FRACTIONS FOR EACH CLASS AND EACH SECTION OF A C-VSM;
!+
!
!
!history  U.MERKEL (BAW), R.KOPMANN (BAW)
!+        01/06/2012
!+        V6P2
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ACLADM         |---| CALCULATED GEOMETRICAL MEAN DIAMETER OF ACT LAY
!| AVAIL          |<--| SEDIMENT FRACTION FOR EACH LAYER, CLASS, POINT
!| CONST_ALAYER   |---|
!| DTS            |---| TIMESTEP LENGTH IN [s]
!| ELAY           |<--| ACTIVE LAYER THICKNESS FOR EACH POINT
!| ELAY0          |---| WANTED ACTIVE LAYER THICKNESS
!| ES             |---| LAYER THICKNESS
!| ESTRAT         |<--| ACTIVE STRATUM THICKNESS FOR EACH POINT
!| ESTRATNEW      |---| temporary ACTIVE STRATUM THICKNESS
!| MASBAS         |---| AREA AROUND NODE
!| NLAYER         |<--| NUMBER OF LAYER FOR EACH POINT
!| NLAYNEW        |---| temporary NUMBER OF LAYER FOR EACH POINT
!| NPOIN          |---| NUMBER OF MESH POINTS
!| NSICLA         |---| NUMBER OF GRAIN CLASSES (FRACTIONS)
!| VOLTOT         |---| TOTAL VOLUME around one POINT
!| ZF             |---| BOTTOM ELEVATION
!| ZFCL_W         |-->| EVOLUTION FOR EACH SEDIMENT CLASS
!| ZR             |---| Rigid Bed
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY: CVSMOUTPUT,CVSM_OUT,CVSM_OUT_FULL,
     &      PRO_F, PRO_D, PRO_MAX, PRO_MAX_MAX, PERCOU, HN,
     &      E, LEOPR,LT,DT,PTINIG, MESH, ENTET, ZF_C,zfcl_c,Z
      !USE DECLARATIONS_TELEMAC2D, ONLY:  H

      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      INTEGER iamcase, isicla, JG
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZFCL_W,ZR,ZF
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASBAS,ACLADM
      INTEGER,          INTENT(IN)    :: NSICLA,NPOIN
      doUBLE PRECISION, INTENT(IN)    :: DTS
      LOGICAL,          INTENT(IN)    :: CONST_ALAYER
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: NLAYER,ESTRAT,ELAY
      doUBLE PRECISION, INTENT(INOUT) :: ELAY0
      doUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
      doUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,10,NSICLA)
      doUBLE PRECISION, INTENT(INOUT) :: VOLTOT(10),ESTRATNEW(NPOIN)
      INTEGER         , INTENT(INOUT) :: NLAYNEW(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      doUBLE PRECISION P_DSUM
      EXTERNAL P_DSUM
      INTEGER  P_ISUM
      EXTERNAL P_ISUM
!
!-----------------------------------------------------------------------
!
      Logical CVSP_CHECK_F, db, ret
      INTEGER I,J,K,ARRET,ARRET2
      doUBLE PRECISION dZFCL,EVL,HEIGH,TEST1,TEST2,AEVOL,AUX,AT
      double precision test,test3, delta
      integer LLT, LTT, kk
      character*50 debugstring
!
!-----------------------------------------------------------------------
!
!     TO CHECK FRACTIONS IN THE RANGE [-ZERO,1+ZERO]
!
      doUBLE PRECISION ZERO
      DATA             ZERO/1.D-10/

      ARRET=0
      AT = DT*LT/PERCOU


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----Check for Rigid bed errors

      do J=1,NPOIN
        if (Z%R(J)-ZF%R(J).lt.0.D0) then
         print *, 'UHM_Z.lt.ZF_Bef ',AT,Z%R(J),ZF%R(j),HN%R(J),
     &            (Z%R(J)-ZF%R(J))-HN%R(J)
            call CVSP_P('./ERR/','Z_', J)
            !call PLANTE(1)
        end if
      enddo


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     For All POINTS AND for all CLASSES
      do J=1,NPOIN

       JG = J
       if (NCSIZE.gt.1) JG = mesh%knolg%I(J)

       EVL = 0.D0
       do isicla = 1,nsicla
         EVL = ZFCL_W%ADR(isicla)%P%R(J) + EVL
       END do


        !Debug Info
        iamcase = 0
        if (db(JG,0).eqv..true.) call CVSP_P('./VSP/','V_A',JG)
        !/Debug Info

!     -----------------------------------------------------------------------
!     -----------------------------------------------------------------------
!     DEPOSITION IN SUM OVER ALL CASES
      if ((EVL.GT.0)) THEN
          call CVSP_ADD_SECTION(J)
      endif
!
!
      do I=1,NSICLA
        dZFCL = ZFCL_W%ADR(I)%P%R(J)

       if (EVL.GT.0D0) THEN
         if (dZFCL.GT.0D0) THEN
              call CVSP_ADD_FRACTION(J,I,dZFCL,EVL)
              iamcase = 1 + iamcase !Debug Info
         elseif( dZFCL.LT.0.D0) THEN
              call CVSP_RM_FRACTION(J,I,dZFCL,EVL)
              iamcase = 10 + iamcase !Debug Info
         endif
       endif
!      END DEPOSITION-----------------------------------------------------------------------
!
!
!      -----------------------------------------------------------------------
!      START EROSION IN SUM OVER ALL CASES
        if (EVL.LT.0.D0) THEN
          if (dZFCL.GT.0.D0) THEN
              call CVSP_ADD_FRACTION(J,I,dZFCL,EVL)
              iamcase = 100 + iamcase !Debug Info
          elseif(dZFCL.LT.0.D0) THEN
              call CVSP_RM_FRACTION(J,I,dZFCL,EVL)
              iamcase = 1000 + iamcase !Debug Info
          endif ! dzfcl
        endif ! evl < 0
!      -----------------------------------------------------------------------
!      END EROSION


      enddo
!     -----------------------------------------------------------------------
!	  END For All CLASSES


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------


      ! We are running out of section memory! COMPRESS NOW!
        if ((PRO_MAX(J).gt.PRO_MAX_MAX/4*3).or.
     &                  (PRO_MAX_MAX-PRO_MAX(J).lt.8*NSICLA)) then
           call  CVSP_COMPRESS_DP(J, 1.0D-5)
        endif


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------


      !Synchronice VSP with LAYER (For Debugging ...)

      delta = ZF%R(J) - PRO_D(J, PRO_MAX(J), 1)

      if (delta.ne.0.D0) then
          do I = 1 , NSICLA
          do K = 1, Pro_MAX(J)
            PRO_D(J, K, I) = PRO_D(J, K, I) + delta
          enddo
          enddo
      endif


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      !FINAL CHECK on new Fractions and Steady Stade

          do K = 1, Pro_MAX(J)
            ! removes numeric instabilities
            ret =  CVSP_CHECK_F(J,K,' FINAL:   ')
          enddo
            call CVSP_CHECK_STEADY(J)

!     END For All POINTS
      enddo
!


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! PRINT OUT Sorting Profile for selected Global POINT NUMBERS
!

      if((CVSM_OUT).or.(db(-1,-1).eqv..true.)) Then
        ! Writes the full VSP as Selaphine!!!!!
        if (CVSM_OUT_FULL) call CVSP_WRITE_PROFILE()
        ! Writes the VSP for single POINTS
        do kk = 1,100
        if (CVSMOUTPUT(kk).gt.0) THEN
                call CVSP_P('./VSP/','V_', CVSMOUTPUT(kk))
        endif
        enddo
      end if



!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! Generate New Layers from Sorting Profile
!

      call CVSP_MAKE_ActLay()


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----Check for Rigid bed errors

      do J=1,NPOIN
        if (Z%R(J)-ZF%R(J).lt.0.D0) then
            print *, 'UHM_Z.lt.ZF ', I,AT,Z%R(J),ZF%R(j),HN%R(J),
     &            (Z%R(J)-ZF%R(J))-HN%R(J)
            call CVSP_P('./ERR/','Z_', J)
 !           call PLANTE(1)
        end if
      enddo


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! PRINT OUT NEW LAYERS for selected Global POINT NUMBERS
      if((CVSM_OUT).or.(db(-1,-1).eqv..true.)) Then
        do kk = 1,100
        if (CVSMOUTPUT(kk).gt.0) THEN
                call LAYERS_P('./LAY/VSP_', CVSMOUTPUT(kk))
        endif
        enddo
      end if


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     CLEAN STOP FOR ALL PROCESSORS if PROBLEM
        ARRET2=ARRET
        if(NCSIZE.GT.1) ARRET2=P_ISUM(ARRET)

      if(ARRET2.GT.0) THEN
        if(LNG.EQ.1) WRITE(LU,*) 'ARRET APRES ERREUR DANS LAYER'
        if(LNG.EQ.2) WRITE(LU,*) 'STOP AFTER AN ERROR IN LAYER'
        if(ARRET.EQ.0) THEN
          if(LNG.EQ.1) WRITE(LU,*) 'DANS ',ARRET2,' PROCESSEUR(S)'
          if(LNG.EQ.2) WRITE(LU,*) 'IN ',ARRET2,' PROCESSOR(S)'
        endif
        call PLANTE(1)
      endif
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE

