!                    ************************
                     SUBROUTINE CVSP_RM_FRACTION
!                    ************************
!
     &(J, I, dZFCL, EVL)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/06/2011
!***********************************************************************
!
!brief    REMOVES (parts) of a fraction after Erosion
!+        from the Vertical Sorting Profile;
!
!history  Uwe Merkel 2012-02-02
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| J              |<--| INDEX of a POINT in MESH
!| I              |<--| INDEX of a FRACTION
!| dZFCL          |<--| VALUE of a FRACTION in cm !
!| EVL            |<--| SUM of all FRACTIONs
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      USE DECLARATIONS_SISYPHE
      USE CVSP_OUTPUTFILES
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      INTEGER,          INTENT(IN)    :: J
      INTEGER,          INTENT(IN)    :: I
      doUBLE PRECISION, INTENT(IN)    :: dZFCL
      doUBLE PRECISION, INTENT(IN)    :: EVL
!PAT      LOGICAL CVSP_CHECK_F, ret, NORMALIZE_FRACTION, db
      LOGICAL ret, NORMALIZE_FRACTION
      LOGICAL, EXTERNAL:: CVSP_CHECK_F, DB
!
      doUBLE PRECISION EROSUM, PROF, PROTH, PROV, REST1,REST2, AT, DFRAC
      double precision sumzfcl, sumprof, erostrength, erodepth
      double precision PRO_D_LOW, PRO_F_LOW,PROV_TOTAL,F1,F2,D1,D2

      INTEGER CNTR,CNTRB, II, KK, iii, LOWPNT,isicla, JG
      character*30 debugfile
      double precision  ST_s, ST_a, ST_m, ST_c     !aux for strength calculations

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


             JG = j
             if (ncsize > 1) JG = mesh%knolg%I(J)

         if (db(JG,0).eqv..true.) call CVSP_P('./VSP/','RFT_IN_',JG)

!         if(J==1247) print*,'in rm_fraction',pro_d(j,pro_max(j),1),
!     & pro_max(j),pro_d(j,pro_max(j)-1,1)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


               AT = DT*LT/PERCOU                       !Time only for Printout
               EROSUM = -1.D0 * dZFCL                  !Erosion Sum POSITIV = EROSION
               CNTR = -1                               !section cntr below PRO_MAX

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !Loop through VSP sections until all erosion is done
      !Counting back EROSUM -> 0
      !Identify the last Section Point affected

      do while ((EROSUM > 0.D0.or.cntr==-1).and.(cntr.lt.PRO_MAX(J)-2))

          cntr  =  cntr + 1

            F1 = PRO_F(J,PRO_MAX(J)-cntr,I)
            F2 = PRO_F(J,PRO_MAX(J)-(cntr+1),I)

            D1 = PRO_D(J,PRO_MAX(J)-cntr,I)
            D2 = PRO_D(J,PRO_MAX(J)-(cntr+1),I)

          PROF  = (F1 + F2) * 0.5D0                    !Mean Fraction

          PROTH = D1 - D2                              !PROFILE section THICKNESS

          PROV  =  PROF * PROTH                        !PROFILE section VOLUME ( eq. Thickness of Fraction)

          DFRAC =  F1 - F2                             !Delta fraction = Change of Fraction over this section




      if (PROTH.GT.0.D0) Then
      !-----------------------------------------------------
      ! CASE 2
      ! This VSP section has more then enough material
      ! Then split it at the depth of maximum erosion,
      ! to keep anthing below unchanged
      ! => Makes CASE 2 to CASE 1
      if ((EROSUM < PROV)) then

        if (db(JG,0).eqv..true.) call CVSP_P('./VSP/','RFT_C2_',JG)

        !------------------------------------------------------------------
        !maximum depth of erosion:      solving quadratic problem by Cross-multiplication


        !Linear or quatratic? Prevents running into floating point cancellation problems
            ST_M = ABS(DFRAC / PROTH)
        !-quatratic
        if ((ST_M.gt.1.0D-7).and.(ABS(DFRAC).gt.1.0D-12)) then

                ST_C = F1 / ST_M
                ST_A = EROSUM + 0.5D0 * ST_M * ST_C**2
                ST_S = sqrt(2.D0 * ST_A / ST_M)
                EROSTRENGTH = ST_S - ST_C

        !-linear (save mode) for almost no change in fraction over depth
        ! = acceptaple fraction shift to other fractions (illegal!!)...
        else

                EROSTRENGTH = EROSUM / PROV * PROTH

        endif

                ERODEPTH = D1 - EROSTRENGTH

                !ATTENTION
                if ((ERODEPTH-D2).lt.0.D0) ERODEPTH = D2



      if (db(JG,0).eqv..true.) then
      !Print*,'UHM2: '
      !Print*, JG, I, cntr, dZFCL, EVL, PRO_MAX(J)-(cntr-1)
      !Print*,'UHM3:' ,  ST_C, ST_S ,erostrength, erosum, PROV
      !Print*,'UHM4: ',        st_a,st_m, DFRAC,PROTH
      !Print*,'UHM5: ',        ERODEPTH,D1,D2,F1,F2
      !Print*, '  '
      endif


          !------------------------------------------------------------------
          !insert a new breakpoint to split the VSP and shift the rest
          !shift
          do KK = 0,cntr
            do II=1,NSICLA
              PRO_F(J,PRO_MAX(J)-KK+1,II) = PRO_F(J,PRO_MAX(J)-KK,II)
              PRO_D(J,PRO_MAX(J)-KK+1,II) = PRO_D(J,PRO_MAX(J)-KK,II)
            enddo
          enddo


          !insert
          do II=1,NSICLA
            PRO_D(J,PRO_MAX(J)-cntr,II) = ERODEPTH
            PRO_F(J,PRO_MAX(J)-cntr,II) = PRO_F(J,PRO_MAX(J)-cntr,II)
     &               - ( PRO_F(J,PRO_MAX(J)-cntr,II)
     &                  -PRO_F(J,PRO_MAX(J)-(cntr+1),II) )
     &               / PROTH * EROSTRENGTH
          enddo


            if (PRO_D(J,PRO_MAX(J)-cntr,1).lt.
     &          PRO_D(J,PRO_MAX(J)-cntr-1,1)) then
          WRITE(LU,*) 'Depthinversion!!!',JG,PRO_MAX(J)-cntr,PRO_MAX(J),
     &           PROTH, EROSTRENGTH
                call plante(1)
            endif

         PRO_MAX(J) = PRO_MAX(J) + 1



        if (db(JG,0).eqv..true.) call CVSP_P('./VSP/','RFT_C3_',JG)

      if(CVSP_CHECK_F(J,PRO_MAX(J)-cntr,'RMF:Cas2 B').eqv..false.)then
      endif


          !------------------------------------------------------------------
          ! After splitting: Update section properties


          PROF  = (PRO_F(J,PRO_MAX(J)-cntr,I) +        !PROFILE section FRACTION
     &             PRO_F(J,PRO_MAX(J)-(cntr+1),I)) * 0.5D0

          PROTH = (PRO_D(J,PRO_MAX(J)-cntr,I) -
     &             PRO_D(J,PRO_MAX(J)-(cntr+1),I))     !PROFILE section THICKNESS

          PROV  =  PROF * PROTH                        !PROFILE section VOLUME ( eq. Thickness of Fraction)

          DFRAC =  PRO_F(J,PRO_MAX(J)-cntr,I) -        !Delta fraction = Change of Fraction over this section
     &             PRO_F(J,PRO_MAX(J)-(cntr+1),I)


          EROSUM = PROV                                !Necessary! to avoid problems with sliver polygons





        endif        ! CASE 2 conversion
      !-----------------------------------------------------




      !-----------------------------------------------------
      !CASE 1 (now all cases, as CASE II is already converted)
      !This VSP section has not enough or exactly enough material to satisfy the hunger
      !means. It is removed completly


            EROSUM = - PROV + EROSUM



        !!  PROTH > 0
        endif
      !End Loop through VSP Section
      End do


         !Deepest/Last touched Section Point
         cntrB = cntr+1



      ! FIXING DEPTH AND FRACTION
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !After digging out 1 fraction, everything above falls down
      !to fill the hole
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          if (db(JG,0).eqv..true.) call CVSP_P('./VSP/','RFT_C4_',JG)

          !------------------------------------------------------------------
          ! shift and duplicate deepest point to produce a clear break when normalizing the fractions
          ! in the next step
             if (cntrB.eq.Pro_Max(J)) cntrB = Pro_Max(J) - 1    !Prevent rigid bed demolition
          do KK = 0,cntrB
                              ! DEBUG
                              if (PRO_MAX(J)-KK+1 > PRO_MAX_MAX-1) then
                                Print*,'PRO_MAX_MAX_: ',J,PRO_MAX(J)
                                call CVSP_P('./ERR/','MAX_I',JG)
                                call plante(1)
                              endif

            do II=1,NSICLA
              PRO_F(J,PRO_MAX(J)-KK+1,II) = PRO_F(J,PRO_MAX(J)-KK,II)
              PRO_D(J,PRO_MAX(J)-KK+1,II) = PRO_D(J,PRO_MAX(J)-KK,II)
            enddo

          enddo

            !1 point more / means one point deeper affected
            PRO_MAX(J) = PRO_MAX(J) + 1

          !------------------------------------------------------------------


          if (db(JG,0).eqv..true.) call CVSP_P('./VSP/','TEST4_',JG)


          !------------------------------------------------------------------
          !All layers above fall down = Every fraction falls, going from deepest to highest affected section

                    LOWPNT = PRO_MAX(J)-cntrB

                    PRO_F_LOW = PRO_F(J,LOWPNT,I)
                    PRO_D_LOW = PRO_D(J,LOWPNT,I)                 !Start at loweset level
                    PROV_TOTAL = 0.D0                             !Accumulating the erosion the higher we go

            do KK = LOWPNT+1,PRO_MAX(J)                           !Loop over upper section point

                    PROF  = ( PRO_F(J,KK,I) + PRO_F_LOW) * 0.5D0  !PROFILE section FRACTION
                    PROTH = ( PRO_D(J,KK,I) - PRO_D_LOW)          !PROFILE section THICKNESS
                    PROV  =  PROF * PROTH                         !PROFILE section VOLUME ( eq. Thickness of Fraction)
                    if (PROV < ZERO) PROV = 0.D0
                    PROV_TOTAL = PROV + PROV_TOTAL
                    REST1 = ( - PRO_F(J,KK,I) + 1.D0)             !Sum of Fractions after Erosion

                    PRO_D_LOW = PRO_D(J,KK,I)                     !Remember This for next section step! cause  PRO_D(J,KK,II) is not availabel any more
                    PRO_F_LOW = PRO_F(J,KK,I)                     !keep this for the next section!!!!

                do II=1,NSICLA

                    PRO_D(J,KK,II) = -PROV_TOTAL + PRO_D(J,KK,II)        !Shift Depth

                    if (PRO_D(J,KK,II).le.PRO_D(J,KK-1,II)) then        !Correcting 1D-15 errors which produce unsteady PDFs
                        PRO_D(J,KK,II) = PRO_D(J,KK-1,II)
                    endif

                    if (rest1.gt.zero) then
                    PRO_F(J,KK,II) = PRO_F(J,KK,II)/ REST1               !Normalize Fraction
                    else
                    PRO_F(J,KK,II) = 1.D0 / NSICLA                       !In case of almost total loss
                    endif

                enddo

                    if (rest1.gt.zero) then
                        PRO_F(J,KK,I) = 0.D0                              !Fraction I is removed
                    else
                        PRO_F(J,KK,I) = 1.D0 / NSICLA                     !In case of almost total loss
                    endif

            enddo



          if (db(JG,0).eqv..true.) call CVSP_P('./VSP/','TEST5_',JG)



               ! Special treatment Lowest point
                  REST1 = (-PRO_F(J,LOWPNT,I)+1.D0)

                do II=1,NSICLA
                  if (rest1.gt.zero) then
                    PRO_F(J,LOWPNT,II) =
     &                    PRO_F(J,LOWPNT,II) / REST1                         !Normalize Fraction
                  else
                    PRO_F(J,LOWPNT,II) = 1.D0 / NSICLA                       !In case of almost total loss
                  endif
                enddo

                  PRO_F(J,LOWPNT,I) = 0.D0                                   ! Fraction I is removed
                  if (rest1.le.zero) PRO_F(J,LOWPNT,I) =
     &                    1.D0 / NSICLA                                      !In case of almost total loss


          !------------------------------------------------------------------


        if (db(JG,0).eqv..true.) call CVSP_P('./VSP/','RFT_OU_',JG)


        !Remove empty sections .....
        call CVSP_COMPRESS_CLEAN(J)


        RETURN
      END SUBROUTINE CVSP_RM_FRACTION

