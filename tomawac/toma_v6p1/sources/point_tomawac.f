!                    ************************
                     SUBROUTINE POINT_TOMAWAC
!                    ************************
!
!
!***********************************************************************
! TOMAWAC   V6P1                                   16/05/2011
!***********************************************************************
!
!brief    ALLOCATES MEMORY.
!
!history  MICHEL BENOIT (EDF R&D LNHE)
!+        06/12/2004
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
!history  G.MATTAROLO (EDF)
!+        16/05/2011
!+        V6P1
!+   Memory allocation for new variables defined by
!+       E. GAGNAIRE-RENOU for solving non linear source terms models
!+       (MDIA and GQM methods)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER MEMT5,ITAMP,IELBT,IELM0,IELB1,CFG(2),NC,NS,I
      INTEGER  P_IMAX
      EXTERNAL P_IMAX
!
!***********************************************************************
!
      IF (LNG.EQ.1) WRITE(LU,20)
      IF (LNG.EQ.2) WRITE(LU,40)
20    FORMAT(1X,///,21X,'*******************************',/,
     &21X,              '* CONSTRUCTION DES POINTEURS: *',/,
     &21X,              '*******************************',/)
40    FORMAT(1X,///,21X,'*******************************',/,
     &21X,              '*     MEMORY ORGANISATION     *',/,
     &21X,              '*******************************',/)
!
!-----------------------------------------------------------------------
!
      IELM0  = 10
      IELM2  = 11
      IELB1 = IELBOR(IELM2,1)
      CFG(1) = 1
      CFG(2) = 1
!
!-----------------------------------------------------------------------
!
!     ALLOCATES THE MESH STRUCTURE
!
      CALL ALMESH(MESH,'MESH  ',IELM2,SPHE,CFG,WAC_FILES(WACGEO)%LU,
     &            EQUA,FILE_FORMAT=WAC_FILES(WACGEO)%FMT)
!
!     ALIAS FOR CERTAIN COMPONENTS OF MESH
!
      IKLE2  => MESH%IKLE%I
      IFABOR => MESH%IFABOR%I
      NBOR   => MESH%NBOR%I
!
      X     => MESH%X%R
      Y     => MESH%Y%R
      XEL   => MESH%XEL%R
      YEL   => MESH%YEL%R
      SURDET=> MESH%SURDET%R
!
      NELEM2=>MESH%NELEM
      NPOIN2=>MESH%NPOIN
      NPTFR =>MESH%NPTFR
!
      IELBT = IELBOR(IELM2,1)
!
      NPOIN3=NPOIN2*NPLAN
!
!-----------------------------------------------------------------------
!
! JMH 05/01/2011 : COMPUTING NPOIN2_G, THE ORIGINAL NUMBER OF POINTS IN
!                  THE MESH (AS THE MAXIMUM OF ALL GLOBAL NUMBERS)
!                  THIS NUMBER IS USED TO DIMENSION ARRAYS LIKE SXRELV,
!                  ETC., THOUGH IT IS ONLY AN ESTIMATION OF THE MAXIMUM
!                  NUMBER OF POINTS GIVEN IN THE VARIOUS DATA FILES,
!                  SUCH AS WIND, ETC.
!
      NPOIN2_G=NPOIN2
      IF(NCSIZE.GT.1) THEN
        DO I=1,NPOIN2
          NPOIN2_G=MAX(NPOIN2_G,MESH%KNOLG%I(I))
        ENDDO
        NPOIN2_G=P_IMAX(NPOIN2_G)
      ENDIF
      NPOIN3_G=NPOIN2_G*NPLAN
!
!-----------------------------------------------------------------------
!
!     VARIABLES 4D TO ADVECT
      CALL BIEF_ALLVEC(1,SF,'SF    ',NPOIN3*NF , 1 , 0 ,MESH)
!
!     COEFFICIENT B FOR ADVECTION
      CALL BIEF_ALLVEC(1,SB,'SB    ',NPOIN2*NF , 1 , 0 ,MESH)
!
!     ARRAY OF DISCRETISED FREQUENCIES, AND OF DELTA F
      CALL BIEF_ALLVEC(1,SFR,'SFR   ' ,NF , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,SDFR,'SDFR  ',NF , 1 , 0 ,MESH)
      FREQ     =>SFR%R
      DFREQ    =>SDFR%R
!
!     "PHYSICAL" VARIABLES OF SIZE NPOIN3
      CALL BIEF_ALLVEC(1,SXK,'SXK   ',NPOIN2*NF, 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,SCG,'SCG   ',NPOIN2*NF, 1 , 0 ,MESH)
!
      IF (TSOU) THEN
        CALL BIEF_ALLVEC(1,STSDER,'STSDER',NF*NPOIN3 , 1 , 0 ,MESH)
        CALL BIEF_ALLVEC(1,STSTOT,'STSTOT',NF*NPOIN3 , 1 , 0 ,MESH)
        CALL BIEF_ALLVEC(1,SDF_LIM,'SDF_LIM',NPOIN2*NF, 1 , 0 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,STSDER,'STSDER', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,STSTOT,'STSTOT', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SDF_LIM,'SDF_LIM',1, 1 , 0 ,MESH)
      ENDIF
      TSDER   => STSDER%R
      TSTOT   => STSTOT%R
      DF_LIM  => SDF_LIM%R
!
!     POINTERS FOR THE BOUNDARY CONDITIONS
      CALL BIEF_ALLVEC(1,SFBOR,'SFBOR ',IELBT, NPLAN*NF , 2 ,MESH)
!
!     ARRAYS FOR NON-LINEAR INTERACTIONS
!GM V6P1 - NEW SOURCE TERMS
      IF (STRIF.EQ.1) THEN
!GM Fin
        CALL BIEF_ALLVEC(1,SCOEF,'SCOEF ',16   , 1 , 0 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,SCOEF ,'SCOEF ', 1, 1, 0 ,MESH)
      ENDIF
      COEFNL   =>SCOEF%R
!
!     ADVECTION FIELD
      IF(COUSTA .OR. MAREE.OR.NAMECODE(1:7).EQ.'TELEMAC') THEN
        NC=NPOIN3*NF
        NS=NPOIN3*NF
      ELSE
        NS=NPOIN3*NF
        NC=NPOIN3
      ENDIF
!
      F     =>SF%R
      B     =>SB%R
      XK    =>SXK%R
      CG    =>SCG%R
      FBOR  =>SFBOR%R
!
      IF(PROP) THEN
!       FOOT OF THE CHARACTERISTICS
        CALL BIEF_ALLVEC(1,SSHP1,'SSHP1 ',NS , 1 , 0 ,MESH)
        CALL BIEF_ALLVEC(1,SSHP2,'SSHP2 ',NS , 1 , 0 ,MESH)
        CALL BIEF_ALLVEC(1,SSHP3,'SSHP3 ',NS , 1 , 0 ,MESH)
        CALL BIEF_ALLVEC(1,SSHZ ,'SSHZ  ',NS , 1 , 0 ,MESH)
!
        CALL BIEF_ALLVEC(1,SCX,'SCX   ',NC , 1 , 0 ,MESH)
        CALL BIEF_ALLVEC(1,SCY,'SCY   ',NC , 1 , 0 ,MESH)
        CALL BIEF_ALLVEC(1,SCT,'SCT   ',NC , 1 , 0 ,MESH)
!
        IF(COURAN.OR.NAMECODE(1:7).EQ.'TELEMAC') THEN
          CALL BIEF_ALLVEC(1,SSHF,'SSHF  ',NS , 1 , 0 ,MESH)
          CALL BIEF_ALLVEC(1,SCF ,'SCF   ',NC , 1 , 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(1,SSHF  ,'SSHF  ', 1, 1, 0 ,MESH)
          CALL BIEF_ALLVEC(1,SCF   ,'SCF   ', 1, 1, 0 ,MESH)
        ENDIF
      ELSE
        CALL BIEF_ALLVEC(1,SSHP1 ,'SSHP1 ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SSHP2 ,'SSHP2 ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SSHP3 ,'SSHP3 ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SSHZ  ,'SSHZ  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SCX   ,'SCX   ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SCY   ,'SCY   ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SCT   ,'SCT   ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SSHF  ,'SSHF  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SCF   ,'SCF   ', 1, 1, 0 ,MESH)
      ENDIF
      SHP1  =>SSHP1%R
      SHP2  =>SSHP2%R
      SHP3  =>SSHP3%R
      SHZ   =>SSHZ%R
      CX    =>SCX%R
      CY    =>SCY%R
      CT    =>SCT%R
      SHF   =>SSHF%R
      CF    =>SCF%R
!
! ARRAYS OF SIZE NPOIN2
!
      CALL BIEF_ALLVEC(1,SZF,'SZF   ',IELM2 , 1 , 2 ,MESH)
      ZF    =>SZF%R
      CALL BIEF_ALLVEC(1,SDEPTH,'SDEPTH',IELM2 , 1 , 2 ,MESH)
      DEPTH =>SDEPTH%R
!
!     ADDED BY JMH 16/12/2008 (MAYBE NOT ALWAYS USED)
!
      CALL BIEF_ALLVEC(1,SBETA,'SBETA ',IELM2,1,2,MESH)
      BETA => SBETA%R
!
!     END OF JMH ADDITION
!
      IF (.NOT.PROINF) THEN
        CALL BIEF_ALLVEC(1,SDZX  ,'SDZX  ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,SDZY  ,'SDZY  ',IELM2 , 1 , 2 ,MESH)
      ELSE
        CALL BIEF_ALLVEC(1,SDZX  ,'SDZX  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,SDZY  ,'SDZY  ', 1, 1, 0 ,MESH)
      ENDIF
      DZX     =>SDZX%R
      DZY     =>SDZY%R
!
!     NAMECODE IS IN DECLARATIONS_TELEMAC AND IS MONITORED BY
!     SUBROUTINE CONFIG_CODE
!
      IF(COURAN.OR.DONTEL.OR.NAMECODE(1:7).EQ.'TELEMAC') THEN
         CALL BIEF_ALLVEC(1,SUC ,'SUC   ',IELM2 , 1 , 2 ,MESH)
         CALL BIEF_ALLVEC(1,SVC ,'SVC   ',IELM2 , 1 , 2 ,MESH)
         CALL BIEF_ALLVEC(1,SDUX,'SDUX  ',IELM2 , 1 , 2 ,MESH)
         CALL BIEF_ALLVEC(1,SDUY,'SDUY  ',IELM2 , 1 , 2 ,MESH)
         CALL BIEF_ALLVEC(1,SDVX,'SDVX  ',IELM2 , 1 , 2 ,MESH)
         CALL BIEF_ALLVEC(1,SDVY,'SDVY  ',IELM2 , 1 , 2 ,MESH)
         CALL BIEF_ALLVEC(1,SDZHDT,'SDZHDT',IELM2 , 1 , 2 ,MESH)
         IF (WAC_FILES(WACCOB)%NAME.NE.' '.OR.
     &       WAC_FILES(WACCOF)%NAME.NE.' ') THEN
           CALL BIEF_ALLVEC(1,SXRELC,'SXRELC',NPOIN3_G, 1 , 0 ,MESH)
           CALL BIEF_ALLVEC(1,SYRELC,'SYRELC',NPOIN3_G, 1 , 0 ,MESH)
         ELSE
           CALL BIEF_ALLVEC(1,SXRELC,'SXRELC', 1, 1, 0 ,MESH)
           CALL BIEF_ALLVEC(1,SYRELC,'SYRELC', 1, 1, 0 ,MESH)
         ENDIF
      ELSE
         CALL BIEF_ALLVEC(1,SUC   ,'SUC   ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SVC   ,'SVC   ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SDUX  ,'SDUX  ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SDUY  ,'SDUY  ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SDVX  ,'SDVX  ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SDVY  ,'SDVY  ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SDZHDT,'SDZHDT', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SXRELC,'SXRELC', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SYRELC,'SYRELC', 1, 1, 0 ,MESH)
      ENDIF
      UC      =>SUC%R
      VC      =>SVC%R
      DUX     =>SDUX%R
      DUY     =>SDUY%R
      DVX     =>SDVX%R
      DVY     =>SDVY%R
      XRELC   =>SXRELC%R
      YRELC   =>SYRELC%R
!
      IF (MAREE.OR.NAMECODE(1:7).EQ.'TELEMAC') THEN
         IF (WAC_FILES(WACCOB)%NAME.NE.' '.OR.
     &       WAC_FILES(WACCOF)%NAME.NE.' ') THEN
           CALL BIEF_ALLVEC(1,SVC1,'SVC1  ',IELM2 , 1 , 2 ,MESH)
           CALL BIEF_ALLVEC(1,SUC1,'SUC1  ',IELM2 , 1 , 2 ,MESH)
           CALL BIEF_ALLVEC(1,SVC2,'SVC2  ',IELM2 , 1 , 2 ,MESH)
           CALL BIEF_ALLVEC(1,SUC2,'SUC2  ',IELM2 , 1 , 2 ,MESH)
         ELSE
           CALL BIEF_ALLVEC(1,SVC1  ,'SVC1  ', 1, 1, 0 ,MESH)
           CALL BIEF_ALLVEC(1,SUC1  ,'SUC1  ', 1, 1, 0 ,MESH)
           CALL BIEF_ALLVEC(1,SVC2  ,'SVC2  ', 1, 1, 0 ,MESH)
           CALL BIEF_ALLVEC(1,SUC2  ,'SUC2  ', 1, 1, 0 ,MESH)
         ENDIF
         CALL BIEF_ALLVEC(1,SZM1,'SZM1  ',IELM2 , 1 , 2 ,MESH)
         CALL BIEF_ALLVEC(1,SZM2,'SZM2  ',IELM2 , 1 , 2 ,MESH)
         IF (WAC_FILES(WACMAB)%NAME.NE.' '.OR.
     &       WAC_FILES(WACMAF)%NAME.NE.' ') THEN
           CALL BIEF_ALLVEC(1,SXRELM,'SXRELM',NPOIN3_G, 1 , 0 ,MESH)
           CALL BIEF_ALLVEC(1,SYRELM,'SYRELM',NPOIN3_G, 1 , 0 ,MESH)
         ELSE
           CALL BIEF_ALLVEC(1,SXRELM,'SXRELM', 1, 1, 0 ,MESH)
           CALL BIEF_ALLVEC(1,SYRELM,'SYRELM', 1, 1, 0 ,MESH)
         ENDIF
      ELSE
         CALL BIEF_ALLVEC(1,SVC1  ,'SVC1  ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SUC1  ,'SUC1  ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SVC2  ,'SVC2  ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SUC2  ,'SUC2  ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SZM1  ,'SZM1  ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SZM2  ,'SZM2  ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SXRELM,'SXRELM', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SYRELM,'SYRELM', 1, 1, 0 ,MESH)
      ENDIF
      DZHDT   =>SDZHDT%R
      UC1     =>SUC1%R
      VC1     =>SVC1%R
      UC2     =>SUC2%R
      VC2     =>SVC2%R
      ZM1     =>SZM1%R
      ZM2     =>SZM2%R
      XRELM   =>SXRELM%R
      YRELM   =>SYRELM%R
!
      IF (VENT) THEN
         CALL BIEF_ALLVEC(1,SUV,'SUV   ',IELM2 , 1 , 2 ,MESH)
         CALL BIEF_ALLVEC(1,SVV,'SVV   ',IELM2 , 1 , 2 ,MESH)
         IF (WAC_FILES(WACVEB)%NAME.NE.' '.OR.
     &       WAC_FILES(WACVEF)%NAME.NE.' ') THEN
           CALL BIEF_ALLVEC(1,SXRELV,'SXRELV',NPOIN3_G,1,0,MESH)
           CALL BIEF_ALLVEC(1,SYRELV,'SYRELV',NPOIN3_G,1,0,MESH)
           CALL BIEF_ALLVEC(1,SVV1,'SVV1  ',IELM2 , 1 , 2 ,MESH)
           CALL BIEF_ALLVEC(1,SUV1,'SUV1  ',IELM2 , 1 , 2 ,MESH)
           CALL BIEF_ALLVEC(1,SVV2,'SVV2  ',IELM2 , 1 , 2 ,MESH)
           CALL BIEF_ALLVEC(1,SUV2,'SUV2  ',IELM2 , 1 , 2 ,MESH)
         ELSE
           CALL BIEF_ALLVEC(1,SXRELV,'SXRELV', 1, 1, 0 ,MESH)
           CALL BIEF_ALLVEC(1,SYRELV,'SYRELV', 1, 1, 0 ,MESH)
           CALL BIEF_ALLVEC(1,SVV1  ,'SVV1  ', 1, 1, 0 ,MESH)
           CALL BIEF_ALLVEC(1,SUV1  ,'SUV1  ', 1, 1, 0 ,MESH)
           CALL BIEF_ALLVEC(1,SVV2  ,'SVV2  ', 1, 1, 0 ,MESH)
           CALL BIEF_ALLVEC(1,SUV2  ,'SUV2  ', 1, 1, 0 ,MESH)
         ENDIF
      ELSE
         CALL BIEF_ALLVEC(1,SUV   ,'SUV   ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SVV   ,'SVV   ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SXRELV,'SXRELV', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SYRELV,'SYRELV', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SVV1  ,'SVV1  ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SUV1  ,'SUV1  ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SVV2  ,'SVV2  ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,SUV2  ,'SUV2  ', 1, 1, 0 ,MESH)
      ENDIF
      UV      =>SUV%R
      VV      =>SVV%R
      VV1     =>SVV1%R
      UV1     =>SUV1%R
      VV2     =>SVV2%R
      UV2     =>SUV2%R
      XRELV   =>SXRELV%R
      YRELV   =>SYRELV%R
!
!
!
!
      IF (SPHE) THEN
         CALL BIEF_ALLVEC(1,SCOSF,'SCOSF ',IELM2 , 1 , 2 ,MESH)
         CALL BIEF_ALLVEC(1,STGF,'STGF  ',IELM2 , 1 , 2 ,MESH)
      ELSE
         CALL BIEF_ALLVEC(1,SCOSF ,'SCOSF ', 1, 1, 0 ,MESH)
         CALL BIEF_ALLVEC(1,STGF  ,'STGF  ', 1, 1, 0 ,MESH)
      ENDIF
      COSF    =>SCOSF%R
      TGF     =>STGF%R
!
!
!
!
!     ARRAYS WITH THE RELATIVE POSITIONS OF THE DIRECTION PLANES,
!     AND WITH THE COS AND SIN TETA
      CALL BIEF_ALLVEC(1,STETA,'STETA ',NPLAN+1 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,SCOSTE,'SCOSTE',NPLAN , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,SSINTE,'SSINTE',NPLAN , 1 , 0 ,MESH)
      TETA     =>STETA%R
      COSTET   =>SCOSTE%R
      SINTET   =>SSINTE%R
!
!
!
!     POINTERS FOR WORKING ARRAYS (BY POINTS AND ELEMENTS)
      CALL BIEF_ALLVEC(1,ST0, 'ST0   ',IELM2 , 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,ST1, 'ST1   ',IELM2 , 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,ST2, 'ST2   ',IELM2 , 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,ST3, 'ST3   ',IELM2 , 1 , 2 ,MESH)
      CALL BIEF_ALLVEC(1,ST4, 'ST4   ',IELM2 , 1 , 2 ,MESH)
      T0      =>ST0%R
      T1      =>ST1%R
      T2      =>ST2%R
      T3      =>ST3%R
      T4      =>ST4%R
!
!
!
!     POINTERS FOR MATRICES, AM1 SYMMETRICAL MATRIX
      CALL BIEF_ALLMAT(AM1,'AM1   ',IELM2,IELM2,CFG,'Q','S',MESH)
!
!
!
!     VARIOUS WORKING ARRAYS
      CALL BIEF_ALLVEC(1,SW1,   'SW1   ',NELEM2 , 4 , 0 ,MESH)
      W1      =>SW1%R
      CALL BIEF_ALLVEC(1,STRA31,'STRA31',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA32,'STRA32',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA33,'STRA33',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA34,'STRA34',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA35,'STRA35',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA36,'STRA36',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA37,'STRA37',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA38,'STRA38',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA39,'STRA39',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA40,'STRA40',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA41,'STRA41',NPOIN2 , 1 , 0 ,MESH)
      CALL BIEF_ALLVEC(1,STRA42,'STRA42',NPOIN2 , 1 , 0 ,MESH)
      TRA31   => STRA31%R
      TRA32   => STRA32%R
      TRA33   => STRA33%R
      TRA34   => STRA34%R
      TRA35   => STRA35%R
      TRA36   => STRA36%R
      TRA37   => STRA37%R
      TRA38   => STRA38%R
      TRA39   => STRA39%R
      TRA40   => STRA40%R
      TRA41   => STRA41%R
      TRA42   => STRA42%R
!
!
!
        CALL BIEF_ALLVEC(1,STRA02,'STRA02',NPOIN3*NF,2, 0 ,MESH)
        TRA02 =>STRA02%R
!
!.......VARIOUS WORKING ARRAYS
!
        CALL BIEF_ALLVEC(1,STRA01,'STRA01',NPOIN3_G,8,0,MESH)
!
        CALL BIEF_ALLVEC(1,STRAB1,'STRAB1', 1, 1, 0 ,MESH)
        TRA01   =>STRA01%R
        TRAB1   =>STRAB1%R
!
        IF (TSOU) THEN
          CALL BIEF_ALLVEC(1,STOLD,'STOLD ',NPOIN3 , 1 , 0 ,MESH)
          CALL BIEF_ALLVEC(1,STNEW,'STNEW ',NPOIN3 , 1 , 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(1,STOLD ,'STOLD ', 1, 1, 0 ,MESH)
          CALL BIEF_ALLVEC(1,STNEW ,'STNEW ', 1, 1, 0 ,MESH)
        ENDIF
        TOLD    => STOLD%R
        TNEW    => STNEW%R
!
        CALL BIEF_ALLVEC(1,STRA43,'STRA43',NPOIN2 , 1 , 0 ,MESH)
        CALL BIEF_ALLVEC(1,STRA44,'STRA44',NPOIN2 , 1 , 0 ,MESH)
        TRA43   => STRA43%R
        TRA44   => STRA44%R
!
        IF (SORLEO(22).OR.SORLEO(32)) THEN
          CALL BIEF_ALLVEC(1,STRA56,'STRA56',NPOIN2 , 1 , 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(1,STRA56,'STRA56', 1, 1, 0 ,MESH)
        ENDIF
        IF (SORLEO(23).OR.SORLEO(33)) THEN
          CALL BIEF_ALLVEC(1,STRA57,'STRA57',NPOIN2 , 1 , 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(1,STRA57,'STRA57', 1, 1, 0 ,MESH)
        ENDIF
        IF (SORLEO(25)) THEN
          CALL BIEF_ALLVEC(1,STRA58,'STRA58',NPOIN2 , 1 , 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(1,STRA58,'STRA58', 1, 1, 0 ,MESH)
        ENDIF
        IF (SORLEO(28)) THEN
          CALL BIEF_ALLVEC(1,STRA61,'STRA61',NPOIN2 , 1 , 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(1,STRA61,'STRA61', 1, 1, 0 ,MESH)
        ENDIF
        IF (SORLEO(29)) THEN
          CALL BIEF_ALLVEC(1,STRA62,'STRA62',NPOIN2 , 1 , 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(1,STRA62,'STRA62', 1, 1, 0 ,MESH)
        ENDIF
        IF (SORLEO(30)) THEN
          CALL BIEF_ALLVEC(1,STRA63,'STRA63',NPOIN2 , 1 , 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(1,STRA63,'STRA63', 1, 1, 0 ,MESH)
        ENDIF
        IF (SORLEO(31)) THEN
          CALL BIEF_ALLVEC(1,STRA64,'STRA64',NPOIN2 , 1 , 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(1,STRA64,'STRA64', 1, 1, 0 ,MESH)
        ENDIF
        IF (SORLEO(32)) THEN
          CALL BIEF_ALLVEC(1,STRA65,'STRA65',NPOIN2 , 1 , 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(1,STRA65,'STRA65', 1, 1, 0 ,MESH)
        ENDIF
        IF (SORLEO(33)) THEN
          CALL BIEF_ALLVEC(1,STRA66,'STRA66',NPOIN2 , 1 , 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(1,STRA66,'STRA66', 1, 1, 0 ,MESH)
        ENDIF
        IF (SORLEO(34)) THEN
          CALL BIEF_ALLVEC(1,STRA60,'STRA60',NPOIN2 , 1 , 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(1,STRA60,'STRA60', 1, 1, 0 ,MESH)
        ENDIF
        IF (.NOT.PROINF) THEN
          IF ( SORLEO(11).OR.SORLEO(12).OR.SORLEO(13).OR.
     &         SORLEO(14).OR.SORLEO(15) ) THEN
            CALL BIEF_ALLVEC(1,STRA51,'STRA51',NPOIN2 , 1 , 0 ,MESH)
            CALL BIEF_ALLVEC(1,STRA52,'STRA52',NPOIN2 , 1 , 0 ,MESH)
            CALL BIEF_ALLVEC(1,STRA53,'STRA53',NPOIN2 , 1 , 0 ,MESH)
            CALL BIEF_ALLVEC(1,STRA54,'STRA54',NPOIN2 , 1 , 0 ,MESH)
            CALL BIEF_ALLVEC(1,STRA55,'STRA55',NPOIN2 , 1 , 0 ,MESH)
          ELSE
            CALL BIEF_ALLVEC(1,STRA51,'STRA51', 1, 1, 0 ,MESH)
            CALL BIEF_ALLVEC(1,STRA52,'STRA52', 1, 1, 0 ,MESH)
            CALL BIEF_ALLVEC(1,STRA53,'STRA53', 1, 1, 0 ,MESH)
            CALL BIEF_ALLVEC(1,STRA54,'STRA54', 1, 1, 0 ,MESH)
            CALL BIEF_ALLVEC(1,STRA55,'STRA55', 1, 1, 0 ,MESH)
          ENDIF
          IF (SORLEO(16)) THEN
            CALL BIEF_ALLVEC(1,STRA59,'STRA59',NPOIN2 , 1 , 0 ,MESH)
          ELSE
            CALL BIEF_ALLVEC(1,STRA59,'STRA59', 1, 1, 0 ,MESH)
          ENDIF
        ELSE
          CALL BIEF_ALLVEC(1,STRA51,'STRA51', 1, 1, 0 ,MESH)
          CALL BIEF_ALLVEC(1,STRA52,'STRA52', 1, 1, 0 ,MESH)
          CALL BIEF_ALLVEC(1,STRA53,'STRA53', 1, 1, 0 ,MESH)
          CALL BIEF_ALLVEC(1,STRA54,'STRA54', 1, 1, 0 ,MESH)
          CALL BIEF_ALLVEC(1,STRA55,'STRA55', 1, 1, 0 ,MESH)
          CALL BIEF_ALLVEC(1,STRA59,'STRA59', 1, 1, 0 ,MESH)
        ENDIF
        TRA51   => STRA51%R
        TRA52   => STRA52%R
        TRA53   => STRA53%R
        TRA54   => STRA54%R
        TRA55   => STRA55%R
        TRA56   => STRA56%R
        TRA57   => STRA57%R
        TRA58   => STRA58%R
        TRA59   => STRA59%R
        TRA60   => STRA60%R
        TRA61   => STRA61%R
        TRA62   => STRA62%R
        TRA63   => STRA63%R
        TRA64   => STRA64%R
        TRA65   => STRA65%R
        TRA66   => STRA66%R
!
!...... USER DEDICATED ARRAY
        CALL BIEF_ALLVEC(1,SPRIVE,'SPRIVE',
     &                   NPOIN3*NPRIV*NF, 1 , 0 ,MESH)
        PRIVE   => SPRIVE%R
!
!.......NOT USED
        CALL BIEF_ALLVEC(1,STRA15,'STRA15', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,STRA16,'STRA16', 1, 1, 0 ,MESH)
!
!.......BLOCK FOR GRAPHICAL OUTPUTS: VARSOR
        CALL ALLBLO(VARSOR,'VARSOR')
        CALL ADDBLO(VARSOR,STRA37)
        CALL ADDBLO(VARSOR,STRA38)
        CALL ADDBLO(VARSOR,STRA32)
        CALL ADDBLO(VARSOR,STRA31)
        CALL ADDBLO(VARSOR,SZF)
        CALL ADDBLO(VARSOR,SDEPTH)
        CALL ADDBLO(VARSOR,SUC)
        CALL ADDBLO(VARSOR,SVC)
        CALL ADDBLO(VARSOR,SUV)
        CALL ADDBLO(VARSOR,SVV)
        CALL ADDBLO(VARSOR,STRA51)
        CALL ADDBLO(VARSOR,STRA52)
        CALL ADDBLO(VARSOR,STRA53)
        CALL ADDBLO(VARSOR,STRA54)
        CALL ADDBLO(VARSOR,STRA55)
        CALL ADDBLO(VARSOR,STRA59)
        CALL ADDBLO(VARSOR,SPRIVE)
        CALL ADDBLO(VARSOR,STRA33)
        CALL ADDBLO(VARSOR,STRA34)
        CALL ADDBLO(VARSOR,STRA35)
        CALL ADDBLO(VARSOR,STRA36)
        CALL ADDBLO(VARSOR,STRA56)
        CALL ADDBLO(VARSOR,STRA57)
        CALL ADDBLO(VARSOR,STRA42)
        CALL ADDBLO(VARSOR,STRA58)
        CALL ADDBLO(VARSOR,STRA44)
        CALL ADDBLO(VARSOR,STRA41)
        CALL ADDBLO(VARSOR,STRA61)
        CALL ADDBLO(VARSOR,STRA62)
        CALL ADDBLO(VARSOR,STRA63)
        CALL ADDBLO(VARSOR,STRA64)
        CALL ADDBLO(VARSOR,STRA65)
        CALL ADDBLO(VARSOR,STRA66)
        CALL ADDBLO(VARSOR,STRA60)
!       VARIABLE 35
        CALL ADDBLO(VARSOR,SBETA)
!
!
!.....BLOCK FOR VALIDATION
!
      IF (VALID) THEN
!
!       POINTERS FOR WORKING ARRAYS (BY POINTS AND ELEMENTS)
        CALL BIEF_ALLVEC(1,ST5, 'ST5   ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,ST6, 'ST6   ',IELM2 , 1 , 2 ,MESH)
        CALL BIEF_ALLVEC(1,ST7, 'ST7   ',IELM2 , 1 , 2 ,MESH)
        T5      =>ST5%R
        T6      =>ST6%R
        T7      =>ST7%R
!
!       MAKE SURE THIS IS CONSISTENT WITH THE ALIRE VECTOR
!       DECLARED IN DECLARATIONS_TOMAWAC.F
!       7 VARIABLES HAVE BEEN USED FOR VALIDATION
!          SIGNIFICANT WAVE HEIGHT    HM0       ( 2)
!          MEAN DIRECTION             DMOY      ( 3)
!          DIRECTIONAL SPREADING      SPD       ( 4)
!          DRIVING FORCE ALONG X      FX        (11)
!          DRIVING FORCE ALONG Y      FY        (12)
!          MEAN FREQUENCY FM-10       FMOY      (18)
!          MEAN FREQUENCY FM01        FM01      (19)
        CALL ALLBLO(BST1,'BST1  ')
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST1)
        CALL ADDBLO(BST1,ST2)
        CALL ADDBLO(BST1,ST3)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST4)
        CALL ADDBLO(BST1,ST5)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST0)
        CALL ADDBLO(BST1,ST6)
        CALL ADDBLO(BST1,ST7)
      ELSE
        CALL BIEF_ALLVEC(1,ST5   ,'ST5   ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,ST6   ,'ST6   ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(1,ST7   ,'ST7   ', 1, 1, 0 ,MESH)
      ENDIF
!
!-----------------------------------------------------------------------
!
!                     **********************
!                     * POINTER: ARRAY IA*
!                     **********************
!
!-----------------------------------------------------------------------
!
!
      CALL BIEF_ALLVEC(2,SLIFBR,'SLIFBR',IELB1, 1 , 1 ,MESH)
      CALL BIEF_ALLVEC(2,SIBOR,'SIBOR ',NELEM2, 7 , 0 ,MESH)
      CALL BIEF_ALLVEC(2,BOUNDARY_COLOUR,'BNDCOL',IELB1,1,1,MESH)
      LIFBOR  => SLIFBR%I
      IBOR    => SIBOR%I
!
! FOOT OF THE CHARACTERISTICS
!
      IF (PROP) THEN
        CALL BIEF_ALLVEC(2,SELT,'SELT  ',NS , 1 , 0 ,MESH)
        CALL BIEF_ALLVEC(2,SETA,'SETA  ',NS , 1 , 0 ,MESH)
        IF (COURAN.OR.NAMECODE(1:7).EQ.'TELEMAC') THEN
          CALL BIEF_ALLVEC(2,SFRE,'SFRE  ',NS , 1 , 0 ,MESH)
        ELSE
          CALL BIEF_ALLVEC(2,SFRE  ,'SFRE  ', 1, 1, 0 ,MESH)
        ENDIF
      ELSE
        CALL BIEF_ALLVEC(2,SELT  ,'SELT  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(2,SETA  ,'SETA  ', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(2,SFRE  ,'SFRE  ', 1, 1, 0 ,MESH)
      ENDIF
      ELT   => SELT%I
      ETA   => SETA%I
      FRE   => SFRE%I
!
! USEFUL ARRAY FOR THE CHARACTERISTICS
!
      CALL BIEF_ALLVEC(2,SETAP1,'SETAP1',NPLAN, 1 , 0 ,MESH)
      ETAP1  => SETAP1%I
!
! WORKING ARRAYS USED IN THE CALL TO INBIEF AND INIPIE
!
      CALL BIEF_ALLVEC(2,SITR31,'SITR31',IELM0 , 1 , 1 ,MESH)
      CALL BIEF_ALLVEC(2,SITR32,'SITR32',IELM0 , 1 , 1 ,MESH)
      CALL BIEF_ALLVEC(2,SITR33,'SITR33',IELM0 , 1 , 1 ,MESH)
      ITR31   => SITR31%I
      ITR32   => SITR32%I
      ITR33   => SITR33%I
!
!
!
!.......WORKING ARRAYS OF INTEGERS
        CALL BIEF_ALLVEC(2,SITR03,'SITR03', 1, 1, 0 ,MESH)
        CALL BIEF_ALLVEC(2,SITRB1,'SITRB1', 1, 1, 0 ,MESH)
        ITR03 => SITR03%I
        ITRB1 => SITRB1%I
        CALL BIEF_ALLVEC(2,SITR01,'SITR01',NPOIN3 ,3,0,MESH)
        ITR01 => SITR01%I
!
!.......NON-LINEAR INTERACTIONS
!GM V6P1 - NEW SOURCE TERMS
        IF (STRIF.EQ.1) THEN
!GM Fin
          CALL BIEF_ALLVEC(2,SIAGNL,'SIAGNL',8*NPLAN,1,0,MESH)
        ELSE
          CALL BIEF_ALLVEC(2,SIAGNL,'SIAGNL',1,1,0,MESH)
        ENDIF
        IANGNL => SIAGNL%I
!GM V6P1 - NEW SOURCE TERMS
!............MDIA method
        IF (STRIF.EQ.2) THEN
          ALLOCATE(XLAMDI(1:MDIA))
          ALLOCATE(XMUMDI(1:MDIA))
          ALLOCATE(IANMDI(1:NPLAN,1:16,1:MDIA))
          ALLOCATE(COEMDI(1:32,1:MDIA))
        ELSE
          ALLOCATE(XLAMDI(1))
          ALLOCATE(XMUMDI(1))
          ALLOCATE(IANMDI(1,1,1))
          ALLOCATE(COEMDI(1,1))
        ENDIF
!............GQM method
        IF (STRIF.EQ.3) THEN
          IF (IQ_OM1.EQ.1) THEN
            NF1=14
          ELSEIF (IQ_OM1.EQ.2) THEN
            NF1=26
          ELSEIF (IQ_OM1.EQ.3) THEN
            NF1=11
          ELSEIF (IQ_OM1.EQ.4) THEN
            NF1=40
          ELSEIF (IQ_OM1.EQ.5) THEN
            WRITE(6,*) 'Progression geometrique : Donnez NF1 : '
            READ(5,*) NF1
          ELSEIF (IQ_OM1.EQ.6) THEN
            WRITE(6,*) 'Progression arithmetique : Donnez NF1 : '
            READ(5,*) NF1
          ELSEIF (IQ_OM1.EQ.7) THEN
            NF1=20
          ELSE
            WRITE(6,*) 'ARRET DANS PRINCI : VALEUR INCORRECTE DE IQ_OM1'
          ENDIF
          NT1=2*NQ_TE1
          NF2=NQ_OM2
          NCONFM=NQ_OM2*NT1*NF1
          ALLOCATE(K_IF1 (1:NF1))
          ALLOCATE(K_IF2 (1:NF2,1:NT1,1:NF1),K_IF3 (1:NF2,1:NT1,1:NF1))
          ALLOCATE(K_1P  (1:NT1,1:NF1)      ,K_1M  (1:NT1,1:NF1))
          ALLOCATE(K_1P2P(1:NF2,1:NT1,1:NF1),K_1P3M(1:NF2,1:NT1,1:NF1))
          ALLOCATE(K_1P2M(1:NF2,1:NT1,1:NF1),K_1P3P(1:NF2,1:NT1,1:NF1))
          ALLOCATE(K_1M2P(1:NF2,1:NT1,1:NF1),K_1M3M(1:NF2,1:NT1,1:NF1))
          ALLOCATE(K_1M2M(1:NF2,1:NT1,1:NF1),K_1M3P(1:NF2,1:NT1,1:NF1))
          ALLOCATE(TB_V14(1:NF1))
          ALLOCATE(TB_V24(1:NF2,1:NT1,1:NF1),TB_V34(1:NF2,1:NT1,1:NF1))
          ALLOCATE(TB_TPM(1:NF2,1:NT1,1:NF1),TB_TMP(1:NF2,1:NT1,1:NF1))
          ALLOCATE(TB_FAC(1:NF2,1:NT1,1:NF1))
          ALLOCATE(IDCONF(1:NCONFM,1:3))
        ELSE
          ALLOCATE(K_IF1 (1))
          ALLOCATE(K_IF2 (1,1,1),K_IF3 (1,1,1))
          ALLOCATE(K_1P  (1,1)  ,K_1M  (1,1))
          ALLOCATE(K_1P2P(1,1,1),K_1P3M(1,1,1))
          ALLOCATE(K_1P2M(1,1,1),K_1P3P(1,1,1))
          ALLOCATE(K_1M2P(1,1,1),K_1M3M(1,1,1))
          ALLOCATE(K_1M2M(1,1,1),K_1M3P(1,1,1))
          ALLOCATE(TB_V14(1))
          ALLOCATE(TB_V24(1,1,1),TB_V34(1,1,1))
          ALLOCATE(TB_TPM(1,1,1),TB_TMP(1,1,1))
          ALLOCATE(TB_FAC(1,1,1))
          ALLOCATE(IDCONF(1,1))
        ENDIF	
!.......END NON LINEAR INTERACTIONS
!GM Fin
!
!.......RELATIVE SPECTRUM ->  ABSOLUTE SPECTRUM (TRANSF)
        IF (COUSTA .OR. MAREE.OR.NAMECODE(1:7).EQ.'TELEMAC') THEN
          CALL BIEF_ALLVEC(2,SITR11,'SITR11',NPOIN2,1,0,MESH)
          CALL BIEF_ALLVEC(2,SITR12,'SITR12',NPOIN2,1,0,MESH)
          CALL BIEF_ALLVEC(2,SITR13,'SITR13',NPOIN2,1,0,MESH)
        ELSE
          CALL BIEF_ALLVEC(2,SITR11,'SITR11', 1, 1, 0 ,MESH)
          CALL BIEF_ALLVEC(2,SITR12,'SITR12', 1, 1, 0 ,MESH)
          CALL BIEF_ALLVEC(2,SITR13,'SITR13', 1, 1, 0 ,MESH)
        ENDIF
        ITR11   => SITR11%I
        ITR12   => SITR12%I
        ITR13   => SITR13%I
!
        CALL BIEF_ALLVEC(2,SKNI  ,'SKNI  ', 1, 1, 0,MESH )
        CALL BIEF_ALLVEC(2,SKNOGL,'SKNOGL', 1, 1, 0,MESH )
        CALL BIEF_ALLVEC(2,SELI  ,'SELI  ', 1, 1, 0,MESH )
        CALL BIEF_ALLVEC(2,SKELGL,'SKELGL', 1, 1, 0,MESH )
        KNI   => SKNI%I
        KNOGL => SKNOGL%I
        ELI   => SELI%I
        KELGL => SKELGL%I
!
!***********************************************************************
!
! CHECKS AND WRITES OUT
!
      IF (LNG.EQ.1) WRITE(LU,22)
      IF (LNG.EQ.2) WRITE(LU,23)
   22 FORMAT(1X,///,21X,'****************************************',/,
     &21X,              '* FIN DE L''ALLOCATION DE LA MEMOIRE  : *',/,
     &21X,              '****************************************',/)
   23 FORMAT(1X,///,21X,'*************************************',/,
     &21X,              '*    END OF MEMORY ORGANIZATION:    *',/,
     &21X,              '*************************************',/)
!
      RETURN
      END
