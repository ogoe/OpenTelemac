
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ADVECTION OF A VARIABLE WITH AN UPWIND FINITE
!>                VOLUME SCHEME.
!><br>           (THE ADVECTION IS DONE EDGE BY EDGE, WHICH ENABLES
!>                LOCAL DEPTHS EQUAL TO ZERO).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  HERE FLUXES IN FLODEL ARE FROM POINT 2 TO POINT 1.
!><br>        SEE FLUX3D (HORIZONTAL FLUXES BASED ON FLUINT)
!>            AND PRECON (VERTICAL FLUXES BASED ON WSCONV)

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
!> </td><td> 19/04/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C
C#######################################################################
C
                        SUBROUTINE MURD3D_POS
     &(FC,FN,VOLU,SVOLU,VOLUN,SVOLUN,VOLU2,SVOLU2,RMASS,
     & TRA01,TRA02,TRA03,STRA01,STRA02,STRA03,MESH2,MESH3,
     & NELEM3,NPOIN3,DT,SCHCF,MSK,MASKEL,INFOR,CALFLU,FLUX,FLUEXT,
     & S0F,NSCE,SOURCES,FSCE,RAIN,PLUIE,NPOIN2,
     & OPTBAN,FLODEL,FLOPAR,GLOSEG,DIMGLO,NSEG,NPLAN,
     & T5,FLUX_REMOVED,SAVED_VOLU2,SAVED_F,OPTION)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CALFLU         |-->| INDIQUE SI ON CALCULE LE FLUX POUR LE BILAN
C| DA,XA          |-->| MATRICE MURD NON SYMETRIQUE OPTION N
C| DB,XB          |<->| MATRICE MURD NON SYMETRIQUE OPTION N
C|                |   | EVENTUELLEMENT TRANSFORME EN OPTION PSI
C| DIMGLO         |-->| FIRST DIMENSION OF ARRAY GLOSEG 
C| DT             |-->| PAS DE TEMPS
C| FC             |<--| VARIABLE APRES CONVECTION
C| FLODEL         |-->| FLUXES BY SEGMENT
C| FLOPAR         |-->| FLUXES BY SEGMENT, ASSEMBLED IN PARALLEL
C| FLUEXT         |-->| FLUX EXTERIEUR PAR NOEUD
C| FLUX           |<->| FLUX GLOBAL A INCREMENTER
C| FN             |-->| VARIABLE AU TEMPS N
C| FSCE           |-->| DIRICHLET BOUNDARY CONDITIONS OF F 
C| GLOSEG         |-->| GLOBAL NUMBERS OF POINTS, PER SEGMENT 
C| INFOR          |-->| INFORMATIONS SUR LES SOLVEURS
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MESH2          |-->| 2D MESH 
C| MESH3          |-->| 3D MESH 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
C| NELEM3         |-->| NOMBRE D'ELEMENTS 3D
C| NPLAN          |---| 
C| NPOIN2         |-->| NUMBER OF POINTS IN 2D
C| NPOIN3         |-->| NOMBRE DE POINTS 3D
C| NSCE           |---| 
C| NSEG           |---| 
C| OPTBAN         |---| 
C| PLUIE          |-->| RAIN IN M/S MULTIPLIED BY VOLU2D
C| RAIN           |-->| IF YES, THERE IS RAIN OR EVAPORATION
C| RMASS          |---| 
C| S0F            |-->| TERME SOURCE EXPLICITE
C| SCHCF          |-->| SCHEMA DE CONVECTION DE F
C| SOURCES        |---| 
C| STRA01         |---| 
C| STRA02         |---| 
C| STRA03         |---| 
C| SVOLU          |---| 
C| SVOLU2         |---| 
C| SVOLUN         |---| 
C| TRA01          |<->| TABLEAU DE TRAVAIL DE DIMENSION NPOIN3
C|                |   | EQUIVALENT DE VOLU2 POUR LE TEMPS FINAL COURANT
C| TRA02          |<->| TABLEAU DE TRAVAIL DE DIMENSION NPOIN3
C| TRA03          |<->| TABLEAU DE TRAVAIL DE DIMENSION NPOIN3 
C| VOLU           |-->| VOLUME DE CONTROLE A L'INSTANT N+1
C| VOLU2          |-->| COMME VOLU MAIS ASSEMBLE EN PARALLELISME
C| VOLUN          |-->| VOLUME DE CONTROLE A L'INSTANT N
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: SCHCF,NELEM3,NPOIN3,NPOIN2
      INTEGER, INTENT(IN)             :: NSCE,OPTBAN,NSEG,NPLAN,DIMGLO
      INTEGER, INTENT(IN)             :: GLOSEG(DIMGLO,2),OPTION
!
      DOUBLE PRECISION, INTENT(INOUT) :: FC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: FN(NPOIN3),PLUIE(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELEM3)
      DOUBLE PRECISION, INTENT(IN), TARGET    :: FLUEXT(NPOIN3)
!
      DOUBLE PRECISION, INTENT(IN)    :: VOLUN(NPOIN3), VOLU(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: VOLU2(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN3),FLUX
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT), TARGET :: TRA03(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: DT,FSCE(NSCE)
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SVOLU2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLUX_REMOVED
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SAVED_VOLU2,SAVED_F,T5
      TYPE(BIEF_OBJ), INTENT(IN)      :: SOURCES,S0F,SVOLU,SVOLUN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: STRA01,STRA02,STRA03
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2,MESH3
!
C     DIMENSION OF FLODEL AND FLOPAR=NSEG2D*NPLAN+NPOIN2*NETAGE
      DOUBLE PRECISION, INTENT(IN)    :: FLODEL(*),FLOPAR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: RMASS(*)
C                                        SIZE IN MEMORY = 30*NELEM
C                                        THIS IS ENOUGH
!
      LOGICAL, INTENT(IN)             :: MSK,INFOR,CALFLU,RAIN
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPOIN,NITER,IS,IIS,I,NSEGH,NSEGV,OPT,IR
      INTEGER I1,I2,IPLAN,ISEG3D,I2D,I3D,IPTFR
      INTEGER REMAIN_SEG,NEWREMAIN,REMAIN_TOT,REMAIN_SEG_INIT
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION P_DSUM,P_DMIN,P_DMAX
      EXTERNAL         P_DSUM,P_DMIN,P_DMAX
      INTEGER  P_ISUM
      EXTERNAL P_ISUM
!
      DOUBLE PRECISION RINIT,C,NEWVOL,RFLUX,RFLUX_OLD
      DOUBLE PRECISION VOLSEG1,VOLSEG2
!
      DOUBLE PRECISION EPS
      DATA EPS /1.D-6/
      DOUBLE PRECISION ALLOW
      DATA ALLOW /1.D-5/
      DOUBLE PRECISION REDUC
      DATA REDUC /1.D-9/
      DOUBLE PRECISION EPS_VOLUME
      DATA EPS_VOLUME /1.D-8/
      INTEGER NITMAX
      DATA NITMAX /100/
!
      LOGICAL TESTING
      DATA    TESTING/.FALSE./
C
C-----------------------------------------------------------------------
C
C     INDIC WILL BE A LIST OF SEGMENTS WITH NON ZERO FLUXES
C
      LOGICAL DEJA
      DATA DEJA/.FALSE./
      INTEGER, ALLOCATABLE :: INDIC(:)
      SAVE
!
      CALL CPSTVC(SVOLU2,STRA01)
      CALL CPSTVC(SVOLU2,STRA02)
      CALL CPSTVC(SVOLU2,STRA03)
!
!     TRA03 WILL BE THE SHARED ASSEMBLED FLUEXT IN PARALLEL
!
      IF(NCSIZE.GT.1) THEN
        CALL OV('X=Y     ',TRA03,FLUEXT,FLUEXT,0.D0,NPOIN3)
C       TRA03 WILL BE THE ASSEMBLED AND SHARED FLUEXT
        CALL PARCOM(STRA03,2,MESH3)
C       SHARES AFTER SUMMING (AS WILL BEEN DONE WITH VOLUMES)
        DO IPTFR=1,NPTIR
          I2D=MESH2%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          DO IPLAN=1,NPLAN
            I3D=I2D+(IPLAN-1)*NPOIN2
            TRA03(I3D)=TRA03(I3D)*MESH3%FAC%R(I3D)
          ENDDO
        ENDDO
      ENDIF
!
      NSEGH=NSEG*NPLAN
      NSEGV=NPOIN2*(NPLAN-1)
      IF(SCHCF.EQ.ADV_LPO_TF) THEN
C       HORIZONTAL AND VERTICAL SEGMENTS
        REMAIN_SEG=NSEGH+NSEGV
        OPT=1
      ELSEIF(SCHCF.EQ.ADV_NSC_TF) THEN
C       ALL SEGMENTS
        REMAIN_SEG=NSEGH+NSEGV+2*NSEG*(NPLAN-1)
        OPT=2
      ELSE
        WRITE(LU,*) 'UNKNOWN SCHEME IN MURD3D_POS:',SCHCF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      REMAIN_SEG_INIT=REMAIN_SEG
!
      IF(.NOT.DEJA) THEN
        ALLOCATE(INDIC(REMAIN_SEG))
        DEJA=.TRUE.
      ENDIF
!
      IF(OPTION.EQ.2) CALL CPSTVC(SVOLU2,FLUX_REMOVED)
!
!***********************************************************************
!
!     COPIES FLUXES FROM FLODEL TO ARRAY RMASS (REMAINING MASSES
!     TO BE TRANSFERRED AND THEIR ADDRESS IN INDIC)
!
      IF(OPTION.EQ.1) THEN
        DO I=1,REMAIN_SEG
          INDIC(I)=I
          RMASS(I)=DT*FLOPAR(I)
        ENDDO
      ELSEIF(OPTION.EQ.2) THEN
        DO I=1,REMAIN_SEG
          INDIC(I)=I
          RMASS(I)=-DT*FLOPAR(I)
        ENDDO
      ENDIF
C
C     SHARES ASSEMBLED FLUXES ON INTERFACE SEGMENTS BY:
C     DIVIDING BY 2 ON INTERFACE HORIZONTAL AND CROSSED SEGMENTS
C     MULTIPLYING BY FAC ON VERTICAL FLUXES
C     THIS WILL GIVE THE SAME UPWINDING INFORMATION
C
      IF(NCSIZE.GT.1) THEN
        CALL SHARE_3D_FLUXES(RMASS,1.D0,NPLAN,MESH2,MESH3,OPT)
      ENDIF
C
C     REMAINING FLUXES (SPLIT INTO POSITIVE AND NEGATIVE
C                       SO THAT THEY SUM CORRECTLY IN PARALLEL
C                       ABSOLUTE VALUES WOULD NOT SUM CORRECTLY)
C
      RFLUX_OLD=0.D0
      DO I=1,REMAIN_SEG
        RFLUX_OLD=RFLUX_OLD+ABS(RMASS(I))
      ENDDO
      IF(NCSIZE.GT.1) RFLUX_OLD=P_DSUM(RFLUX_OLD)
      RINIT=RFLUX_OLD
      IF(TESTING) WRITE(LU,*) 'SOMME INITIALE DES ABS(FLUX)=',RINIT/DT
!
!     INITIAL VALUE OF TRACER = FN
!
      CALL OV ('X=Y     ',FC,FN,FN,C,NPOIN3)
!
!     VOLU2 WILL BE THE VOLUME CHANGING PROGRESSIVELY FROM VOLUN TO VOLU
!
      CALL OS ('X=Y     ',X=SVOLU2,Y=SVOLUN)
!
!     TAKES INTO ACCOUNT ENTERING EXTERNAL FLUXES
!     THIS IS DONE WITHOUT CHANGING THE TRACER
!    (BUT THE MASS OF TRACER CHANGES)
!
      IF(NCSIZE.GT.1) THEN
        DO I=1,NPOIN3
C         FLUEXT SHARED IN PARALLEL (HENCE SAME SIGN)
          VOLU2(I)=VOLU2(I)-MIN(TRA03(I),0.D0)*DT
        ENDDO
      ELSE
        DO I=1,NPOIN3
C         FLUEXT SHARED IN PARALLEL (HENCE SAME SIGN)
          VOLU2(I)=VOLU2(I)-MIN(FLUEXT(I),0.D0)*DT
        ENDDO
      ENDIF
!
      IF(CALFLU) THEN
        IF(NCSIZE.GT.1) THEN
          DO IPOIN = 1,NPOIN3
            FLUX = FLUX + DT*FC(IPOIN)*MIN(TRA03(IPOIN),0.D0)
          ENDDO
        ELSE
          DO IPOIN = 1,NPOIN3
            FLUX = FLUX + DT*FC(IPOIN)*MIN(FLUEXT(IPOIN),0.D0)
          ENDDO
        ENDIF
C       ENTERING SOURCES
        IF(NSCE.GT.0) THEN
          DO IS=1,NSCE
            IIS=IS
C           HERE IN PARALLEL SOURCES WITHOUT PARCOM
C           ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
            IF(NCSIZE.GT.1) IIS=IIS+NSCE
            DO IPOIN=1,NPOIN3
              IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
                FLUX=FLUX-DT*FSCE(IS)*SOURCES%ADR(IIS)%P%R(IPOIN)
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1) CALL PARCOM(SVOLU2,2,MESH3)
!
C     ENTERING SOURCES (WITH FC = FSCE)
C     IT WILL ALWAYS GIVE POSITIVE VOLUMES
      IF(NSCE.GT.0) THEN
        DO IS=1,NSCE
          DO IPOIN=1,NPOIN3
C           HERE VERSION OF SOURCES ASSEMBLED IN PARALLEL
            IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
              VOLU2(IPOIN)=VOLU2(IPOIN)+DT*SOURCES%ADR(IS)%P%R(IPOIN)
              FC(IPOIN)=FC(IPOIN)+DT*(FSCE(IS)-FC(IPOIN))
     &                       *SOURCES%ADR(IS)%P%R(IPOIN)/VOLU2(IPOIN)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
C     RAIN-EVAPORATION (HERE ONLY RAIN, NOT EVAPORATION)
!
      IF(RAIN) THEN
        DO IPOIN=1,NPOIN2
          IF(PLUIE(IPOIN).GT.0.D0) THEN
            IS=NPOIN3-NPOIN2+IPOIN
C           ASSEMBLED FORM OF PLUIE NEEDED HERE
            VOLU2(IS)=VOLU2(IS)+DT*PLUIE(IPOIN)
            FC(IS)=FC(IS)-DT*FC(IS)*PLUIE(IPOIN)/VOLU2(IS)
          ENDIF
        ENDDO
      ENDIF
!
      NITER = 0
!
777   CONTINUE
!
      NITER = NITER + 1
!
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     FOR DISTRIBUTING THE VOLUMES BETWEEN SEGMENTS
C
      IF(OPTION.EQ.2) THEN
C
C       FLUX_REMOVED (T6)    : TOTAL FLUX REMOVED OF EACH POINT 
C       SAVED_VOLU2 (T8)     : VOLUME VOLU2 SAVED
C       SAVED_F (T9)         : TRACER SAVED
C
        IF(NITER.EQ.1) THEN
          DO I=1,NPOIN3
            FLUX_REMOVED%R(I)=0.D0
            SAVED_VOLU2%R(I)=VOLU2(I)
            SAVED_F%R(I)=FC(I)
            T5%R(I)=FC(I)*VOLU2(I)
          ENDDO
          IF(NCSIZE.GT.1) THEN
C           SHARES AFTER SUMMING (AS HAS BEEN DONE WITH FLUXES)
            DO IPTFR=1,NPTIR
              I2D=MESH2%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
              DO IPLAN=1,NPLAN
                I3D=I2D+(IPLAN-1)*NPOIN2
                VOLU2(I3D)=VOLU2(I3D)*MESH3%FAC%R(I3D)   
                T5%R(I3D) = T5%R(I3D)*MESH3%FAC%R(I3D)
              ENDDO
            ENDDO
          ENDIF
        ELSE
C         NOT ALL THE POINTS NEED TO BE INITIALISED NOW
          DO IR=1,REMAIN_SEG
            I=INDIC(IR)
            I1=GLOSEG(I,1)
            I2=GLOSEG(I,2)
            FLUX_REMOVED%R(I1)=0.D0
            FLUX_REMOVED%R(I2)=0.D0
C           SAVING THE DEPTH AND TRACER
            SAVED_VOLU2%R(I1)=VOLU2(I1)
            SAVED_VOLU2%R(I2)=VOLU2(I2)
            SAVED_F%R(I1)=FC(I1)
            SAVED_F%R(I2)=FC(I2)
            T5%R(I1)=FC(I1)*VOLU2(I1)
            T5%R(I2)=FC(I2)*VOLU2(I2)
          ENDDO
C         CANCELLING INTERFACE POINTS (SOME MAY BE ISOLATED IN A SUBDOMAIN
C         AT THE TIP OF AN ACTIVE SEGMENT WHICH IS ELSEWHERE)   
          IF(NCSIZE.GT.1) THEN
            DO IPTFR=1,NPTIR
              I2D=MESH3%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
              DO IPLAN=1,NPLAN
                I3D=(IPLAN-1)*NPOIN2+I2D
                FLUX_REMOVED%R(I3D)=0.D0
C               SAVING THE VOLUME AND TRACER
                SAVED_VOLU2%R(I3D)=VOLU2(I3D)
                SAVED_F%R(I3D)=FC(I3D)
                VOLU2(I3D)=VOLU2(I3D)*MESH3%FAC%R(I3D)
                T5%R(I3D) = T5%R(I3D)*MESH3%FAC%R(I3D)
              ENDDO
            ENDDO
          ENDIF 
        ENDIF
        DO I=1,REMAIN_SEG
          ISEG3D=INDIC(I)
          I1=GLOSEG(ISEG3D,1)
          I2=GLOSEG(ISEG3D,2)
!         POSITIVE FLUXES FROM 1 TO 2 !!! 
          IF(RMASS(ISEG3D).GT.EPS_VOLUME) THEN
            FLUX_REMOVED%R(I1)=FLUX_REMOVED%R(I1)+RMASS(ISEG3D)
            VOLU2(I1)=0.D0
            T5%R(I1)=0.D0
          ELSEIF(RMASS(ISEG3D).LT.-EPS_VOLUME) THEN
            FLUX_REMOVED%R(I2)=FLUX_REMOVED%R(I2)-RMASS(ISEG3D)
            VOLU2(I2)=0.D0
            T5%R(I2)=0.D0
          ENDIF
        ENDDO
!
        IF(NCSIZE.GT.1) CALL PARCOM(FLUX_REMOVED,2,MESH3)
!
C       FOR ISOLATED POINTS CONNECTED TO AN ACTIVE SEGMENT
C       THAT IS IN ANOTHER SUBDOMAIN     
        IF(NCSIZE.GT.1) THEN
          DO IPTFR=1,NPTIR
            I2D=MESH3%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
            DO IPLAN=1,NPLAN
              I3D=(IPLAN-1)*NPOIN2+I2D
              IF(FLUX_REMOVED%R(I3D).GT.EPS_VOLUME) THEN
!               ALL VOLUME SHARED
                VOLU2(I3D)=0.D0
                T5%R(I3D)=0.D0
              ENDIF
            ENDDO
          ENDDO
        ENDIF 
!
      ELSEIF(OPTION.EQ.1) THEN
!
        IF(NCSIZE.GT.1) THEN
C         SHARES AFTER SUMMING (AS HAS BEEN DONE WITH FLUXES)
          DO IPTFR=1,NPTIR
            I2D=MESH2%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
            DO IPLAN=1,NPLAN
              I3D=I2D+(IPLAN-1)*NPOIN2
              VOLU2(I3D)=VOLU2(I3D)*MESH3%FAC%R(I3D)
            ENDDO
          ENDDO
        ENDIF
!
      ENDIF
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
!
!     FROM HERE RMASS IS THE REMAINING MASSES TO BE PASSED BETWEEN POINTS
!
!     COMPUTES THE NEW VOLUME WITH FV SCHEME + DB
!
!     HORIZONTAL AND VERTICAL FLUXES TREATED TOGETHER
!
      RFLUX=0.D0
      NEWREMAIN=0
C
      IF(OPTION.EQ.1) THEN
C
      DO I=1,REMAIN_SEG
        ISEG3D=INDIC(I)
        IF(RMASS(ISEG3D).GT.EPS_VOLUME) THEN
C         FLUX FROM 2 TO 1 !!! (SEE REMARKS AND HOW RMASS INITIALISED)
          I1=GLOSEG(ISEG3D,1)
          I2=GLOSEG(ISEG3D,2)
          IF(RMASS(ISEG3D).GT.VOLU2(I2)) THEN
            RMASS(ISEG3D)=RMASS(ISEG3D)-VOLU2(I2)
            NEWVOL=VOLU2(I1)+VOLU2(I2)
            IF(NEWVOL.GT.0.D0) THEN
              FC(I1)=FC(I1)+VOLU2(I2)*(FC(I2)-FC(I1))/NEWVOL
              VOLU2(I1)=NEWVOL
              VOLU2(I2)=0.D0
            ENDIF
            RFLUX=RFLUX+RMASS(ISEG3D)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=ISEG3D
          ELSE
            NEWVOL=VOLU2(I1)+RMASS(ISEG3D)
            IF(NEWVOL.GT.0.D0) THEN
              FC(I1)=FC(I1)+RMASS(ISEG3D)*(FC(I2)-FC(I1))/NEWVOL
            ENDIF
            VOLU2(I1)=NEWVOL
            VOLU2(I2)=VOLU2(I2)-RMASS(ISEG3D)
          ENDIF
        ELSEIF(RMASS(ISEG3D).LT.-EPS_VOLUME) THEN
C         FLUX FROM 1 TO 2 !!! (SEE REMARKS AND HOW RMASS INITIALISED)
          I1=GLOSEG(ISEG3D,1)
          I2=GLOSEG(ISEG3D,2)
          IF(-RMASS(ISEG3D).GT.VOLU2(I1)) THEN
            RMASS(ISEG3D)=RMASS(ISEG3D)+VOLU2(I1)
            NEWVOL=VOLU2(I2)+VOLU2(I1)
            IF(NEWVOL.GT.0.D0) THEN
              FC(I2)=FC(I2)+VOLU2(I1)*(FC(I1)-FC(I2))/NEWVOL
              VOLU2(I2)=NEWVOL
              VOLU2(I1)=0.D0
            ENDIF
            RFLUX=RFLUX-RMASS(ISEG3D)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=ISEG3D
          ELSE
            NEWVOL=VOLU2(I2)-RMASS(ISEG3D)
            IF(NEWVOL.GT.0.D0) THEN
              FC(I2)=FC(I2)-RMASS(ISEG3D)*(FC(I1)-FC(I2))/NEWVOL
            ENDIF
            VOLU2(I2)=NEWVOL
            VOLU2(I1)=VOLU2(I1)+RMASS(ISEG3D)
          ENDIF
        ENDIF
      ENDDO
C
      ELSEIF(OPTION.EQ.2) THEN
C
      DO IR=1,REMAIN_SEG
        I=INDIC(IR)
        IF(RMASS(I).GT.EPS_VOLUME) THEN
          I1=GLOSEG(I,1)
C         FLUX FROM 1 TO 2 !!! (SEE REMARKS AND HOW RMASS INITIALISED)
          IF(SAVED_VOLU2%R(I1).GT.0.D0) THEN
            I2=GLOSEG(I,2)
!           SHARING ON DEMAND: RMASS(I)/FLUX_REMOVED%R(I1) IS A PERCENTAGE 
            VOLSEG1=SAVED_VOLU2%R(I1)*RMASS(I)/FLUX_REMOVED%R(I1)
!           END OF SHARING ON DEMAND
            IF(RMASS(I).GE.VOLSEG1) THEN
!             ALL VOLSEG1 WILL BE TRANSFERED TO POINT2
!             VOLSEG1 > 0, HENCE VOLU2(I2) ALSO
              RMASS(I) =RMASS(I) -VOLSEG1
              VOLU2(I2)=VOLU2(I2)+VOLSEG1
!             GROUPING H*F 
              T5%R(I2)=T5%R(I2)+VOLSEG1*SAVED_F%R(I1)
!             THIS MAY BE DONE SEVERAL TIMES FOR THE SAME POINT
!             BUT THE LAST ONE WILL BE THE GOOD ONE
              FC(I2)=T5%R(I2)/VOLU2(I2)
              RFLUX=RFLUX+RMASS(I)
              NEWREMAIN=NEWREMAIN+1
              INDIC(NEWREMAIN)=I
            ELSE
              VOLSEG1=VOLSEG1-RMASS(I)
!             GATHERING VOLUMES (HERE VOLU2(I2) WILL REMAIN POSITIVE)
              VOLU2(I2)=VOLU2(I2)+RMASS(I)
              VOLU2(I1)=VOLU2(I1)+VOLSEG1
              T5%R(I1)=T5%R(I1)+VOLSEG1*SAVED_F%R(I1)
              T5%R(I2)=T5%R(I2)+RMASS(I)*SAVED_F%R(I1)
              FC(I1)=T5%R(I1)/VOLU2(I1)
              FC(I2)=T5%R(I2)/VOLU2(I2)
            ENDIF
          ELSE
            RFLUX=RFLUX+RMASS(I)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I
          ENDIF
        ELSEIF(RMASS(I).LT.-EPS_VOLUME) THEN
          I2=GLOSEG(I,2)
C         FLUX FROM 2 TO 1 !!! (SEE REMARKS AND HOW RMASS INITIALISED)
          IF(SAVED_VOLU2%R(I2).GT.0.D0) THEN
            I1=GLOSEG(I,1)
!           SHARING ON DEMAND
            VOLSEG2=-SAVED_VOLU2%R(I2)*RMASS(I)/FLUX_REMOVED%R(I2)
!           END OF SHARING ON DEMAND
            IF(-RMASS(I).GE.VOLSEG2) THEN
!             ALL VOLSEG2 WILL BE TRANSFERED TO POINT 1
!             VOLSEG2 > 0, HENCE VOLU2(I1) ALSO
              VOLU2(I1)=VOLU2(I1)+VOLSEG2
              RMASS(I) =RMASS(I) +VOLSEG2
              T5%R(I1)=T5%R(I1)+VOLSEG2*SAVED_F%R(I2)
              FC(I1)=T5%R(I1)/VOLU2(I1)   
              RFLUX=RFLUX-RMASS(I)     
              NEWREMAIN=NEWREMAIN+1
              INDIC(NEWREMAIN)=I       
            ELSE
              VOLSEG2=VOLSEG2+RMASS(I)
!             GATHERING VOLUMES (HERE VOLU2(I1) WILL REMAIN POSITIVE)
              VOLU2(I1)=VOLU2(I1)-RMASS(I)
              VOLU2(I2)=VOLU2(I2)+VOLSEG2 
              T5%R(I1)=T5%R(I1)-RMASS(I)*SAVED_F%R(I2)           
              T5%R(I2)=T5%R(I2)+VOLSEG2*SAVED_F%R(I2) 
              FC(I1)=T5%R(I1)/VOLU2(I1)     
              FC(I2)=T5%R(I2)/VOLU2(I2) 
            ENDIF
          ELSE
            RFLUX=RFLUX-RMASS(I)     
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I    
          ENDIF
        ENDIF
      ENDDO
C
C     ELSE
C       UNKNOWN OPTION
      ENDIF
!
      REMAIN_SEG=NEWREMAIN
!
!     MERGES VOLUMES AND FC AT INTERFACE POINTS
!
      IF(NCSIZE.GT.1) THEN
        DO IPTFR=1,NPTIR
          I2D=MESH2%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          DO IPLAN=1,NPLAN
            I3D=I2D+(IPLAN-1)*NPOIN2
C           ARRAY WITH VOLUME*FC AT INTERFACE POINTS
            TRA01(I3D)=VOLU2(I3D)*FC(I3D)
          ENDDO
        ENDDO
C       SUMS VOLUME*FC AT INTERFACE POINTS
        CALL PARCOM(STRA01,2,MESH3)
C       SUMS THE NEW POSITIVE PARTIAL VOLUMES OF INTERFACE POINTS
        CALL PARCOM(SVOLU2,2,MESH3)
        DO IPTFR=1,NPTIR
          I2D=MESH2%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          DO IPLAN=1,NPLAN
            I3D=I2D+(IPLAN-1)*NPOIN2
C           ARRAY WITH VOLUME*F AT INTERFACE POINTS
            IF(VOLU2(I3D).GT.0.D0) THEN
              FC(I3D)=TRA01(I3D)/VOLU2(I3D)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
      IF(TESTING) WRITE(LU,*) 'FLUX NON PRIS EN COMPTE=',C
!
      REMAIN_TOT=REMAIN_SEG
      IF(NCSIZE.GT.1) THEN
        RFLUX=P_DSUM(RFLUX)
!       WILL NOT SUM CORRECTLY IN PARALLEL, BUT ONLY TEST IF .EQ.0
        REMAIN_TOT=P_ISUM(REMAIN_TOT)
      ENDIF
!
!     4 POSSIBLE REASONS FOR STOPPING:
!
!     1) THERE IS NO REMAINING FLUX
!     2) REMAINING FLUXES DO NOT CHANGE (POSITIVE AND NEGATIVE)
!        WITH SOME ABSOLUTE ALLOWANCE OR REDUCTION COEFFICIENT
!     3) ALL SEGMENTS HAVE BEEN TREATED
!     4) MAXIMUM NUMBER OF ITERATIONS IS REACHED
!
      IF( RFLUX.NE.0.D0                                   .AND.
     &    ABS(RFLUX-RFLUX_OLD).GT.MIN(RINIT*REDUC,ALLOW)  .AND.
     &               REMAIN_TOT.NE.0                      .AND.
     &               NITER.LT.NITMAX                   ) THEN
        RFLUX_OLD=RFLUX
        GO TO 777
      ENDIF
!
!     TAKES INTO ACCOUNT EXITING EXTERNAL FLUXES
!     THIS IS DONE WITHOUT CHANGING THE TRACER
!    (BUT THE MASS OF TRACER CHANGES)
!
      IF(CALFLU) THEN
C       EXITING FLUXES
        IF(NCSIZE.GT.1) THEN
          DO IPOIN = 1,NPOIN3
            FLUX = FLUX + DT*FC(IPOIN)*MAX(TRA03(IPOIN),0.D0)
          ENDDO
        ELSE
          DO IPOIN = 1,NPOIN3
            FLUX = FLUX + DT*FC(IPOIN)*MAX(FLUEXT(IPOIN),0.D0)
          ENDDO
        ENDIF
C       EXITING SOURCES
        IF(NSCE.GT.0) THEN
          DO IS=1,NSCE
            IIS=IS
C           HERE IN PARALLEL SOURCES WITHOUT PARCOM
C           ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
            IF(NCSIZE.GT.1) IIS=IIS+NSCE
            DO IPOIN=1,NPOIN3
              IF(SOURCES%ADR(IS)%P%R(IPOIN).LT.0.D0) THEN
                FLUX=FLUX
     &              -DT*FC(IPOIN)*SOURCES%ADR(IIS)%P%R(IPOIN)
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
C     RAIN-EVAPORATION (HERE ONLY EVAPORATION, NOT RAIN)
!
      IF(RAIN) THEN
        DO IPOIN=1,NPOIN2
          IF(PLUIE(IPOIN).LT.0.D0) THEN
            IS=NPOIN3-NPOIN2+IPOIN
            VOLU2(IS)=VOLU2(IS)+DT*PLUIE(IPOIN)
C           DIVISION BY 0 NOT CHECKED
            FC(IS)=FC(IS)-DT*FC(IS)*PLUIE(IPOIN)/VOLU2(IS)
          ENDIF
        ENDDO
      ENDIF
!
      IF(TESTING) THEN
C       EXITING SOURCES (WITH FC UNCHANGED)
        IF(NSCE.GT.0) THEN
          DO IS=1,NSCE
            DO IPOIN=1,NPOIN3
C             HERE VERSION OF SOURCES ASSEMBLED IN PARALLEL
              IF(SOURCES%ADR(IS)%P%R(IPOIN).LT.0.D0) THEN
                VOLU2(IPOIN)=VOLU2(IPOIN)+
     &                      DT*SOURCES%ADR(IS)%P%R(IPOIN)
              ENDIF
            ENDDO
          ENDDO
        ENDIF
C       EXITING FLUXES
        IF(NCSIZE.GT.1) THEN
          DO I=1,NPOIN3
            TRA02(I)=MAX(TRA03(I),0.D0)
C           SHARED FLUEXT IN TRA03 ERASED NOW (NOT USED AFTER)
            TRA03(I)=VOLU(I)
          ENDDO
        ELSE
          DO I=1,NPOIN3
            TRA02(I)=MAX(FLUEXT(I),0.D0)
            TRA03(I)=VOLU(I)
          ENDDO
        ENDIF
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(STRA02,2,MESH3)
          CALL PARCOM(STRA03,2,MESH3)
        ENDIF
        DO I=1,NPOIN3
          VOLU2(I)=VOLU2(I)-TRA02(I)*DT
          IF(VOLU2(I).LT.0.D0) THEN
            WRITE(LU,*) 'POINT ',I,' VOLU2=',VOLU2(I)
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
C       CHECKS EQUALITY OF ASSEMBLED VOLUMES
        C=0.D0
        IF(NCSIZE.GT.1) THEN
          DO I=1,NPOIN3
            C = C+ABS(VOLU2(I)-TRA03(I))*MESH3%FAC%R(I)
          ENDDO
          C=P_DSUM(C)
        ELSE
          DO I=1,NPOIN3
            C=C+ABS(VOLU2(I)-TRA03(I))
          ENDDO
        ENDIF
        WRITE(LU,*) 'ERROR ON VOLUMES = ',C
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EXPLICIT SOURCE TERMS
!
      IF(S0F%TYPR.NE.'0') THEN
        DO IPOIN=1,NPOIN3
          IF(VOLU2(IPOIN).GT.EPS) THEN
            FC(IPOIN)=FC(IPOIN)+DT*S0F%R(IPOIN)/VOLU2(IPOIN)
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(INFOR) THEN
        IF(LNG.EQ.1) WRITE(LU,101) SCHCF,NITER
        IF(LNG.EQ.2) WRITE(LU,102) SCHCF,NITER
      ENDIF
!
101   FORMAT(1X,'MURD3D_POS OPTION: ',1I4,'  ',1I4,' ITERATIONS')
102   FORMAT(1X,'MURD3D_POS OPTION: ',1I4,'  ',1I4,' ITERATIONS')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C
