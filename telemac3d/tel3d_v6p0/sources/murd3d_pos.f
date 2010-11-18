C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ADVECTION OF A VARIABLE WITH AN UPWIND FINITE
!>                VOLUME SCHEME.
!><br>           (THE ADVECTION IS DONE EDGE BY EDGE, WHICH ENABLES
!>                LOCAL DEPTHS EQUAL TO ZERO).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  HERE FLUXES IN FLODEL ARE FROM POINT 2 TO POINT 1.
!><br>        SEE FLUX3D (HORIZONTAL FLUXES BASED ON FLUINT)
!>            AND PRECON (VERTICAL FLUXES BASED ON WSCONV)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CALFLU, DIMGLO, DT, FC, FLODEL, FLOPAR, FLUEXT, FLUX, FN, FSCE, GLOSEG, INFOR, MASKEL, MASKPT, MESH2, MESH3, MSK, NELEM3, NPLAN, NPOIN2, NPOIN3, NSCE, NSEG, OPTBAN, PLUIE, RAIN, RMASS, S0F, SCHCF, SOURCES, STRA01, STRA02, STRA03, SVOLU, SVOLU2, SVOLUN, TRA01, TRA02, TRA03, VOLU, VOLU2, VOLUN
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NBMAXNSHARE NBMAXNSHARE@endlink, 
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink, 
!> @link BIEF_DEF::NPTIR NPTIR@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::ADV_LPO_TF ADV_LPO_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC_TF ADV_NSC_TF@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ALLOW, C, DEJA, EPS, EPS_VOLUME, I, I1, I2, I2D, I3, I3D, I4, ICR1, ICR2, IHOR, IIS, INDIC, IPLAN, IPOIN, IPTFR, IS, ISEG2D, ISEG3D, J, NEWREMAIN, NEWVOL, NITER, NITMAX, NSEGH, NSEGV, OPT, PERIOD_SHARE, REDUC, REMAIN_SEG, REMAIN_SEG_INIT, REMAIN_TOT, RINIT, RNEG, RNEG_OLD, RPOS, RPOS_OLD, TESTING
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), MITTIT(), OS(), OV(), PARCOM(), PLANTE(), P_DSUM(), P_ISUM(), SHARE_3D_FLUXES()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CVDF3D()

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

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CALFLU
!></td><td>--></td><td>INDIQUE SI ON CALCULE LE FLUX POUR LE BILAN
!>    </td></tr>
!>          <tr><td>DA,XA
!></td><td>--></td><td>MATRICE MURD NON SYMETRIQUE OPTION N
!>    </td></tr>
!>          <tr><td>DB,XB
!></td><td><-></td><td>MATRICE MURD NON SYMETRIQUE OPTION N
!>                  EVENTUELLEMENT TRANSFORME EN OPTION PSI
!>    </td></tr>
!>          <tr><td>DIMGLO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>FC
!></td><td><--</td><td>VARIABLE APRES CONVECTION
!>    </td></tr>
!>          <tr><td>FLODEL
!></td><td>--></td><td>FLUX PAR SEGMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>FLOPAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUEXT
!></td><td>--></td><td>FLUX EXTERIEUR PAR NOEUD
!>    </td></tr>
!>          <tr><td>FLUX
!></td><td><-></td><td>FLUX GLOBAL A INCREMENTER
!>    </td></tr>
!>          <tr><td>FN
!></td><td>--></td><td>VARIABLE AU TEMPS N
!>    </td></tr>
!>          <tr><td>FSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INFOR
!></td><td>--></td><td>INFORMATIONS SUR LES SOLVEURS
!>    </td></tr>
!>          <tr><td>LV
!></td><td>--></td><td>LONGUEUR DU VECTEUR POUR LA VECTORISATION
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>MASQUAGE DES ELEMENTS
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES
!>    </td></tr>
!>          <tr><td>NELEM3
!></td><td>--></td><td>NOMBRE D'ELEMENTS 3D
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF POINTS IN 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS 3D
!>    </td></tr>
!>          <tr><td>NSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTBAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PLUIE
!></td><td>--></td><td>RAIN IN M/S MULTIPLIED BY VOLU2D
!>    </td></tr>
!>          <tr><td>RAIN
!></td><td>--></td><td>IF YES, THERE IS RAIN OR EVAPORATION
!>    </td></tr>
!>          <tr><td>RMASS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>S0F
!></td><td>--></td><td>TERME SOURCE EXPLICITE
!>    </td></tr>
!>          <tr><td>SCHCF
!></td><td>--></td><td>SCHEMA DE CONVECTION DE F
!>    </td></tr>
!>          <tr><td>SOURCES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>STRA01
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>STRA02
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>STRA03
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SVOLU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SVOLU2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SVOLUN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td><-></td><td>TABLEAU DE TRAVAIL DE DIMENSION NPOIN3
!>                  EQUIVALENT DE VOLU2 POUR LE TEMPS FINAL COURANT
!>    </td></tr>
!>          <tr><td>TRA02,3
!></td><td><-></td><td>TABLEAUX DE TRAVAIL DE DIMENSION NPOIN3
!>    </td></tr>
!>          <tr><td>TRA03
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VOLU
!></td><td>--></td><td>VOLUME DE CONTROLE A L'INSTANT N+1
!>    </td></tr>
!>          <tr><td>VOLU2
!></td><td>--></td><td>COMME VOLU MAIS ASSEMBLE EN PARALLELISME
!>    </td></tr>
!>          <tr><td>VOLUN
!></td><td>--></td><td>VOLUME DE CONTROLE A L'INSTANT N
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MURD3D_POS
     &(FC,FN,VOLU,SVOLU,VOLUN,SVOLUN,VOLU2,SVOLU2,RMASS,
     & TRA01,TRA02,TRA03,STRA01,STRA02,STRA03,MESH2,MESH3,
     & NELEM3,NPOIN3,DT,SCHCF,MSK,MASKEL,INFOR,CALFLU,FLUX,FLUEXT,
     & S0F,NSCE,SOURCES,FSCE,RAIN,PLUIE,NPOIN2,MASKPT,
     & OPTBAN,FLODEL,FLOPAR,GLOSEG,DIMGLO,NSEG,NPLAN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CALFLU         |-->| INDIQUE SI ON CALCULE LE FLUX POUR LE BILAN
C| DA,XA          |-->| MATRICE MURD NON SYMETRIQUE OPTION N
C| DB,XB          |<->| MATRICE MURD NON SYMETRIQUE OPTION N
C|                |   | EVENTUELLEMENT TRANSFORME EN OPTION PSI
C| DIMGLO         |---| 
C| DT             |-->| PAS DE TEMPS
C| FC             |<--| VARIABLE APRES CONVECTION
C| FLODEL         |-->| FLUX PAR SEGMENTS DU MAILLAGE
C| FLOPAR         |---| 
C| FLUEXT         |-->| FLUX EXTERIEUR PAR NOEUD
C| FLUX           |<->| FLUX GLOBAL A INCREMENTER
C| FN             |-->| VARIABLE AU TEMPS N
C| FSCE           |---| 
C| GLOSEG         |---| 
C| INFOR          |-->| INFORMATIONS SUR LES SOLVEURS
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MASKPT         |---| 
C| MESH2          |---| 
C| MESH3          |---| 
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
C| TRA02,3        |<->| TABLEAUX DE TRAVAIL DE DIMENSION NPOIN3
C| TRA03          |---| 
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
      INTEGER, INTENT(IN)             :: GLOSEG(DIMGLO,2)
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
      DOUBLE PRECISION, INTENT(IN)    :: DT,FSCE(NSCE),MASKPT(NPOIN3)
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SVOLU2
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
      INTEGER IPOIN,NITER,J,IS,IIS,I,NSEGH,NSEGV,OPT,IHOR
      INTEGER I1,I2,I3,I4,IPLAN,ISEG2D,ISEG3D,I2D,I3D,IPTFR,ICR1,ICR2
      INTEGER REMAIN_SEG,NEWREMAIN,REMAIN_TOT,REMAIN_SEG_INIT
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION P_DSUM,P_DMIN,P_DMAX
      EXTERNAL         P_DSUM,P_DMIN,P_DMAX
      INTEGER  P_ISUM
      EXTERNAL P_ISUM
!
      DOUBLE PRECISION RINIT,C,NEWVOL,RPOS,RNEG,RPOS_OLD,RNEG_OLD
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
C     TO PERIODICALLY SHARE FLUXES IN PARALLEL
C     MAY SPEED-UP THE ALGORITHM. IF>NITMAX DOES NOTHING
      INTEGER PERIOD_SHARE
      DATA PERIOD_SHARE /200/
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
C     TRA03 WILL BE THE SHARED ASSEMBLED FLUEXT IN PARALLEL
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
            TRA03(I3D)=TRA03(I3D)*MESH2%FAC%R(I2D)
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
!***********************************************************************
!
      IF(INFOR) CALL MITTIT(15,0.D0,0)
!
C     COPIES FLUXES FROM FLODEL TO ARRAY RMASS (REMAINING MASSES
C     TO BE TRANSFERRED AND THEIR ADDRESS IN INDIC)
!
      DO I=1,REMAIN_SEG
        INDIC(I)=I
        RMASS(I)=DT*FLOPAR(I)
      ENDDO
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
      RPOS_OLD=0.D0
      RNEG_OLD=0.D0
      DO I=1,REMAIN_SEG
        IF(RMASS(I).GT.0.D0) THEN
          RPOS_OLD=RPOS_OLD+RMASS(I)
        ELSE
          RNEG_OLD=RNEG_OLD+RMASS(I)
        ENDIF
      ENDDO
      IF(NCSIZE.GT.1) THEN
        RPOS_OLD=P_DSUM(RPOS_OLD)
        RNEG_OLD=P_DSUM(RNEG_OLD)
      ENDIF
      RINIT=RPOS_OLD-RNEG_OLD
      IF(TESTING) THEN
        WRITE(LU,*) 'SOMME INITIALE DES ABS(FLUX)=',RINIT/DT
      ENDIF
!
C     INITIAL VALUE OF TRACER = FN
!
      CALL OV ('X=Y     ',FC,FN,FN,C,NPOIN3)
!
C     VOLU2 WILL BE THE VOLUME CHANGING PROGRESSIVELY FROM VOLUN TO VOLU
!
      CALL OS ('X=Y     ',X=SVOLU2,Y=SVOLUN)
!
C     TAKES INTO ACCOUNT ENTERING EXTERNAL FLUXES
C     THIS IS DONE WITHOUT CHANGING THE TRACER
C    (BUT THE MASS OF TRACER CHANGES)
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
      IF(NCSIZE.GT.1) THEN
C       SHARES AFTER SUMMING (AS HAS BEEN DONE WITH FLUXES)
        DO IPTFR=1,NPTIR
          I2D=MESH2%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          DO IPLAN=1,NPLAN
            I3D=I2D+(IPLAN-1)*NPOIN2
            VOLU2(I3D)=VOLU2(I3D)*MESH2%FAC%R(I2D)
C           VOLU2(I3D)=VOLU2(I3D)*MESH3%FAC%R(I3D)   SHOULD WORK AS WELL
          ENDDO
        ENDDO
!
C       SHARES AGAIN FLUXES EVERY UMPTEEN ITERATIONS IN PARALLEL
C       THIS IS NOT MANDATORY, ONLY TO SPEED UP
!
C       IF(PERIOD_SHARE*(NITER/PERIOD_SHARE).EQ.NITER) THEN
C         CALL PARCOM2_SEG(RMASS,RMASS,RMASS,NSEG,NPLAN,2,1,MESH2,OPT)
C         CALL SHARE_3D_FLUXES(RMASS,1.D0,NPLAN,MESH2,MESH3,OPT)
C         ALL FLUXES REACTIVATED
C         COULD BE OPTIMISED BUT PARCOM2+SHARE MAY REACTIVATE
C         A FLUX THAT HAD BEEN CANCELLED
C         REMAIN_SEG=REMAIN_SEG_INIT
C         DO I=1,REMAIN_SEG
C           INDIC(I)=I
C         ENDDO
C       ENDIF
!
      ENDIF
!
C     FROM HERE RMASS IS THE REMAINING MASSES TO BE PASSED BETWEEN POINTS
!
C     COMPUTES THE NEW VOLUME WITH FV SCHEME + DB
!
C     HORIZONTAL AND VERTICAL FLUXES TREATED TOGETHER
!
      RPOS=0.D0
      RNEG=0.D0
      NEWREMAIN=0
      DO I=1,REMAIN_SEG
        ISEG3D=INDIC(I)
        IF(RMASS(ISEG3D).GT.EPS_VOLUME) THEN
C         FLUX FROM 2 TO 1 (SEE REMARK IN THE HEADER)
          I1=GLOSEG(ISEG3D,1)
          I2=GLOSEG(ISEG3D,2)
          IF(RMASS(ISEG3D).GT.VOLU2(I2)) THEN
            RMASS(ISEG3D)=RMASS(ISEG3D)*(1.D0-VOLU2(I2)/RMASS(ISEG3D))
            NEWVOL=VOLU2(I1)+VOLU2(I2)
            IF(NEWVOL.GT.0.D0) THEN
              FC(I1)=FC(I1)+VOLU2(I2)*(FC(I2)-FC(I1))/NEWVOL
              VOLU2(I1)=NEWVOL
              VOLU2(I2)=0.D0
            ENDIF
            RPOS=RPOS+RMASS(ISEG3D)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=ISEG3D
          ELSE
            NEWVOL=VOLU2(I1)+RMASS(ISEG3D)
            IF(NEWVOL.GT.0.D0) THEN
              FC(I1)=FC(I1)+RMASS(ISEG3D)*(FC(I2)-FC(I1))/NEWVOL
            ENDIF
            VOLU2(I1)=NEWVOL
            VOLU2(I2)=VOLU2(I2)-RMASS(ISEG3D)
C           NECESSARY IN CASE OF FLUXES REACTIVATED IN PARALLEL MODE
C           IF PERIOD_SHARE IS USED
C           RMASS(ISEG3D)=0.D0
          ENDIF
        ELSEIF(RMASS(ISEG3D).LT.-EPS_VOLUME) THEN
C         FLUX FROM 1 TO 2 (SEE REMARK IN THE HEADER)
          I1=GLOSEG(ISEG3D,1)
          I2=GLOSEG(ISEG3D,2)
          IF(-RMASS(ISEG3D).GT.VOLU2(I1)) THEN
            RMASS(ISEG3D)=RMASS(ISEG3D)*(1.D0+VOLU2(I1)/RMASS(ISEG3D))
            NEWVOL=VOLU2(I2)+VOLU2(I1)
            IF(NEWVOL.GT.0.D0) THEN
              FC(I2)=FC(I2)+VOLU2(I1)*(FC(I1)-FC(I2))/NEWVOL
              VOLU2(I2)=NEWVOL
              VOLU2(I1)=0.D0
            ENDIF
            RNEG=RNEG+RMASS(ISEG3D)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=ISEG3D
          ELSE
            NEWVOL=VOLU2(I2)-RMASS(ISEG3D)
            IF(NEWVOL.GT.0.D0) THEN
              FC(I2)=FC(I2)-RMASS(ISEG3D)*(FC(I1)-FC(I2))/NEWVOL
            ENDIF
            VOLU2(I2)=NEWVOL
            VOLU2(I1)=VOLU2(I1)+RMASS(ISEG3D)
C           NECESSARY IN CASE OF FLUXES REACTIVATED IN PARALLEL MODE
C           IF PERIOD_SHARE IS USED
C           RMASS(ISEG3D)=0.D0
          ENDIF
        ENDIF
      ENDDO
!
      REMAIN_SEG=NEWREMAIN
!
C     MERGES VOLUMES AND FC AT INTERFACE POINTS
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
        RPOS=P_DSUM(RPOS)
        RNEG=P_DSUM(RNEG)
        REMAIN_TOT=P_ISUM(REMAIN_TOT)
      ENDIF
!
C     4 POSSIBLE REASONS FOR STOPPING:
!
C     1) THERE IS NO REMAINING FLUX
C     2) REMAINING FLUXES DO NOT CHANGE (POSITIVE AND NEGATIVE)
C        WITH SOME ABSOLUTE ALLOWANCE OR REDUCTION COEFFICIENT
C     3) ALL SEGMENTS HAVE BEEN TREATED
C     4) MAXIMUM NUMBER OF ITERATIONS IS REACHED
!
      IF(  (RPOS.NE.0.D0.OR.RNEG.NE.0.D0)        .AND.
     &    MAX(ABS(RPOS-RPOS_OLD),ABS(RNEG-RNEG_OLD)).GT.
     &                   MIN(RINIT*REDUC,ALLOW)  .AND.
     &               REMAIN_TOT.NE.0             .AND.
     &               NITER.LT.NITMAX                   ) THEN
        RPOS_OLD=RPOS
        RNEG_OLD=RNEG
        GO TO 777
      ENDIF
!
C     TAKES INTO ACCOUNT EXITING EXTERNAL FLUXES
C     THIS IS DONE WITHOUT CHANGING THE TRACER
C    (BUT THE MASS OF TRACER CHANGES)
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
C     EXPLICIT SOURCE TERMS
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
101   FORMAT('MURD3D_POS OPTION: ',1I4,'  ',1I4,' ITERATIONS')
102   FORMAT('MURD3D_POS OPTION: ',1I4,'  ',1I4,' ITERATIONS')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C