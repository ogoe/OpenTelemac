C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES RELATIVE WATER AND TRACER MASS BALANCES
!>                DURING A TIMESTEP, AS WELL AS ABSOLUTE CUMULATIVE
!>                BALANCES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BYPASS, DM1, DT, FLBOR, FLODEL, FLOPAR, FLUEXT, FLUINT, FLULIM, FLUX, GRAPRD, IELM2H, IELM2V, IELM3, KDIR, LIMPRO, LT, MASK8, MASKBR, MASKEL, MESH2, MESH3, MSK, NELEM3, NETAGE, NPLAN, NPOIN2, NPOIN3, NPTFR, OPT_HNEG, PLUIE, RAIN, SIGMAG, SVIDE, TRA01, TRA02, TRA03, TRAV2, UCONV, VCONV, VOLU, VOLUN, W1, YACVVF, ZCONV
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> FORMUL, I, IOPT, IPLAN, IPTFR, ISEG2D, ISEG3D, SUM_FLUEXT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ASSEG_3D(), EXTMSK(), FLUX3DLIM(), FLUX_EF_VF_3D(), OS(), OSDB(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PRECON()

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
!> </td><td> 26/04/2010
!> </td><td> J-M HERVOUET(LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/1999
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AMESH3
!></td><td>--></td><td>BLOC DES TABLEAUX DE REELS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>BYPASS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLODEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLOPAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUEXT
!></td><td><--</td><td>FLUX EXTERIEUR PAR NOEUD
!>    </td></tr>
!>          <tr><td>FLUINT
!></td><td><--</td><td>FLUX INTERIEUR PAR NOEUD
!>    </td></tr>
!>          <tr><td>FLULIM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAPRD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM2H
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM2V
!></td><td>--></td><td>TYPE DE DISCRETISATION 2DV
!>    </td></tr>
!>          <tr><td>IELM3
!></td><td>--></td><td>TYPE DE DISCRETISATION 3D
!>    </td></tr>
!>          <tr><td>IMESH3
!></td><td>--></td><td>BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMPRO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASK8
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES FACES DE BORD 2D
!>    </td></tr>
!>          <tr><td>MASKBR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS 3D
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
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NETAGE
!></td><td>--></td><td>NOMBRE D'ETAGES SUR LA VERTICALE
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPT_HNEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PLUIE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RAIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SIGMAG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SVIDE
!></td><td>--></td><td>STRUCTURE VIDE
!>    </td></tr>
!>          <tr><td>TRA01,02
!></td><td><-></td><td>TABLEAUX DE TRAVAIL DE DIMENSION NPOIN3
!>    </td></tr>
!>          <tr><td>TRA02
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRA03
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRAV2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UCONV,
!></td><td>--></td><td>COMPOSANTES DU CHAMP CONVECTEUR
!>    </td></tr>
!>          <tr><td>VCONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VOLU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VOLUN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YACVVF
!></td><td>--></td><td>THERE IS AN ADVECTION WITH FINITE VOLUMES
!>                  (HENCE COMPUTATION OF FLUXES REQUIRED)
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>COORDONNEE VERTICALE A LA FIN DU PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>ZCONV
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLUX3D
     &(FLUINT,FLUEXT,UCONV,VCONV,TRA01,TRA02,TRA03,
     & W1,NETAGE,NPLAN,NELEM3,IELM3,IELM2H,IELM2V,SVIDE,MESH3,
     & MASK8,MSK,MASKEL,MASKBR,LIMPRO,KDIR,NPTFR,DT,VOLU,VOLUN,
     & MESH2,GRAPRD,SIGMAG,TRAV2,NPOIN2,NPOIN3,FLUX,DM1,ZCONV,
     & FLBOR,PLUIE,RAIN,FLODEL,FLOPAR,OPT_HNEG,FLULIM,YACVVF,LT,BYPASS)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AMESH3         |-->| BLOC DES TABLEAUX DE REELS DU MAILLAGE 3D
C| BYPASS         |---| 
C| DM1            |---| 
C| DT             |---| 
C| FLBOR          |---| 
C| FLODEL         |---| 
C| FLOPAR         |---| 
C| FLUEXT         |<--| FLUX EXTERIEUR PAR NOEUD
C| FLUINT         |<--| FLUX INTERIEUR PAR NOEUD
C| FLULIM         |---| 
C| FLUX           |---| 
C| GRAPRD         |---| 
C| IELM2H         |---| 
C| IELM2V         |-->| TYPE DE DISCRETISATION 2DV
C| IELM3          |-->| TYPE DE DISCRETISATION 3D
C| IMESH3         |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE 3D
C| KDIR           |---| 
C| LIMPRO         |---| 
C| LT             |---| 
C| MASK8          |-->| TABLEAU DE MASQUAGE DES FACES DE BORD 2D
C| MASKBR         |---| 
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS 3D
C| MESH2          |---| 
C| MESH3          |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
C| NELEM3         |---| 
C| NETAGE         |-->| NOMBRE D'ETAGES SUR LA VERTICALE
C| NPLAN          |---| 
C| NPOIN2         |---| 
C| NPOIN3         |---| 
C| NPTFR          |---| 
C| OPT_HNEG       |---| 
C| PLUIE          |---| 
C| RAIN           |---| 
C| SIGMAG         |---| 
C| SVIDE          |-->| STRUCTURE VIDE
C| TRA01,02       |<->| TABLEAUX DE TRAVAIL DE DIMENSION NPOIN3
C| TRA02          |---| 
C| TRA03          |---| 
C| TRAV2          |---| 
C| UCONV,         |-->| COMPOSANTES DU CHAMP CONVECTEUR
C| VCONV          |---| 
C| VOLU           |---| 
C| VOLUN          |---| 
C| W1             |---| 
C| YACVVF         |-->| THERE IS AN ADVECTION WITH FINITE VOLUMES
C|                |   | (HENCE COMPUTATION OF FLUXES REQUIRED)
C| Z             |-->| COORDONNEE VERTICALE A LA FIN DU PAS DE TEMPS
C| ZCONV          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NETAGE,NPLAN,NELEM3,NPOIN2,NPOIN3,LT
      INTEGER, INTENT(IN) :: IELM3,IELM2H,IELM2V,OPT_HNEG
      INTEGER, INTENT(IN) :: KDIR,NPTFR,GRAPRD
      INTEGER, INTENT(IN) :: LIMPRO(NPTFR,6)
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FLUINT,FLUEXT
      TYPE(BIEF_OBJ), INTENT(IN)    :: VOLU,VOLUN,DM1,ZCONV
      TYPE(BIEF_OBJ), INTENT(IN)    :: UCONV,VCONV,PLUIE
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASKEL,MASKBR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: FLOPAR,FLULIM
      TYPE(BIEF_OBJ), INTENT(INOUT), TARGET :: FLODEL
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASK8,FLBOR
      TYPE(BIEF_OBJ), INTENT(INOUT) :: SVIDE,TRA01,TRA02,TRA03,W1,TRAV2
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH3, MESH2
!
      DOUBLE PRECISION, INTENT(IN)    :: DT
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(100)
      LOGICAL, INTENT(IN)             :: MSK,SIGMAG,RAIN,YACVVF,BYPASS
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPLAN,IPTFR,I,ISEG2D,ISEG3D,IOPT
      CHARACTER(LEN=16) FORMUL
      DOUBLE PRECISION SUM_FLUEXT
!
!***********************************************************************
!
      CALL OS('X=0     ',X=FLUEXT)
!
!=======================================================================
!
C   COMPUTES INTERNAL ADVECTIVE FLUXES
!
!=======================================================================
!
C                /            D(PSII*)           D(PSII*)
C COMPUTES :    /     H * U * -------- + H * V * --------   D(OMEGA*)
C              /OMEGA*           DX                 DY
!
!
      FORMUL = 'VGRADP 2     HOR'
      CALL VECTOR(FLUINT,'=',FORMUL,IELM3,1.D0,DM1,ZCONV,SVIDE,
     &            UCONV,VCONV,SVIDE,MESH3,MSK,MASKEL)
!
C COMPUTES POINT TO POINT FLUXES (FOR TREATMENT OF TIDAL FLATS
C                              OR FOR ADVECTION WITH FINITE VOLUMES)
C HERE THE CONVENTION FOR SEGMENTS, DUE TO THE CHOICE OF FLUINT, IS
C THAT A SEGMENT WITH POSITIVE FLUX IS MEANT WITH A FLOW FROM POINT 2
C TO POINT 1
!
      IF(OPT_HNEG.EQ.2.OR.YACVVF) THEN
        IOPT=2
        CALL FLUX_EF_VF_3D(FLODEL%R,MESH2%W%R,MESH3%W%R,
     &                     MESH2%NSEG,MESH3%NSEG,MESH2%NELEM,
     &                     MESH3%NELEM,MESH2,MESH3,.TRUE.,IOPT,1)
      ENDIF
!
C LIMITS FLUXES ACCORDING TO WHAT IS DONE IN 2D CONTINUITY EQUATION
!
      IF(OPT_HNEG.EQ.2) THEN
C       LIMITATION OF 3D FLUXES WITH 2D LIMITATIONS
        CALL FLUX3DLIM(FLODEL%R,FLULIM%R,NPLAN,MESH2%NSEG)
C       NEW ASSEMBLY OF FLUINT (IN THIS CASE ASSEMBLING FLUINT
C                               IN VECTOR ABOVE IS USELESS)
        CALL ASSEG_3D(FLODEL%R,FLUINT%R,NPOIN3,NPLAN,MESH2%NSEG,
     &                MESH3%GLOSEG%I,MESH3%GLOSEG%DIM1,.TRUE.)
!
      ENDIF
!
!=======================================================================
!
C   COMPUTES THE ADVECTIVE FLUXES ON THE LATERAL LIQUID BOUNDARIES
!
!=======================================================================
!
C                /        ->  ->
C COMPUTES :    /     H * U . N  PSII*  D(OMEGA*)
C              /
C             /LIQUID BOUNDARIES*
!
      FORMUL = 'FLUBOR          '
!
C     SETS A MASK ON LATERAL LIQUID BOUNDARIES
!
      CALL EXTMSK(MASKBR,MASK8%R,NPTFR,NETAGE)
!
      CALL VECTOR
     & (TRA02, '=', FORMUL, IELM2V, 1.D0, SVIDE, SVIDE, SVIDE,
     &  UCONV, VCONV, SVIDE, MESH3, .TRUE., MASKBR)
!
      CALL OSDB( 'X=Y     ' , FLUEXT , TRA02 , TRA02 , 0.D0 , MESH3 )
!
!-----------------------------------------------------------------------
!
C     COMPUTES FLUEXT ON POINTS WITH PRESCRIBED DEPTH
C     SO THAT CONTINUITY IS ENSURED
!
C     EXCEPT AT THE FIRST CALL (BY THE FIRST CALL TO PRECON) FLBOR
C     HAS ALREADY BEEN COMPUTED (WAVE_EQUATION AND POSSIBLY
C     POSITIVE_DEPTHS). IT SHOULD GIVE THE SAME VALUE HERE
!
      IF(NPTFR.GT.0) THEN
        DO IPTFR = 1,NPTFR
          IF(LIMPRO(IPTFR,1).EQ.KDIR) THEN
            FLBOR%R(IPTFR)=0.D0
            DO IPLAN = 1,NPLAN
              I=(IPLAN-1)*NPOIN2+MESH2%NBOR%I(IPTFR)
C             FLUEXT COMPUTED TO SOLVE CONTINUITY IN 3D
C             WITH ASSUMPTION THAT W* IS ZERO
              FLUEXT%R(I)=FLUINT%R(I)+(VOLUN%R(I)-VOLU%R(I))/DT
              FLBOR%R(IPTFR)=FLBOR%R(IPTFR)+FLUEXT%R(I)
            ENDDO
C
C           CHECKS THAT SUM OF FLUEXT STILL EQUALS FLBOR
C           IN THIS CASE DO NOT COMPUTE FLBOR ABOVE
C
C           SUM_FLUEXT=0.D0
C           DO IPLAN = 1,NPLAN
C             I=(IPLAN-1)*NPOIN2+MESH2%NBOR%I(IPTFR)
C             SUM_FLUEXT=SUM_FLUEXT+FLUEXT%R(I)
C           ENDDO
C           IF(ABS(SUM_FLUEXT-FLBOR%R(IPTFR)).GT.1.D-10) THEN
C             PRINT*,'PROBLEM AT POINT ',IPTFR
C             PRINT*,'FLBOR= ',FLBOR%R(IPTFR),' SUM_FLUEXT=',SUM_FLUEXT
C             POSSIBLE CORRECTION
C             DO IPLAN = 1,NPLAN
C               I=(IPLAN-1)*NPOIN2+MESH2%NBOR%I(IPTFR)
C               FLUEXT%R(I)=FLUEXT%R(I)*(FLBOR%R(IPTFR)/SUM_FLUEXT)
C             ENDDO
C             STOP
C           ENDIF
          ENDIF
        ENDDO
C
C       SPECIFIC TREATMENT OF POINTS THAT REMAIN WITHOUT VOLUME
C       THIS DOES NOT CHANGE FLBOR
C
        IF((OPT_HNEG.EQ.2.OR.SIGMAG).AND.BYPASS) THEN
          DO IPTFR = 1,NPTFR
            I=MESH2%NBOR%I(IPTFR)
            DO IPLAN = 1,NPLAN-1
              IF(VOLUN%R(I).LT.1.D-14.AND.VOLU%R(I).LT.1.D-14) THEN
C               FLUEXT GIVEN TO UPPER LAYER
                FLUEXT%R(I+NPOIN2)=FLUEXT%R(I+NPOIN2)+FLUEXT%R(I)
                FLUEXT%R(I)=0.D0
              ENDIF
              I=I+NPOIN2
            ENDDO
          ENDDO
        ENDIF
!
      ENDIF
!
!=======================================================================
!
C   COMPUTES THE ADVECTIVE FLUXES THROUGH THE BOTTOM AND FREE SURFACE
!
!=======================================================================
!
C  DIRICHLET TERMS AT THE BOTTOM AND FREE SURFACE
!
!                /
C COMPUTES :    /     H * W*  PSII*  D(OMEGA*)
C              /SURFACE*-BOTTOM*
!
C   BOTTOM :
!
C     CALL VECTOR
C    &(TRAV2, '=', 'FLUBOR          ', IELM2H, -1.D0, SVIDE, SVIDE,
C    & SVIDE, SVIDE, SVIDE, WSBORF, MESH2,MSK,MASKEL)
!
C     CALL OV ( 'X=X+Y   ' ,FLUEXT%R(1:NPOIN2),
C    &                      TRAV2%R, TRAV2%R, 0.D0, NPOIN2)
!
C   SURFACE :
!
C     CALL VECTOR
C    &(TRAV2, '=', 'FLUBOR          ', IELM2H, 1.D0, SVIDE, SVIDE,
C    & SVIDE, SVIDE, SVIDE, WSBORS, MESH2,MSK,MASKEL)
!
C     CALL OV ( 'X=X+Y   ' ,FLUEXT%R((NPOIN3-NPOIN2+1):NPOIN3),
C    &                      TRAV2%R, TRAV2%R, 0.D0, NPOIN2)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C