C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ADVECTION OF A VARIABLE WITH THE DISTRIBUTIVE SCHEME
!>                AFTER HAVING COMPUTED THE DISTRIBUTION MATRIX WHICH IS:
!><br>            - COMMON TO ALL THE VARIABLES (N SCHEME),
!><br>            - SPECIFIC TO EACH VARIABLE (PSI SCHEME).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  FOR THE N SCHEME
!>            MATRIX B MUST BE CONFUSED WITH MATRIX A

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CALFLU, DA, DB, DIMGLO, DT, FC, FLODEL, FLOPAR, FLUEXT, FLUX, FN, FSCE, GLOSEG, IKLE3, INFOR, LV, MASKEL, MASKPT, MAXFC, MESH3, MINFC, MSK, NELEM3, NPLAN, NPOIN2, NPOIN3, NSCE, NSEG, OPTBAN, PLUIE, RAIN, S0F, SCHCF, SOURCES, STRA01, STRA02, STRA03, SVOLU2, TRA01, TRA02, TRA03, VOLU, VOLU2, VOLUN, W1, XA, XB
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::ADV_LPO ADV_LPO@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC ADV_NSC@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_PSI ADV_PSI@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ALFA, ALFA2, C, DTJ, DTJALFA, EPS, I1, I2, I2D, I3, I4, I5, I6, IELEM, IGUILT, IIS, IMAX, IMIN, IPLAN, IPOIN, IS, ISEG2D, ISEG3D, J, M12, M13, M14, M15, M16, M23, M24, M25, M26, M34, M35, M36, M45, M46, M56, MAXEL, MINEL, NITER, NSEGH, NSEGV, OPT, PHIM, PHIP, T1, T2, T3, T4, T5, T6, TOTITER
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), MITTIT(), OV(), PARCOM(), PLANTE(), P_DMIN()
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
!> </td><td>
!> </td><td> J.M. JANIN  (LNH) 01 30 87 72 84
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 11/08/09
!> </td><td>
!> </td><td> FINITE VOLUME SCHEME OPTIMISED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 22/06/09
!> </td><td>
!> </td><td> FINITE VOLUME SCHEME ADDED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 04/08/08
!> </td><td>
!> </td><td> DIMENSIONS OF XA AND XB INVERTED (SEE ALSO MT14PP)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 29/07/08
!> </td><td>
!> </td><td> TIDAL FLATS WITH OPTBAN=2
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 19/12/07
!> </td><td>
!> </td><td> CHANGED MONOTONICITY CRITERION
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 19/11/07
!> </td><td>
!> </td><td> RAIN HAD BEEN LEFT OUT IN THE PART WITH ALFA
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 28/08/07
!> </td><td>
!> </td><td> PSI SCHEME RE-WRITTEN, NO THEORETICAL CHANGES BUT
!>           MORE COMPLIANT WITH THE DOC
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 12/06/07
!> </td><td>
!> </td><td> SOURCES IN PARALLEL LOOK AT THE USE OF IIS
!>           AND MODIFICATIONS IN SOURCES_SINKS
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 10/11/05
!> </td><td>
!> </td><td> SOURCES TAKEN INTO ACCOUNT IN MONOTONICITY CRITERION
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 16/06/05
!> </td><td>
!> </td><td> SOURCES MODIFIED FOR INTAKES
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
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
!>          <tr><td>IKLE3
!></td><td>--></td><td>CORRESPONDANCE NUMEROTATION LOCALE ET GLOBALE
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
!>          <tr><td>MAXFC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MINFC
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
!>          <tr><td>SVOLU2
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
!>          <tr><td>W1
!></td><td><-></td><td>TABLEAU DE TRAVAIL (CALCUL DES MATRICES...)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MURD3D
     &(FC,FN,VOLU,VOLUN,VOLU2,SVOLU2,DA,XA,DB,XB,
     & TRA01,TRA02,TRA03,STRA01,STRA02,STRA03,W1,IKLE3,MESH3,
     & NELEM3,NPOIN3,DT,SCHCF,LV,MSK,MASKEL,INFOR,CALFLU,FLUX,FLUEXT,
     & S0F,NSCE,SOURCES,FSCE,RAIN,PLUIE,NPOIN2,MINFC,MAXFC,MASKPT,
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
C| IKLE3          |-->| CORRESPONDANCE NUMEROTATION LOCALE ET GLOBALE
C| INFOR          |-->| INFORMATIONS SUR LES SOLVEURS
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MASKPT         |---| 
C| MAXFC          |---| 
C| MESH3          |---| 
C| MINFC          |---| 
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
C| S0F            |-->| TERME SOURCE EXPLICITE
C| SCHCF          |-->| SCHEMA DE CONVECTION DE F
C| SOURCES        |---| 
C| STRA01         |---| 
C| STRA02         |---| 
C| STRA03         |---| 
C| SVOLU2         |---| 
C| TRA01          |<->| TABLEAU DE TRAVAIL DE DIMENSION NPOIN3
C|                |   | EQUIVALENT DE VOLU2 POUR LE TEMPS FINAL COURANT
C| TRA02,3        |<->| TABLEAUX DE TRAVAIL DE DIMENSION NPOIN3
C| TRA03          |---| 
C| VOLU           |-->| VOLUME DE CONTROLE A L'INSTANT N+1
C| VOLU2          |-->| COMME VOLU MAIS ASSEMBLE EN PARALLELISME
C| VOLUN          |-->| VOLUME DE CONTROLE A L'INSTANT N
C| W1             |<->| TABLEAU DE TRAVAIL (CALCUL DES MATRICES...)
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
      INTEGER, INTENT(IN)             :: SCHCF,NELEM3,NPOIN3,LV,NPOIN2
      INTEGER, INTENT(IN)             :: IKLE3(NELEM3,6),NSCE,OPTBAN
      INTEGER, INTENT(IN)             :: NSEG,NPLAN,DIMGLO
      INTEGER, INTENT(IN)             :: GLOSEG(DIMGLO,2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: FC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: FN(NPOIN3),PLUIE(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: W1(NELEM3,6)
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELEM3), FLUEXT(NPOIN3)
!
      DOUBLE PRECISION, INTENT(IN)    :: VOLUN(NPOIN3), VOLU(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: VOLU2(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN3), TRA03(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX
      DOUBLE PRECISION, INTENT(IN)    :: DT,FSCE(NSCE),MASKPT(NPOIN3)
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SVOLU2,MINFC,MAXFC
      TYPE(BIEF_OBJ), INTENT(IN)      :: SOURCES,S0F
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: STRA01,STRA02,STRA03
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH3

      DOUBLE PRECISION, INTENT(INOUT) :: DA(NPOIN3),XA(30,NELEM3)
      DOUBLE PRECISION, INTENT(INOUT) :: DB(NPOIN3),XB(30,NELEM3)
!
C     DIMENSION OF FLODEL AND FLOPAR=NSEG2D*NPLAN+NPOIN2*NETAGE
      DOUBLE PRECISION, INTENT(IN)    :: FLODEL(*),FLOPAR(*)
!
      LOGICAL, INTENT(IN)             :: MSK,INFOR,CALFLU,RAIN
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION DTJ,PHIP,PHIM,ALFA,C,ALFA2,DTJALFA,MINEL,MAXEL
      DOUBLE PRECISION M12,M13,M14,M15,M16,M23,M24,M25,M26,M34
      DOUBLE PRECISION M35,M36,M45,M46,M56,T1,T2,T3,T4,T5,T6
!
      INTEGER IELEM,IPOIN,NITER,J,IMIN,IMAX,IS,IGUILT,IIS
      INTEGER I1,I2,I3,I4,I5,I6,OPT,IPLAN,ISEG2D,ISEG3D,I2D,NSEGH,NSEGV
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION P_DMIN,P_DSUM
      EXTERNAL P_DMIN,P_DSUM
!
      DOUBLE PRECISION EPS
      DATA EPS /1.D-6/
C     DATA EPS /10.D0/
!
      INTEGER TOTITER
      DATA TOTITER /0/
!
!***********************************************************************
!
      IF(INFOR) CALL MITTIT(15,0.D0,0)
!
      CALL CPSTVC(SVOLU2,STRA01)
      CALL CPSTVC(SVOLU2,STRA02)
      CALL CPSTVC(SVOLU2,STRA03)
      CALL CPSTVC(SVOLU2,MINFC)
      CALL CPSTVC(SVOLU2,MAXFC)
!
      NITER = 0
      DTJ = DT
!
      CALL OV ('X=Y     ',FC,FN,FN,C,NPOIN3)
!
      CALL OV ('X=Y     ',TRA01, VOLUN, VOLUN, C, NPOIN3)
      IF (NCSIZE.GT.1) CALL PARCOM(STRA01,2,MESH3)
!
      CALL OV ('X=Y     ',VOLU2, VOLU, VOLU, C, NPOIN3)
      IF (NCSIZE.GT.1) CALL PARCOM(SVOLU2,2,MESH3)
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
C  BUILDS THE PSI SCHEME FROM THE N SCHEME IF SCHCF=5
C  SEE "HYDRODYNAMICS OF FREE SURFACE FLOWS" PAGE 193
!
      IF(SCHCF.EQ.ADV_PSI) THEN
!
         DO IPOIN=1,NPOIN3
           DB(IPOIN)=0.D0
         ENDDO
!
         DO 20 IELEM = 1,NELEM3
!
            I1 = IKLE3(IELEM,1)
            I2 = IKLE3(IELEM,2)
            I3 = IKLE3(IELEM,3)
            I4 = IKLE3(IELEM,4)
            I5 = IKLE3(IELEM,5)
            I6 = IKLE3(IELEM,6)
!
            T1 = FC(I1)
            T2 = FC(I2)
            T3 = FC(I3)
            T4 = FC(I4)
            T5 = FC(I5)
            T6 = FC(I6)
!
            M12 = (XA(01,IELEM)-XA(16,IELEM)) * (T1-T2)
            M13 = (XA(02,IELEM)-XA(17,IELEM)) * (T1-T3)
            M14 = (XA(03,IELEM)-XA(18,IELEM)) * (T1-T4)
            M15 = (XA(04,IELEM)-XA(19,IELEM)) * (T1-T5)
            M16 = (XA(05,IELEM)-XA(20,IELEM)) * (T1-T6)
            M23 = (XA(06,IELEM)-XA(21,IELEM)) * (T2-T3)
            M24 = (XA(07,IELEM)-XA(22,IELEM)) * (T2-T4)
            M25 = (XA(08,IELEM)-XA(23,IELEM)) * (T2-T5)
            M26 = (XA(09,IELEM)-XA(24,IELEM)) * (T2-T6)
            M34 = (XA(10,IELEM)-XA(25,IELEM)) * (T3-T4)
            M35 = (XA(11,IELEM)-XA(26,IELEM)) * (T3-T5)
            M36 = (XA(12,IELEM)-XA(27,IELEM)) * (T3-T6)
            M45 = (XA(13,IELEM)-XA(28,IELEM)) * (T4-T5)
            M46 = (XA(14,IELEM)-XA(29,IELEM)) * (T4-T6)
            M56 = (XA(15,IELEM)-XA(30,IELEM)) * (T5-T6)
!
            PHIP = MAX( M12,0.D0) + MAX( M13,0.D0) + MAX( M14,0.D0)
     &           + MAX( M15,0.D0) + MAX( M16,0.D0) + MAX( M23,0.D0)
     &           + MAX( M24,0.D0) + MAX( M25,0.D0) + MAX( M26,0.D0)
     &           + MAX( M34,0.D0) + MAX( M35,0.D0) + MAX( M36,0.D0)
     &           + MAX( M45,0.D0) + MAX( M46,0.D0) + MAX( M56,0.D0)
            PHIM = MAX(-M12,0.D0) + MAX(-M13,0.D0) + MAX(-M14,0.D0)
     &           + MAX(-M15,0.D0) + MAX(-M16,0.D0) + MAX(-M23,0.D0)
     &           + MAX(-M24,0.D0) + MAX(-M25,0.D0) + MAX(-M26,0.D0)
     &           + MAX(-M34,0.D0) + MAX(-M35,0.D0) + MAX(-M36,0.D0)
     &           + MAX(-M45,0.D0) + MAX(-M46,0.D0) + MAX(-M56,0.D0)
!
            IF(PHIP.GE.PHIM) THEN
               ALFA = (PHIP - PHIM) / MAX(PHIP,1.D-10)
               IF(T2.GT.T1) THEN
                 XB(01,IELEM) = 0.D0
                 XB(16,IELEM) = XA(16,IELEM) * ALFA
               ELSE
                 XB(01,IELEM) = XA(01,IELEM) * ALFA
                 XB(16,IELEM) = 0.D0
               ENDIF
               IF(T3.GT.T1) THEN
                 XB(02,IELEM) = 0.D0
                 XB(17,IELEM) = XA(17,IELEM) * ALFA
               ELSE
                 XB(02,IELEM) = XA(02,IELEM) * ALFA
                 XB(17,IELEM) = 0.D0
               ENDIF
               IF(T4.GT.T1) THEN
                 XB(03,IELEM) = 0.D0
                 XB(18,IELEM) = XA(18,IELEM) * ALFA
               ELSE
                 XB(03,IELEM) = XA(03,IELEM) * ALFA
                 XB(18,IELEM) = 0.D0
               ENDIF
               IF(T5.GT.T1) THEN
                 XB(04,IELEM) = 0.D0
                 XB(19,IELEM) = XA(19,IELEM) * ALFA
               ELSE
                 XB(04,IELEM) = XA(04,IELEM) * ALFA
                 XB(19,IELEM) = 0.D0
               ENDIF
               IF(T6.GT.T1) THEN
                 XB(05,IELEM) = 0.D0
                 XB(20,IELEM) = XA(20,IELEM) * ALFA
               ELSE
                 XB(05,IELEM) = XA(05,IELEM) * ALFA
                 XB(20,IELEM) = 0.D0
               ENDIF
               IF(T3.GT.T2) THEN
                 XB(06,IELEM) = 0.D0
                 XB(21,IELEM) = XA(21,IELEM) * ALFA
               ELSE
                 XB(06,IELEM) = XA(06,IELEM) * ALFA
                 XB(21,IELEM) = 0.D0
               ENDIF
               IF(T4.GT.T2) THEN
                 XB(07,IELEM) = 0.D0
                 XB(22,IELEM) = XA(22,IELEM) * ALFA
               ELSE
                 XB(07,IELEM) = XA(07,IELEM) * ALFA
                 XB(22,IELEM) = 0.D0
               ENDIF
               IF(T5.GT.T2) THEN
                 XB(08,IELEM) = 0.D0
                 XB(23,IELEM) = XA(23,IELEM) * ALFA
               ELSE
                 XB(08,IELEM) = XA(08,IELEM) * ALFA
                 XB(23,IELEM) = 0.D0
               ENDIF
               IF(T6.GT.T2) THEN
                 XB(09,IELEM) = 0.D0
                 XB(24,IELEM) = XA(24,IELEM) * ALFA
               ELSE
                 XB(09,IELEM) = XA(09,IELEM) * ALFA
                 XB(24,IELEM) = 0.D0
               ENDIF
               IF(T4.GT.T3) THEN
                 XB(10,IELEM) = 0.D0
                 XB(25,IELEM) = XA(25,IELEM) * ALFA
               ELSE
                 XB(10,IELEM) = XA(10,IELEM) * ALFA
                 XB(25,IELEM) = 0.D0
               ENDIF
               IF(T5.GT.T3) THEN
                 XB(11,IELEM) = 0.D0
                 XB(26,IELEM) = XA(26,IELEM) * ALFA
               ELSE
                 XB(11,IELEM) = XA(11,IELEM) * ALFA
                 XB(26,IELEM) = 0.D0
               ENDIF
               IF(T6.GT.T3) THEN
                 XB(12,IELEM) = 0.D0
                 XB(27,IELEM) = XA(27,IELEM) * ALFA
               ELSE
                 XB(12,IELEM) = XA(12,IELEM) * ALFA
                 XB(27,IELEM) = 0.D0
               ENDIF
               IF(T5.GT.T4) THEN
                 XB(13,IELEM) = 0.D0
                 XB(28,IELEM) = XA(28,IELEM) * ALFA
               ELSE
                 XB(13,IELEM) = XA(13,IELEM) * ALFA
                 XB(28,IELEM) = 0.D0
               ENDIF
               IF(T6.GT.T4) THEN
                 XB(14,IELEM) = 0.D0
                 XB(29,IELEM) = XA(29,IELEM) * ALFA
               ELSE
                 XB(14,IELEM) = XA(14,IELEM) * ALFA
                 XB(29,IELEM) = 0.D0
               ENDIF
               IF(T6.GT.T5) THEN
                 XB(15,IELEM) = 0.D0
                 XB(30,IELEM) = XA(30,IELEM) * ALFA
               ELSE
                 XB(15,IELEM) = XA(15,IELEM) * ALFA
                 XB(30,IELEM) = 0.D0
               ENDIF
            ELSE
               ALFA = (PHIM - PHIP) / MAX(PHIM,1.D-10)
               IF(T2.GT.T1) THEN
                 XB(01,IELEM) = XA(01,IELEM) * ALFA
                 XB(16,IELEM) = 0.D0
               ELSE
                 XB(01,IELEM) = 0.D0
                 XB(16,IELEM) = XA(16,IELEM) * ALFA
               ENDIF
               IF(T3.GT.T1) THEN
                 XB(02,IELEM) = XA(02,IELEM) * ALFA
                 XB(17,IELEM) = 0.D0
               ELSE
                 XB(02,IELEM) = 0.D0
                 XB(17,IELEM) = XA(17,IELEM) * ALFA
               ENDIF
               IF(T4.GT.T1) THEN
                 XB(03,IELEM) = XA(03,IELEM) * ALFA
                 XB(18,IELEM) = 0.D0
               ELSE
                 XB(03,IELEM) = 0.D0
                 XB(18,IELEM) = XA(18,IELEM) * ALFA
               ENDIF
               IF(T5.GT.T1) THEN
                 XB(04,IELEM) = XA(04,IELEM) * ALFA
                 XB(19,IELEM) = 0.D0
               ELSE
                 XB(04,IELEM) = 0.D0
                 XB(19,IELEM) = XA(19,IELEM) * ALFA
               ENDIF
               IF(T6.GT.T1) THEN
                 XB(05,IELEM) = XA(05,IELEM) * ALFA
                 XB(20,IELEM) = 0.D0
               ELSE
                 XB(05,IELEM) = 0.D0
                 XB(20,IELEM) = XA(20,IELEM) * ALFA
               ENDIF
               IF(T3.GT.T2) THEN
                 XB(06,IELEM) = XA(06,IELEM) * ALFA
                 XB(21,IELEM) = 0.D0
               ELSE
                 XB(06,IELEM) = 0.D0
                 XB(21,IELEM) = XA(21,IELEM) * ALFA
               ENDIF
               IF(T4.GT.T2) THEN
                 XB(07,IELEM) = XA(07,IELEM) * ALFA
                 XB(22,IELEM) = 0.D0
               ELSE
                 XB(07,IELEM) = 0.D0
                 XB(22,IELEM) = XA(22,IELEM) * ALFA
               ENDIF
               IF(T5.GT.T2) THEN
                 XB(08,IELEM) = XA(08,IELEM) * ALFA
                 XB(23,IELEM) = 0.D0
               ELSE
                 XB(08,IELEM) = 0.D0
                 XB(23,IELEM) = XA(23,IELEM) * ALFA
               ENDIF
               IF(T6.GT.T2) THEN
                 XB(09,IELEM) = XA(09,IELEM) * ALFA
                 XB(24,IELEM) = 0.D0
               ELSE
                 XB(09,IELEM) = 0.D0
                 XB(24,IELEM) = XA(24,IELEM) * ALFA
               ENDIF
               IF(T4.GT.T3) THEN
                 XB(10,IELEM) = XA(10,IELEM) * ALFA
                 XB(25,IELEM) = 0.D0
               ELSE
                 XB(10,IELEM) = 0.D0
                 XB(25,IELEM) = XA(25,IELEM) * ALFA
               ENDIF
               IF(T5.GT.T3) THEN
                 XB(11,IELEM) = XA(11,IELEM) * ALFA
                 XB(26,IELEM) = 0.D0
               ELSE
                 XB(11,IELEM) = 0.D0
                 XB(26,IELEM) = XA(26,IELEM) * ALFA
               ENDIF
               IF(T6.GT.T3) THEN
                 XB(12,IELEM) = XA(12,IELEM) * ALFA
                 XB(27,IELEM) = 0.D0
               ELSE
                 XB(12,IELEM) = 0.D0
                 XB(27,IELEM) = XA(27,IELEM) * ALFA
               ENDIF
               IF(T5.GT.T4) THEN
                 XB(13,IELEM) = XA(13,IELEM) * ALFA
                 XB(28,IELEM) = 0.D0
               ELSE
                 XB(13,IELEM) = 0.D0
                 XB(28,IELEM) = XA(28,IELEM) * ALFA
               ENDIF
               IF(T6.GT.T4) THEN
                 XB(14,IELEM) = XA(14,IELEM) * ALFA
                 XB(29,IELEM) = 0.D0
               ELSE
                 XB(14,IELEM) = 0.D0
                 XB(29,IELEM) = XA(29,IELEM) * ALFA
               ENDIF
               IF(T6.GT.T5) THEN
                 XB(15,IELEM) = XA(15,IELEM) * ALFA
                 XB(30,IELEM) = 0.D0
               ELSE
                 XB(15,IELEM) = 0.D0
                 XB(30,IELEM) = XA(30,IELEM) * ALFA
               ENDIF
            ENDIF
!
            DB(I1)=DB(I1) - XB(01,IELEM) - XB(02,IELEM)
     &                    - XB(03,IELEM)
     &                    - XB(04,IELEM) - XB(05,IELEM)
            DB(I2)=DB(I2) - XB(16,IELEM) - XB(06,IELEM)
     &                    - XB(07,IELEM)
     &                    - XB(08,IELEM) - XB(09,IELEM)
            DB(I3)=DB(I3) - XB(17,IELEM) - XB(21,IELEM)
     &                    - XB(10,IELEM)
     &                    - XB(11,IELEM) - XB(12,IELEM)
            DB(I4)=DB(I4) - XB(18,IELEM) - XB(22,IELEM)
     &                    - XB(25,IELEM)
     &                    - XB(13,IELEM) - XB(14,IELEM)
            DB(I5)=DB(I5) - XB(19,IELEM) - XB(23,IELEM)
     &                    - XB(26,IELEM)
     &                    - XB(28,IELEM) - XB(15,IELEM)
            DB(I6)=DB(I6) - XB(20,IELEM) - XB(24,IELEM)
     &                    - XB(27,IELEM)
     &                    - XB(29,IELEM) - XB(30,IELEM)
!
20       CONTINUE
!
C        DB IS : - SUM ON J OF LAMBDA(I,J)
!
C        CALL ASSVEC
C    &   (DB,IKLE3,NPOIN3,NELEM3,NELEM3,41,W1,.TRUE.,LV,MSK,MASKEL)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(SCHCF.EQ.ADV_NSC.OR.SCHCF.EQ.ADV_PSI) THEN
!
C  COMPUTES THE FLUX TERMS PER UNIT OF TIME:
!
C  I.E. : SUM ON J (LAMBDA(I,J)*(FC(J)-FC(I))
!
C  DECOMPOSED INTO : -(SUM ON J (LAMBDA(I,J)*(FC(I)) WHICH IS DB * FC(I)
C                    +(SUM ON J (LAMBDA(I,J)*(FC(J)) WHICH IS XB * FC
!
C     CALL MV0606
C    &('X=AY    ', TRA02, DB, 'Q', XB, 'Q', FC, C, IKLE3(1,1),
C    & IKLE3(1,2),IKLE3(1,3),IKLE3(1,4),IKLE3(1,5),IKLE3(1,6),
C    & NPOIN3, NELEM3, NELEM3,
C    & W1(1,1),W1(1,2),W1(1,3),W1(1,4),W1(1,5),W1(1,6))
C     CALL ASSVEC
C    &(TRA02,IKLE3,NPOIN3,NELEM3,NELEM3,41,W1,.FALSE.,LV,MSK,MASKEL)
C
C     REPLACES CALL MV0606 AND CALL ASSVEC
C
      CALL OV ('X=YZ    ',TRA02,DB,FC,C,NPOIN3)
C
      DO IELEM = 1 , NELEM3
C
        I1 = IKLE3(IELEM,1)
        I2 = IKLE3(IELEM,2)
        I3 = IKLE3(IELEM,3)
        I4 = IKLE3(IELEM,4)
        I5 = IKLE3(IELEM,5)
        I6 = IKLE3(IELEM,6)
C
        TRA02(I1) = TRA02(I1) + XB(01,IELEM) * FC(I2)
     &                        + XB(02,IELEM) * FC(I3)
     &                        + XB(03,IELEM) * FC(I4)
     &                        + XB(04,IELEM) * FC(I5)
     &                        + XB(05,IELEM) * FC(I6)
C
        TRA02(I2) = TRA02(I2) + XB(16,IELEM) * FC(I1)
     &                        + XB(06,IELEM) * FC(I3)
     &                        + XB(07,IELEM) * FC(I4)
     &                        + XB(08,IELEM) * FC(I5)
     &                        + XB(09,IELEM) * FC(I6)
C
        TRA02(I3) = TRA02(I3) + XB(17,IELEM) * FC(I1)
     &                        + XB(21,IELEM) * FC(I2)
     &                        + XB(10,IELEM) * FC(I4)
     &                        + XB(11,IELEM) * FC(I5)
     &                        + XB(12,IELEM) * FC(I6)
C
        TRA02(I4) = TRA02(I4) + XB(18,IELEM) * FC(I1)
     &                        + XB(22,IELEM) * FC(I2)
     &                        + XB(25,IELEM) * FC(I3)
     &                        + XB(13,IELEM) * FC(I5)
     &                        + XB(14,IELEM) * FC(I6)
C
        TRA02(I5) = TRA02(I5) + XB(19,IELEM) * FC(I1)
     &                        + XB(23,IELEM) * FC(I2)
     &                        + XB(26,IELEM) * FC(I3)
     &                        + XB(28,IELEM) * FC(I4)
     &                        + XB(15,IELEM) * FC(I6)
C
        TRA02(I6) = TRA02(I6) + XB(20,IELEM) * FC(I1)
     &                        + XB(24,IELEM) * FC(I2)
     &                        + XB(27,IELEM) * FC(I3)
     &                        + XB(29,IELEM) * FC(I4)
     &                        + XB(30,IELEM) * FC(I5)
C
      ENDDO
C
C     END OF THE REPLACEMENT OF CALL MV0606
!
      ENDIF
!
      IF(SCHCF.EQ.ADV_LPO) THEN
!
        NSEGH=NSEG*NPLAN
        NSEGV=(NPLAN-1)*NPOIN2
!
C       COMPUTES DB AND TRA02 IN UPWIND EXPLICIT FINITE VOLUMES
!
        DO IPOIN=1,NPOIN3
          DB(IPOIN)=0.D0
          TRA02(IPOIN)=0.D0
        ENDDO
!
C       HORIZONTAL AND VERTICAL FLUXES ONLY
C       BEWARE : FLUXES FROM POINT 2 TO 1 IN SEGMENT
C                WITH POINTS 1 AND 2 (SEE PRECON AND FLUX3D)
!
        DO ISEG3D = 1,NSEGH+NSEGV
          I1=GLOSEG(ISEG3D,1)
          I2=GLOSEG(ISEG3D,2)
          IF(FLOPAR(ISEG3D).GT.0.D0) THEN
            DB(I1)   = DB(I1)   -FLODEL(ISEG3D)
            TRA02(I1)= TRA02(I1)-FLODEL(ISEG3D)*(FC(I1)-FC(I2))
          ELSEIF(FLOPAR(ISEG3D).LT.0.D0) THEN
            DB(I2)   = DB(I2)   +FLODEL(ISEG3D)
            TRA02(I2)= TRA02(I2)+FLODEL(ISEG3D)*(FC(I2)-FC(I1))
          ENDIF
        ENDDO
!
      ENDIF
!
      IF(NCSIZE.GT.1) CALL PARCOM(STRA02,2,MESH3)
!
!-----------------------------------------------------------------------
!
C  COMPUTES THE LIMITING SUB-TIMESTEP :
C     MULTIPLIES THE REMAINING SUB TIMESTEP BY DTJ
C     AND ADDS TO VOLU NEGATIVE TERM (MASS AT N+1)
!
C     OPT=1 : OLD CRITERION
C     OPT=2 : NEW CRITERION LESS RESTRICTIVE
      OPT=2
!
      IF(OPT.EQ.2) THEN
!
C     COMPUTES THE LOCAL EXTREMA
      DO IPOIN=1,NPOIN3
        MINFC%R(IPOIN)=FC(IPOIN)
        MAXFC%R(IPOIN)=FC(IPOIN)
      ENDDO
      DO IELEM=1,NELEM3
        I1=IKLE3(IELEM,1)
        I2=IKLE3(IELEM,2)
        I3=IKLE3(IELEM,3)
        I4=IKLE3(IELEM,4)
        I5=IKLE3(IELEM,5)
        I6=IKLE3(IELEM,6)
        MINEL=MIN(FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6))
        MAXEL=MAX(FC(I1),FC(I2),FC(I3),FC(I4),FC(I5),FC(I6))
        MINFC%R(I1)=MIN(MINFC%R(I1),MINEL)
        MINFC%R(I2)=MIN(MINFC%R(I2),MINEL)
        MINFC%R(I3)=MIN(MINFC%R(I3),MINEL)
        MINFC%R(I4)=MIN(MINFC%R(I4),MINEL)
        MINFC%R(I5)=MIN(MINFC%R(I5),MINEL)
        MINFC%R(I6)=MIN(MINFC%R(I6),MINEL)
        MAXFC%R(I1)=MAX(MAXFC%R(I1),MAXEL)
        MAXFC%R(I2)=MAX(MAXFC%R(I2),MAXEL)
        MAXFC%R(I3)=MAX(MAXFC%R(I3),MAXEL)
        MAXFC%R(I4)=MAX(MAXFC%R(I4),MAXEL)
        MAXFC%R(I5)=MAX(MAXFC%R(I5),MAXEL)
        MAXFC%R(I6)=MAX(MAXFC%R(I6),MAXEL)
      ENDDO
!
C     IN PARALLEL MODE: GLOBAL EXTREMA
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM(MAXFC,3,MESH3)
        CALL PARCOM(MINFC,4,MESH3)
      ENDIF
!
C     NEW COMPUTATION OF TRA03
!
      DO IPOIN=1,NPOIN3
        IF(TRA02(IPOIN).GT.0.D0) THEN
          TRA03(IPOIN)=VOLU2(IPOIN)-DTJ*TRA02(IPOIN)/
     &                 MAX(MAXFC%R(IPOIN)-FC(IPOIN),1.D-12)
        ELSE
          TRA03(IPOIN)=VOLU2(IPOIN)+DTJ*TRA02(IPOIN)/
     &                 MAX(FC(IPOIN)-MINFC%R(IPOIN),1.D-12)
        ENDIF
      ENDDO
C     POSITIVE SOURCES CHANGE THE MONOTONICITY CRITERION
      IF(NSCE.GT.0) THEN
        DO IS=1,NSCE
          DO IPOIN=1,NPOIN3
            IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
              TRA03(IPOIN)=TRA03(IPOIN)
     &                    -DTJ*SOURCES%ADR(IS)%P%R(IPOIN)
C                                          WITH PARCOM
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
      ELSEIF(OPT.EQ.1) THEN
!
C     TRA03 : COEFFICIENT OF FC(I), THAT MUST REMAIN POSITIVE FOR MONOTONICITY
      CALL OV('X=Y+CZ  ',TRA03, VOLU, DB, DTJ, NPOIN3)
!
C     POSITIVE SOURCES CHANGE THE MONOTONICITY CRITERION
      IF(NSCE.GT.0) THEN
        DO IS=1,NSCE
          DO IPOIN=1,NPOIN3
            IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
              TRA03(IPOIN)=TRA03(IPOIN)
     &                    -DTJ*SOURCES%ADR(IS+NSCE)%P%R(IPOIN)
C                                          WITHOUT PARCOM
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
      IF(NCSIZE.GT.1) CALL PARCOM(STRA03,2,MESH3)
!
      ENDIF
!
C     IF MONOTONICITY IS NOT ENSURED, REDUCTION OF TIME-STEP BY FACTOR ALFA
C     THE MINIMUM ON ALL POINTS WILL BE TAKEN
!
      ALFA = 1.D0
      IGUILT=1
      IF(OPTBAN.EQ.2) THEN
      DO IPOIN = 1,NPOIN3
         IF(TRA03(IPOIN).LT.0.D0.AND.MASKPT(IPOIN).GT.0.5D0) THEN
            IF(ABS(TRA01(IPOIN)-TRA03(IPOIN)).GT.EPS) THEN
C             CONSIDERING THAT THE NEW TIME-STEP WILL BE ALFA*DTJ
C             VOLU WILL BE AT THAT TIME ALFA*VOLU(N+1)+(1-ALFA)*VOLU(IN TRA01)
C             MAXIMUM POSSIBLE ALFA IS SUCH THAT VOLU+DB*ALFA*DTJ=0
C             HENCE THE FOLLOWING FORMULA :
              ALFA2=TRA01(IPOIN)/(TRA01(IPOIN)-TRA03(IPOIN))
              IF(ALFA.GT.ALFA2) THEN
                ALFA = ALFA2
                IGUILT=IPOIN
              ENDIF
            ENDIF
         ENDIF
      ENDDO
      ELSE
      DO IPOIN = 1,NPOIN3
C                                          TIDAL FLATS : VOLUN=0
         IF(TRA03(IPOIN).LT.0.D0.AND.TRA01(IPOIN).GT.EPS) THEN
            IF(ABS(TRA01(IPOIN)-TRA03(IPOIN)).GT.EPS) THEN
C             CONSIDERING THAT THE NEW TIME-STEP WILL BE ALFA*DTJ
C             VOLU WILL BE AT THAT TIME ALFA*VOLU(N+1)+(1-ALFA)*VOLU(IN TRA01)
C             MAXIMUM POSSIBLE ALFA IS SUCH THAT VOLU+DB*ALFA*DTJ=0
C             HENCE THE FOLLOWING FORMULA :
              ALFA2=TRA01(IPOIN)/(TRA01(IPOIN)-TRA03(IPOIN))
              IF(ALFA.GT.ALFA2) THEN
                ALFA = ALFA2
                IGUILT=IPOIN
              ENDIF
            ENDIF
         ENDIF
      ENDDO
      ENDIF
!
      IF(NCSIZE.GT.1) ALFA = P_DMIN(ALFA)
      DTJALFA=DTJ*ALFA
!
C     COMPUTES VOLU AFTER AN EXTRA ALFA*DTJ
!
      CALL OV('X=CX    ',TRA01,TRA01,TRA01,1.D0-ALFA,NPOIN3)
      CALL OV('X=X+CY  ',TRA01,VOLU2,VOLU2,     ALFA,NPOIN3)
!
      IF(CALFLU) THEN
        DO IPOIN = 1,NPOIN3
          FLUX = FLUX + DTJALFA*FC(IPOIN)*FLUEXT(IPOIN)
        ENDDO
        IF(NSCE.GT.0) THEN
          DO IS=1,NSCE
            IIS=IS
C           HERE IN PARALLEL SOURCES WITHOUT PARCOM
C           ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
            IF(NCSIZE.GT.1) IIS=IIS+NSCE
            DO IPOIN=1,NPOIN3
              IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
                FLUX=FLUX
     &              -DTJALFA*FSCE(IS)*SOURCES%ADR(IIS)%P%R(IPOIN)
              ELSE
                FLUX=FLUX
     &              -DTJALFA*FC(IPOIN)*SOURCES%ADR(IIS)%P%R(IPOIN)
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
C     ADVECTION DURING ALFA*DTJ
!
C     SOURCES (BUT WHEN INTAKE, FSCE=FC)
      IF(NSCE.GT.0) THEN
        DO IS=1,NSCE
          IF(OPTBAN.EQ.2) THEN
            DO IPOIN=1,NPOIN3
              IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IPOIN).GT.EPS) THEN
                FC(IPOIN)=FC(IPOIN)+DTJALFA*(FSCE(IS)-FC(IPOIN))
     &          *MAX(SOURCES%ADR(IS)%P%R(IPOIN),0.D0)/TRA01(IPOIN)
              ENDIF
            ENDDO
          ELSE
            DO IPOIN=1,NPOIN3
              IF(TRA01(IPOIN).GT.EPS) THEN
                FC(IPOIN)=FC(IPOIN)+DTJALFA*(FSCE(IS)-FC(IPOIN))
     &          *MAX(SOURCES%ADR(IS)%P%R(IPOIN),0.D0)/TRA01(IPOIN)
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
C     RAIN
      IF(RAIN) THEN
        DO IPOIN=1,NPOIN2
          IS=NPOIN3-NPOIN2+IPOIN
          IF(OPTBAN.EQ.2) THEN
            IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IS).GT.EPS) THEN
              FC(IS)=FC(IS)-DTJALFA*FC(IS)*PLUIE(IPOIN)/TRA01(IS)
            ENDIF
          ELSE
            IF(TRA01(IS).GT.EPS) THEN
              FC(IS)=FC(IS)-DTJALFA*FC(IS)*PLUIE(IPOIN)/TRA01(IS)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
C     FLUXES
!
      IF(S0F%TYPR.NE.'0') THEN
        DO IPOIN=1,NPOIN3
          IF(OPTBAN.EQ.2) THEN
          IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IPOIN).GT.EPS) THEN
            FC(IPOIN)=FC(IPOIN)+DTJALFA*
     &                     (S0F%R(IPOIN)+TRA02(IPOIN))/TRA01(IPOIN)
          ENDIF
          ELSE
          IF(TRA01(IPOIN).GT.EPS) THEN
            FC(IPOIN)=FC(IPOIN)+DTJALFA*
     &                     (S0F%R(IPOIN)+TRA02(IPOIN))/TRA01(IPOIN)
          ENDIF
          ENDIF
        ENDDO
      ELSE
        IF(OPTBAN.EQ.2) THEN
        DO IPOIN=1,NPOIN3
          IF(MASKPT(IPOIN).GT.0.5D0.AND.TRA01(IPOIN).GT.EPS) THEN
            FC(IPOIN)=FC(IPOIN)+DTJALFA*TRA02(IPOIN)/TRA01(IPOIN)
          ENDIF
        ENDDO
        ELSE
        DO IPOIN=1,NPOIN3
          IF(TRA01(IPOIN).GT.EPS) THEN
            FC(IPOIN)=FC(IPOIN)+DTJALFA*TRA02(IPOIN)/TRA01(IPOIN)
          ENDIF
        ENDDO
        ENDIF
      ENDIF
!
C     DTJ WAS THE REMAINING TIME, ALFA*DTJ HAS BEEN DONE, THE REST IS:
      DTJ = DTJ * (1.D0-ALFA)
      NITER = NITER + 1
      TOTITER = TOTITER + 1
      IF(NITER.GE.100) THEN
        WRITE (LU,*) 'MURD3D: ITERATION NO. REACHED ',NITER,', STOP.'
        WRITE (LU,*) 'ALFA = ',ALFA
        WRITE (LU,*) 'GUILTY POINT = ',IGUILT
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(ALFA.LT.1.D0) GOTO 10
!
!-----------------------------------------------------------------------
!
      IF(INFOR) THEN
        IF(LNG.EQ.1) WRITE(LU,101) SCHCF,NITER
        IF(LNG.EQ.2) WRITE(LU,102) SCHCF,NITER
      ENDIF
!
101   FORMAT(' MURD3D OPTION : ',1I2,'    ',1I4,' ITERATIONS')
102   FORMAT(' MURD3D OPTION: ',1I2,'    ',1I4,' ITERATIONS')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C