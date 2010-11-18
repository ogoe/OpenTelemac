C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GENERATES A RESULT FILE THAT REPRESENTS GRAPHICALLY
!>                THE MUD BED EVOLUTION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CFDEP, CFMAX, CONC, EPAI, EPAI0, FLUER, GIBSON, HDEP, IVIDE, LISPRD, LT, NCOUCH, NPF, NPFMAX, NPOIN2, NPOIN3, PDEPOT, PRIVE, RHOS, TA, TASSE, TEMP, WC, X, Y, ZF, ZR
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 5.4                                       </center>
!> </td><td> 06/05/93
!> </td><td> C LE NORMANT (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CFDEP
!></td><td>--></td><td>CONCENTRATION(G/L) OF FRESH DEPOSIT
!>    </td></tr>
!>          <tr><td>CFMAX
!></td><td>--></td><td>CONCENTRATION( G/L) OF CONSOLIDATED MUD
!>    </td></tr>
!>          <tr><td>CONC
!></td><td>--></td><td>CONCENTRATION OF MUD BED LAYER
!>                  (ONLY FOR MULTILAYER MODEL)
!>    </td></tr>
!>          <tr><td>EPAI
!></td><td>--></td><td>THICKNESS OF SOLID BED LAYER
!>    </td></tr>
!>          <tr><td>EPAI0
!></td><td>--></td><td>REFERENCE THICKNESS
!>                  FOR NEW GRID POINTS GENERATION
!>    </td></tr>
!>          <tr><td>FLUER
!></td><td>--></td><td>EROSION FLUX
!>    </td></tr>
!>          <tr><td>GIBSON
!></td><td>--></td><td>LOGICAL FOR GIBSON MODEL
!>    </td></tr>
!>          <tr><td>HDEP
!></td><td>--></td><td>THICKNESS OF FRESH DEPOSIT(FLUID MUD LAYER)
!>    </td></tr>
!>          <tr><td>IVIDE
!></td><td>--></td><td>VOID RATIO
!>                  (GIBSON MODEL ONLY)
!>    </td></tr>
!>          <tr><td>LISPRD
!></td><td>--></td><td>TIME STEP FOR GRAPHICAL PRINTOUTS
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>NUMBER OF TIME STEP
!>    </td></tr>
!>          <tr><td>NCOUCH
!></td><td>--></td><td>NUMBER OF BED LAYERS
!>                  (MULTILAYE CONSOLIDATION MODEL)
!>    </td></tr>
!>          <tr><td>NPF
!></td><td>--></td><td>NUMBER OF POINTS WITHIN THE BED ALONG THE VERTICAL
!>    </td></tr>
!>          <tr><td>NPFMAX
!></td><td>--></td><td>MAXIMUM NUMBER OF HORIZONTAL PLANES
!>                  WITHIN THE MUD BED (GIBSON MODEL)
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF POINTS IN 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NUMBER OF POINTS IN 3D
!>    </td></tr>
!>          <tr><td>PDEPOT
!></td><td>--></td><td>PROBABILITY OF DEPOSITION
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>PRIVATE ARRAYS FOR USERS
!>    </td></tr>
!>          <tr><td>RHOS
!></td><td>--></td><td>SEDIMENT DENSITY
!>    </td></tr>
!>          <tr><td>TA
!></td><td>--></td><td>SUSPENDED SEDIMENT CONCENTRATION (G/L)
!>    </td></tr>
!>          <tr><td>TASSE
!></td><td>--></td><td>LOGICAL FOR MULTILAYER CONSOLIDATION MODEL
!>    </td></tr>
!>          <tr><td>TEMP
!></td><td>--></td><td>TIME COUNTER FOR CONSOLIDATION MODEL
!>                  (MULTILAYER MODEL ONLY)
!>    </td></tr>
!>          <tr><td>WC
!></td><td>--></td><td>SETTLING VELOCITY (M/S)
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDINATES OF 2D MESH
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>BOTTOM LEVEL
!>    </td></tr>
!>          <tr><td>ZR
!></td><td>--></td><td>RIGID BED LEVEL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                              SUBROUTINE IMPSED
     &(IVIDE , EPAI  , CONC  , TEMP  , HDEP  , PDEPOT,
     & FLUER , ZR    , ZF    , TA    , WC    , X     ,
     & Y     , NPOIN2, NPOIN3, NPFMAX, NCOUCH, NPF   ,
     & LT    , RHOS  , CFMAX , CFDEP , EPAI0 ,
     & TASSE , GIBSON, PRIVE , LISPRD)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CFDEP          |-->| CONCENTRATION(G/L) OF FRESH DEPOSIT
C| CFMAX          |-->| CONCENTRATION( G/L) OF CONSOLIDATED MUD
C| CONC           |-->| CONCENTRATION OF MUD BED LAYER
C|                |   | (ONLY FOR MULTILAYER MODEL)
C| EPAI           |-->| THICKNESS OF SOLID BED LAYER
C| EPAI0          |-->| REFERENCE THICKNESS
C|                |   | FOR NEW GRID POINTS GENERATION
C| FLUER          |-->| EROSION FLUX
C| GIBSON         |-->| LOGICAL FOR GIBSON MODEL
C| HDEP           |-->| THICKNESS OF FRESH DEPOSIT(FLUID MUD LAYER)
C| IVIDE          |-->| VOID RATIO
C|                |   | (GIBSON MODEL ONLY)
C| LISPRD         |-->| TIME STEP FOR GRAPHICAL PRINTOUTS
C| LT             |-->| NUMBER OF TIME STEP
C| NCOUCH         |-->| NUMBER OF BED LAYERS
C|                |   | (MULTILAYE CONSOLIDATION MODEL)
C| NPF            |-->| NUMBER OF POINTS WITHIN THE BED ALONG THE VERTICAL
C| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES
C|                |   | WITHIN THE MUD BED (GIBSON MODEL)
C| NPOIN2         |-->| NUMBER OF POINTS IN 2D
C| NPOIN3         |-->| NUMBER OF POINTS IN 3D
C| PDEPOT         |-->| PROBABILITY OF DEPOSITION
C| PRIVE          |-->| PRIVATE ARRAYS FOR USERS
C| RHOS           |-->| SEDIMENT DENSITY
C| TA             |-->| SUSPENDED SEDIMENT CONCENTRATION (G/L)
C| TASSE          |-->| LOGICAL FOR MULTILAYER CONSOLIDATION MODEL
C| TEMP           |-->| TIME COUNTER FOR CONSOLIDATION MODEL
C|                |   | (MULTILAYER MODEL ONLY)
C| WC             |-->| SETTLING VELOCITY (M/S)
C| X,Y            |-->| COORDINATES OF 2D MESH
C| ZF             |-->| BOTTOM LEVEL
C| ZR             |-->| RIGID BED LEVEL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN2,NPOIN3,NPFMAX,NCOUCH,LT,LISPRD
!
      DOUBLE PRECISION, INTENT(IN)  :: EPAI(NPFMAX-1,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: HDEP(NPOIN2) , PDEPOT(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: FLUER(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: CONC(NCOUCH)
      DOUBLE PRECISION, INTENT(IN)  :: TEMP(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: ZR(NPOIN2), ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)  :: TA(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)  :: WC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)  :: X(NPOIN3), Y(NPOIN3)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: PRIVE
!
      DOUBLE PRECISION, INTENT(IN)  :: RHOS,CFMAX,CFDEP,EPAI0
!
      INTEGER, INTENT(IN) :: NPF(NPOIN2)
!
      LOGICAL, INTENT(IN) :: TASSE , GIBSON
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTRINSIC MOD
!
!=======================================================================
!
      IF (MOD(LT,LISPRD).NE.0) RETURN
!
C      PRINTOUTS SPECIFIED BY USER
!
      RETURN
      END SUBROUTINE IMPSED

C
C#######################################################################
C