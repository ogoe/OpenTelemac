C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES TYPES OF 3D BOUNDARY CONDITIONS.
!><br>            SETS THE VALUE OF SOME COEFFICIENTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::AEBORF AEBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AEBORL AEBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AEBORS AEBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AKBORF AKBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AKBORL AKBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AKBORS AKBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ATABOF ATABOF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ATABOL ATABOL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ATABOS ATABOS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AUBORF AUBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AUBORL AUBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AUBORS AUBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AVBORF AVBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AVBORS AVBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AWBORF AWBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AWBORL AWBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AWBORS AWBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BEBORF BEBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BEBORL BEBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BEBORS BEBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BKBORF BKBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BKBORL BKBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BKBORS BKBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BTABOF BTABOF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BTABOL BTABOL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BTABOS BTABOS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BUBORF BUBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BUBORL BUBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BUBORS BUBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BVBORF BVBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BVBORL BVBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BVBORS BVBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BWBORF BWBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BWBORL BWBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BWBORS BWBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ITURBV ITURBV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIPBOF LIPBOF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIPBOL LIPBOL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIPBOS LIPBOS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LITABF LITABF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LITABL LITABL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LITABS LITABS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIUBOF LIUBOF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIUBOL LIUBOL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIUBOS LIUBOS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIVBOF LIVBOF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIVBOL LIVBOL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIVBOS LIVBOS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIWBOF LIWBOF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIWBOL LIWBOL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LIWBOS LIWBOS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NONHYD NONHYD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPTFR2 NPTFR2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPTFR3 NPTFR3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NTRAC NTRAC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PBORF PBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PBORL PBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PBORS PBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RUGOL RUGOL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RUGOL0 RUGOL0@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TABORF TABORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TABORL TABORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TABORS TABORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UBORF UBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UBORL UBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UBORS UBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VBORF VBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VBORL VBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VBORS VBORS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::WBORF WBORF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::WBORL WBORL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::WBORS WBORS@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::KENT KENT@endlink, 
!> @link DECLARATIONS_TELEMAC::KLOG KLOG@endlink, 
!> @link DECLARATIONS_TELEMAC::KSORT KSORT@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IPLAN, IPOIN2, IPTFR, IPTFR3, ITRAC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV()
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
!>      <td><center> 6.0                                       </center>
!> </td><td>
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
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
!>          <tr><td>A,B)PBOR(F,L,S
!></td><td><--</td><td>NEUMANN BC VALUES FOR DYNAMIC PRESSURE
!>    </td></tr>
!>          <tr><td>A,B)WBOR(F,L,S
!></td><td><--</td><td>NEUMANN BC VALUES FOR W VELOCITY COMPONENT
!>    </td></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TIME
!>    </td></tr>
!>          <tr><td>ATABO,BTABO
!></td><td><--</td><td>LOG LAW: NU*DTA/DN = ATABO*TA + BTABO
!>                  TYPES OF BOUNDARY CONDITIONS
!>    </td></tr>
!>          <tr><td>AUBOR,BUBOR
!></td><td><--</td><td>LOG LAW: NU*DU/DN = AUBOR*U + BUBOR
!>    </td></tr>
!>          <tr><td>AUBOR,BVBOR
!></td><td><--</td><td>LOG LAW: NU*DV/DN = AVBOR*V + BVBOR
!>    </td></tr>
!>          <tr><td>COTIMP
!></td><td>--></td><td>ARRAY OF PRESCRIBED ELEVATIONS
!>    </td></tr>
!>          <tr><td>DEBIMP
!></td><td>--></td><td>ARRAY OF PRESCRIBED DISCHARGES
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>TIME-STEP
!>    </td></tr>
!>          <tr><td>FAIR
!></td><td>--></td><td>DRAG COEFFICIENT OF WIND
!>    </td></tr>
!>          <tr><td>HBOR
!></td><td>--></td><td>PRESCRIBED DEPTH ON LATERAL BOUNDARIES
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>DEPTH AT TIME TN
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td>--></td><td>CONNECTIVITY TABLE IN 3D
!>    </td></tr>
!>          <tr><td>ITURBV
!></td><td>--></td><td>TURBULENCE MODEL (1:LAMINAR 2: MIXING LENGTH..)
!>    </td></tr>
!>          <tr><td>KADH
!></td><td>--></td><td>NO SLIP CONDITION
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>--></td><td>PRESCRIBED VALUE
!>    </td></tr>
!>          <tr><td>KENTU
!></td><td>--></td><td>PRESCRIBED VELOCITY
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>SOLID BOUNDARY
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>IN 2D : NEXT POINT ON THE BOUNDARY
!>                  (BOUNDARY NUMBERING)
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>--></td><td>FREE (E.G. AT AN OUTPUT)
!>    </td></tr>
!>          <tr><td>LIHBOR
!></td><td>--></td><td>TYPE OF BOUNDARY CONDITIONS ON DEPTH
!>    </td></tr>
!>          <tr><td>LIPBO(F,L,S)
!></td><td><--</td><td>BC TYPE FOR DYNAMIC PRESSURE
!>    </td></tr>
!>          <tr><td>LITA,BF
!></td><td><-></td><td>ON BOTTOM FOR TRACERS
!>    </td></tr>
!>          <tr><td>LITA,BL
!></td><td><-></td><td>ON LATERAL BOUNDARIES FOR TRACERS
!>    </td></tr>
!>          <tr><td>LITA,BS
!></td><td><-></td><td>AT FREE SURFACE FOR TRACERS
!>    </td></tr>
!>          <tr><td>LIU,V,WBOF
!></td><td><-></td><td>ON BOTTOM FOR U,V,W
!>    </td></tr>
!>          <tr><td>LIU,V,WBOL
!></td><td><-></td><td>ON LATERAL BOUNDARIES FOR U,V,W
!>    </td></tr>
!>          <tr><td>LIU,V,WBOS
!></td><td><-></td><td>AT FREE SURFACE FOR U,V,W
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>CURRENT NUMBER OF TIME-STEP
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>GLOBAL NUMBER OF 2D BOUNDARY POINTS
!>    </td></tr>
!>          <tr><td>NCOTE
!></td><td>--></td><td>NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
!>    </td></tr>
!>          <tr><td>NDEBIT
!></td><td>--></td><td>NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NUMBER OF ELEMENTS IN 2D
!>    </td></tr>
!>          <tr><td>NELEM3
!></td><td>--></td><td>NUMBER OF 3D ELEMENTS
!>    </td></tr>
!>          <tr><td>NETAGE
!></td><td>--></td><td>NUMBER OF LAYERS OF 3D ELEMENTS
!>    </td></tr>
!>          <tr><td>NONHYD
!></td><td>--></td><td>NON-HYDROSTATIC FLAG
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NUMBER OF PLANES ON THE VERTICAL
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF POINTS IN 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NUMBER OF POINTS IN 3D
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td>--></td><td>NUMBER OF ARRAYS IN BLOCK PRIVE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NUMBER OF BOUNDARY POINTS IN 2D
!>    </td></tr>
!>          <tr><td>NPTFR3
!></td><td>--></td><td>NUMBER OF BOUNDARY POINTS IN 3D
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NUMBER OF TRACERS
!>    </td></tr>
!>          <tr><td>NVIT
!></td><td>--></td><td>NUMBER OF BOUNDARIES WITH PRESCRIBED VELOCITY
!>    </td></tr>
!>          <tr><td>PBOR(F,L,S)
!></td><td><--</td><td>DIRICHLET BC VALUES FOR DYNAMIC PRESSURE
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>BLOCK OF ARRAYS FOR THE USER
!>    </td></tr>
!>          <tr><td>SEDI
!></td><td>--></td><td>IF YES, THERE IS SEDIMENT
!>    </td></tr>
!>          <tr><td>TA
!></td><td>--></td><td>CONCENTRATION OF TRACERS
!>    </td></tr>
!>          <tr><td>TABORF
!></td><td><--</td><td>PRESCRIBED TRACERS ON THE BOTTOM
!>    </td></tr>
!>          <tr><td>TABORL
!></td><td><--</td><td>PRESCRIBED TRACERS ON THE LATERAL BOUNDARY
!>    </td></tr>
!>          <tr><td>TABORS
!></td><td><--</td><td>PRESCRIBED TRACERS AT FREE SURFACE
!>                  LOGARITHMIC LAWS : AUBORF,L,S AND BUBORF,L,S
!>                  FOR VELOCITIES
!>                  ATABO,L,S AND BTABO,L,S
!>                  FOR TRACERS
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPONENTS OF VELOCITY
!>    </td></tr>
!>          <tr><td>UBORF
!></td><td><--</td><td>PRESCRIBED VELOCITY ALONG X ON THE BOTTOM
!>    </td></tr>
!>          <tr><td>UBORL
!></td><td><--</td><td>PRESCRIBED VELOCITY ALONG X ON THE LATERAL
!>                  BOUNDARY
!>    </td></tr>
!>          <tr><td>UBORS
!></td><td><--</td><td>PRESCRIBED VELOCITY ALONG X AT FREE SURFACE
!>    </td></tr>
!>          <tr><td>UMOY,VMOY
!></td><td>--></td><td>DEPTH AVERAGED VELOCITY
!>    </td></tr>
!>          <tr><td>VBORF
!></td><td><--</td><td>PRESCRIBED VELOCITY ALONG Y ON THE BOTTOM
!>    </td></tr>
!>          <tr><td>VBORL
!></td><td><--</td><td>PRESCRIBED VELOCITY ALONG Y ON THE LATERAL
!>                  BOUNDARY
!>    </td></tr>
!>          <tr><td>VBORS
!></td><td><--</td><td>PRESCRIBED VELOCITY ALONG Y AT FREE SURFACE
!>    </td></tr>
!>          <tr><td>VENT
!></td><td>--></td><td>WITH WIND (.TRUE.) OR WITHOUT (.FALSE.)
!>    </td></tr>
!>          <tr><td>VENTX
!></td><td>--></td><td>WIND VELOCITY ALONG X
!>    </td></tr>
!>          <tr><td>VENTY
!></td><td>--></td><td>WIND VELOCITY ALONG Y
!>    </td></tr>
!>          <tr><td>VITIMP
!></td><td>--></td><td>ARRAY OF PRESCRIBED VELOCITIES
!>    </td></tr>
!>          <tr><td>WBORF
!></td><td><--</td><td>PRESCRIBED VELOCITY ALONG Z ON THE BOTTOM
!>    </td></tr>
!>          <tr><td>WBORL
!></td><td><--</td><td>PRESCRIBED VELOCITY ALONG Z ON THE LATERAL
!>                  BOUNDARY
!>    </td></tr>
!>          <tr><td>WBORS
!></td><td><--</td><td>PRESCRIBED VELOCITY ALONG Z AT FREE SURFACE
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>MESH COORDINATES
!>    </td></tr>
!>          <tr><td>XSGBOR,YSGBOR
!></td><td>--></td><td>2D NORMAL VECTORS TO THE SEGMENTS
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>BOTTOM ELEVATION
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE LIMI3D
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A,B)PBOR(F,L,S |<--| NEUMANN BC VALUES FOR DYNAMIC PRESSURE
C| A,B)WBOR(F,L,S |<--| NEUMANN BC VALUES FOR W VELOCITY COMPONENT
C| AT             |-->| TIME
C| ATABO,BTABO    |<--| LOG LAW: NU*DTA/DN = ATABO*TA + BTABO
C|                |   | TYPES OF BOUNDARY CONDITIONS
C| AUBOR,BUBOR    |<--| LOG LAW: NU*DU/DN = AUBOR*U + BUBOR
C| AUBOR,BVBOR    |<--| LOG LAW: NU*DV/DN = AVBOR*V + BVBOR
C| COTIMP         |-->| ARRAY OF PRESCRIBED ELEVATIONS
C| DEBIMP         |-->| ARRAY OF PRESCRIBED DISCHARGES
C| DT             |-->| TIME-STEP
C| FAIR           |-->| DRAG COEFFICIENT OF WIND
C| HBOR           |-->| PRESCRIBED DEPTH ON LATERAL BOUNDARIES
C| HN             |-->| DEPTH AT TIME TN
C| IKLE3          |-->| CONNECTIVITY TABLE IN 3D
C| ITURBV         |-->| TURBULENCE MODEL (1:LAMINAR 2: MIXING LENGTH..)
C| KADH           |-->| NO SLIP CONDITION
C| KENT           |-->| PRESCRIBED VALUE
C| KENTU          |-->| PRESCRIBED VELOCITY
C| KLOG           |-->| SOLID BOUNDARY
C| KP1BOR         |-->| IN 2D : NEXT POINT ON THE BOUNDARY
C|                |   | (BOUNDARY NUMBERING)
C| KSORT          |-->| FREE (E.G. AT AN OUTPUT)
C| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
C| LIPBO(F,L,S)   |<--| BC TYPE FOR DYNAMIC PRESSURE
C| LITA,BF        |<->| ON BOTTOM FOR TRACERS
C| LITA,BL        |<->| ON LATERAL BOUNDARIES FOR TRACERS
C| LITA,BS        |<->| AT FREE SURFACE FOR TRACERS
C| LIU,V,WBOF     |<->| ON BOTTOM FOR U,V,W
C| LIU,V,WBOL     |<->| ON LATERAL BOUNDARIES FOR U,V,W
C| LIU,V,WBOS     |<->| AT FREE SURFACE FOR U,V,W
C| LT             |-->| CURRENT NUMBER OF TIME-STEP
C| NBOR           |-->| GLOBAL NUMBER OF 2D BOUNDARY POINTS
C| NCOTE          |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED ELEVATION
C| NDEBIT         |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED DISCHARGE
C| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
C| NELEM3         |-->| NUMBER OF 3D ELEMENTS
C| NETAGE         |-->| NUMBER OF LAYERS OF 3D ELEMENTS
C| NONHYD         |-->| NON-HYDROSTATIC FLAG
C| NPLAN          |-->| NUMBER OF PLANES ON THE VERTICAL
C| NPOIN2         |-->| NUMBER OF POINTS IN 2D
C| NPOIN3         |-->| NUMBER OF POINTS IN 3D
C| NPRIV          |-->| NUMBER OF ARRAYS IN BLOCK PRIVE
C| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN 2D
C| NPTFR3         |-->| NUMBER OF BOUNDARY POINTS IN 3D
C| NTRAC          |-->| NUMBER OF TRACERS
C| NVIT           |-->| NUMBER OF BOUNDARIES WITH PRESCRIBED VELOCITY
C| PBOR(F,L,S)    |<--| DIRICHLET BC VALUES FOR DYNAMIC PRESSURE
C| PRIVE          |-->| BLOCK OF ARRAYS FOR THE USER
C| SEDI           |-->| IF YES, THERE IS SEDIMENT
C| TA             |-->| CONCENTRATION OF TRACERS
C| TABORF         |<--| PRESCRIBED TRACERS ON THE BOTTOM
C| TABORL         |<--| PRESCRIBED TRACERS ON THE LATERAL BOUNDARY
C| TABORS         |<--| PRESCRIBED TRACERS AT FREE SURFACE
C|                |   | LOGARITHMIC LAWS : AUBORF,L,S AND BUBORF,L,S
C|                |   | FOR VELOCITIES
C|                |   | ATABO,L,S AND BTABO,L,S
C|                |   | FOR TRACERS
C| U,V,W          |-->| COMPONENTS OF VELOCITY
C| UBORF          |<--| PRESCRIBED VELOCITY ALONG X ON THE BOTTOM
C| UBORL          |<--| PRESCRIBED VELOCITY ALONG X ON THE LATERAL
C|                |   | BOUNDARY
C| UBORS          |<--| PRESCRIBED VELOCITY ALONG X AT FREE SURFACE
C| UMOY,VMOY      |-->| DEPTH AVERAGED VELOCITY
C| VBORF          |<--| PRESCRIBED VELOCITY ALONG Y ON THE BOTTOM
C| VBORL          |<--| PRESCRIBED VELOCITY ALONG Y ON THE LATERAL
C|                |   | BOUNDARY
C| VBORS          |<--| PRESCRIBED VELOCITY ALONG Y AT FREE SURFACE
C| VENT           |-->| WITH WIND (.TRUE.) OR WITHOUT (.FALSE.)
C| VENTX          |-->| WIND VELOCITY ALONG X
C| VENTY          |-->| WIND VELOCITY ALONG Y
C| VITIMP         |-->| ARRAY OF PRESCRIBED VELOCITIES
C| WBORF          |<--| PRESCRIBED VELOCITY ALONG Z ON THE BOTTOM
C| WBORL          |<--| PRESCRIBED VELOCITY ALONG Z ON THE LATERAL
C|                |   | BOUNDARY
C| WBORS          |<--| PRESCRIBED VELOCITY ALONG Z AT FREE SURFACE
C| X,Y,Z          |-->| MESH COORDINATES
C| XSGBOR,YSGBOR  |-->| 2D NORMAL VECTORS TO THE SEGMENTS
C| ZF             |-->| BOTTOM ELEVATION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!-----------------------------------------------------------------------
!
      INTEGER IPOIN2, IPLAN, IPTFR, IPTFR3, ITRAC
!
!***********************************************************************
!
C     BOUNDARY CONDITIONS ON VELOCITIES
!     *********************************
!
C     BOTTOM
!     ======
!
C     DEFAULT: IMPERMEABILITY AND NO FRICTION (SEE ALSO BORD3D)
!
      DO IPOIN2 = 1,NPOIN2
         LIUBOF%I(IPOIN2) = KLOG
         LIVBOF%I(IPOIN2) = KLOG
         LIWBOF%I(IPOIN2) = KLOG
         UBORF%R(IPOIN2)  = 0.D0
         VBORF%R(IPOIN2)  = 0.D0
         WBORF%R(IPOIN2)  = 0.D0
C        AUBORF%R(IPOIN2) = 0.D0
C        AVBORF%R(IPOIN2) = 0.D0
C        BUBORF%R(IPOIN2) = 0.D0
C        BVBORF%R(IPOIN2) = 0.D0
      ENDDO
      AUBORF%TYPR='0'
      AVBORF%TYPR='0'
      BUBORF%TYPR='0'
      BVBORF%TYPR='0'
!
      IF(NONHYD) THEN
C       DO IPOIN2=1,NPOIN2
C         AWBORF%R(IPOIN2) = 0.D0
C         BWBORF%R(IPOIN2) = 0.D0
C       ENDDO
        AWBORF%TYPR='0'
        BWBORF%TYPR='0'
      ENDIF
!
C     LATERAL BOUNDARIES
!     ==================
!
C     DEFAULT: 2D CONDITIONS DUPLICATED ON THE VERTICAL
C              FREE FOR W
C              NO FRICTION
!
      DO IPLAN = 2,NPLAN
         DO IPTFR = 1,NPTFR2
            IPTFR3 = (IPLAN-1)*NPTFR2 + IPTFR
            LIUBOL%I(IPTFR3) = LIUBOL%I(IPTFR)
            LIVBOL%I(IPTFR3) = LIVBOL%I(IPTFR)
            UBORL%R(IPTFR3)  = UBORL%R(IPTFR)
            VBORL%R(IPTFR3)  = VBORL%R(IPTFR)
            AUBORL%R(IPTFR3) = AUBORL%R(IPTFR)
         ENDDO
      ENDDO
!
      DO IPTFR3 = 1,NPTFR3
C                           KSORT: W FREE ON LATERAL BOUNDARIES
         LIWBOL%I(IPTFR3) = KSORT
         WBORL%R(IPTFR3)  = 0.D0
C        BUBORL%R(IPTFR3) = 0.D0
C        BVBORL%R(IPTFR3) = 0.D0
      ENDDO
      BUBORL%TYPR='0'
      BVBORL%TYPR='0'
!
      IF(NONHYD) THEN
C       DO IPTFR3 = 1,NPTFR3
C         AWBORL%R(IPTFR3) = 0.D0
C         BWBORL%R(IPTFR3) = 0.D0
C       ENDDO
        AWBORL%TYPR='0'
        BWBORL%TYPR='0'
      ENDIF
!
C     FREE SURFACE
!     ============
!
C     DEFAULT: IMPERMEABILITY AND NO FRICTION (SEE ALSO BORD3D)
!
      DO IPOIN2 = 1,NPOIN2
         LIUBOS%I(IPOIN2) = KLOG
         LIVBOS%I(IPOIN2) = KLOG
         LIWBOS%I(IPOIN2) = KLOG
         UBORS%R(IPOIN2)  = 0.D0
         VBORS%R(IPOIN2)  = 0.D0
         WBORS%R(IPOIN2)  = 0.D0
C        AUBORS%R(IPOIN2) = 0.D0
C        BUBORS%R(IPOIN2) = 0.D0
C        BVBORS%R(IPOIN2) = 0.D0
      ENDDO
      AUBORS%TYPR='0'
      BUBORS%TYPR='0'
      AVBORS%TYPR='0'
      BVBORS%TYPR='0'
!
      IF(NONHYD) THEN
C       DO IPOIN2 = 1,NPOIN2
C         AWBORS%R(IPOIN2) = 0.D0
C         BWBORS%R(IPOIN2) = 0.D0
C       ENDDO
        AWBORS%TYPR='0'
        BWBORS%TYPR='0'
      ENDIF
!
!     **************
C     TRACERS BC'S
!     **************
!
      IF (NTRAC.NE.0) THEN
         DO ITRAC = 1,NTRAC
!
C     BOTTOM
!     ======
!
C     DEFAULT: NEUMANN BC'S
!
            DO IPOIN2 = 1,NPOIN2
               LITABF%ADR(ITRAC)%P%I(IPOIN2) = KLOG
               TABORF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
C              ATABOF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
C              BTABOF%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
            ENDDO
            ATABOF%ADR(ITRAC)%P%TYPR='0'
            BTABOF%ADR(ITRAC)%P%TYPR='0'
!
C     SIDES
!     =====
!
C     DEFAULT: NEUMANN BC'S
!
C           WHAT HAS BEEN READ IN THE BOUNDARY CONDITIONS FILE
C           FOR 1 TRACER IS DUPLICATED ON THE VERTICAL AND FOR
C           ALL TRACERS
!
            DO IPLAN = 1,NPLAN
              DO IPTFR = 1,NPTFR2
                IPTFR3 = (IPLAN-1)*NPTFR2 + IPTFR
                LITABL%ADR(ITRAC)%P%I(IPTFR3) = LITABL%ADR(1)%P%I(IPTFR)
                TABORL%ADR(ITRAC)%P%R(IPTFR3) = TABORL%ADR(1)%P%R(IPTFR)
C               ATABOL%ADR(ITRAC)%P%R(IPTFR3) = ATABOL%ADR(1)%P%R(IPTFR)
C               BTABOL%ADR(ITRAC)%P%R(IPTFR3) = BTABOL%ADR(1)%P%R(IPTFR)
              ENDDO
            ENDDO
            ATABOL%ADR(ITRAC)%P%TYPR='0'
            BTABOL%ADR(ITRAC)%P%TYPR='0'
!
C     FREE SURFACE
!     =============
!
C     DEFAULT: NEUMANN BC'S
!
            DO IPOIN2 = 1,NPOIN2
               LITABS%ADR(ITRAC)%P%I(IPOIN2) = KLOG
               TABORS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
C              ATABOS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
C              BTABOS%ADR(ITRAC)%P%R(IPOIN2) = 0.D0
            ENDDO
            ATABOS%ADR(ITRAC)%P%TYPR='0'
            BTABOS%ADR(ITRAC)%P%TYPR='0'
!
         ENDDO
      ENDIF
!
C     SOLID BOUNDARIES FOR K AND EPSILON
!     **********************************
!
      IF(ITURBV.EQ.3.OR.ITURBV.EQ.7) THEN
!
C     BOTTOM
!     ======
!
C     DEFAULT : NO GRADIENT
!
         DO IPOIN2 = 1,NPOIN2
           AKBORF%R(IPOIN2) = 0.D0
           BKBORF%R(IPOIN2) = 0.D0
           AEBORF%R(IPOIN2) = 0.D0
           BEBORF%R(IPOIN2) = 0.D0
         ENDDO
         AKBORF%TYPR = '0'
         BKBORF%TYPR = '0'
         AEBORF%TYPR = '0'
         BEBORF%TYPR = '0'
!
C     SIDES
!     =====
!
C     DEFAULT : NO GRADIENT
!
         DO IPTFR3 = 1,NPTFR3
           AKBORL%R(IPTFR3) = 0.D0
           BKBORL%R(IPTFR3) = 0.D0
           AEBORL%R(IPTFR3) = 0.D0
           BEBORL%R(IPTFR3) = 0.D0
         ENDDO
!
C     FREE SURFACE
!     ============
!
C     DEFAULT : NO GRADIENT
!
         DO IPOIN2 = 1,NPOIN2
            AKBORS%R(IPOIN2) = 0.D0
            BKBORS%R(IPOIN2) = 0.D0
            AEBORS%R(IPOIN2) = 0.D0
            BEBORS%R(IPOIN2) = 0.D0
         ENDDO
!
      ENDIF
!
!
C     FRICTION COEFFICIENTS
!     *********************
!
C     DEFAULT: VALUE GIVEN IN STEERING FILE
!
      CALL OV('X=C     ',RUGOL%R,RUGOL%R,RUGOL%R,RUGOL0,NPTFR2*NPLAN)
!
!======================================================================
C DEFAULT BOUNDARY CONDITION TYPES AND VALUES FOR THE
C PRESSURE POISSON EQUATION
!======================================================================
!
      IF(NONHYD) THEN
!
!-----------------------------------------------------------------------
!
C DEFAULT TYPES AND VALUES FOR THE PRESSURE BOUNDARY CONDITIONS
C BOTTOM AND FREE SURFACE
!
C       AT ALL LATERAL BOUNDARIES AND BOTTOM DP/DN = 0;
C       DIRICHLET = 0 AT THE SURFACE; DIRICHLET CONDITIONS SET TO 0 ALL OVER
C       (CORRECT AT THE SURFACE ONLY)
!
C       BOTTOM AND SURFACE
C       CHECK KLOG BOTTOM
!
        DO IPOIN2=1,NPOIN2
          LIPBOF%I(IPOIN2) = KLOG
          LIPBOS%I(IPOIN2) = KENT
          PBORF%R(IPOIN2)  = 0.D0
          PBORS%R(IPOIN2)  = 0.D0
        ENDDO
!
C       LATERAL SURFACES: ALL TREATED AS NEUMANN
!
        DO IPTFR3=1,NPTFR3
          LIPBOL%I(IPTFR3) = KLOG
          PBORL%R(IPTFR3)  = 0.D0
        ENDDO
!
C       LATERAL SURFACES: DIRICHLET ON ENTRANCES, NEUMANN ELSEWHERE
!
C       DO IPTFR3=1,NPTFR3
C         PBORL%R(IPTFR3)  = 0.D0
C         IF(LIUBOL%I(IPTFR3).EQ.KENT.OR.
C    *       LIUBOL%I(IPTFR3).EQ.KENTU) THEN
C           LIPBOL%I(IPTFR3) = KENT
C         ELSE
C           LIPBOL%I(IPTFR3) = KLOG
C         ENDIF
C       ENDDO
!
      ENDIF
!
!======================================================================
!
      RETURN
      END
C
C#######################################################################
C