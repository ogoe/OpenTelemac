C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note     UBOR AND VBOR NOT USED (JMH)
!>  @note     PORTABILITY: CRAY

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AMINF, EPS, G, KDDL, KDIR, KNEU, LIMPRO, NBOR, NPOIN, NPTFR, UBOR, VBOR, W, XNEBOR, YNEBOR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> HI, HJ, IEL, K, PI, R, R1, RLAMB0, UI, UJ, VI, VJ, XN, YN
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>RESOLU()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 09/09/1994
!> </td><td> N. GOUTAL
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AMINF
!></td><td><--</td><td>A-(WINF,NINF).WINF AVEC  /NINF/ = 1
!>    </td></tr>
!>          <tr><td>EPS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>G
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KDDL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KNEU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMPRO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NDIM
!></td><td>--></td><td>DIMENSION DE L'ESPACE.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VALBOR
!></td><td>--></td><td>VALBOR I.E. (RHO,U,V,P) INITIAL A L'INFINI
!>    </td></tr>
!>          <tr><td>VBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XNEBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YNEBOR
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLUSEW
     &(AMINF,UBOR,VBOR,NPOIN,EPS,G,W,
     & XNEBOR,YNEBOR,NPTFR,LIMPRO,NBOR,KDIR,KNEU,KDDL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AMINF          |<--| A-(WINF,NINF).WINF AVEC  /NINF/ = 1
C| EPS            |---| 
C| G             |---| 
C| KDDL           |---| 
C| KDIR           |---| 
C| KNEU           |---| 
C| LIMPRO         |---| 
C| NBOR           |---| 
C| NDIM           |-->| DIMENSION DE L'ESPACE.
C| NPOIN          |---| 
C| NPTFR          |---| 
C| UBOR           |---| 
C| VALBOR         |-->| VALBOR I.E. (RHO,U,V,P) INITIAL A L'INFINI
C| VBOR           |---| 
C| W             |---| 
C| XNEBOR         |---| 
C| YNEBOR         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,KDIR,KNEU,KDDL
      INTEGER, INTENT(IN)             :: LIMPRO(NPTFR,6),NBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: XNEBOR(NPTFR),YNEBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: UBOR(NPTFR),VBOR(NPTFR),EPS,G
      DOUBLE PRECISION, INTENT(INOUT) :: AMINF(3,NPTFR)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IEL,K
C
      DOUBLE PRECISION HI,UI,VI,XN,YN,R1,RLAMB0,HJ,UJ,VJ,R,PI
C
C------
C 1. COMPUTES FLUXES AT THE INFLOW BOUNDARIES
C------
C
      DO 10 K = 1 , NPTFR
C
C     IF H IS FREE OR DISCHARGE IS FREE
      IF(LIMPRO(K,1).EQ.KDDL.OR.LIMPRO(K,2).EQ.KDDL) THEN
      IEL = NBOR(K)
      XN = XNEBOR(K)
      YN = YNEBOR(K)
      HJ = W(1,IEL)
      IF(HJ.GT.EPS) THEN
        UJ = W(2,IEL)/HJ
        VJ = W(3,IEL)/HJ
        R  =  UJ*XN + VJ*YN-2.D0*SQRT(G*HJ)
        R1 =  UJ*XN + VJ*YN+2.D0*SQRT(G*HJ)
C
C       IF DISCHARGE IMPOSED
C
        IF(LIMPRO(K,2).EQ.KDIR) THEN
C
C   Q GIVEN; COMPUTES H FOR A SUBCRITICAL INFLOW BOUNDARY
C
          RLAMB0 = UJ * XN + VJ * YN
          IF ( RLAMB0.LE.0.D0) THEN
            PI = -R+RLAMB0
            HI = (PI**2/4.D0)/G
            UI = AMINF(2,K)/HI
            VI = AMINF(3,K)/HI
            AMINF(1,K) = HI
          ENDIF
        ENDIF
C
C       IF H IMPOSED
C
        IF(LIMPRO(K,1).EQ.KDIR) THEN
C
C   H GIVEN; COMPUTES Q FOR A SUBCRITICAL OUTFLOW BOUNDARY
C
           HI = AMINF(1,K)
C
           RLAMB0 = (UJ * XN) + (VJ * YN)
           IF ( RLAMB0.GE.-0.0001D0) THEN
             UI = (R1-2.D0*SQRT(G*HI))*XN
             VI = (R1-2.D0*SQRT(G*HI))*YN
             AMINF(2,K) = UI*HI
             AMINF(3,K) = VI*HI
           ENDIF
         ENDIF
       ENDIF
C
       ENDIF
C
10    CONTINUE
C
1000  FORMAT(' LE SEGMENT DE NO LOCAL ',I2,' EST UN SEGMENT SORTIE !! ')
1010  FORMAT(' LE SEGMENT DE NO LOCAL ',I2,' EST UN SEGMENT ENTREE !! ')
1001  FORMAT(' VALEUR DE < U , N > : ',E12.5)
C
      RETURN
      END
C
C#######################################################################
C