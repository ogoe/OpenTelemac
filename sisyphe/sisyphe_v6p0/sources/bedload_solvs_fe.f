C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES:
!>  @code
!>     D(HZ)
!>     ---- + DIV(T) = 0
!>      DT
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DIMGLO, DT, EBOR, ENTET, FLODEL, FLULIM, GLOSEG, HZ, HZN, IELMT, KDIR, KENT, LIMTEC, MASK, MASKEL, MESH, MSK, NPOIN, NPTFR, NSEG, QSX, QSY, S, T1, T4, T8, UNSV2D, ZFCL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> K
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_SOLVS_FE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), IELBOR(), NBPTS(), OS(), POSITIVE_DEPTHS(), VECTOR(), VECTOS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_EVOL()

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
!> </td><td> 05/09/2009
!> </td><td> J.-M. HERVOUET
!> </td><td> NEW METHOD
!> </td></tr>
!>      <tr>
!>      <td><center> 5.8                                       </center>
!> </td><td> 29/10/2007
!> </td><td> J.-M. HERVOUET
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.5                                       </center>
!> </td><td> 14/09/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.3                                       </center>
!> </td><td> **/**/2002
!> </td><td> B. MINH DUC
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 11/09/1995
!> </td><td> E. PELTIER;C. LENORMANT; J.-M. HERVOUET
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DIMGLO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>EBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ENTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLODEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLULIM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HZ
!></td><td><--</td><td>NEW AVAILABLE LAYER OF SEDIMENT
!>    </td></tr>
!>          <tr><td>HZN
!></td><td>--></td><td>OLD AVAILABLE LAYER OF SEDIMENT
!>    </td></tr>
!>          <tr><td>IELMT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMTEC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T8
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZFCL
!></td><td><--</td><td>ZFCL=HZ-HZN
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_SOLVS_FE !
     &(MESH,S,EBOR,MASKEL,MASK,
     & QSX,QSY,IELMT,NPOIN,NPTFR,KENT,KDIR,LIMTEC,DT,
     & MSK, ENTET, T1, T4, T8,
     & ZFCL,HZ,HZN,GLOSEG,DIMGLO,FLODEL,FLULIM,NSEG,UNSV2D)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DIMGLO         |---| 
C| DT             |---| 
C| EBOR           |---| 
C| ENTET          |---| 
C| FLODEL         |---| 
C| FLULIM         |---| 
C| GLOSEG         |---| 
C| HZ             |<--| NEW AVAILABLE LAYER OF SEDIMENT
C| HZN            |-->| OLD AVAILABLE LAYER OF SEDIMENT
C| IELMT          |---| 
C| KDIR           |---| 
C| KENT           |---| 
C| LIMTEC         |---| 
C| MASK           |---| 
C| MASKEL         |---| 
C| MESH           |---| 
C| MSK            |---| 
C| NPOIN          |---| 
C| NPTFR          |---| 
C| NSEG           |---| 
C| QSX            |---| 
C| QSY            |---| 
C| S             |---| 
C| T1             |---| 
C| T4             |---| 
C| T8             |---| 
C| UNSV2D         |---| 
C| ZFCL           |<--| ZFCL=HZ-HZN
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_SISYPHE, EX_BEDLOAD_SOLVS_FE => BEDLOAD_SOLVS_FE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: S,LIMTEC,MASKEL,MASK,QSX,QSY
      INTEGER,          INTENT(IN)    :: IELMT,NPOIN,NPTFR,KENT,KDIR
      INTEGER,          INTENT(IN)    :: DIMGLO,NSEG
      INTEGER,          INTENT(IN)    :: GLOSEG(DIMGLO,2)
      DOUBLE PRECISION, INTENT(IN)    :: DT
      DOUBLE PRECISION, INTENT(INOUT) :: FLULIM(NSEG)
      LOGICAL,          INTENT(IN)    :: MSK,ENTET
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: FLODEL,T1,T4,T8
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HZ,EBOR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ZFCL
      TYPE(BIEF_OBJ),   INTENT(IN)    :: HZN,UNSV2D
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K
C
C-----------------------------------------------------------------------
C
C     BOUNDARY FLUXES
C
      CALL CPSTVC(QSX,T4)
      CALL OS('X=C     ',X=T4,C=1.D0)
      CALL VECTOR(T8, '=', 'FLUBDF          ',IELBOR(IELMT,1),1.D0,
     &            T4,S,S,QSX,QSY,S,MESH,.TRUE.,MASK)
C
C     HERE THE VARIABLE WILL BE THE LAYER DEPTH OF THE SEDIMENT CLASS
C     NOT THE EVOLUTION
C
      DO K=1,NPTFR
        IF(LIMTEC%I(K).EQ.KDIR) THEN
          EBOR%R(K)=EBOR%R(K)+HZN%R(MESH%NBOR%I(K))
        ENDIF
      ENDDO
C
C     CALL VECTOR(T1,'=','VGRADP          ',QSX%ELM,-1.D0,
C    *            S,S,S,QSX,QSY,S,MESH,MSK,MASKEL)
C                 T1 AS HUGRADP IS NOT USED AS AN ASSEMBLED VECTOR
C                 BUT TO GET THE NON ASSEMBLED FORM MESH%W
C     JUST LIKE CALL VECTOR BUT WITHOUT ASSEMBLING T1 BECAUSE LEGO IS SET
C     TO FALSE (ONLY NON ASSEMBLED MESH%W%R IS USED AFTER)
      CALL VECTOS(T1%R,'=','VGRADP          ',-1.D0,
     &            S%R,S%R,S%R,QSX%R,QSY%R,S%R,
     &            S,S,S,QSX,QSY,S,
C                           LEGO
     &            MESH%W%R,.FALSE.,
     &            MESH%XEL%R  , MESH%YEL%R  , MESH%ZEL%R  ,
     &            MESH%SURFAC%R,MESH%IKLE%I,MESH%NBOR%I,
     &            MESH%XSGBOR%R, MESH%YSGBOR%R, MESH%ZSGBOR%R,
     &            NBPTS(QSX%ELM),MESH%NELEM,MESH%NELMAX,
     &            QSX%ELM,MESH%LV,MSK,MASKEL%R,MESH)
C
      CALL POSITIVE_DEPTHS(T1,T4,HZ,HZN,MESH,
     &                     FLODEL,.TRUE.,T8,DT,UNSV2D,NPOIN,
     &                     GLOSEG(1:DIMGLO,1),GLOSEG(1:DIMGLO,2),
     &                     MESH%NBOR%I,NPTFR,.FALSE.,T8,.FALSE.,
     &                     1,FLULIM,
     &                     LIMTEC%I,EBOR%R,KDIR,ENTET,MESH%W%R)
C
      CALL OS('X=Y-Z   ' ,X=ZFCL,Y=HZ,Z=HZN)
C
C     DIRICHLET CONDITIONS
C
      DO K=1,NPTFR
        IF(LIMTEC%I(K).EQ.KDIR) THEN
          EBOR%R(K)=EBOR%R(K)-HZN%R(MESH%NBOR%I(K))
          ZFCL%R(MESH%NBOR%I(K)) = EBOR%R(K)
        ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C