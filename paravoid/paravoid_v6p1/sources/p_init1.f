!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!>  @brief       INITIALISES PARAMETERS FOR TOMAWAC.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IPID, IREP, ITID, NCSIZE, NPTIR, REP
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU<hr>
!> TOMAPA : PREPA, NPID1, NDEMM
!>   </td></tr>
!>     </table>
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
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
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 11/12/1997                                              </td>
!>    <td> F MARCOS (LNH)                                          </td>
!>    <td> MODIFIED                                                </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 17/12/1996                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> MODIFIED                                                </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 08/06/1996                                              </td>
!>    <td> REINHARD HINKELMANN (HANOVER)                           </td>
!>    <td> MODIFIED                                                </td>
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> **/06/1996                                              </td>
!>    <td> HANS HERRMANN (HANOVER)                                 </td>
!>    <td>                                                         </td>
!>  </table>
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IPID
!></td><td>---</td><td>PROCESSOR NUMBER
!>    </td></tr>
!>          <tr><td>IREP
!></td><td>--></td><td>NOMBRE MAX DE CARACTERE DE LA CHAINE REP
!>    </td></tr>
!>          <tr><td>ITID
!></td><td>---</td><td>PROCESS NUMBER FIELD (NOT NEEDED ON NCUBE)
!>    </td></tr>
!>          <tr><td>NCSIZE
!></td><td>---</td><td>NUMBER OF PROCESSORS
!>    </td></tr>
!>          <tr><td>NPTIR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>REP
!></td><td>--></td><td>NOM DU REPERTOIRE DE TRAVAIL
!>    </td></tr>
!>     </table>
!
!#######################################################################
!
                        SUBROUTINE P_INIT1
     &(REP,IREP,IPID,NCSIZE,ITID,NPTIR)
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IPID           |---| PROCESSOR NUMBER
!| IREP           |-->| NOMBRE MAX DE CARACTERE DE LA CHAINE REP
!| ITID           |---| PROCESS NUMBER FIELD (NOT NEEDED ON NCUBE)
!| NCSIZE         |---| NUMBER OF PROCESSORS
!| NPTIR          |---|
!| REP            |-->| NOM DU REPERTOIRE DE TRAVAIL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER NPTIR,IPID,NCSIZE,ITID(0:255),NPID1,NDEMM
      LOGICAL PREPA
      COMMON/TOMAPA/PREPA,NPID1,NDEMM
!
      CHARACTER*144 REP
      INTEGER IREP
!
      REP = ' '
      IREP = 0
      NCSIZE = 1
      NPTIR=0
      IPID=0
      ITID(0)=0
!
!     IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_INIT1 VERSION VIDE'
!     IF(LNG.EQ.2) WRITE(LU,*) 'CALLING P_INIT1 IN ITS VOID VERSION'
!
!-----------------------------------------------------------------------
!
       RETURN
       END
!
!#######################################################################
!