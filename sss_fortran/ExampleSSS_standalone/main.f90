!  ExampleSSS.f90 
!
!  FUNCTIONS:
!  ExampleSSS - Instructive application of sequential steady state class
!

!****************************************************************************
!
!  PROGRAM: ExampleSSS
!
!  PURPOSE:  Illustrates the use of the sequential steady state class simulating the unsaturated zone
!
!****************************************************************************

program ExampleSSS_standalone
use netcdf
use globals
use sss
use dumpres
use database_io
implicit none
type (t_SequentialSteadyStateSingle) :: svat(2)
type (t_sssparam) :: param
type(t_databaseSet), target :: dbset
character(len=:), allocatable :: dbpath
real(kind=hp), allocatable :: theta(:), h(:) 
real(kind=hp) :: pond, qrun, qbot, qrot, gwl, reva, sc1
real(kind=hp), target :: dtgw
logical :: suc6
integer :: itime, iter
integer, parameter :: ntime = 200
integer, parameter :: niter = 200
integer, parameter :: nbox = 18
real(kind=hp) :: qrain(ntime), qevap(ntime)

! results to netcdf file for check
type (t_dumpnc) :: dump
type (t_results) :: results

integer :: ierr, ncid
integer :: dimid_ib, dimid_time

qrain(1:60) = 0.45
qrain(61:80) = 0.26
qrain(81:100) =0.32
qrain(101:ntime) =0.d0
qevap(:) = 1.0d0
qrot = 0.d0
dtgw = 1.d0

! Read set of databases. Each database corresponds with a soil physical unit and a rootzone depth
! dbpath contains all the netcdf files, dbset is the instance holding the resulting set
dbpath = "D:/leander/MetaSWAP/test_compare_megaswap/van_hendrik_new/metaswap_a/unsa_nc"
suc6 = dbset%readNCset(dbpath)

! Fill convenient struct with parameters for this sequential steady state instance
param%area         = 100._hp     ! area
param%top          =   0._hp     ! elevation
param%spu          = 300._hp     ! soil physical unit number
param%init_phead   =  -1.5136_hp ! initial phead
param%init_gwl     =  -3._hp     ! initial gwl
param%dprz         =   1._hp     ! rootzone depth, thickness
param%zmax_ponding =   0.02_hp   ! ponding reservoir depth
param%maxinf       =   0.0032_hp ! infiltration rate limit

svat(1)%dtgw => dtgw
suc6 = svat(1)%initialize(param,dbset)   ! init svat sequential steady state instance
allocate(h(nbox))                     ! vertical profile of pressure heads
allocate(theta(nbox))                 ! vertical profile of moisture content

! open some files for output
!call results%init(nbox)
!call dump%init('results.nc') 

! set timestept
dtgw = 1._hp                          ! timestep size
do itime = 1, ntime                   ! time-loop
   call svat(1)%prepare(dtgw,qrain(itime),qevap(itime),qrot)

   call svat(1)%calc(results%gwl, &
                  results%h,      &
                  results%qbot,   &
                  results%sc1)   ! set initial groundwater level as the current level, no modflow interaction

   call svat(1)%finalize(results%gwl,   &
                      results%pond,  &
                      results%qrun,  &
                      results%qmodf, &
                      results%thbox, &
                      results%h, &
                      results%reva)
   write(0,'(a,i4.4,a)') 'Timestep #', itime, ' completed  ... '
enddo ! itime
end program ExampleSSS_standalone
